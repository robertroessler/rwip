/*
	RWip.cpp - Windows Inactivity Proxy (a small but useful Windows app)

	Copyright(c) 2016-2019, Robert Roessler
	All rights reserved.

	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions are met:

	1. Redistributions of source code must retain the above copyright notice,
	this list of conditions and the following disclaimer.

	2. Redistributions in binary form must reproduce the above copyright notice,
	this list of conditions and the following disclaimer in the documentation
	and/or other materials provided with the distribution.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
	AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
	IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
	ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
	LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
	CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
	SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
	INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
	CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
	ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
	POSSIBILITY OF SUCH DAMAGE.
*/

#include <windows.h>
#include <winsafer.h>
#include <shlobj.h>
#include <shlwapi.h>
#include <comdef.h>
#include <sddl.h>

#include <string>
#include <vector>
#include <cwchar>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <utility>
#include <set>
#include <unordered_map>

//	UN-comment the following line for tracing with ::OutputDebugString()
//#define TRACE_ENABLED

using std::wstring;
using std::endl;

/*
	Control ID defs... so we don't use [unscoped] numeric constants. :)
*/
enum class ControlID {
	Add = 8, Executable = 2, Remove = 9,
	Period = 4,
	Restricted = 5,
	Startup = 6,
	RunNow = 7,
	CountDown = 3
};

/*
	Template class to help with "lifetime management" of Windows HANDLEs - note
	that either "direct" use with HANDLEs is supported, OR we can "inherit" an
	already-opened handle by instantiating the template with a HANDLE&.

	Typically, just declare and initialize with something like

	Win32Handle<HANDLE> hWnd{ nullptr };

	... and then just use it when a HANDLE is needed - with these "extras":

	* ::CloseHandle() will be called for you when the enclosing scope is exited
	* after the close, the underlying/wrapped HANDLE is set to NULL
	* casting to bool explicitly or implicitly works, returning true if non-NULL
*/
template<typename T>
class Win32Handle {
	T h;

public:
	Win32Handle() = delete;				// (be clear we INSIST on initializing)
	Win32Handle(T h_) : h(h_) {}		// (with either a HANDLE or HANDLE&...)
	~Win32Handle() { close(); }

	operator HANDLE() const { return h; }
	explicit operator bool() const { return h != nullptr; }
	HANDLE* operator&() { return &h; }
	HANDLE& operator=(T h_) { return h = h_; }
	void close() { if (*this) ::CloseHandle(h), h = nullptr; }
};

/*
	Helper class for matching CoInitialize[Ex]/CoUninitialize calls in a scope.
*/
class COMInitialize {
public:
	COMInitialize(DWORD initModel = COINIT_MULTITHREADED) { static_cast<void>(::CoInitializeEx(nullptr, initModel)); }
	~COMInitialize() { ::CoUninitialize(); }
};

/*
	Yet anoher "lifetime management" helper template class, this time for items
	created by the default OLE allocator.
*/
template<typename T>
class COMTaskMemPtr {
	T p = nullptr;

public:
	~COMTaskMemPtr() { if (*this) ::CoTaskMemFree(p), p = nullptr; }

	operator T() const { return p; }
	explicit operator bool() const { return p != nullptr; }
	T* operator&() { return &p; }
};

static PROCESS_INFORMATION procInfo{ 0 };

static std::vector<HPOWERNOTIFY> regs;
static const std::pair<wchar_t*, GUID> powerMsgs[]{
	{ L"GUID_CONSOLE_DISPLAY_STATE", GUID_CONSOLE_DISPLAY_STATE },
	{ L"GUID_SESSION_DISPLAY_STATUS", GUID_SESSION_DISPLAY_STATUS },
	{ L"GUID_SESSION_USER_PRESENCE", GUID_SESSION_USER_PRESENCE }
};
constexpr std::pair<wchar_t*, WPARAM> powerMsgsOther[]{
	{ L"PBT_APMSUSPEND", PBT_APMSUSPEND },
	{ L"PBT_APMRESUMESUSPEND", PBT_APMRESUMESUSPEND },
	{ L"PBT_APMPOWERSTATUSCHANGE", PBT_APMPOWERSTATUSCHANGE },
	{ L"PBT_APMRESUMEAUTOMATIC", PBT_APMRESUMEAUTOMATIC }
};

constexpr std::pair<wchar_t*, unsigned long long> intervals[]{
	{ L"1 minute", 1 * 60 * 1000ULL },
	{ L"2 minutes", 2 * 60 * 1000ULL },
	{ L"3 minutes", 3 * 60 * 1000ULL },
	{ L"5 minutes", 5 * 60 * 1000ULL },
	{ L"10 minutes", 10 * 60 * 1000ULL },
	{ L"15 minutes", 15 * 60 * 1000ULL },
	{ L"20 minutes", 20 * 60 * 1000ULL },
	{ L"30 minutes", 30 * 60 * 1000ULL },
	{ L"45 minutes", 45 * 60 * 1000ULL },
	{ L"1 hour", 60 * 60 * 1000ULL }
};

/*
	Config "database" support, including defaults and *primitive* accessor fns.
*/
static std::unordered_map<wstring, wstring> configDB{
	{ L"cmd", LR"(c:\Windows\System32\Bubbles.scr /s)" },
	{ L"lib", LR"("c:\\Windows\\System32\\Bubbles.scr /s")" }, // (will be used with std::quoted)
	{ L"del", L"4" },
	{ L"run", L"1" }
};

template<typename T>
constexpr T getProp(const wstring& name) {
	const auto umi = configDB.find(name);
	return umi != configDB.end() ? umi->second : T();
}
template<>
inline long getProp(const wstring& name) {
	const auto umi = configDB.find(name);
	return umi != configDB.end() ? wcstol(umi->second.c_str(), nullptr, 10) : 0;
}
template<>
inline bool getProp(const wstring& name) { return getProp<long>(name) != 0; }

static VOID CALLBACK timerCallback(PVOID w, BOOLEAN timerOrWait);

static Win32Handle<HANDLE> timer{ nullptr };
static auto last = 0ULL;

static HWND desktopH = ::GetDesktopWindow();
static RECT desktopR{ 0 };
static HWND shellH = ::GetShellWindow();

static HWND executableH = nullptr;
static HFONT countdownF = nullptr;
static HWND countdownH = nullptr;
static HWND restrictedH = nullptr;
static HWND startWithWindowsH = nullptr;
static HWND periodH = nullptr;
static HWND runNowH = nullptr;
static WNDPROC oldButtonProc = nullptr;
static WNDPROC oldListBoxProc = nullptr;
static auto periodId = 0;
static COMTaskMemPtr<wchar_t*> start_path;

static auto monitorOn = true;
static auto userPresent = true;
static auto quitting = false;
static auto forceRun = false;

//	SCALED (x100) and ROUNDED % => pixels
constexpr auto pc2px(long long w, int pc) { return (int)(w * pc / 10000e0 + ((w > 0) ? 0.5e0 : -0.5e0)); }
//	SCALED (x100) and ROUNDED pixels => %
constexpr auto px2pc(int w, int px) { return (int)(px * 10000e0 / w + 0.5e0); }

//	RECT helpers
constexpr auto widthOf(const RECT& r) { return r.right - r.left; }
constexpr auto heightOf(const RECT& r) { return r.bottom - r.top; }
constexpr auto sameSize(const RECT& r1, const RECT& r2) {
	return widthOf(r1) == widthOf(r2) && heightOf(r1) == heightOf(r2);
}

/*
	"Old school" version of optional tracing... now implemented with the CPP
	(aka the C/C++ Preprocessor), rather than cool new stuff like template meta-
	programming and constexpr if, as I wanted there to be NO evaluation of exprs
	at the trace(...) call sites in the "NO trace" mode of operation.
*/
#ifdef TRACE_ENABLED
static const wstring traceTag{ L"RWipTRACE> " };
#define trace(args) ::OutputDebugString((traceTag + args).c_str())
#else
#define trace(args) void(0)
#endif

/*
	Format for display the POWERBROADCAST_SETTING param from a WM_POWERBROADCAST
	message... note that the "user presence" values use 0 -> PRESENT, 2 -> NOT!
*/
static wstring powerChange2string(const POWERBROADCAST_SETTING* pbs)
{
	auto f = [](GUID g, DWORD d) {
		constexpr const wchar_t* displayState[]{ L"off", L"on", L"dimmed" };
		constexpr const wchar_t* userPresence[]{ L"present", L"", L"inactive" };
		if (g == GUID_CONSOLE_DISPLAY_STATE || g == GUID_SESSION_DISPLAY_STATUS)
			return displayState[d];
		else if (g == GUID_SESSION_USER_PRESENCE)
			return userPresence[d];
		return L""; // ("shouldn't happen")
	};
	// (N.B. - the Data member will be a DWORD whenever ...->Data is evaluated!)
	for (const auto& [label, guid] : powerMsgs)
		if (pbs->PowerSetting == guid)
			return wstring(label) + L'=' + f(guid, *(DWORD*)pbs->Data);
	return L"<other power-management GUID>";
}

/*
	Format for display the older, non-POWERBROADCAST_SETTING param variant of a
	WM_POWERBROADCAST message.
*/
constexpr const wchar_t* powerMsgOther2string(WPARAM wp)
{
	for (const auto& [label, msg] : powerMsgsOther)
		if (wp == msg)
			return label;
	return L"<other power-management event>";
}

/*
	Create a new [callback] timer, passing the supplied window to the callback.
*/
inline auto createTimer(HWND w)
{
	return ::CreateTimerQueueTimer(&timer, nullptr, timerCallback, w, 1000, 1000, WT_EXECUTELONGFUNCTION);
}

/*
	Safely delete an existing [callback] timer.
*/
inline void deleteTimer()
{
	if (timer)
		static_cast<void>(::DeleteTimerQueueTimer(nullptr, timer, nullptr)), timer = nullptr;
}

/*
	Format the time remaining on the inactivity timer as MINUTES AND SECONDS...
	note that this ONLY supports times up to 1 hour (minus 1 millisecond).
*/
static auto formatTimeRemaining(wchar_t* buf, size_t n, decltype(last) dT)
{
	const int seconds = int(dT / 1000);
	const int minutes = int(seconds / 60);
	const int displaySeconds = seconds % 60;
	return swprintf(buf, n, L"%02d:%02d", minutes, displaySeconds);
}

/*
	Create a process to execute the supplied command (and parameters), returning
	only when the process completes OR in case of error on the "create"... the
	credentials and privileges of the RWip user will be used.
*/
static auto runProcessAndWait(wchar_t* cmd, DWORD& exit)
{
	trace(wstring(L"RUN INACTIVITY task => ") + cmd);
	STARTUPINFO startInfo{ sizeof(STARTUPINFO), 0 };
	if (!::CreateProcess(nullptr, cmd, nullptr, nullptr, FALSE, CREATE_NO_WINDOW, nullptr, nullptr, &startInfo, &procInfo))
		return trace(L"*** FAILED CreateProcess"), false;
	Win32Handle<HANDLE&> pH(procInfo.hProcess), tH(procInfo.hThread);
	::WaitForSingleObject(procInfo.hProcess, INFINITE);
	::GetExitCodeProcess(procInfo.hProcess, &exit);
	return true;
}

/*
	Create a process to execute the supplied command (and parameters), returning
	only when the process completes OR in case of error on the "create"... a
	"restricted" version of the RWip user's credentials/integrity will be used.
*/
static auto runRestrictedProcessAndWait(wchar_t* cmd, DWORD& exit)
{
	trace(wstring(L"RUN *restricted* INACTIVITY task => ") + cmd);
	SAFER_LEVEL_HANDLE safer = nullptr;
	// set "Safer" level to... "SAFER_LEVELID_CONSTRAINED"
	// N.B. - maybe SAFER_LEVELID_NORMALUSER if this is too "tight"?
	if (!::SaferCreateLevel(SAFER_SCOPEID_USER, SAFER_LEVELID_CONSTRAINED, SAFER_LEVEL_OPEN, &safer, nullptr))
		return false;
	Win32Handle<HANDLE> restricted{ nullptr };
	if (!::SaferComputeTokenFromLevel(safer, nullptr, &restricted, SAFER_TOKEN_NULL_IF_EQUAL, nullptr) || !restricted)
		return trace(L"*** FAILED SaferComputeTokenFromLevel"), ::SaferCloseLevel(safer), false;
	::SaferCloseLevel(safer);
	// set [new] process integrity... to "Low Mandatory Level"
	// N.B. - maybe "Medium Mandatory Level" ("S-1-16-8192") if needed?
	TOKEN_MANDATORY_LABEL tml{ 0 };
	tml.Label.Attributes = SE_GROUP_INTEGRITY;
	if (!::ConvertStringSidToSid(L"S-1-16-4096", &tml.Label.Sid))
		return false;
	if (!::SetTokenInformation(restricted, TokenIntegrityLevel, &tml, sizeof tml + ::GetLengthSid(tml.Label.Sid)))
		return trace(L"*** FAILED SetTokenInformation"), ::LocalFree(tml.Label.Sid), false;
	::LocalFree(tml.Label.Sid);
	STARTUPINFO startInfo{ sizeof(STARTUPINFO), 0 };
	if (!::CreateProcessAsUser(restricted, nullptr, cmd, nullptr, nullptr, FALSE, CREATE_NO_WINDOW, nullptr, nullptr, &startInfo, &procInfo))
		return trace(L"*** FAILED CreateProcessAsUser"), false;
	Win32Handle<HANDLE&> pH(procInfo.hProcess), tH(procInfo.hThread);
	::WaitForSingleObject(procInfo.hProcess, INFINITE);
	::GetExitCodeProcess(procInfo.hProcess, &exit);
	return true;
}

/*
	Detect current foreground app running as "fullscreen" (games, videos, etc).
*/
static auto runningFullscreenApp()
{
	const auto fW = ::GetForegroundWindow();
	if (fW == NULL || fW == desktopH || fW == shellH)
		return false;
	RECT r;
	if (!::GetWindowRect(fW, &r))
		return false;
	return sameSize(r, desktopR);
}

/*
	Check once a second to see if

	a) the requested timeout has elapsed, in which case we will create a process
	and run the user-specified executable (often the "Bubbles.scr" screen saver)

	b) the user has interacted with the keyboard and/or mouse, which results in
	a "re-setting" of the timer - then update the displayed "countdown" value
*/
static VOID CALLBACK timerCallback(PVOID w, BOOLEAN timerOrWait)
{
	if (quitting)
		return;
	const auto ticks = ::GetTickCount64();
	auto dT = ticks - last;
	const auto& [_, period] = intervals[periodId];
	if (dT >= period || forceRun) {
		forceRun = false;
		if (runningFullscreenApp()) {
			last = ticks; // fullscreen mode is NOT treated as "inactivity"
			return;
		}
		trace(L"DELETING inactivity timer, STARTING INACTIVITY task...");
		deleteTimer();
		wchar_t cmd[MAX_PATH + 16];
		const auto n = ::SendMessage(executableH, WM_GETTEXT, MAX_PATH + 16, (LPARAM)cmd);
		const auto checked = ::SendMessage(restrictedH, BM_GETCHECK, 0, 0) == BST_CHECKED;
		DWORD exit = 0;
		if (n < MAX_PATH + 16 && (checked ? runRestrictedProcessAndWait : runProcessAndWait)(cmd, exit)) {
			trace(wstring(L"INACTIVITY task finished, exit code=") + std::to_wstring(exit));
			if (userPresent && monitorOn && !timer)
				trace(L"... RESTARTING inactivity timer!"),
				last = ::GetTickCount64(),
				createTimer((HWND)w);
			else
				trace(L"... NOT RESTARTING inactivity timer!");
		} else
			trace(wstring(L"*** FAILURE executing ") + cmd);
	} else {
		LASTINPUTINFO history{ sizeof(LASTINPUTINFO) };
		if (::GetLastInputInfo(&history) && history.dwTime > last)
			last = decltype(last)(history.dwTime), dT = 1; // max time to display will be 59:59
		WINDOWPLACEMENT wP{ sizeof(WINDOWINFO) };
		::GetWindowPlacement((HWND)w, &wP);
		if (wP.showCmd != SW_SHOWMINIMIZED) {
			wchar_t buf[16];
			formatTimeRemaining(buf, std::size(buf), period - dT);
			::SetWindowText(countdownH, buf);
		}
	}
}

/*
	Collect cmds from the "lib" property and return as [ordered] set.
*/
static auto collectCmdsFromProperties()
{
	std::set<wstring> elements;
	std::wstringstream ss(getProp<wstring>(L"lib"));
	wstring c;
	while (ss >> std::quoted(c))
		elements.emplace(c);
	return elements;
}

/*
	Create and initialize all of the supporting windows and controls, using both
	"percent of" support and dynamic sizing in doing the detailed layout... this
	will provide SOME amount of resiliency with respect to different window and
	font sizes, BUT "your mileage may vary".
*/
static void createChildren(HWND w, CREATESTRUCT* cs)
{
	RECT r;
	::GetClientRect(w, &r);
	// create "computed constants" to assist in control positioning
	const auto Rw = widthOf(r), Rh = heightOf(r);	// app dimensions
	const auto Bw = pc2px(Rw, 250), Bh = Bw;		// window borders
	constexpr auto Ch = 32;							// control height

	executableH = ::CreateWindowEx(WS_EX_CLIENTEDGE, L"COMBOBOX",
		nullptr,
		WS_CHILD | WS_VISIBLE | WS_BORDER | CBS_DROPDOWN | CBS_HASSTRINGS | CBS_NOINTEGRALHEIGHT | CBS_DISABLENOSCROLL | CBS_SORT,
		Bw + Ch + 4, Bh, Rw - Bw * 2 - Ch * 2 - 4 * 2, Rh - Bh * 3 - Ch,
		w, (HMENU)ControlID::Executable, cs->hInstance, nullptr);
	for (const auto& c : collectCmdsFromProperties())
		::SendMessage(executableH, CB_ADDSTRING, 0, (LPARAM)c.c_str());
	::SetWindowText(executableH, getProp<wstring>(L"cmd").c_str());

	::CreateWindow(L"BUTTON",
		L"+",
		WS_CHILD | WS_VISIBLE | WS_BORDER | BS_DEFPUSHBUTTON,
		Bw, Bh, Ch, Ch,
		w, (HMENU)ControlID::Add, cs->hInstance, nullptr);

	::CreateWindow(L"BUTTON",
		L"--",
		WS_CHILD | WS_VISIBLE | WS_BORDER | BS_DEFPUSHBUTTON,
		Rw - Bw - Ch, Bh, Ch, Ch,
		w, (HMENU)ControlID::Remove, cs->hInstance, nullptr);

	periodH = ::CreateWindowEx(WS_EX_CLIENTEDGE, L"COMBOBOX",
		L"5",
		WS_CHILD | WS_VISIBLE | WS_BORDER | CBS_DROPDOWNLIST | CBS_HASSTRINGS | CBS_NOINTEGRALHEIGHT | CBS_DISABLENOSCROLL,
		Bw, Ch + Bh * 2, pc2px(Rw, 4000), Rh - Bh * 3 - Ch,
		w, (HMENU)ControlID::Period, cs->hInstance, nullptr);
	for (const auto& [label, _] : intervals)
		::SendMessage(periodH, CB_ADDSTRING, 0, (LPARAM)label);
	::SendMessage(periodH, CB_SETCURSEL, periodId = getProp<long>(L"del"), 0);

	if (COMBOBOXINFO cbI{ sizeof(COMBOBOXINFO), 0 }; ::SendMessage(periodH, CB_GETCOMBOBOXINFO, 0, (LPARAM)&cbI))
		oldListBoxProc = (WNDPROC)::SetWindowLongPtr(cbI.hwndList, GWLP_WNDPROC,
			/*
				Special-purpose "mini-wndProc" - used solely for interpreting
				mouse "wheel" messages to a sub-classed LISTBOX control in a
				COMBOBOX... which are typically just discarded.
			*/
			(LONG_PTR)WNDPROC([](HWND w, UINT mId, WPARAM wp, LPARAM lp)->LRESULT {
				if (mId == WM_MOUSEWHEEL) {
					const auto dY = int(wp) >> 16;
					auto id = periodId + (dY < 0 ? 1 : dY > 0 ? -1 : 0);
					if (id < 0)
						id = 0;
					else if (id >= decltype(id)(size(intervals)))
						id = decltype(id)(size(intervals)) - 1;
					if (id != periodId)
						::SendMessage(periodH, CB_SETCURSEL, periodId = id, 0);
					return 0;
				}
				// not for us, leave with "default" processing
				return ::CallWindowProc(oldListBoxProc, w, mId, wp, lp);
			}));

	restrictedH = ::CreateWindow(L"BUTTON",
		L"RunAs restricted and low-integrity process",
		WS_CHILD | WS_VISIBLE | BS_AUTOCHECKBOX | BS_MULTILINE,
		Bw, Bh * 2 + Ch * 2, pc2px(Rw, 4250), Ch * 2 - Bh / 2,
		w, (HMENU)ControlID::Restricted, cs->hInstance, nullptr);
	::SendMessage(restrictedH, BM_SETCHECK, getProp<bool>(L"run") ? BST_CHECKED : BST_UNCHECKED, 0);

	startWithWindowsH = ::CreateWindow(L"BUTTON",
		L"Start with Windows",
		WS_CHILD | WS_VISIBLE | BS_AUTOCHECKBOX,
		Bw, Bh * 4 + Ch * 3, pc2px(Rw, 4250), Ch - Bh / 2,
		w, (HMENU)ControlID::Startup, cs->hInstance, nullptr);
	IPersistFilePtr ppf;
	bool shortcutPresent = false;
	if (IShellLinkWPtr psl(CLSID_ShellLink); psl && SUCCEEDED(psl.QueryInterface(IID_IPersistFile, &ppf)))
		shortcutPresent = SUCCEEDED(ppf->Load((wstring(start_path) + L"/RWip.lnk").c_str(), 0));
	::SendMessage(startWithWindowsH, BM_SETCHECK, shortcutPresent ? BST_CHECKED : BST_UNCHECKED, 0);

	runNowH = ::CreateWindow(L"BUTTON",
		L"Begin Inactivity Proxy Task NOW",
		WS_CHILD | WS_VISIBLE | WS_BORDER | BS_DEFPUSHBUTTON,
		Bw, Rh - Bh - Ch, Rw - Bw * 2, Ch,
		w, (HMENU)ControlID::RunNow, cs->hInstance, nullptr);
	oldButtonProc = (WNDPROC)::SetWindowLongPtr(runNowH, GWLP_WNDPROC,
		/*
			Special-purpose "mini-wndProc" - emulate dialog box Enter key.
		*/
		(LONG_PTR)WNDPROC([](HWND w, UINT mId, WPARAM wp, LPARAM lp)->LRESULT {
			if (mId == WM_CHAR && wp == VK_RETURN) {
				::SendMessage(w, BM_CLICK, 0, 0);
				return 0;
			}
			// not for us, leave with "default" processing
			return ::CallWindowProc(oldButtonProc, w, mId, wp, lp);
		}));

	const auto cX = ::GetSystemMetrics(SM_CXEDGE);
	const auto cY = ::GetSystemMetrics(SM_CYEDGE);
	auto hDC = ::GetDC(w);
	const auto yPixels = ::GetDeviceCaps(hDC, LOGPIXELSY);
	const auto logicalHeight = -::MulDiv(56, yPixels, 72);
	LOGFONT f{ 0 };
	f.lfHeight = logicalHeight, f.lfWeight = FW_REGULAR, wcscpy(f.lfFaceName, L"Lucida Sans");
	countdownF = ::CreateFontIndirect(&f);
	const auto oldF = ::SelectObject(hDC, countdownF);
	RECT t{ 0 };
	::DrawText(hDC, L"59:59", 5, &t, DT_CALCRECT);
	::SelectObject(hDC, oldF);
	::ReleaseDC(w, hDC);

	countdownH = ::CreateWindowEx(WS_EX_CLIENTEDGE, L"STATIC",
		L"",
		WS_CHILD | WS_VISIBLE | WS_BORDER,
		Rw - Bw - widthOf(t) - cX * 2, Ch + Bh * 2, widthOf(t) + cX * 2, heightOf(t) + cY * 2,
		w, (HMENU)ControlID::CountDown, cs->hInstance, nullptr);
	::SendMessage(countdownH, WM_SETFONT, (WPARAM)countdownF, TRUE);
}

/*
	Reflect new/changed monitor power status AS REQUIRED.
*/
static auto updateMonitorStatus(const POWERBROADCAST_SETTING* pbs)
{
	if (const auto& g = pbs->PowerSetting; g == GUID_CONSOLE_DISPLAY_STATE || g == GUID_MONITOR_POWER_ON || g == GUID_SESSION_DISPLAY_STATUS) {
		const auto old = monitorOn;
		monitorOn = *(DWORD*)pbs->Data != 0;
		return monitorOn != old;
	}
	return false;
}

/*
	Reflect new/changed "user presence" status AS REQUIRED.
*/
static auto updateUserStatus(const POWERBROADCAST_SETTING* pbs)
{
	if (const auto& g = pbs->PowerSetting; g == GUID_SESSION_USER_PRESENCE) {
		const auto old = userPresent;
		userPresent = *(DWORD*)pbs->Data == 0;
		return userPresent != old;
	}
	return false;
}

/*
	The classic Windows "wndProc", or "Windows Procedure" - for handling all of
	the "messages" that will be sent to your application... in our case, these
	will either be helping us monitor the user's presence and/or the power state
	of the [main] display, OR supporting the [minimal] GUI interactions.

	Note that as this is a fairly simple Windows GUI, once the child windows and
	controls are created, we only really watch for:

	* "button clicks" from the Add/Remove (+/-) controls supporting maintenance
	  of the "library" of inactivity executables
	* notifications from the "drop list" control setting a new inactivity delay
	* "background painting" msgs from the "RunAs" checkbox control (implementing
	  our desired visual style)
	* "power broadcast / power setting change" msgs - which we interpret and use
	  to guide our monitoring of the monitor power state and whether the user
	  appears to be "active"
	* "button clicks" from the RunNow ("Begin Inactivity Task NOW") control
*/
static LRESULT CALLBACK wndProc(HWND w, UINT mId, WPARAM wp, LPARAM lp)
{
	switch (mId) {
	case WM_CLOSE:
		quitting = true, ::PostQuitMessage(0);
		return 0;
	case WM_CREATE:
		createChildren(w, (CREATESTRUCT*)lp);
		return 0;
	case WM_ACTIVATE:
		if (wp != WA_INACTIVE) {
			::SetFocus(runNowH);
			return 0;
		}
		break;
	case WM_COMMAND:
		switch (HIWORD(wp)) {
		case CBN_SELCHANGE:
			if (LOWORD(wp) == (WORD)ControlID::Period) {
				const auto i = ::SendMessage((HWND)lp, CB_GETCURSEL, 0, 0);
				if (i != CB_ERR) {
					periodId = (int)i;
					return 0;
				}
			}
			break;
		case BN_CLICKED:
			if (LOWORD(wp) == (WORD)ControlID::RunNow) {
				forceRun = true, ::ShowWindow(w, SW_MINIMIZE);
				return 0;
			} else if (LOWORD(wp) == (WORD)ControlID::Add || LOWORD(wp) == (WORD)ControlID::Remove) {
				wchar_t cmd[MAX_PATH + 16];
				::SendMessage(executableH, WM_GETTEXT, MAX_PATH + 16, (LPARAM)cmd);
				if (const auto i = ::SendMessage(executableH, CB_FINDSTRINGEXACT, -1, (LPARAM)cmd); LOWORD(wp) == (WORD)ControlID::Add) {
					if (i == CB_ERR) {
						// (... only if we DIDN'T find it, i.e., disallow DUPES)
						const auto j = ::SendMessage(executableH, CB_ADDSTRING, 0, (LPARAM)cmd);
						::SendMessage(executableH, CB_SETCURSEL, j, 0);
					}
				} else /*if (LOWORD(wp) == Remove)*/ {
					if (i != CB_ERR) {
						// (... only if we FOUND it, otherwise, a NO-OP)
						const auto n = ::SendMessage(executableH, CB_DELETESTRING, i, 0);
						::SendMessage(executableH, CB_SETCURSEL, i < n ? i : i - 1, 0);
					}
				}
				return 0;
			}
			break;
		}
		break;
	case WM_CTLCOLORSTATIC:
		if ((HWND)lp == restrictedH || (HWND)lp == startWithWindowsH) {
			::SetBkMode((HDC)wp, TRANSPARENT);
			::SetTextColor((HDC)wp, ::GetSysColor(COLOR_BTNTEXT));
			return (LRESULT)::GetStockObject(NULL_BRUSH);
		}
		break;
	case WM_POWERBROADCAST:
		if (wp == PBT_POWERSETTINGCHANGE) {
			const auto pbs = (const POWERBROADCAST_SETTING*)lp;
			trace(wstring(L"WM_POWERBROADCAST: ") + powerChange2string(pbs));
			if (const auto monitorChanged = updateMonitorStatus(pbs), userChanged = updateUserStatus(pbs); monitorChanged || userChanged)
				if (!timer && monitorChanged && !monitorOn && procInfo.hProcess != nullptr) {
					if (::TerminateProcess(procInfo.hProcess, 0))
						trace(L"WM_POWERBROADCAST, monitor off, TERMINATING inactivity task!");
				} else if (!timer && ((monitorChanged && monitorOn) || (userChanged && userPresent)))
					last = ::GetTickCount64(),
					createTimer(w),
					trace(L"WM_POWERBROADCAST, monitor/user BACK, RESTARTING inactivity timer!");
				else if (timer && !monitorOn && !userPresent)
					deleteTimer(),
					trace(L"WM_POWERBROADCAST, monitor/user GONE, DELETING inactivity timer!");
		} else
			trace(wstring(L"WM_POWERBROADCAST(other): ") + powerMsgOther2string(wp));
		break;
	}
	return ::DefWindowProc(w, mId, wp, lp); // (go with "default" processing)
}

/*
	Synthesize and return the FULL path to the RWip config... in the unlikely
	event that the USERPROFILE env var is NOT set, just use the current dir.
*/
static wstring configpath()
{
	wchar_t path[MAX_PATH];
	const int n = ::GetEnvironmentVariable(L"USERPROFILE", path, MAX_PATH);
	if (n == 0 || n == MAX_PATH)
		return L".rwipconfig";
	wcsncat(path, L"\\.rwipconfig", (size_t)MAX_PATH - n - 1);
	return path;
}

/*
	Load our config if possible, otherwise defaults will be used.

	N.B. - property names are exactly 3 characters... for now.
*/
static void loadConfig()
{
	std::wifstream ls(configpath());
	wstring line;
	while (std::getline(ls, line), ls.is_open() && !ls.eof())
		if (line.length() >= 4 && line[3] == L'=')
			configDB[line.substr(0, 3)] = line.substr(4);
}

/*
	Collect cmds from the dropdown list and return as [ordered] set.
*/
static auto collectCmdsFromUI()
{
	std::set<wstring> elements;
	const auto n = ::SendMessage(executableH, CB_GETCOUNT, 0, 0);
	for (auto i = 0; i < n; i++) {
		wchar_t cmd[MAX_PATH + 16];
		::SendMessage(executableH, CB_GETLBTEXT, i, (LPARAM)cmd);
		elements.emplace(cmd);
	}
	return elements;
}

/*
	Save our config from the current RWip operating state.

	N.B. - permanent state (i.e., files) [re-]written ONLY as needed
*/
static void saveConfig()
{
	wchar_t cmd[MAX_PATH + 16];
	const auto n = ::SendMessage(executableH, WM_GETTEXT, MAX_PATH + 16, (LPARAM)cmd);
	const auto uiCmds = collectCmdsFromUI();
	const auto propCmds = collectCmdsFromProperties();
	const auto restrict = ::SendMessage(restrictedH, BM_GETCHECK, 0, 0) == BST_CHECKED;
	if (getProp<wstring>(L"cmd") != cmd || propCmds != uiCmds ||
		getProp<long>(L"del") != periodId ||
		getProp<bool>(L"run") != restrict) {
		std::wofstream ss(configpath(), std::ios::out);
		ss << L"cmd=" << cmd << endl;
		ss << L"lib="; for (const auto& c : uiCmds) ss << std::quoted(c) << L' '; ss << endl;
		ss << L"del=" << periodId << endl;
		ss << L"run=" << restrict << endl;
	}
	const auto startup = ::SendMessage(startWithWindowsH, BM_GETCHECK, 0, 0) == BST_CHECKED;
	IPersistFilePtr ppf;
	if (IShellLinkWPtr psl(CLSID_ShellLink); psl && SUCCEEDED(psl.QueryInterface(IID_IPersistFile, &ppf))) {
		wstring lnkPath = wstring(start_path) + L"/RWip.lnk";
		if (const auto shortcutPresent = SUCCEEDED(ppf->Load(lnkPath.c_str(), 0)); startup) {
			if (!shortcutPresent) {
				// create new "start with Windows" shortcut
				wchar_t exePath[MAX_PATH];
				if (::GetModuleFileNameW(nullptr, exePath, MAX_PATH) != MAX_PATH) {
					psl->SetDescription(L"RWip 1.x");
					psl->SetPath(exePath);
					psl->SetShowCmd(SW_SHOWMINNOACTIVE);
					ppf->Save(lnkPath.c_str(), TRUE);
				}
			}
		} else
			if (shortcutPresent) {
				// remove existing "start with Windows" shortcut
				COMTaskMemPtr<wchar_t*> path;
				if (SUCCEEDED(ppf->GetCurFile(&path))) {
					::DeleteFileW(path);
					::SHChangeNotify(SHCNE_DELETE, SHCNF_PATH | SHCNF_FLUSHNOWAIT, path, nullptr);
				}
			}
	}
}

/*
	Create the tree of windows and controls, load an existing config, register
	for "power setting" notifications, start a 1-second repeating timer, do the
	Windows message-processing thing until time to clean up, then clean up, and
	save the [possibly updated] config.
*/
int WINAPI WinMain(HINSTANCE inst, HINSTANCE prev, LPSTR cmd, int show)
{
	WNDCLASS wC{ 0, wndProc, 0, 0, inst, nullptr, nullptr, HBRUSH(COLOR_BACKGROUND), nullptr, L"RWipClass" };
	COMInitialize init(COINIT_APARTMENTTHREADED);
	::SHGetKnownFolderPath(FOLDERID_Startup, 0, nullptr, &start_path);
	const ATOM wA = ::RegisterClass(&wC);
	if (!wA)
		return 1;
	::GetWindowRect(desktopH, &desktopR);
	loadConfig();
	const HWND wH = ::CreateWindow(LPCTSTR(wA),
		L"RWip 1.6 - Windows Inactivity Proxy",
		WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | WS_VISIBLE,
		0, 0, 560, 280, 0, 0, inst, nullptr);
	if (wH == nullptr)
		return 2;
	for (const auto& [_, guid] : powerMsgs)
		regs.push_back(::RegisterPowerSettingNotification(wH, &guid, 0));
	last = ::GetTickCount64();
	if (!createTimer(wH))
		return 3;
	trace(L"INITIAL START of inactivity timer and message loop!");
	MSG m{ 0 };
	while (::GetMessage(&m, nullptr, 0, 0) > 0)
		::TranslateMessage(&m), ::DispatchMessage(&m);
	deleteTimer();
	for (const auto& r : regs)
		::UnregisterPowerSettingNotification(r);
	::DeleteObject(countdownF);
	saveConfig();
	return 0;
}
