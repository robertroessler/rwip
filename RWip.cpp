/*
	RWip.cpp - Windows Inactivity Proxy (a small but useful Windows app)

	Copyright(c) 2016, Robert Roessler
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
#include <sddl.h>
#include <string>
#include <vector>
#include <algorithm>
#include <cwchar>
#include <fstream>
#include <utility>

//	UN-comment the following line for tracing with ::OutputDebugString()
#define TRACE_ENABLED

using namespace std;

/*
	Template class to help with "lifetime management" of Windows HANDLEs - note
	that either "direct" use with HANDLEs is supported, OR we can "inherit" an
	already-opened handle by instantiating the template with a HANDLE&.

	Typically, just declare and initialize with something like

	Win32Handle<HANDLE> hWnd{ NULL };

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
	explicit operator bool() const { return h != NULL; }
	HANDLE* operator&() { return &h; }
	HANDLE& operator=(T h_) { return h = h_; }
	void close() { if (*this) ::CloseHandle(h), h = NULL; }
};

static PROCESS_INFORMATION procInfo{ 0 };

static vector<HPOWERNOTIFY> regs;
static const GUID powerMsgs[] = {
	GUID_CONSOLE_DISPLAY_STATE,
	GUID_MONITOR_POWER_ON,
	GUID_SESSION_DISPLAY_STATUS,
	GUID_SESSION_USER_PRESENCE
};

static const pair<wchar_t*, unsigned long> intervals[]{
	{ L"1 minute", 1 * 60 * 1000 },
	{ L"2 minutes", 2 * 60 * 1000 },
	{ L"3 minutes", 3 * 60 * 1000 },
	{ L"5 minutes", 5 * 60 * 1000 },
	{ L"10 minutes", 10 * 60 * 1000 },
	{ L"15 minutes", 15 * 60 * 1000 },
	{ L"20 minutes", 20 * 60 * 1000 },
	{ L"30 minutes", 30 * 60 * 1000 },
	{ L"45 minutes", 45 * 60 * 1000 },
	{ L"1 hour", 60 * 60 * 1000 }
};
constexpr int periodIdMax = sizeof intervals / sizeof intervals[0];

static HWND executableH = NULL;
static HFONT countdownF = NULL;
static HWND countdownH = NULL;
static HWND restrictedH = NULL;
static HWND periodH = NULL;
static WNDPROC oldListBoxProc = NULL;
static int periodId = 4;

static Win32Handle<HANDLE> timer{ NULL };
static DWORD last = 0;
static bool monitorOn = true;
static bool userPresent = true;
static bool quitting = false;

//	SCALED (x100) and ROUNDED % => pixels
constexpr int pc2px(int w, int pc) { return (int)(w * pc / 10000e0 + ((w > 0) ? 0.5e0 : -0.5e0)); }
//	SCALED (x100) and ROUNDED pixels => %
constexpr int px2pc(int w, int px) { return (int)(px * 10000e0 / w + 0.5e0); }

//	RECT helpers
inline int widthOf(RECT& r) { return r.right - r.left; }
inline int heightOf(RECT& r) { return r.bottom - r.top; }

template <typename ...Params>
static void trace(Params&&... params) {
#ifdef TRACE_ENABLED
	::OutputDebugString(forward<Params>(params)...);
#endif
}

/*
	Format for display the POWERBROADCAST_SETTING param from a WM_POWERBROADCAST
	message... note that the "user presence" values use 0 -> PRESENT, 2 -> NOT!
*/
static wstring powerChange2string(const POWERBROADCAST_SETTING* pbs)
{
	auto f = [](auto pbs) { return to_wstring(*(DWORD*)&pbs->Data); };
	const GUID& g = pbs->PowerSetting;
	if (g == GUID_CONSOLE_DISPLAY_STATE)
		return wstring(L"GUID_CONSOLE_DISPLAY_STATE=") + f(pbs);
	if (g == GUID_MONITOR_POWER_ON)
		return wstring(L"GUID_MONITOR_POWER_ON=") + f(pbs);
	if (g == GUID_SESSION_DISPLAY_STATUS)
		return wstring(L"GUID_SESSION_DISPLAY_STATUS=") + f(pbs);
	if (g == GUID_SESSION_USER_PRESENCE)
		return wstring(L"GUID_SESSION_USER_PRESENCE=") + f(pbs);
	return L"<other power-management GUID>";
}

/*
	Format for display the older, non-POWERBROADCAST_SETTING param variant of a
	WM_POWERBROADCAST message.
*/
static const wchar_t* powerMsgOther2string(WPARAM wp)
{
	if (wp == PBT_APMSUSPEND)
		return L"PBT_APMSUSPEND";
	if (wp == PBT_APMRESUMESUSPEND)
		return L"PBT_APMRESUMESUSPEND";
	if (wp == PBT_APMPOWERSTATUSCHANGE)
		return L"PBT_APMPOWERSTATUSCHANGE";
	if (wp == PBT_APMRESUMEAUTOMATIC)
		return L"PBT_APMRESUMEAUTOMATIC";
	return L"<other power-management event>";
}

/*
	Format the time remaining on the inactivity timer as MINUTES AND SECONDS...
	note that this ONLY supports times up to 1 hour (minus 1 millisecond).
*/
static void formatTimeRemaining(wchar_t* buf, size_t n, DWORD dT)
{
	const int seconds = dT / 1000;
	const int minutes = seconds / 60;
	const int displaySeconds = seconds % 60;
	swprintf(buf, n, L"%02d:%02d", minutes, displaySeconds);
}

/*
	Reflect new/changed monitor power status AS REQUIRED.
*/
static bool updateMonitorStatus(const POWERBROADCAST_SETTING* pbs)
{
	const GUID& g = pbs->PowerSetting;
	if (g == GUID_CONSOLE_DISPLAY_STATE || g == GUID_MONITOR_POWER_ON || g == GUID_SESSION_DISPLAY_STATUS) {
		const auto old = monitorOn;
		monitorOn = *(DWORD*)&pbs->Data != 0;
		return monitorOn != old;
	}
	return false;
}

/*
	Reflect new/changed "user presence" status AS REQUIRED.
*/
static bool updateUserStatus(const POWERBROADCAST_SETTING* pbs)
{
	const GUID& g = pbs->PowerSetting;
	if (g == GUID_SESSION_USER_PRESENCE) {
		const auto old = userPresent;
		userPresent = *(DWORD*)&pbs->Data == 0;
		return userPresent != old;
	}
	return false;
}

/*
	Create a process to execute the supplied command (and parameters), returning
	only when the process completes OR in case of error on the "create"... the
	credentials and privileges of the RWip user will be used.
*/
static bool runProcessAndWait(wchar_t* cmd, DWORD& exit)
{
	trace((wstring(L"RUN INACTIVITY task => ") + cmd).c_str());
	STARTUPINFO startInfo{ sizeof(STARTUPINFO), 0 };
	if (!::CreateProcess(NULL, cmd, NULL, NULL, FALSE, CREATE_NO_WINDOW, NULL, NULL, &startInfo, &procInfo))
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
static bool runRestrictedProcessAndWait(wchar_t* cmd, DWORD& exit)
{
	trace((wstring(L"RUN *restricted* INACTIVITY task => ") + cmd).c_str());
	SAFER_LEVEL_HANDLE safer = NULL;
	// set "Safer" level to... "SAFER_LEVELID_CONSTRAINED"
	// N.B. - maybe SAFER_LEVELID_NORMALUSER if this is too "tight"?
	if (!::SaferCreateLevel(SAFER_SCOPEID_USER, SAFER_LEVELID_CONSTRAINED, SAFER_LEVEL_OPEN, &safer, NULL))
		return false;
	Win32Handle<HANDLE> restricted{ NULL };
	if (!::SaferComputeTokenFromLevel(safer, NULL, &restricted, SAFER_TOKEN_NULL_IF_EQUAL, NULL) || !restricted)
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
	if (!::CreateProcessAsUser(restricted, NULL, cmd, NULL, NULL, FALSE, CREATE_NO_WINDOW, NULL, NULL, &startInfo, &procInfo))
		return trace(L"*** FAILED CreateProcessAsUser"), false;
	Win32Handle<HANDLE&> pH(procInfo.hProcess), tH(procInfo.hThread);
	::WaitForSingleObject(procInfo.hProcess, INFINITE);
	::GetExitCodeProcess(procInfo.hProcess, &exit);
	return true;
}

/*
	Check once a second to see if

	a) the requested timeout has elapsed, in which case we will create a process
	and run the user-specified executable (often the "Bubbles.scr" screen saver)

	b) the user has interacted with the keyboard and/or mouse, which results in
	a "re-setting" of the timer - then update the displayed "countdown" value
*/
static VOID CALLBACK timerCallback(PVOID pvoid, BOOLEAN timerOrWait)
{
	if (quitting)
		return;
	auto dT = ::GetTickCount() - last;
	const auto period = intervals[periodId].second;
	if (dT >= period) {
		trace(L"DELETING inactivity timer, STARTING INACTIVITY task...");
		::DeleteTimerQueueTimer(NULL, timer, NULL), timer = NULL;
		wchar_t cmd[256];
		const auto n = ::SendMessage(executableH, WM_GETTEXT, 256, (LPARAM)cmd);
		const auto checked = ::SendMessage(restrictedH, BM_GETCHECK, 0, 0) == BST_CHECKED;
		DWORD exit = 0;
		if (n < 256 && (checked ? runRestrictedProcessAndWait : runProcessAndWait)(cmd, exit)) {
			trace((wstring(L"INACTIVITY task finished, exit code=") + to_wstring(exit)).c_str());
			if (userPresent && monitorOn && !timer)
				trace(L"... RESTARTING inactivity timer!"),
				last = ::GetTickCount(),
				::CreateTimerQueueTimer(&timer, NULL, timerCallback, 0, 1000, 1000, WT_EXECUTELONGFUNCTION);
			else
				trace(L"... NOT RESTARTING inactivity timer!");
		} else
			trace((wstring(L"*** FAILURE executing ") + cmd).c_str());
	} else {
		LASTINPUTINFO history{ sizeof(LASTINPUTINFO) };
		if (::GetLastInputInfo(&history) && history.dwTime > last)
			last = history.dwTime, dT = 1; // max time to display will be 59:59
		wchar_t buf[16];
		formatTimeRemaining(buf, 16, period - dT);
		::SetWindowText(countdownH, buf);
	}
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
	executableH = ::CreateWindowEx(WS_EX_CLIENTEDGE, L"EDIT",
		L"c:\\Windows\\System32\\Bubbles.scr /s",
		WS_CHILD | WS_VISIBLE | WS_BORDER | ES_LEFT,
		pc2px(widthOf(r), 250), pc2px(widthOf(r), 250), pc2px(widthOf(r), 9500), 32,
		w, (HMENU)2, cs->hInstance, NULL);

	periodH = ::CreateWindowEx(WS_EX_CLIENTEDGE, L"COMBOBOX",
		L"5",
		WS_CHILD | WS_VISIBLE | WS_BORDER | CBS_DROPDOWNLIST | CBS_HASSTRINGS | CBS_NOINTEGRALHEIGHT | CBS_DISABLENOSCROLL,
		pc2px(widthOf(r), 250), 32 + pc2px(widthOf(r), 500), pc2px(widthOf(r), 4000), heightOf(r) - pc2px(widthOf(r), 750) - 32,
		w, (HMENU)4, cs->hInstance, NULL);
	for (const auto& p : intervals)
		::SendMessage(periodH, CB_ADDSTRING, 0, (LPARAM)p.first);
	::SendMessage(periodH, CB_SETCURSEL, periodId = 4, 0);

	COMBOBOXINFO cbI{ sizeof(COMBOBOXINFO), 0 };
	if (::SendMessage(periodH, CB_GETCOMBOBOXINFO, 0, (LPARAM)&cbI))
		oldListBoxProc = (WNDPROC)::SetWindowLongPtr(cbI.hwndList, GWLP_WNDPROC,
			/*
				Special-purpose "mini-wndProc" - used solely for interpreting
				mouse "wheel" messages to a sub-classed LISTBOX control in a
				COMBOBOX... which are typically just discarded.
			*/
			(LONG_PTR)WNDPROC([](HWND w, UINT mId, WPARAM wp, LPARAM lp)->LRESULT {
				if (mId == WM_MOUSEWHEEL) {
					const int dY = int(wp) >> 16;
					periodId += dY < 0 ? 1 : dY > 0 ? -1 : 0;
					if (periodId < 0)
						periodId = 0;
					else if (periodId >= periodIdMax)
						periodId = periodIdMax - 1;
					::SendMessage(periodH, CB_SETCURSEL, periodId, 0);
					return 0;
				}
				// not for us, leave with "default" processing
				return ::CallWindowProc(oldListBoxProc, w, mId, wp, lp);
			}));

	restrictedH = ::CreateWindow(L"BUTTON",
		L"RunAs restricted and low-integrity process",
		WS_CHILD | WS_VISIBLE | BS_AUTOCHECKBOX | BS_MULTILINE,
		pc2px(widthOf(r), 250), heightOf(r) - pc2px(widthOf(r), 250) - 32 * 2, pc2px(widthOf(r), 4500), 32 * 2,
		w, (HMENU)5, cs->hInstance, NULL);
	::SendMessage(restrictedH, BM_SETCHECK, BST_CHECKED, 0);

	const auto cX = ::GetSystemMetrics(SM_CXEDGE);
	const auto cY = ::GetSystemMetrics(SM_CYEDGE);
	auto hDC = ::GetDC(w);
	const auto yPixels = ::GetDeviceCaps(hDC, LOGPIXELSY);
	const auto logicalHeight = -::MulDiv(48, yPixels, 72);
	LOGFONT f{ 0 };
	f.lfHeight = logicalHeight, f.lfWeight = FW_REGULAR, wcscpy(f.lfFaceName, L"Lucida Sans Typewriter");
	countdownF = ::CreateFontIndirect(&f);
	const auto oldF = ::SelectObject(hDC, countdownF);
	RECT t{ 0 };
	::DrawText(hDC, L"59:59", 5, &t, DT_CALCRECT);
	::SelectObject(hDC, oldF);
	::ReleaseDC(w, hDC);

	countdownH = ::CreateWindowEx(WS_EX_CLIENTEDGE, L"EDIT",
		L"",
		WS_CHILD | WS_VISIBLE | WS_BORDER | ES_RIGHT | ES_READONLY,
		widthOf(r) - pc2px(widthOf(r), 250) - widthOf(t) - cX * 2, 32 + pc2px(widthOf(r), 500), widthOf(t) + cX * 2, heightOf(t) + cY * 2,
		w, (HMENU)3, cs->hInstance, NULL);
	::SendMessage(countdownH, WM_SETFONT, (WPARAM)countdownF, TRUE);
}

/*
	The classic Windows "wndProc", or "Windows Procedure" - for handling all of
	the "messages" that will be sent to your application... in our case, these
	will either be helping us monitor the user's presence and/or the power state
	of the [main] display, OR supporting the [minimal] GUI interactions.

	Note that as this is a fairly simple Windows GUI, once the child windows and
	controls are created, we only really watch for notifications from the "drop
	list" control (signaling a new inactivity delay), "background painting" msgs
	from the "RunAs" checkbox control (implementing our desired visual style),
	and the "power broadcast / power setting change" msgs - which we interpret
	and use to guide our monitoring of the monitor power state and whether the
	user appears to be "active".
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
	case WM_COMMAND:
		if (HIWORD(wp) == CBN_SELCHANGE) {
			const auto i = ::SendMessage((HWND)lp, CB_GETCURSEL, 0, 0);
			if (i != -1) {
				periodId = (int)i;
				return 0;
			}
		}
		break;
	case WM_CTLCOLORSTATIC:
		if ((HWND)lp == restrictedH) {
			::SetBkMode((HDC)wp, TRANSPARENT);
			::SetTextColor((HDC)wp, ::GetSysColor(COLOR_BTNTEXT));
			return (LRESULT)::GetStockObject(NULL_BRUSH);
		}
		break;
	case WM_POWERBROADCAST:
		if (wp == PBT_POWERSETTINGCHANGE) {
			const auto pbs = (const POWERBROADCAST_SETTING*)lp;
			trace((wstring(L"WM_POWERBROADCAST: ") + powerChange2string(pbs)).c_str());
			const auto monitorChanged = updateMonitorStatus(pbs), userChanged = updateUserStatus(pbs);
			if (monitorChanged || userChanged)
				if (!timer && monitorChanged && !monitorOn && procInfo.hProcess != NULL) {
					if (::TerminateProcess(procInfo.hProcess, 0))
						trace(L"WM_POWERBROADCAST, monitor off, TERMINATING inactivity task!");
				} else if (!timer && ((monitorChanged && monitorOn) || (userChanged && userPresent)))
					last = ::GetTickCount(),
					::CreateTimerQueueTimer(&timer, NULL, timerCallback, 0, 1000, 1000, WT_EXECUTELONGFUNCTION),
					trace(L"WM_POWERBROADCAST, monitor/user BACK, RESTARTING inactivity timer!");
				else if (timer && !monitorOn && !userPresent)
					::DeleteTimerQueueTimer(NULL, timer, NULL), timer = NULL,
					trace(L"WM_POWERBROADCAST, monitor/user GONE, DELETING inactivity timer!");
		} else
			trace((wstring(L"WM_POWERBROADCAST: ") + powerMsgOther2string(wp)).c_str());
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
	wcsncat(path, L"\\.rwipconfig", MAX_PATH - n - 1);
	return path;
}

/*
	Load our config if possible, otherwise defaults will be used.
*/
static void loadConfig()
{
	wstring path = configpath();
	wchar_t* last = nullptr;
	wifstream ls(path);
	wstring line;
	while (getline(ls, line), ls.is_open() && !ls.eof())
		if (line.find(L"cmd=") == 0)
			::SendMessage(executableH, WM_SETTEXT, 0, (LPARAM)(line.substr(4).c_str()));
		else if (line.find(L"del=") == 0)
			::SendMessage(periodH, CB_SETCURSEL, periodId = wcstol(line.substr(4).c_str(), &last, 10), 0);
		else if (line.find(L"run=") == 0)
			::SendMessage(restrictedH, BM_SETCHECK, wcstol(line.substr(4).c_str(), &last, 10) != 0 ? BST_CHECKED : BST_UNCHECKED, 0);
}

/*
	Save our config from the current RWip operating state.
*/
static void saveConfig()
{
	wstring path = configpath();
	wchar_t cmd[MAX_PATH + 16];
	const auto n = ::SendMessage(executableH, WM_GETTEXT, MAX_PATH + 16, (LPARAM)cmd);
	const auto checked = ::SendMessage(restrictedH, BM_GETCHECK, 0, 0) == BST_CHECKED;
	wofstream ss(path, ios::out);
	ss << L"cmd=" << cmd << endl;
	ss << L"del=" << periodId << endl;
	ss << L"run=" << checked << endl;
}

/*
	Create the tree of windows and controls, load an existing config, register
	for "power setting" notifications, start a 1-second repeating timer, do the
	Windows message-processing thing until time to clean up, then clean up, and
	save the [possibly updated] config.
*/
int WINAPI WinMain(HINSTANCE inst, HINSTANCE prev, LPSTR cmd, int show)
{
	WNDCLASS wC{ 0, wndProc, 0, 0, inst, NULL, NULL, HBRUSH(COLOR_BACKGROUND), NULL, L"RWipClass" };
	const ATOM wA = ::RegisterClass(&wC);
	if (!wA)
		return 1;
	const HWND wH = ::CreateWindow(LPCTSTR(wA),
		L"RWip 1.1 - Windows Inactivity Proxy",
		WS_BORDER | WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | WS_VISIBLE,
		0, 0, 512, 224, 0, 0, inst, NULL);
	if (wH == NULL)
		return 2;
	loadConfig();
	for (const auto& g : powerMsgs)
		regs.push_back(::RegisterPowerSettingNotification(wH, &g, 0));
	last = ::GetTickCount();
	if (!::CreateTimerQueueTimer(&timer, NULL, timerCallback, 0, 1000, 1000, WT_EXECUTELONGFUNCTION))
		return 3;
	trace(L"INITIAL START of inactivity timer and message loop!");
	MSG m{ 0 };
	while (::GetMessage(&m, NULL, 0, 0) > 0)
		::TranslateMessage(&m), ::DispatchMessage(&m);
	::DeleteTimerQueueTimer(NULL, timer, NULL), timer = NULL;
	for_each(cbegin(regs), cend(regs), [](auto r) {
		if (r != NULL)
			::UnregisterPowerSettingNotification(r);
	});
	::DeleteObject(countdownF);
	saveConfig();
	return 0;
}
