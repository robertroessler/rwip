# RWip

[![Build status](https://ci.appveyor.com/api/projects/status/github/robertroessler/rwip?svg=true)](https://ci.appveyor.com/project/robertroessler/rwip)

The RWip (rwip) project was originally created to do one simple thing: restore
the popular behavior of the built-in Windows screensaver "Bubbles.scr", lost in
the transition to Windows 8.x (and later).

Of course, "specialized" apps can seemingly demand generalization, so that led
to the creation of a full, non-specific "inactivity proxy", that is able to wait
for the user to become inactive for a specified period, and then run a chosen
executable.

Besides the described overt functionality, RWip also serves as an example of how
simple a [GUI] Windows app can be... with nary a bulky framework, manifest file,
or DLL in sight, one can create a functional piece of code with a pleasing and
useful interface - that may be easily extended.

It is written in fairly idiomatic modern C++ 11/14, and in addition to the main
"inactivity" functionality, also includes "helper" templates and functions that
can assist with Windows "handle" lifetime management, as well as aiding in the
layout of windows and controls.

Usage of RWip is simple:

*	set any "official" Windows screen saver to the "(None)" entry - note that
	RWip happily co-exists with any "turn off the screen" system timeout you choose

*	when first run, the presumed executable and parameter is set to

	"c:\Windows\System32\Bubbles.scr /s"

	...note the "/s" - this is part of the standard screensaver interface, and
	is REQUIRED to make the screensaver run "normally"

*	the initial "inactivity" interval is 10 minutes - this may be adjusted to
	a desired value between [the minimum] 1 minute and [the maximum] 1 hour

*	the specified executable will normally run with "restricted" rights and low
	process integrity - if this proves inadequate for your chosen executable, it
	can be changed to execute with the full credentials/capabilities of the user

After the first run, your settings will be saved in the following "config" file:

%USERPROFILE%\.rwipconfig

(typically in the Windows per-user home directory @ C:\Users\<user-name>)

In addition to the explicitly-saved options mentioned above, RWip can now also
"sign up" to be started with Windows - optionally, of course.  When RWip is told
to shutdown, it checks the "Start with Windows" UI checkbox, and sets or removes
the "RWip.lnk" shortcut in

%USERPROFILE%\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup

... depending on the requested "Start" state.  Note that this ONLY happens upon
RWip shutdown, so you will need to shutdown and restart RWip if you change this
option.

Besides being "pure" C++, the code is believed to be both 32/64 -bit "safe", but
do note that this is a Windows-only application... in fact, in addition to being
"safe", pre-built executables are available in the releases tree of this repo -
for those eager to start using RWip without waiting to build it.

## Advanced Usage

To make the most effective use of the "library" (allowing choosing between multiple inactivity apps), here are a few tips:

* to *Add* a new entry, just edit the contents of the currently displayed "inactivity" command - this could involve minor changes to an existing entry, or a complete replacement - but **no** changes will be "official" until you click the "+" (Add) button

* to *Remove* an entry, just scroll it into view and select it, then click the "-" (Remove) button

* the above points notwithstanding, the current/last-edited command is actually maintained independently of the library, so feel free to edit up a new one for testing (or whatever)... as long as you don't actually do the *Add*, it will not change the library contents  

A word on RWip's behavior if "fullscreen" apps are detected: RWip uses heuristics to attempt to guess when you are playing a game or watching a [fullscreen] video, and then operates on the assumption that you don't *want* to have your chosen inactivity app run and interfere with the experience... so, if you are expecting RWip do behave normally, don't walk away from your system with anything running in fullscreen mode.

## More Details

The reason the much-liked "Bubbles.scr" screensaver lost a lot of its appeal in
the transition to Windows 8.x is that - for security reasons - Windows started
running screensavers in their own desktop "sessions", which means they typically
start with a blank background... however, the most visually appealing feature of
"Bubbles.scr" was that it appeared to float numerous translucent and colliding
bubbles over the top of *your* desktop.

By executing "Bubbles.scr" *not* as a screen saver per se, and just as a normal
app, the old behavior of running "on top of" your desktop is now back.  RWip of
course does monitor the screen "power state" and the user's "presence" to know
when it should shut down the chosen executable (usually a screen saver) and in
general get out of the way.

## ToDo

Possible items to work on - for myself or collaborators - include

* make RWip *not* activate the designated "inactivity" app if the user appears to be gaming or watching a [fullscreen] video - actually, *any* fullscreen app... DONE!

* support a "library" of inactivity apps via a dropdown list with Add and Remove buttons... DONE!

* enable RWip to be yet another "always run on system start-up" app... DONE!

* a smarter "auto-sizing" algorithm for layout of the UI, as the current one is
really only optimal for what used to be called "Large Fonts", scaled @ 125%

* a cool icon (of course)

* one *could* limit the re-painting of the countdown display to ONLY the areas
known to have changed, e.g., "59:59" -> 1 second earlier only requires painting
a single digit... on the other hand, the entire app consumes ~1 sec per DAY (when running in the typical **minimized** state)

## ProbablyNot

Things that most likely should NOT happen include

* let's see - adding a script language interpreter for the "inactivity" function
is probably a bit over the top
