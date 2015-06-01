# About

This is my personal StumpWM configuration. This repository is meant as a
personal backup, and for others to steal ideas from. While you are free to use
it wholesale, I only advise taking bits and pieces, as I'm no expert.


# Dependencies

This StumpWM configuration makes some assumptions about which programs are
installed, and will not work correctly without them. Here I list the programs,
along with a description of their usage, and a link to the Arch Linux package
which provides it.

- [acpi](https://www.archlinux.org/packages/community/x86_64/acpi/)
    - used to obtain battery status in `mode-line.lisp`
- [amixer](https://www.archlinux.org/packages/extra/x86_64/alsa-utils/)
    - used to display volume level in `mode-line.lisp`
    - used to modify volume level in `system.lisp`
    - will only work if your system is running ALSA for its sound
- [awk](https://www.archlinux.org/packages/core/x86_64/gawk/),
  [cut](https://www.archlinux.org/packages/core/x86_64/coreutils/),
  [grep](https://www.archlinux.org/packages/core/x86_64/grep/),
  [sed](https://www.archlinux.org/packages/core/x86_64/sed/),
  [tail](https://www.archlinux.org/packages/core/x86_64/coreutils/),
  and
  [tr](https://www.archlinux.org/packages/core/x86_64/coreutils/)
    - used to filter output streams from shell commands in various scripts
- [xbacklight](https://www.archlinux.org/packages/extra/x86_64/xorg-xbacklight/)
    - used to modify backlight in `system.lisp`
- [xrdb](https://www.archlinux.org/packages/core/x86_64/coreutils/)
    - used to query X resources in `xresources.lisp`


# Links

This configuration was made with the help of others I found online. Here is a
list of some of them, and how they were used.

- [XSteve's StumpWM](http://www.xsteve.at/prg/stumpwm/)
    - a very simple configuration that helped me get started
- [alezost's stumpwmrc](https://github.com/alezost/stumpwmrc)
    - helped me split my configuration across multiple files
- [Bill Zimmerly's stumpwmrc](https://gist.github.com/dbjergaard/8776184)
    - helpful in formatting my mode-line
- [vlnx's useless gaps](https://gist.github.com/vlnx/5651256)
    - used this code verbatim to introduce "useless gaps"
    - all I changed was the size of the gaps, and enabled them by default
- [StumpWM's source code](https://github.com/stumpwm/stumpwm/)
    - reading the source was very helpful, especially
        - `bindings.lisp` to override the default bindings
        - `command.lisp` to define new commands
