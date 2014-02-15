# solar-theme-switcher.el

Author: Ryan Smith (rnsmith2@gmail.com)
Date 02/14/2014
Inspired (and forked) from moe-theme-switcher by kuanyui (https://github.com/kuanyui/moe-theme.el)

## Installation
### Manually
```lisp
    (add-to-list 'load-path "/path/to/solar-theme-switcher.el/")
	(require 'solar-theme-switcher)
```

## Usage

Run like this to have the moe-light theme used when daylight outside and moe-dark theme used when dark, replacing with light and dark themes of your choice:
```lisp
    (initialize-solar-theme-switcher 'moe-light 'moe-dark)
```

If you don't wish to load a timer to have the theme continually checked for daylight, but just have it run once then just run the function `switch-theme-by-daylight` with the light theme and dark theme of choice:
```lisp
    (switch-theme-by-daylight 'moe-light 'moe-dark)
```

By default which theme should be loaded is rechecked every 10 minutes, this can be varied with the variable `solar-theme-switcher-check-daylight-every` or you can cancel the running switcher with `(cancel-solar-theme-switcher)`

### Sunrise and sunset times

Sunrise and sunset times are read using `(solar-sunrise-and-sunset)` from built in solar package. In order to use this longitude and latitude must be set, call `(solar-setup)` to configure this information. Or before calling `initialize-solar-theme-switcher` set latitude and longitude in .emacs.d like:

```lisp
    ;;; White House coordinates
    (setq calendar-latitude +38.8977)
	(setq calendar-longitude -77.0366)
```

If we're not able to actually get real sunrise and sunset we'll use the fixed ones set in `fixed-sunrise` (default 7:00) and `fixed-sunset` (default 18:00). If you want to always use these fixed times, then set `use-fixed-sunrise-sunset` to a non-nil value

Sunrise & Sunset times can be varied by use of the `sunrise-flex-time` and `sunset-flex-time` variables. Setting these to 15, for example, will cause anything 15 minutes before sunrise and 15 minutes before sunset to be treated as sunrise/sunset.

## Installing themes

If the themes you wish to use don't come with emacs, then be sure to load the directory they're contained in to `custom-theme-load-path`, such as: `(add-to-list 'custom-theme-load-path "/path/to/moe-theme.el/")`
