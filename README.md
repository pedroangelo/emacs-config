# emacs-config
My personal emacs configuration files

## todo
- define test-internet-connection: check if there's internet connection or not, set has-internet-connection as true or false
- improve auto-package-installer:
	- if package-archive-contents is false then if has-internet-connection test is true:  install missing packages
	- ignore eventual errors: ignore-errors
- define theme-handler:
	- set target-themes: solarized-light, solarized-dark
	- define load-themes: load each of target-themes
	- if all target-themes are loaded, then call auto-dark-mode else if has-internet-connection is true then load-themes
- define auto-dark-mode (switch theme according to time of day).
	- check: https://stackoverflow.com/questions/14760567/emacs-auto-load-color-theme-by-time
	- custom-enabled-themes shows what themes are enabled, maybe avoid loading theme already enabled
  - desktop-save-mode, or rather, desktop-globals-to-save saves global variables.
    then when changing theme, set current-theme variable.
  - try solarized themes

## Resources
- https://unkertmedia.com/how-to-setup-emacs-for-web-development/