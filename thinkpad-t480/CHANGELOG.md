# Changelog

All notable changes to this repository will be documented in this file.

This project follows a **pragmatic versioning approach**, inspired by Semantic Versioning.

---

## [1.0.0] – 2025-12-24
### Initial stable release (Gruvnode)

🇩🇪  
Erste stabile Version des **Gruvnode Debian 13 XMonad Setups** für das ThinkPad T480.  
Dieses Release stellt einen **voll reproduzierbaren Systemzustand** dar und dient als persönliches System-Backup.

🇬🇧  
Initial stable release of the **Gruvnode Debian 13 XMonad setup** for the ThinkPad T480.  
This release represents a **fully reproducible system state** and serves as a personal system backup.

### Added
- Debian 13 minimal installation workflow
- Interactive install script with **Dry-Run mode**
- Optional **Install-All** mode after Dry-Run
- XMonad (X11) configuration with classic keybindings
- Kitty terminal configuration (0.80 opacity)
- dmenu application launcher
- Fish shell as default shell
- Fastfetch auto-run in interactive shells
- Intel iGPU optimized setup (Mesa + Vulkan)
- Steam installation with proper i386 multiarch support
- zram configuration for reduced swap pressure
- T480-specific performance and stability tweaks
- systemd oneshot service for persistent CPU governor (performance)
- Volume and brightness keybindings (pamixer / brightnessctl)
- Wallpaper deployment via feh
- Screenshot support (scrot)

### Fixed
- Steam installation on Debian minimal systems (missing i386 architecture)
- XMonad recompilation issues (missing GHC and dev libraries)
- Deprecated cpufrequtils usage (replaced with cpupower oneshot service)
- Debian 13 compatibility issues in performance tuning

### Notes
- Wayland and compositors are intentionally **not used**
- Configuration is **hardware-specific** (ThinkPad T480)
- Focus is on **stability, low overhead, and long sessions**
- Laptop can remain powered off for extended periods without issues

---

## [Unreleased]
### Planned
- Minor documentation improvements
- Optional workspace rules for Steam / browser
- Further cleanup and comments in install script

