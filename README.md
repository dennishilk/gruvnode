# gruvnode

Debian 13 (Trixie) + XMonad for the **Lenovo ThinkPad T480**, designed for:

- X11 only
- no display manager
- clean TTY-first workflow (`startx`)
- minimal but polished daily-driver setup

Boot flow:

```text
Boot -> TTY login -> startx -> XMonad
```

## Why no display manager?

This project intentionally avoids a display manager to keep the session simple and debuggable:

- fewer moving parts during boot/login
- easier troubleshooting from pure TTY
- lower background overhead
- predictable behavior after suspend/resume

## Repository layout

```text
gruvnode/
├── install.sh
├── README.md
├── assets/
│   └── wallpapers/
│       └── example-gruvnode.jpg
├── configs/
│   ├── xmonad/
│   │   ├── xmonad.hs
│   │   └── build.sh
│   ├── xmobar/
│   │   └── xmobarrc
│   ├── kitty/
│   │   └── kitty.conf
│   ├── xinit/
│   │   └── xinitrc
│   ├── picom/
│   │   └── picom.conf
│   └── system/
│       ├── tlp.conf
│       ├── powertweaks.sh
│       ├── intel.conf
│       └── input.conf
```

## What you get

- **Reliable xmobar integration** via `spawnPipe` + `dynamicLogWithPP` in XMonad.
- **Kitty** with a stable, gruvbox-ish configuration.
- **Wallpaper handling** with `feh` and a bundled example wallpaper copied to:
  - `~/.local/share/wallpapers/gruvnode.jpg`
- **ThinkPad power tuning** with TLP + safe sysctl tweaks.
- **Intel Xorg tuning** (`TearFree`, `SNA`, `DRI3`).
- **Touchpad + TrackPoint defaults** tuned for laptop usability.

## Install

```bash
chmod +x install.sh
./install.sh
```

The installer is idempotent and safe to re-run.

It will:

1. Install required packages (`xorg`, `xinit`, `xmonad`, `xmobar`, `kitty`, `dmenu`, `feh`, `picom`, `tlp`, etc.).
2. Copy user configs to:
   - `~/.config/xmonad/xmonad.hs`
   - `~/.config/xmobar/xmobarrc`
   - `~/.config/kitty/kitty.conf`
   - `~/.config/picom/picom.conf`
   - `~/.xinitrc`
   - `~/.local/share/wallpapers/gruvnode.jpg`
3. Install system configs to:
   - `/etc/X11/xorg.conf.d/20-intel.conf`
   - `/etc/X11/xorg.conf.d/30-input.conf`
   - `/etc/tlp.conf`
   - `/usr/local/sbin/powertweaks.sh`
4. Enable/start TLP and apply additional power tweaks.
5. Run `xmonad --recompile` once during install.

Then:

```bash
reboot
# login on TTY
startx
```

`xmobar` appears automatically after `startx` because XMonad launches it via `spawnPipe`.

## XMonad keybinds

- `Super + Enter`: open Kitty
- `Super + d`: launch dmenu
- `Super + b`: open Firefox
- `Super + Shift + r`: recompile + restart XMonad
- `Super + Shift + q`: quit XMonad
- Brightness keys: `brightnessctl`
- Audio keys: `amixer`

## Power tuning notes (ThinkPad T480)

`configs/system/tlp.conf`:

- battery-first defaults when unplugged
- better performance when plugged in
- Wi-Fi power savings on battery
- USB autosuspend and PCIe ASPM tuning

`configs/system/powertweaks.sh` adds safe VM tuning:

- `vm.dirty_writeback_centisecs`
- `vm.dirty_background_ratio`
- `vm.dirty_ratio`
- `vm.laptop_mode`

All settings are commented and can be adjusted.

## Xmobar reliability / troubleshooting

### Symptom

> “Xmobar only appears when running `xmobar --recompile`, then disappears.”

### Cause

`xmobar --recompile` only compiles xmobar and exits. It is not a long-running launcher command.

### Correct behavior in this repo

Xmobar is started by XMonad itself via `spawnPipe`, and workspace/title updates are pushed with `dynamicLogWithPP`.

This makes xmobar persist as part of the XMonad session lifecycle without relying on removed/deprecated helper hooks.

### Checks

- Verify xmonad config path: `~/.config/xmonad/xmonad.hs`
- Recompile manually if needed:
  ```bash
  xmonad --recompile
  xmonad --restart
  ```
- Check logs/errors:
  - `.xsession-errors`
  - `~/.xmonad/xmonad.errors` (or distro-specific XMonad state path)

## Updating / re-running installer

Any time you pull updates:

```bash
git pull
./install.sh
```

Then restart XMonad or reboot.

## Screenshots

Placeholder section:

- add your desktop screenshot(s) here after first boot.

## Notes

- `nm-applet` autostart is optional and only runs when present.
- picom autostart is optional; if you see issues, comment out its startup line in `xmonad.hs`.
- replace `assets/wallpapers/example-gruvnode.jpg` with your own wallpaper image if preferred.
