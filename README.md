# gruvnode

A minimal, production-ready Debian 13 (Trixie) XMonad setup for the **Lenovo ThinkPad T480**.

Boot flow is intentionally simple and stable:

**Boot → TTY login → `startx` → XMonad session**

No display manager is used.

## System overview

- **OS**: Debian 13 (Trixie)
- **Window manager**: XMonad (X11)
- **Status bar**: Xmobar
- **Terminal**: Kitty
- **Compositor**: Picom (lightweight, optional)
- **Wallpaper**: Nitrogen restore
- **Power management**: TLP + custom powertweaks
- **Startup method**: `startx` from TTY

## Hardware target

- **Device**: Lenovo ThinkPad T480
- **CPU**: Intel (8th gen U-series friendly tuning)
- **GPU**: Intel UHD Graphics (TearFree + SNA + DRI3)

## Why no display manager

This repo deliberately avoids display managers to keep:

- lower boot complexity
- fewer background services
- easier debugging from pure TTY
- predictable behavior on suspend/resume

`systemd-logind` handles power actions such as lid-close suspend.

## Repository structure

```text
gruvnode/
├── install.sh
├── README.md
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
│   └── system/
│       ├── tlp.conf
│       ├── powertweaks.sh
│       └── intel.conf
```

## What is configured

### XMonad

- EWMH + fullscreen support
- tiled layouts with spacing
- no borders on fullscreen
- `Super` as modifier
- `kitty` as terminal, `dmenu` launcher
- browser shortcut: `Super + b`
- restart: `Super + Shift + r`
- quit: `Super + Shift + q`
- media keys via `amixer`
- brightness keys via `brightnessctl`
- startupHook launches:
  - Xmobar
  - Picom
  - German keyboard layout (`setxkbmap de`)
  - Nitrogen wallpaper restore

### Xmobar

Minimal dark ThinkPad style:

- **left**: workspaces
- **center**: focused window title
- **right**: CPU, memory, battery, UTC time
- font: JetBrainsMono Nerd Font

### Kitty

- JetBrainsMono Nerd Font
- Gruvbox-like dark palette
- 0.90 background transparency
- cursor blink disabled
- extended scrollback
- bright bold colors enabled

### ThinkPad optimizations

- TLP tuned for battery-first usage
- CPU governor powersave on battery
- turbo enabled on AC
- USB autosuspend enabled
- Wi-Fi power saving enabled
- Intel graphics TearFree + SNA + DRI3
- touchpad + TrackPoint defaults:
  - natural scrolling off
  - tap-to-click on
  - disable while typing on
  - TrackPoint acceleration tuned
- lid close suspend via logind override

## Installation

```bash
chmod +x install.sh
./install.sh
```

The installer will:

1. Install required packages.
2. Copy user configs into:
   - `~/.xmonad/xmonad.hs`
   - `~/.config/xmobar/xmobarrc`
   - `~/.config/kitty/kitty.conf`
   - `~/.xinitrc`
3. Deploy system files:
   - `/etc/tlp.conf`
   - `/etc/X11/xorg.conf.d/20-intel.conf`
   - `/etc/X11/xorg.conf.d/30-thinkpad-input.conf`
   - `/etc/systemd/logind.conf.d/thinkpad-power.conf`
   - `/usr/local/sbin/powertweaks.sh`
4. Enable and start TLP.
5. Apply additional kernel/sysctl battery tweaks.

After install:

```bash
startx
```

## Rebuild xmonad

```bash
chmod +x ~/.xmonad/build.sh
~/.xmonad/build.sh
```

Or directly:

```bash
xmonad --recompile && xmonad --restart
```

## Power optimization notes

TLP handles dynamic policy based on AC/BAT. `powertweaks.sh` adds aggressive laptop-friendly VM behavior:

- lower swappiness
- lower cache pressure
- delayed writeback tuning
- laptop mode enabled
- Intel pstate active mode where available

This combination aims for low heat, stable suspend/resume, and longer battery runtime without heavy daemons.

## Screenshots

> Placeholder: add screenshots of desktop, terminal, and tiled layout.

## Philosophy

- minimal
- stable
- keyboard-driven
- ThinkPad-native
- no bloat
- understandable and maintainable
