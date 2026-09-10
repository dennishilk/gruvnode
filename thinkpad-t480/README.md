# ThinkPad T480 — legacy Gruvnode generation

> **Status:** historical / previous Gruvnode hardware. This ThinkPad T480 has been sold and is no longer the active machine.

This directory preserves the Debian 13 (Trixie) + XMonad Gruvnode setup that originally occupied the repository root. It remains useful as a historical, reproducible T480 profile, but its machine-specific tuning must not be assumed to apply to later Gruvnode hardware.

The original setup was designed for:

- X11 only
- no display manager
- clean TTY-first workflow (`startx`)
- minimal but polished daily-driver setup

Boot flow:

```text
Boot -> TTY login -> startx -> XMonad
```

## Why no display manager?

This generation intentionally avoided a display manager to keep the session simple and debuggable:

- fewer moving parts during boot/login
- easier troubleshooting from pure TTY
- lower background overhead
- predictable behavior after suspend/resume

## Directory layout

```text
thinkpad-t480/
├── install.sh
├── README.md
├── CHANGELOG.md
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
└── wow-classic/
```

## What this generation provided

- **Reliable xmobar integration** via `spawnPipe` + `dynamicLogWithPP` in XMonad.
- **Kitty** with a stable, gruvbox-ish configuration.
- **Wallpaper handling** with `feh` and the generation's historical example wallpaper copied to `~/.local/share/wallpapers/gruvnode.jpg`.
- **ThinkPad T480 power tuning** with TLP + sysctl tweaks.
- **Intel Xorg tuning** (`TearFree`, `SNA`, `DRI3`).
- **Touchpad + TrackPoint defaults** tuned for the T480 setup.

These details are intentionally retained here as T480 history. In particular, the Intel Xorg, TLP, input, and power-tuning files are not templates for the X1 Carbon until independently validated.

## Historical install

Run this only when reproducing the T480 generation:

```bash
cd thinkpad-t480
chmod +x install.sh
./install.sh
```

The installer uses Debian/APT package names and deploys the T480-specific system configuration. It is **not** an Arch Linux installer and must not be used as the X1 Carbon bootstrap script.

Historically it installed packages and deployed configuration to locations including:

- `~/.config/xmonad/xmonad.hs`
- `~/.config/xmobar/xmobarrc`
- `~/.config/kitty/kitty.conf`
- `~/.config/picom/picom.conf`
- `~/.xinitrc`
- `/etc/X11/xorg.conf.d/20-intel.conf`
- `/etc/X11/xorg.conf.d/30-input.conf`
- `/etc/tlp.conf`
- `/usr/local/sbin/powertweaks.sh`

## XMonad keybinds

- `Super + Enter`: open Kitty
- `Super + d`: launch dmenu
- `Super + b`: open Firefox / browser action from the historical config
- `Super + Shift + r`: recompile + restart XMonad
- `Super + Shift + q`: quit XMonad
- Brightness keys: `brightnessctl`
- Audio keys: `amixer`

## Power tuning notes

`configs/system/tlp.conf` contains the T480 generation's battery/performance policy, Wi-Fi power saving, USB autosuspend, and PCIe ASPM choices.

`configs/system/powertweaks.sh` adds VM tuning including:

- `vm.dirty_writeback_centisecs`
- `vm.dirty_background_ratio`
- `vm.dirty_ratio`
- `vm.laptop_mode`

They are preserved for reference, not promoted as generic Gruvnode defaults.

## Xmobar reliability / troubleshooting

### Symptom

> “Xmobar only appears when running `xmobar --recompile`, then disappears.”

### Cause

`xmobar --recompile` only compiles xmobar and exits. It is not a long-running launcher command.

### Historical solution

Xmobar is started by XMonad via `spawnPipe`, and workspace/title updates are pushed with `dynamicLogWithPP`. This keeps xmobar in the XMonad session lifecycle without relying on removed/deprecated helper hooks.

Useful checks:

- verify XMonad config path: `~/.config/xmonad/xmonad.hs`
- recompile/restart with `xmonad --recompile` and `xmonad --restart`
- check `.xsession-errors` and the distro-specific XMonad error/state path

## Preservation note

The files in this directory describe the previous machine as it existed. Future Gruvnode generations should reuse only the parts that survive review and hardware-specific validation.
