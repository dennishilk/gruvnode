# Gruvnode

![Arch Linux](https://img.shields.io/badge/Arch%20Linux-current-1793D1?logo=archlinux&logoColor=white)
![XMonad](https://img.shields.io/badge/XMonad-window%20manager-5E5086?logo=haskell&logoColor=white)
![ThinkPad](https://img.shields.io/badge/ThinkPad-X1%20Carbon-E2231A?logo=lenovo&logoColor=white)
![X11](https://img.shields.io/badge/display-X11-111111?logo=x.org&logoColor=white)

**Gruvnode** is my personal Linux workstation setup: minimal, keyboard-driven and meant to stay understandable when the hardware underneath it changes.

The current machine is a ThinkPad X1 Carbon running Arch Linux + XMonad. The previous T480 generation is kept here as history rather than being flattened into the new setup.

![Gruvnode background](assets/wallpapers/gruvnode-background.png)

## Current machine

**Lenovo ThinkPad X1 Carbon — 16 GB RAM — Arch Linux + X11 + XMonad**

The active profile lives in [`thinkpad-x1-carbon/`](thinkpad-x1-carbon/). That directory contains the installer, XMonad desktop configuration and the practical setup notes for a fresh Arch system.

## Generations

| Machine | System | Status |
| --- | --- | --- |
| ThinkPad X1 Carbon | Arch Linux + XMonad | **Current** |
| ThinkPad T480 | Debian 13 + XMonad | Legacy |

The T480 files remain under [`thinkpad-t480/`](thinkpad-t480/) with their Debian-era tuning intact.

## Philosophy

Minimal. Keyboard-driven. Reproducible enough to rebuild. Simple enough to debug.

Portable desktop choices can move forward between generations; hardware-specific tuning earns its way back in only after the new machine proves it needs it.

## Repository layout

```text
gruvnode/
├── thinkpad-x1-carbon/     # current Arch + XMonad profile
│   ├── configs/
│   ├── scripts/
│   ├── tests/
│   ├── install.sh
│   └── README.md
├── thinkpad-t480/          # historical Debian 13 profile
├── assets/
│   └── wallpapers/
│       └── gruvnode-background.png
├── LICENSE
└── README.md
```

`assets/wallpapers/gruvnode-background.png` is the shared Gruvnode wallpaper and visual identity. Hardware generations reference it rather than carrying separate copies.
