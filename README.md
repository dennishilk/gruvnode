# Gruvnode

![Arch Linux](https://img.shields.io/badge/Arch%20Linux-current-1793D1?logo=archlinux&logoColor=white)
![XMonad](https://img.shields.io/badge/XMonad-window%20manager-5E5086?logo=haskell&logoColor=white)
![ThinkPad](https://img.shields.io/badge/ThinkPad-X1%20Carbon-E2231A?logo=lenovo&logoColor=white)
![X11](https://img.shields.io/badge/display-X11-111111?logo=x.org&logoColor=white)

**Gruvnode** is my personal Linux workstation concept: a small, practical collection of notes and configuration that evolves with the laptop carrying it.

The name belongs to the workstation setup, not to one specific machine. Hardware-specific configuration therefore lives in generation directories instead of at the repository root.

![Gruvnode background](assets/wallpapers/gruvnode-background.png)

## Hardware generations

### Current

**ThinkPad X1 Carbon — Arch Linux + XMonad**

The X1 Carbon is the active Gruvnode target. Its profile starts deliberately conservative: only verified machine facts and reviewed reusable configuration belong there. Hardware-specific details such as CPU, graphics, display, audio, webcam behavior, docking quirks, and battery tuning remain pending until they are checked on the machine.

See [`thinkpad-x1-carbon/`](thinkpad-x1-carbon/).

### Legacy

**ThinkPad T480 — Debian 13 + XMonad**

The T480 was the previous Gruvnode machine. Its installer, desktop configuration, tuning files, changelog, and other historical material are preserved under [`thinkpad-t480/`](thinkpad-t480/) rather than being rewritten as if they apply to the X1 Carbon.

## Repository layout

```text
gruvnode/
├── thinkpad-t480/          # previous Gruvnode generation
├── thinkpad-x1-carbon/     # current Gruvnode generation
├── assets/
│   └── wallpapers/
│       └── gruvnode-background.png
├── LICENSE
└── README.md
```

## Visual identity

`assets/wallpapers/gruvnode-background.png` is the canonical Gruvnode background artwork. It belongs to the Gruvnode identity independently of any particular laptop generation.

## Principle

Reuse the portable parts. Re-validate the hardware-specific parts.

That means XMonad and user-level configuration can be considered for reuse, while old Xorg, Intel, power-management, input, or TLP tuning is not carried to new hardware blindly.
