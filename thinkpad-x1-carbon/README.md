# ThinkPad X1 Carbon — current Gruvnode generation

> **Status:** active Gruvnode target.

This directory represents the current Gruvnode machine: a **Lenovo ThinkPad X1 Carbon with 16 GB RAM**, intended to run **Arch Linux + XMonad** as a personal workstation and development laptop.

The profile is intentionally documentation-first at this stage. The exact X1 Carbon generation, CPU, graphics stack, display characteristics, audio hardware, webcam behavior, docking quirks, and battery-tuning requirements have not been audited here, so they are not guessed or inherited from the previous T480.

## Known baseline

- Machine family: Lenovo ThinkPad X1 Carbon
- Memory: 16 GB RAM
- Distribution: Arch Linux
- Primary window manager: XMonad
- Role: current personal Gruvnode workstation / development laptop

## Bootstrap direction

The intended first installation flow is simple:

1. Install Arch Linux using `archinstall`.
2. Select XMonad during installation if the current `archinstall` profile makes that appropriate; otherwise finish the base Arch install and add XMonad afterwards.
3. Boot the installed system and clone this repository.
4. Use `thinkpad-x1-carbon/` as the hardware-specific base for the new machine.
5. Reuse portable Gruvnode pieces only after reviewing them against Arch and the actual X1 hardware.

Example handoff after the base OS is running:

```bash
git clone https://github.com/dennishilk/gruvnode.git
cd gruvnode/thinkpad-x1-carbon
```

There is deliberately no one-shot installer in this directory yet. Adding one before the hardware and package choices are validated would imply a level of completeness that does not exist.

## Planned software direction

Likely candidates for the active profile include:

- XMonad as the window manager
- xmobar or an equivalent lightweight status bar, after review
- Kitty as a terminal, if the existing user-level configuration still fits
- a minimal X11 session around XMonad
- common development and workstation tooling selected for Arch rather than copied from Debian package lists
- the canonical Gruvnode wallpaper from `../assets/wallpapers/gruvnode-background.png`

User-level T480 configuration under `../thinkpad-t480/configs/` can be treated as a source of ideas. System-level files are **not** migration defaults.

## Hardware validation before tuning

Before adding machine-specific configuration, verify and document the actual hardware and behavior on this X1 Carbon. Useful checks include the machine generation/model data, CPU, graphics driver path, display modes, input devices, networking, audio, camera, suspend/resume, battery reporting, and any dock actually used.

Only after those checks should the profile gain Xorg snippets, power-management policy, input overrides, or other hardware-specific tuning.

## Migration rule

Do not blindly copy these T480-era files into this profile:

- `configs/system/intel.conf`
- `configs/system/tlp.conf`
- `configs/system/powertweaks.sh`
- `configs/system/input.conf`
- the Debian/APT-oriented `install.sh`

Portable XMonad, xmobar, Kitty, picom, and session ideas may be reusable, but each should be reviewed for Arch package names, current upstream behavior, and the real X1 Carbon hardware before becoming active configuration.

## Next milestone

Once Arch + XMonad boots on the machine, capture the verified hardware baseline and then add the smallest working Gruvnode configuration here. Until then, this README is the authoritative bootstrap direction rather than a claim of finished support.
