# ThinkPad X1 Carbon

This is the current Gruvnode: a ThinkPad X1 Carbon with 16 GB RAM running Arch Linux, X11 and XMonad.

The profile is deliberately boring in the useful sense. It sets up the desktop and everyday laptop tooling without carrying over the T480's old Intel Xorg or power-tuning assumptions.

## Stack

- Arch Linux + X11
- XMonad + xmobar
- Rofi
- Kitty
- Picom
- feh
- Fastfetch
- OBS Studio + V4L2 tools
- Thunar
- Mousepad
- PipeWire + WirePlumber
- ALSA utilities + SOF firmware
- Linux firmware bundle
- NetworkManager + iwd
- BlueZ + Blueman
- `yay` + Google Chrome

The canonical wallpaper lives at [`../assets/wallpapers/gruvnode-background.png`](../assets/wallpapers/gruvnode-background.png).

## Install

Gruvnode is not an Arch installer. Start with a normal Arch installation, preferably using `archinstall`.

1. Install Arch Linux. Select XMonad during `archinstall` if that fits the current installer flow.
2. Boot into the installed system and log in on a TTY.
3. Install Git if it is not already present:
   ```bash
   sudo pacman -S --needed git
   ```
4. Clone Gruvnode and enter this profile:
   ```bash
   git clone https://github.com/dennishilk/gruvnode.git
   cd gruvnode/thinkpad-x1-carbon
   ```
5. Run:
   ```bash
   chmod +x install.sh
   ./install.sh
   ```
6. Reboot once after the first full provisioning run so newly installed firmware is available from a clean boot, then start the session with:
   ```bash
   startx
   ```

The installer updates Arch, installs the official package set with `pacman`, installs the Linux/SOF firmware and ALSA diagnostics, configures NetworkManager to use the iwd Wi-Fi backend, starts the PipeWire/WirePlumber user services, enables NetworkManager and Bluetooth, deploys the user configuration, links the shared Gruvnode wallpaper, installs `yay` when needed, installs Google Chrome through `yay`, and finishes with `xmonad --recompile`.

Existing Gruvnode-managed user config files are backed up before they are replaced. Re-running the installer after a `git pull` is expected and does not overwrite unrelated files.

Fastfetch is installed but is not injected into shell startup. Add `fastfetch` to your shell init yourself if you want it on every terminal login.

### yay and Google Chrome

Google Chrome is part of the X1 Gruvnode baseline and comes from the AUR. If `yay` is not already installed, the installer clones the `yay` package from `https://aur.archlinux.org/yay.git`, builds it locally with `makepkg`, and installs it as the normal user. It then installs `google-chrome` through `yay`.

If `yay` already exists, that installation is reused. Official repository packages still go through `pacman`; the AUR path is used for `yay` itself and Google Chrome.

## Keybindings

| Key | Action |
| --- | --- |
| `Super + Return` | Kitty |
| `Super + d` | Rofi |
| `Super + b` | Google Chrome |
| `Super + o` | OBS Studio |
| `Super + t` | Thunar |
| `Super + q` | Close focused window |
| `Super + Shift + q` | Exit XMonad |
| `Super + Space` | Next layout |
| `Super + j / k` | Focus down / up |
| `Super + m` | Focus master |
| `Super + Shift + j / k` | Swap down / up |
| `Super + h / l` | Shrink / expand master area |
| `Super + Shift + r` | Recompile and restart XMonad |
| `Print` | Screenshot to `~/Pictures/Screenshots/` |
| Volume keys | PipeWire volume via `wpctl` |
| Brightness keys | Backlight via `brightnessctl` |

Mousepad is installed as the lightweight graphical text editor but intentionally has no dedicated XMonad keybinding; launch it through Rofi when needed.

The layouts are `Tall` and `Full`, with small spacing, smart borders and EWMH fullscreen support. Normal windows tile. Fullscreen windows full-float; EWMH dialogs, including normal OBS properties/source dialogs, are centered and floated.

At session start XMonad applies the Gruvnode wallpaper, starts picom and starts the polkit authentication agent once. Xmobar is managed by XMonad's status-bar lifecycle and reads the `_XMONAD_LOG` property, so restart cleanup/startup stays in one place instead of relying on a fragile pipe.

## OBS and cameras

OBS Studio and `v4l-utils` are installed. `Super + o` launches OBS. A USB camera that appears as a normal V4L2 device can be selected in OBS as a Video Capture Device source. PipeWire/WirePlumber provide the audio side.

No camera model, dock, webcam chipset or capture format is assumed here. Those still need to be checked on the actual X1 Carbon and whatever external camera is connected.

Useful checks:

```bash
v4l2-ctl --list-devices
wpctl status
```

## Audio

The profile installs `pipewire`, `pipewire-audio`, `pipewire-pulse`, `wireplumber`, `alsa-utils`, `linux-firmware` and `sof-firmware`. The installer explicitly starts/enables the PipeWire, PipeWire Pulse and WirePlumber user services.

Xmobar gets its volume from `wpctl`, so `vol n/a` means no usable default PipeWire sink was available when the helper queried it. After the provisioning run and reboot, useful checks are:

```bash
wpctl status
wpctl get-volume @DEFAULT_AUDIO_SINK@
aplay -l
systemctl --user status pipewire pipewire-pulse wireplumber
```

## Wi-Fi

The profile installs the standard Linux firmware bundle and `iwd`. NetworkManager remains the network manager, with iwd configured as its Wi-Fi backend. Do not separately enable `iwd.service`; NetworkManager starts and manages it.

`iwctl` is therefore installed for low-level inspection, while normal connection management should use NetworkManager tools such as:

```bash
nmcli device
nmcli radio wifi
nmcli device wifi list
nmtui
```

If the Wi-Fi device is still missing after the first provisioning run, reboot once so the freshly installed firmware is available when the kernel driver initializes, then re-check `nmcli device` and the kernel log before adding hardware-specific driver tweaks.

## Laptop behavior

This profile uses the standard Xorg modesetting path and libinput. It does not install the old T480 `xf86-video-intel` config or TLP tuning.

Power handling stays conservative: systemd/logind handles normal lid/suspend behavior and the kernel handles device power management. There is no second power-management daemon competing with that setup. Battery display in xmobar discovers the first `BAT*` power-supply device at runtime rather than assuming `BAT0`.

NetworkManager and Blueman are installed, but no extra tray is forced into xmobar. `nm-connection-editor` and `blueman-manager` are available when a GUI is useful.

For external displays, use `xrandr` until an actual dock/display layout is worth codifying.

## Updating

Update the repository/profile with:

```bash
git pull
./install.sh
```

For normal Arch updates, `pacman` remains the official package manager. Gruvnode also installs `yay`, so the usual:

```bash
yay
```

can update repository packages and AUR packages such as Google Chrome.

If the laptop has been off for a week or two, update normally before settling back into work. Check Arch news when an update announces manual intervention; Arch updates should not be treated as blindly unattended.

## Troubleshooting

If X does not start:

```bash
xmonad --recompile
startx
```

Check the XMonad compiler output and Xorg log/journal messages before changing drivers.

If picom is suspected, stop it with `pkill picom` and restart XMonad. The desktop works without compositing.

If audio is missing:

```bash
wpctl status
aplay -l
systemctl --user status pipewire pipewire-pulse wireplumber
```

If networking or Bluetooth is missing:

```bash
systemctl status NetworkManager
systemctl status bluetooth
nmcli device
nmcli device wifi list
```

Hardware-specific behavior that has not yet been checked on the physical X1 Carbon should be fixed from observed evidence, not by copying the T480 profile.
