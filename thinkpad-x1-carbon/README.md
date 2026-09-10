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
- PipeWire + WirePlumber
- NetworkManager
- BlueZ + Blueman
- Google Chrome when an existing `yay` installation is available

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
6. Start the session:
   ```bash
   startx
   ```

The installer updates Arch, installs the official package set with `pacman`, enables NetworkManager and Bluetooth, deploys the user configuration, links the shared Gruvnode wallpaper, and finishes with `xmonad --recompile`.

Existing Gruvnode-managed user config files are backed up before they are replaced. Re-running the installer after a `git pull` is expected and does not overwrite unrelated files.

Fastfetch is installed but is not injected into shell startup. Add `fastfetch` to your shell init yourself if you want it on every terminal login.

### Google Chrome

Chrome is the one intentional exception to the official-package-only path. Gruvnode does **not** install `yay` or another AUR helper.

If `yay` already exists, the installer uses that existing workflow for `google-chrome`. If it does not exist, installation still succeeds and Chrome is left as a manual/AUR follow-up.

## Keybindings

| Key | Action |
| --- | --- |
| `Super + Return` | Kitty |
| `Super + d` | Rofi |
| `Super + b` | Google Chrome |
| `Super + o` | OBS Studio |
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

The layouts are `Tall` and `Full`, with small spacing, smart borders and EWMH fullscreen support. Normal windows tile. Fullscreen windows full-float; EWMH dialogs, including normal OBS properties/source dialogs, are centered and floated.

At session start XMonad applies the Gruvnode wallpaper, starts picom and starts the polkit authentication agent once. Xmobar is managed by XMonad's status-bar lifecycle and reads the `_XMONAD_LOG` property, so restart cleanup/startup stays in one place instead of relying on a fragile pipe.

## OBS and cameras

OBS Studio and `v4l-utils` are installed. A USB camera that appears as a normal V4L2 device can be selected in OBS as a Video Capture Device source. PipeWire/WirePlumber provide the audio side.

No camera model, dock, webcam chipset or capture format is assumed here. Those still need to be checked on the actual X1 Carbon and whatever external camera is connected.

Useful checks:

```bash
v4l2-ctl --list-devices
wpctl status
```

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

For normal Arch updates, `pacman` is the official package manager. If you already use `yay`, your usual:

```bash
yay
```

can update both repository packages and AUR packages such as Chrome. Gruvnode never installs `yay` for you.

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
systemctl --user status pipewire pipewire-pulse wireplumber
```

If networking or Bluetooth is missing:

```bash
systemctl status NetworkManager
systemctl status bluetooth
nmcli device
```

Hardware-specific behavior that has not yet been checked on the physical X1 Carbon should be fixed from observed evidence, not by copying the T480 profile.
