#!/usr/bin/env bash
set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

PACKAGES=(
  xorg
  xinit
  xmonad
  xmobar
  kitty
  tlp
  brightnessctl
  picom
  nitrogen
  dmenu
  fonts-jetbrains-mono
  pulseaudio-utils
  build-essential
  ghc
  xbacklight
  x11-xserver-utils
  xserver-xorg-video-intel
  xserver-xorg-input-libinput
  libghc-xmonad-dev
  libghc-xmonad-contrib-dev
)

echo "==> Installing required packages"
sudo apt update
sudo apt install -y "${PACKAGES[@]}"

echo "==> Creating user config directories"
mkdir -p "$HOME/.xmonad" "$HOME/.config/xmobar" "$HOME/.config/kitty"

echo "==> Copying XMonad/Xmobar/Kitty/Xinit configs"
cp "$REPO_DIR/configs/xmonad/xmonad.hs" "$HOME/.xmonad/xmonad.hs"
cp "$REPO_DIR/configs/xmonad/build.sh" "$HOME/.xmonad/build.sh"
cp "$REPO_DIR/configs/xmobar/xmobarrc" "$HOME/.config/xmobar/xmobarrc"
cp "$REPO_DIR/configs/kitty/kitty.conf" "$HOME/.config/kitty/kitty.conf"
cp "$REPO_DIR/configs/xinit/xinitrc" "$HOME/.xinitrc"

chmod +x "$REPO_DIR/configs/xmonad/build.sh"
chmod +x "$HOME/.xmonad/build.sh"
chmod +x "$REPO_DIR/configs/system/powertweaks.sh"
chmod +x "$HOME/.xinitrc"

echo "==> Deploying system tuning files"
sudo cp "$REPO_DIR/configs/system/tlp.conf" /etc/tlp.conf
sudo mkdir -p /etc/X11/xorg.conf.d
sudo install -m 0644 "$REPO_DIR/configs/system/intel.conf" /etc/X11/xorg.conf.d/20-intel.conf
sudo install -m 0755 "$REPO_DIR/configs/system/powertweaks.sh" /usr/local/sbin/powertweaks.sh

echo "==> Configuring touchpad + TrackPoint defaults"
sudo tee /etc/X11/xorg.conf.d/30-thinkpad-input.conf > /dev/null <<'INPUT'
Section "InputClass"
  Identifier "Touchpad defaults"
  MatchIsTouchpad "on"
  Driver "libinput"
  Option "NaturalScrolling" "false"
  Option "Tapping" "on"
  Option "DisableWhileTyping" "on"
EndSection

Section "InputClass"
  Identifier "TrackPoint defaults"
  MatchProduct "TrackPoint"
  Driver "libinput"
  Option "AccelSpeed" "0.6"
EndSection
INPUT

echo "==> Configuring logind power handling (lid suspend, no DM required)"
sudo mkdir -p /etc/systemd/logind.conf.d
sudo tee /etc/systemd/logind.conf.d/thinkpad-power.conf > /dev/null <<'LOGIND'
[Login]
HandleLidSwitch=suspend
HandleLidSwitchExternalPower=suspend
HandleLidSwitchDocked=ignore
LOGIND

echo "==> Enabling TLP and applying power tweaks"
sudo systemctl enable tlp
sudo systemctl restart tlp
sudo /usr/local/sbin/powertweaks.sh

echo
echo "Installation complete."
echo "Add 'exec xmonad' to ~/.xinitrc if missing"
echo "Run startx"
