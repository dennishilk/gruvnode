#!/usr/bin/env bash
set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

PACKAGES=(
  xorg
  xinit
  xmonad
  xmobar
  libghc-xmonad-dev
  libghc-xmonad-contrib-dev
  ghc
  kitty
  dmenu
  feh
  picom
  tlp
  powertop
  brightnessctl
  pulseaudio-utils
  fonts-jetbrains-mono
  xserver-xorg-video-intel
  xserver-xorg-input-libinput
  scrot
  build-essential
  git
  curl
)

info() { printf '\n==> %s\n' "$1"; }
warn() { printf 'WARNING: %s\n' "$1"; }

copy_file() {
  local src="$1"
  local dst="$2"
  mkdir -p "$(dirname "$dst")"
  install -m 0644 "$src" "$dst"
}

info "Installing required packages"
sudo apt update
sudo apt install -y "${PACKAGES[@]}"

info "Creating user directories"
mkdir -p \
  "$HOME/.config/xmonad" \
  "$HOME/.config/xmobar" \
  "$HOME/.config/kitty" \
  "$HOME/.config/picom" \
  "$HOME/.local/share/wallpapers"

info "Deploying user configuration files"
copy_file "$REPO_DIR/configs/xmonad/xmonad.hs" "$HOME/.config/xmonad/xmonad.hs"
install -m 0755 "$REPO_DIR/configs/xmonad/build.sh" "$HOME/.config/xmonad/build.sh"
copy_file "$REPO_DIR/configs/xmobar/xmobarrc" "$HOME/.config/xmobar/xmobarrc"
copy_file "$REPO_DIR/configs/kitty/kitty.conf" "$HOME/.config/kitty/kitty.conf"
copy_file "$REPO_DIR/configs/picom/picom.conf" "$HOME/.config/picom/picom.conf"
install -m 0755 "$REPO_DIR/configs/xinit/xinitrc" "$HOME/.xinitrc"
copy_file "$REPO_DIR/assets/wallpapers/example-gruvnode.jpg" "$HOME/.local/share/wallpapers/gruvnode.jpg"

# Backward-compatible copy for users still using ~/.xmonad.
mkdir -p "$HOME/.xmonad"
copy_file "$REPO_DIR/configs/xmonad/xmonad.hs" "$HOME/.xmonad/xmonad.hs"
install -m 0755 "$REPO_DIR/configs/xmonad/build.sh" "$HOME/.xmonad/build.sh"

info "Deploying system configuration files"
sudo mkdir -p /etc/X11/xorg.conf.d
sudo install -m 0644 "$REPO_DIR/configs/system/intel.conf" /etc/X11/xorg.conf.d/20-intel.conf
sudo install -m 0644 "$REPO_DIR/configs/system/input.conf" /etc/X11/xorg.conf.d/30-input.conf
sudo install -m 0644 "$REPO_DIR/configs/system/tlp.conf" /etc/tlp.conf
sudo install -m 0755 "$REPO_DIR/configs/system/powertweaks.sh" /usr/local/sbin/powertweaks.sh

info "Enabling TLP"
sudo systemctl enable tlp
sudo systemctl start tlp

info "Applying powertweaks"
sudo /usr/local/sbin/powertweaks.sh

info "Compiling xmonad once"
if ! xmonad --recompile; then
  warn "xmonad --recompile failed. Check ~/.xmonad/xmonad.errors or ~/.local/state/xmonad/xmonad.errors"
fi

echo
echo "Installation complete."
echo "Reboot recommended"
echo "Login on TTY"
echo "Run: startx"
echo
echo "Keybinds (Super = Mod4):"
echo "  Super + Enter        -> Kitty"
echo "  Super + d            -> dmenu"
echo "  Super + b            -> Browser via xdg-open"
echo "  Super + Shift + c    -> Recompile xmonad"
echo "  Super + Shift + r    -> Recompile + restart xmonad"
echo "  Super + Shift + q    -> Quit xmonad"
echo "  Brightness keys       -> brightnessctl"
echo "  Audio keys            -> amixer"
