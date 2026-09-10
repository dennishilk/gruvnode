#!/usr/bin/env bash
set -euo pipefail

PROFILE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd -- "$PROFILE_DIR/.." && pwd)"
BACKUP_ROOT="${XDG_STATE_HOME:-$HOME/.local/state}/gruvnode/backups"
BACKUP_DIR="$BACKUP_ROOT/$(date +%Y%m%d-%H%M%S)"
BACKUP_USED=0

PACKAGES=(
  base-devel
  git
  xorg-server
  xorg-xinit
  xorg-xrandr
  xf86-input-libinput
  mesa
  xmonad
  xmonad-contrib
  xmobar
  rofi
  picom
  feh
  kitty
  fastfetch
  obs-studio
  pipewire
  pipewire-audio
  pipewire-pulse
  wireplumber
  pavucontrol
  brightnessctl
  networkmanager
  network-manager-applet
  bluez
  bluez-utils
  blueman
  v4l-utils
  scrot
  ttf-jetbrains-mono
  polkit-gnome
)

info() {
  printf '\n==> %s\n' "$1"
}

warn() {
  printf 'WARNING: %s\n' "$1" >&2
}

die() {
  printf 'ERROR: %s\n' "$1" >&2
  exit 1
}

require_arch() {
  [[ -r /etc/os-release ]] || die "Cannot identify the operating system: /etc/os-release is missing."
  # shellcheck disable=SC1091
  source /etc/os-release
  [[ "${ID:-}" == "arch" ]] || die "This profile supports Arch Linux only (detected: ${PRETTY_NAME:-unknown})."
}

ensure_regular_user() {
  (( EUID != 0 )) || die "Run this installer as your normal user, not as root. It will use sudo when needed."
  command -v sudo >/dev/null 2>&1 || die "sudo is required. Install/configure sudo, then run this installer again."
}

backup_destination() {
  local dst="$1"
  local rel

  [[ -e "$dst" || -L "$dst" ]] || return 0

  rel="${dst#"$HOME"/}"
  [[ "$rel" != "$dst" ]] || die "Refusing to back up a path outside HOME: $dst"

  mkdir -p "$BACKUP_DIR/$(dirname -- "$rel")"
  cp -a -- "$dst" "$BACKUP_DIR/$rel"
  BACKUP_USED=1
}

install_file() {
  local src="$1"
  local dst="$2"
  local mode="${3:-0644}"

  [[ -f "$src" ]] || die "Missing repository file: $src"

  if [[ -f "$dst" ]] && cmp -s -- "$src" "$dst"; then
    return 0
  fi

  if [[ -e "$dst" || -L "$dst" ]]; then
    backup_destination "$dst"
  fi

  install -D -m "$mode" -- "$src" "$dst"
}

install_wallpaper_link() {
  local src="$REPO_DIR/assets/wallpapers/gruvnode-background.png"
  local dst="$HOME/.local/share/wallpapers/gruvnode-background.png"

  [[ -f "$src" ]] || die "Canonical wallpaper is missing: $src"
  mkdir -p "$(dirname -- "$dst")"

  if [[ -L "$dst" && "$(readlink -f -- "$dst" 2>/dev/null || true)" == "$(readlink -f -- "$src")" ]]; then
    return 0
  fi

  if [[ -e "$dst" || -L "$dst" ]]; then
    backup_destination "$dst"
    rm -f -- "$dst"
  fi

  ln -s -- "$src" "$dst"
}

install_official_packages() {
  info "Updating Arch and installing official packages"
  sudo pacman -Syu --needed --noconfirm "${PACKAGES[@]}"
}

configure_services() {
  info "Enabling laptop services"
  sudo systemctl enable --now NetworkManager.service
  sudo systemctl enable --now bluetooth.service
}

deploy_profile() {
  info "Installing Gruvnode user configuration"

  install_file "$PROFILE_DIR/configs/xmonad/xmonad.hs" "$HOME/.config/xmonad/xmonad.hs"
  install_file "$PROFILE_DIR/configs/xmobar/xmobarrc" "$HOME/.config/xmobar/xmobarrc"
  install_file "$PROFILE_DIR/configs/kitty/kitty.conf" "$HOME/.config/kitty/kitty.conf"
  install_file "$PROFILE_DIR/configs/picom/picom.conf" "$HOME/.config/picom/picom.conf"
  install_file "$PROFILE_DIR/configs/rofi/config.rasi" "$HOME/.config/rofi/config.rasi"
  install_file "$PROFILE_DIR/configs/xinit/xinitrc" "$HOME/.xinitrc" 0755

  install_file "$PROFILE_DIR/scripts/xmobar-battery" "$HOME/.local/bin/gruvnode-xmobar-battery" 0755
  install_file "$PROFILE_DIR/scripts/xmobar-network" "$HOME/.local/bin/gruvnode-xmobar-network" 0755
  install_file "$PROFILE_DIR/scripts/xmobar-volume" "$HOME/.local/bin/gruvnode-xmobar-volume" 0755

  mkdir -p "$HOME/Pictures/Screenshots"
  install_wallpaper_link
}

install_chrome_if_possible() {
  info "Checking Google Chrome"

  if command -v google-chrome-stable >/dev/null 2>&1 || command -v google-chrome >/dev/null 2>&1; then
    printf 'Google Chrome is already installed.\n'
    return 0
  fi

  if ! command -v yay >/dev/null 2>&1; then
    warn "Google Chrome was not installed because yay is not present."
    warn "Gruvnode does not install an AUR helper. Install Chrome manually later, or install it with your existing AUR workflow."
    return 0
  fi

  printf 'yay is available; attempting optional AUR install of google-chrome.\n'
  if ! yay -S --needed google-chrome; then
    warn "Google Chrome installation through yay did not complete. The rest of Gruvnode is installed."
  fi
}

validate_xmonad() {
  info "Validating XMonad configuration"
  if xmonad --recompile; then
    printf 'XMonad configuration compiled successfully.\n'
  else
    die "xmonad --recompile failed. Check the compiler output before starting X."
  fi
}

print_summary() {
  printf '\nGruvnode X1 Carbon profile is installed.\n'
  printf 'Session: TTY login -> startx -> XMonad\n'
  printf 'Wallpaper: %s\n' "$HOME/.local/share/wallpapers/gruvnode-background.png"

  if (( BACKUP_USED )); then
    printf 'Previous configuration was backed up under: %s\n' "$BACKUP_DIR"
  fi

  if ! command -v google-chrome-stable >/dev/null 2>&1 && ! command -v google-chrome >/dev/null 2>&1; then
    printf '\nOptional follow-up: Google Chrome is still not installed.\n'
    if command -v yay >/dev/null 2>&1; then
      printf 'Try: yay -S google-chrome\n'
    else
      printf 'Gruvnode intentionally does not install yay. Use your preferred AUR/manual method.\n'
    fi
  fi

  printf '\nStart the desktop with: startx\n'
  printf 'For future repo updates: git pull && ./install.sh\n'
}

main() {
  require_arch
  ensure_regular_user
  install_official_packages
  configure_services
  deploy_profile
  install_chrome_if_possible
  validate_xmonad
  print_summary
}

main "$@"
