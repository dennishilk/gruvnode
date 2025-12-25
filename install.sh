#!/usr/bin/env bash
set -e

DRY_RUN=false
INSTALL_ALL=false

ask() {
  read -rp "$1 [y/N]: " answer
  [[ "$answer" =~ ^[Yy]$ ]]
}

run() {
  if $DRY_RUN; then
    echo "[DRY-RUN] $*"
  else
    eval "$@"
  fi
}

# ─────────────────────────────────────────────
# DRY RUN
# ─────────────────────────────────────────────
if ask "Enable DRY-RUN mode (no changes will be made)?"; then
  DRY_RUN=true
  echo ">>> DRY-RUN ENABLED"
fi

if $DRY_RUN; then
  if ask "After DRY-RUN, install EVERYTHING automatically (INSTALL ALL)?"; then
    INSTALL_ALL=true
  fi
fi

if ! sudo -v; then
  echo "This script requires sudo privileges."
  exit 1
fi

echo "== Debian 13 T480 XMonad Gruvnode Setup =="

# Helper for conditional steps
step() {
  local prompt="$1"
  shift
  if $INSTALL_ALL || ask "$prompt"; then
    "$@"
  fi
}

# ─────────────────────────────────────────────
# Repositories
# ─────────────────────────────────────────────
step "Enable contrib / non-free / firmware repositories?" bash -c '
  run "sudo sed -i '\''s/main/main contrib non-free non-free-firmware/'\'' /etc/apt/sources.list"
  run "sudo apt update"
'

# ─────────────────────────────────────────────
# Base system
# ─────────────────────────────────────────────
step "Install base system (X11, LightDM, Audio, Network)?" bash -c '
  run "sudo apt install --no-install-recommends -y \
    xorg xinit dbus-x11 \
    network-manager \
    lightdm lightdm-gtk-greeter \
    pipewire pipewire-audio wireplumber alsa-utils \
    firmware-misc-nonfree \
    mesa-vulkan-drivers intel-media-va-driver \
    xserver-xorg-video-intel \
    fonts-dejavu fonts-jetbrains-mono \
    git curl unzip wget"

  run "sudo systemctl enable NetworkManager"
  run "sudo systemctl enable lightdm"
'

# ─────────────────────────────────────────────
# XMonad + tools
# ─────────────────────────────────────────────
step "Install XMonad, Kitty, dmenu, build deps?" bash -c '
  run "sudo apt install --no-install-recommends -y \
    xmonad xmobar suckless-tools \
    ghc libghc-xmonad-dev libghc-xmonad-contrib-dev \
    kitty dmenu feh scrot \
    pamixer brightnessctl"
'

# ─────────────────────────────────────────────
# Deploy Gruvnode configs
# ─────────────────────────────────────────────
step "Deploy Gruvnode XMonad / Kitty / Wallpaper configs?" bash -c '
  if ! $DRY_RUN; then
    mkdir -p ~/.xmonad ~/.config/kitty ~/Pictures/wallpapers ~/Pictures/screenshots
    cp ./xmonad/xmonad.hs ~/.xmonad/xmonad.hs
    cp ./assets/wallpaper/1.png ~/Pictures/wallpapers/1.png
    cp ./kitty/kitty.conf ~/.config/kitty/kitty.conf
  else
    echo "[DRY-RUN] copy xmonad.hs, kitty.conf, wallpaper"
  fi
'

# ─────────────────────────────────────────────
# Steam
# ─────────────────────────────────────────────
step "Install Steam (enable i386)?" bash -c '
  run "sudo dpkg --add-architecture i386"
  run "sudo apt update"
  run "sudo apt install -y steam"
'

# ─────────────────────────────────────────────
# Fish + Fastfetch
# ─────────────────────────────────────────────
step "Install fish shell and fastfetch?" bash -c '
  run "sudo apt install -y fish fastfetch"

  if ! $DRY_RUN; then
    mkdir -p ~/.config/fish
    cat > ~/.config/fish/config.fish <<EOF
if status is-interactive
    fastfetch
end
EOF
    chsh -s /usr/bin/fish
  else
    echo "[DRY-RUN] configure fish + fastfetch"
  fi
'

# ─────────────────────────────────────────────
# ZRAM
# ─────────────────────────────────────────────
step "Enable zram?" bash -c '
  run "sudo apt install -y zram-tools"
  run "sudo sed -i '\''s/#ALGO=.*/ALGO=zstd/'\'' /etc/default/zramswap"
  run "sudo sed -i '\''s/#PERCENT=.*/PERCENT=25/'\'' /etc/default/zramswap"
  run "sudo systemctl enable zramswap"
'

# ─────────────────────────────────────────────
# T480 performance tweaks
# ─────────────────────────────────────────────
step "Apply T480 performance & stability tweaks?" bash -c '
  run "sudo apt install -y linux-cpupower thermald"

  run "sudo tee /etc/systemd/system/cpupower-performance.service <<EOF
[Unit]
Description=Set CPU governor to performance
After=multi-user.target

[Service]
Type=oneshot
ExecStart=/usr/bin/cpupower frequency-set -g performance
RemainAfterExit=yes

[Install]
WantedBy=multi-user.target
EOF"

  run "sudo systemctl daemon-reload"
  run "sudo systemctl enable cpupower-performance.service"
  run "sudo systemctl start cpupower-performance.service"
  run "sudo systemctl enable thermald"

  run "echo '\''vm.swappiness=10'\'' | sudo tee /etc/sysctl.d/99-swappiness.conf"

  run "sudo mkdir -p /etc/systemd/journald.conf.d"
  run "echo -e '\''[Journal]\nSystemMaxUse=200M'\'' | sudo tee /etc/systemd/journald.conf.d/limit.conf"

  run "sudo tee /etc/NetworkManager/conf.d/wifi-powersave.conf <<EOF
[connection]
wifi.powersave = 2
EOF"

  run "sudo systemctl restart NetworkManager"
'

# ─────────────────────────────────────────────
# Cleanup
# ─────────────────────────────────────────────
step "Run cleanup?" bash -c '
  run "sudo apt autoremove -y"
  run "sudo apt clean"
'

echo
echo "============================================"
$DRY_RUN && echo "DRY-RUN completed — no changes were made."
echo "DONE. Reboot recommended."
echo "LightDM → Session → XMonad"
echo "============================================"
