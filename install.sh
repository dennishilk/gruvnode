#!/usr/bin/env bash
set -e

DRY_RUN=false

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
# Dry-Run
# ─────────────────────────────────────────────
if ask "Enable DRY-RUN mode (no changes will be made)?"; then
  DRY_RUN=true
  echo ">>> DRY-RUN ENABLED"
fi

if ! sudo -v; then
  echo "This script requires sudo privileges."
  exit 1
fi

echo "== Debian 13 T480 XMonad Gaming Setup =="

# ─────────────────────────────────────────────
# Repositories
# ─────────────────────────────────────────────
if ask "Enable contrib / non-free / non-free-firmware repositories?"; then
  run "sudo sed -i 's/main/main contrib non-free non-free-firmware/' /etc/apt/sources.list"
  run "sudo apt update"
fi

# ─────────────────────────────────────────────
# Base system
# ─────────────────────────────────────────────
if ask "Install base system (X11, LightDM, Audio, Network)?"; then
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
fi

# ─────────────────────────────────────────────
# XMonad + tools
# ─────────────────────────────────────────────
if ask "Install XMonad, Kitty, dmenu, tools?"; then
  run "sudo apt install --no-install-recommends -y \
    xmonad xmobar suckless-tools \
    ghc libghc-xmonad-dev libghc-xmonad-contrib-dev \
    kitty dmenu feh scrot \
    pamixer brightnessctl"
fi

# ─────────────────────────────────────────────
# Deploy Gruvnode config
# ─────────────────────────────────────────────
if ask "Deploy Gruvnode XMonad config, wallpaper and kitty config?"; then
  if ! $DRY_RUN; then
    mkdir -p ~/.xmonad
    mkdir -p ~/.config/kitty
    mkdir -p ~/Pictures/wallpapers
    mkdir -p ~/Pictures/screenshots

    cp ./xmonad/xmonad.hs ~/.xmonad/xmonad.hs
    cp ./assets/wallpaper/1.png ~/Pictures/wallpapers/1.png
    cp ./kitty/kitty.conf ~/.config/kitty/kitty.conf
  else
    echo "[DRY-RUN] deploy xmonad.hs, wallpaper, kitty.conf"
  fi
fi

# ─────────────────────────────────────────────
# Steam
# ─────────────────────────────────────────────
if ask "Install Steam (enable i386)?"; then
  run "sudo dpkg --add-architecture i386"
  run "sudo apt update"
  run "sudo apt install -y steam"
fi

# ─────────────────────────────────────────────
# Fastfetch
# ─────────────────────────────────────────────
if ask "Install fastfetch?"; then
  run "sudo apt install -y fastfetch"
fi

# ─────────────────────────────────────────────
# zram
# ─────────────────────────────────────────────
if ask "Enable zram?"; then
  run "sudo apt install -y zram-tools"
  run "sudo sed -i 's/#ALGO=.*/ALGO=zstd/' /etc/default/zramswap"
  run "sudo sed -i 's/#PERCENT=.*/PERCENT=25/' /etc/default/zramswap"
  run "sudo systemctl enable zramswap"
fi

# ─────────────────────────────────────────────
# T480 tweaks
# ─────────────────────────────────────────────
# CPU governor via systemd oneshot
run "sudo apt install -y linux-cpupower thermald"

run "sudo tee /etc/systemd/system/cpupower-performance.service <<'EOF'
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

  run "echo 'vm.swappiness=10' | sudo tee /etc/sysctl.d/99-swappiness.conf"

  run "sudo mkdir -p /etc/systemd/journald.conf.d"
  run "echo -e '[Journal]\nSystemMaxUse=200M' | sudo tee /etc/systemd/journald.conf.d/limit.conf"

  run "sudo tee /etc/NetworkManager/conf.d/wifi-powersave.conf <<EOF
[connection]
wifi.powersave = 2
EOF"

  run "sudo systemctl restart NetworkManager"
fi

# ─────────────────────────────────────────────
# Cleanup
# ─────────────────────────────────────────────
if ask "Run cleanup (autoremove & clean)?"; then
  run "sudo apt autoremove -y"
  run "sudo apt clean"
fi

echo
echo "============================================"
$DRY_RUN && echo "DRY-RUN finished — no changes were made."
echo "DONE. Reboot recommended."
echo "LightDM → Session → XMonad"
echo "============================================"
