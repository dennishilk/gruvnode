#!/usr/bin/env bash
set -e

# ==========================================================
# Gruvnode – Debian 13 XMonad Setup (ThinkPad T480)
# ==========================================================

DRY_RUN=false
INSTALL_ALL=false

# --------------------------
# Helpers
# --------------------------
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

step() {
  local prompt="$1"
  local func="$2"
  if $INSTALL_ALL || ask "$prompt"; then
    $func
  fi
}

# --------------------------
# Dry-Run handling
# --------------------------
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

echo "== Gruvnode Debian 13 XMonad Setup =="

# ==========================================================
# Functions
# ==========================================================

enable_repos() {
  run "sudo sed -i 's/main/main contrib non-free non-free-firmware/' /etc/apt/sources.list"
  run "sudo apt update"
}

install_base() {
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
}

install_xmonad() {
  run "sudo apt install --no-install-recommends -y \
    xmonad xmobar suckless-tools \
    ghc libghc-xmonad-dev libghc-xmonad-contrib-dev \
    kitty dmenu feh scrot \
    pamixer brightnessctl"
}

deploy_configs() {
  if $DRY_RUN; then
    echo "[DRY-RUN] deploy xmonad.hs, kitty.conf, wallpaper"
    return
  fi

  mkdir -p ~/.xmonad
  mkdir -p ~/.config/kitty
  mkdir -p ~/.config/fish
  mkdir -p ~/Pictures/wallpapers
  mkdir -p ~/Pictures/screenshots

  cp ./xmonad/xmonad.hs ~/.xmonad/xmonad.hs
  cp ./kitty/kitty.conf ~/.config/kitty/kitty.conf
  cp ./assets/wallpaper/1.png ~/Pictures/wallpapers/1.png
}

install_steam() {
  run "sudo dpkg --add-architecture i386"
  run "sudo apt update"
  run "sudo apt install -y steam"
}

install_fish_fastfetch() {
  run "sudo apt install -y fish fastfetch"

  if $DRY_RUN; then
    echo "[DRY-RUN] configure fish + fastfetch"
    return
  fi

  cat > ~/.config/fish/config.fish <<'EOF'
if status is-interactive
    fastfetch
end
EOF

  chsh -s /usr/bin/fish
}

enable_zram() {
  run "sudo apt install -y zram-tools"
  run "sudo sed -i 's/#ALGO=.*/ALGO=zstd/' /etc/default/zramswap"
  run "sudo sed -i 's/#PERCENT=.*/PERCENT=25/' /etc/default/zramswap"
  run "sudo systemctl enable zramswap"
}

t480_tweaks() {
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

  run "sudo tee /etc/NetworkManager/conf.d/wifi-powersave.conf <<'EOF'
[connection]
wifi.powersave = 2
EOF"

  run "sudo systemctl restart NetworkManager"
}

cleanup_system() {
  run "sudo apt autoremove -y"
  run "sudo apt clean"
}

# ==========================================================
# Execution
# ==========================================================

step "Enable contrib / non-free repositories?" enable_repos
step "Install base system (X11, LightDM, Audio, Network)?" install_base
step "Install XMonad, Kitty, dmenu and build dependencies?" install_xmonad
step "Deploy Gruvnode configs (xmonad, kitty, wallpaper)?" deploy_configs
step "Install Steam (enable i386)?" install_steam
step "Install fish shell and fastfetch?" install_fish_fastfetch
step "Enable zram?" enable_zram
step "Apply T480 performance & stability tweaks?" t480_tweaks
step "Run cleanup?" cleanup_system

echo
echo "============================================"
$DRY_RUN && echo "DRY-RUN completed — no changes were made."
echo "DONE. Reboot recommended."
echo "LightDM → Session → XMonad"
echo "============================================"
