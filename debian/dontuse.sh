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

if ask "Enable DRY-RUN mode (no changes will be made)?"; then
  DRY_RUN=true
  echo ">>> DRY-RUN ENABLED — no commands will be executed"
fi

if ! sudo -v; then
  echo "This script requires sudo privileges."
  exit 1
fi

echo "== Debian 13 T480 XMonad Gaming Setup (Interactive) =="

# ─────────────────────────────────────────────
# 1. Enable non-free repositories
# ─────────────────────────────────────────────
if ask "Enable contrib / non-free / non-free-firmware repositories?"; then
  run "sudo sed -i 's/main/main contrib non-free non-free-firmware/' /etc/apt/sources.list"
  run "sudo apt update"
fi

# ─────────────────────────────────────────────
# 2. Base system
# ─────────────────────────────────────────────
if ask "Install base system (X11, LightDM, Audio, NetworkManager)?"; then
  run "sudo apt install --no-install-recommends -y \
    xorg xinit dbus-x11 network-manager \
    lightdm lightdm-gtk-greeter \
    pipewire pipewire-audio wireplumber alsa-utils \
    firmware-misc-nonfree mesa-vulkan-drivers intel-media-va-driver \
    xserver-xorg-video-intel \
    fonts-dejavu fonts-jetbrains-mono \
    git curl unzip wget"

  run "sudo systemctl enable NetworkManager"
  run "sudo systemctl enable lightdm"
fi

# ─────────────────────────────────────────────
# 3. XMonad
# ─────────────────────────────────────────────
if ask "Install XMonad (minimal, X11)?"; then
  run "sudo apt install --no-install-recommends -y \
    xmonad xmobar suckless-tools feh xterm"

  if ! $DRY_RUN; then
    mkdir -p ~/.xmonad
    cat > ~/.xmonad/xmonad.hs <<'EOF'
import XMonad
main :: IO ()
main = xmonad def
EOF
  else
    echo "[DRY-RUN] create ~/.xmonad/xmonad.hs"
  fi
fi

# ─────────────────────────────────────────────
# 4. Steam
# ─────────────────────────────────────────────
if ask "Install Steam?"; then
  run "sudo apt install -y steam"
fi

# ─────────────────────────────────────────────
# 5. Google Chrome
# ─────────────────────────────────────────────
if ask "Install Google Chrome?"; then
  run "cd /tmp && wget -q https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb"
  run "sudo apt install -y /tmp/google-chrome-stable_current_amd64.deb"
fi

# ─────────────────────────────────────────────
# 6. Fastfetch
# ─────────────────────────────────────────────
if ask "Install fastfetch?"; then
  run "sudo apt install -y fastfetch"
fi

# ─────────────────────────────────────────────
# 7. zram
# ─────────────────────────────────────────────
if ask "Enable zram (compressed RAM swap)?"; then
  run "sudo apt install -y zram-tools"
  run "sudo sed -i 's/#ALGO=.*/ALGO=zstd/' /etc/default/zramswap"
  run "sudo sed -i 's/#PERCENT=.*/PERCENT=25/' /etc/default/zramswap"
  run "sudo systemctl enable zramswap"
fi

# ─────────────────────────────────────────────
# 8. T480 tweaks
# ─────────────────────────────────────────────
if ask "Apply T480 performance & stability tweaks?"; then
  run "sudo apt install -y cpufrequtils thermald"
  run "echo 'GOVERNOR=\"performance\"' | sudo tee /etc/default/cpufrequtils"
  run "sudo systemctl restart cpufrequtils"
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
# 9. Cleanup
# ─────────────────────────────────────────────
if ask "Run cleanup (autoremove & apt clean)?"; then
  run "sudo apt autoremove -y"
  run "sudo apt clean"
fi

echo
echo "============================================"
echo " DONE!"
$DRY_RUN && echo " DRY-RUN completed — no changes were made."
echo "============================================"
