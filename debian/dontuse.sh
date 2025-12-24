#!/usr/bin/env bash
set -e

if ! sudo -v; then
  echo "This script requires sudo privileges."
  exit 1
fi

echo "== Debian 13 Minimal → XMonad Gaming Setup =="

# ─────────────────────────────────────────────
# 1. Enable non-free repositories
# ─────────────────────────────────────────────
echo "[1/7] Enable non-free repositories"

sudo sed -i 's/main/main contrib non-free non-free-firmware/' /etc/apt/sources.list
sudo apt update

# ─────────────────────────────────────────────
# 2. Base system (X11, Login, Audio, Network)
# ─────────────────────────────────────────────
echo "[2/7] Install base system"

sudo apt install --no-install-recommends -y \
  xorg \
  xinit \
  dbus-x11 \
  network-manager \
  lightdm \
  lightdm-gtk-greeter \
  pipewire \
  pipewire-audio \
  wireplumber \
  alsa-utils \
  firmware-misc-nonfree \
  mesa-vulkan-drivers \
  intel-media-va-driver \
  fonts-dejavu \
  fonts-jetbrains-mono \
  git \
  curl \
  unzip \
  wget

sudo systemctl enable NetworkManager
sudo systemctl enable lightdm

# ─────────────────────────────────────────────
# 3. XMonad (minimal)
# ─────────────────────────────────────────────
echo "[3/7] Install XMonad"

sudo apt install --no-install-recommends -y \
  xmonad \
  xmobar \
  suckless-tools \
  feh \
  xterm

mkdir -p ~/.xmonad

cat > ~/.xmonad/xmonad.hs <<'EOF'
import XMonad

main :: IO ()
main = xmonad def
EOF

# ─────────────────────────────────────────────
# 4. Steam
# ─────────────────────────────────────────────
echo "[4/7] Install Steam"

sudo apt install -y steam

# ─────────────────────────────────────────────
# 5. Google Chrome
# ─────────────────────────────────────────────
echo "[5/7] Install Google Chrome"

cd /tmp
if wget -q https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb; then
  sudo apt install -y ./google-chrome-stable_current_amd64.deb
else
  echo "Chrome download failed – skipping."
fi

# ─────────────────────────────────────────────
# 6. Cleanup
# ─────────────────────────────────────────────
echo "[6/7] Cleanup"

sudo apt autoremove -y
sudo apt clean

# ─────────────────────────────────────────────
# 7. Done
# ─────────────────────────────────────────────
echo "[7/7] DONE"
echo
echo "Reboot now."
echo "At LightDM login: select session → XMonad"
echo "Steam → Settings → Enable Proton Experimental"
echo
