#!/usr/bin/env bash
set -euo pipefail

PROFILE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
REPO_DIR="$(cd -- "$PROFILE_DIR/.." && pwd)"

fail() {
  printf 'FAIL: %s\n' "$1" >&2
  exit 1
}

pass() {
  printf 'PASS: %s\n' "$1"
}

required_files=(
  "$PROFILE_DIR/install.sh"
  "$PROFILE_DIR/configs/xmonad/xmonad.hs"
  "$PROFILE_DIR/configs/xmobar/xmobarrc"
  "$PROFILE_DIR/configs/kitty/kitty.conf"
  "$PROFILE_DIR/configs/picom/picom.conf"
  "$PROFILE_DIR/configs/rofi/config.rasi"
  "$PROFILE_DIR/configs/xinit/xinitrc"
  "$PROFILE_DIR/scripts/xmobar-battery"
  "$PROFILE_DIR/scripts/xmobar-network"
  "$PROFILE_DIR/scripts/xmobar-volume"
  "$REPO_DIR/assets/wallpapers/gruvnode-background.png"
)

for file in "${required_files[@]}"; do
  [[ -f "$file" ]] || fail "missing required file: $file"
done
pass "required files exist"

bash -n "$PROFILE_DIR/install.sh"
bash -n "$PROFILE_DIR/scripts/xmobar-battery"
bash -n "$PROFILE_DIR/scripts/xmobar-network"
bash -n "$PROFILE_DIR/scripts/xmobar-volume"
pass "shell syntax"

package_checks=(
  xorg-server
  xorg-xinit
  xorg-xrandr
  xf86-input-libinput
  xmonad
  xmonad-contrib
  xmobar
  rofi
  picom
  feh
  kitty
  fastfetch
  obs-studio
  thunar
  mousepad
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
)

for package in "${package_checks[@]}"; do
  grep -Eq "^[[:space:]]+${package}[[:space:]]*$" "$PROFILE_DIR/install.sh" \
    || fail "missing official package from installer: $package"
done
pass "required Arch package set"

xmonad_checks=(
  'Tall 1'
  'noBorders Full'
  'smartBorders'
  'spacing 6'
  'isFullscreen --> doFullFloat'
  'isDialog --> doCenterFloat'
  'spawnOnce "feh --no-fehbg --bg-fill'
  'spawnOnce "picom --config'
  'statusBarProp'
  'withSB myStatusBar'
)

for check in "${xmonad_checks[@]}"; do
  grep -Fq "$check" "$PROFILE_DIR/configs/xmonad/xmonad.hs" \
    || fail "missing XMonad behavior: $check"
done

grep -Fq 'Run XMonadLog' "$PROFILE_DIR/configs/xmobar/xmobarrc" \
  || fail "xmobar is not using property-based XMonad logging"
if grep -Fq 'Run StdinReader' "$PROFILE_DIR/configs/xmobar/xmobarrc"; then
  fail "xmobar should not use the pipe-based StdinReader path"
fi
pass "XMonad layout/manage/startup/status-bar behavior"

grep -Fxq 'background_opacity 0.8' "$PROFILE_DIR/configs/kitty/kitty.conf" \
  || fail "Kitty opacity must be exactly 0.8"
pass "Kitty opacity is exactly 0.8"

key_checks=(
  '("M-<Return>", spawn myTerminal)'
  '("M-d", spawn "rofi -show drun")'
  '("M-b", spawn "google-chrome-stable")'
  '("M-o", spawn "obs")'
  '("M-t", spawn "thunar")'
  '("M-q", kill)'
  '("M-S-q", io exitSuccess)'
  '("M-<Space>", sendMessage NextLayout)'
  '("M-j", windows W.focusDown)'
  '("M-k", windows W.focusUp)'
  '("M-m", windows W.focusMaster)'
  '("M-S-j", windows W.swapDown)'
  '("M-S-k", windows W.swapUp)'
  '("M-h", sendMessage Shrink)'
  '("M-l", sendMessage Expand)'
  '("M-S-r", spawn "xmonad --recompile && xmonad --restart")'
  '("<Print>", spawn'
  '("<XF86AudioRaiseVolume>", spawn'
  '("<XF86AudioLowerVolume>", spawn'
  '("<XF86AudioMute>", spawn'
  '("<XF86MonBrightnessUp>", spawn'
  '("<XF86MonBrightnessDown>", spawn'
)

for binding in "${key_checks[@]}"; do
  grep -Fq "$binding" "$PROFILE_DIR/configs/xmonad/xmonad.hs" \
    || fail "missing XMonad binding: $binding"
done

if grep -Fq 'mousepad' "$PROFILE_DIR/configs/xmonad/xmonad.hs"; then
  fail "Mousepad should not have a dedicated XMonad binding"
fi
pass "Gruvnode keybindings and Mousepad no-binding rule"

grep -Fq 'gruvnode-background.png' "$PROFILE_DIR/configs/xmonad/xmonad.hs" \
  || fail "XMonad does not reference the canonical wallpaper"
grep -Fq 'assets/wallpapers/gruvnode-background.png' "$PROFILE_DIR/install.sh" \
  || fail "installer does not reference the shared canonical wallpaper"
pass "canonical wallpaper path"

if grep -Eq '(^|[[:space:]])(tlp|xf86-video-intel)([[:space:]]|$)' "$PROFILE_DIR/install.sh"; then
  fail "T480-era power/video package leaked into the X1 package set"
fi
if find "$PROFILE_DIR" -type f \( -name 'intel.conf' -o -name 'tlp.conf' -o -name 'powertweaks.sh' \) | grep -q .; then
  fail "T480-specific system tuning file exists in X1 profile"
fi
pass "no T480 system tuning in X1 profile"

if grep -Eq '^[[:space:]]+yay[[:space:]]*$' "$PROFILE_DIR/install.sh"; then
  fail "yay must not be treated as an official pacman package"
fi
grep -Fq 'https://aur.archlinux.org/yay.git' "$PROFILE_DIR/install.sh" \
  || fail "installer does not bootstrap yay from the AUR"
grep -Fq 'makepkg -si --noconfirm' "$PROFILE_DIR/install.sh" \
  || fail "installer does not build/install yay with makepkg"
grep -Fq 'yay -S --needed --noconfirm google-chrome' "$PROFILE_DIR/install.sh" \
  || fail "installer does not install Google Chrome through yay"
pass "yay bootstrap and Google Chrome installation"

if command -v shellcheck >/dev/null 2>&1; then
  shellcheck \
    "$PROFILE_DIR/install.sh" \
    "$PROFILE_DIR/scripts/xmobar-battery" \
    "$PROFILE_DIR/scripts/xmobar-network" \
    "$PROFILE_DIR/scripts/xmobar-volume"
  pass "shellcheck"
else
  printf 'SKIP: shellcheck is not installed\n'
fi

if command -v ghc >/dev/null 2>&1 \
  && ghc-pkg latest xmonad >/dev/null 2>&1 \
  && ghc-pkg latest xmonad-contrib >/dev/null 2>&1; then
  ghc -fno-code -v0 "$PROFILE_DIR/configs/xmonad/xmonad.hs"
  pass "XMonad Haskell compile check"
else
  printf 'SKIP: XMonad/GHC compile environment is not available\n'
fi

if git -C "$REPO_DIR" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  git -C "$REPO_DIR" diff --check
  pass "git diff --check"
else
  printf 'SKIP: git worktree is not available for diff --check\n'
fi
