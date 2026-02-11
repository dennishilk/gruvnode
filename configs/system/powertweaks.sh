#!/usr/bin/env bash
set -euo pipefail

if [[ -w /sys/devices/system/cpu/intel_pstate/status ]]; then
  echo active > /sys/devices/system/cpu/intel_pstate/status
fi

if [[ -w /proc/sys/vm/laptop_mode ]]; then
  echo 5 > /proc/sys/vm/laptop_mode
fi

if [[ -w /proc/sys/vm/dirty_writeback_centisecs ]]; then
  echo 3000 > /proc/sys/vm/dirty_writeback_centisecs
fi

cat > /etc/sysctl.d/99-thinkpad-tweaks.conf <<'SYSCTL'
vm.swappiness=10
vm.vfs_cache_pressure=50
vm.dirty_background_ratio=3
vm.dirty_ratio=10
vm.dirty_expire_centisecs=3000
vm.dirty_writeback_centisecs=3000
kernel.nmi_watchdog=0
SYSCTL

sysctl --system >/dev/null
