#!/usr/bin/env bash
set -euo pipefail

# Apply safe VM writeback settings for laptop workloads.
cat > /etc/sysctl.d/99-gruvnode-powertweaks.conf <<'SYSCTL'
# Writeback less frequently to let disks sleep more.
vm.dirty_writeback_centisecs=3000
# Start background writeback when 3% of RAM is dirty.
vm.dirty_background_ratio=3
# Force writeback when 10% of RAM is dirty.
vm.dirty_ratio=10
# Keep dirty pages up to 30s before expiry.
vm.dirty_expire_centisecs=3000
# Reduce swap aggressiveness.
vm.swappiness=10
# Keep inode/dentry cache around longer.
vm.vfs_cache_pressure=50
# Disable NMI watchdog for lower idle overhead.
kernel.nmi_watchdog=0
SYSCTL

# Laptop mode defers disk writes (if supported by current kernel settings).
if [[ -w /proc/sys/vm/laptop_mode ]]; then
  echo 5 > /proc/sys/vm/laptop_mode
fi

sysctl --system >/dev/null
