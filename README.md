# Gruvnode

🇩🇪 **Minimaler Debian-13-XMonad-Setup für das ThinkPad T480**  
🇬🇧 **Minimal Debian 13 XMonad setup for the ThinkPad T480**

---

## 🇩🇪 Beschreibung

**Gruvnode** ist ein bewusst minimal gehaltener **Debian-13-Setup für das ThinkPad T480**, aufgebaut rund um **XMonad (X11)**.  
Der Fokus liegt auf **Stabilität**, **niedrigem Ressourcenverbrauch** und **vorhersagbarem Verhalten** – auch wenn das System längere Zeit nicht genutzt wird.

Dieses Repository dient außerdem als **persönliches System-Backup und Referenz**.  
Es dokumentiert, wie das System installiert, konfiguriert und reproduzierbar neu aufgebaut werden kann.

---

## 🇬🇧 Description

**Gruvnode** is a deliberately minimal **Debian 13 setup for the ThinkPad T480**, built around **XMonad (X11)**.  
The focus is on **stability**, **low resource usage**, and **predictable behavior**, even after long periods of inactivity.

This repository also serves as a **personal system backup and reference**.  
It documents how the system is installed, configured, and rebuilt from scratch.

---

## 🎯 Goals / Ziele

- Debian 13 minimal (no desktop environment)
- XMonad on X11 (no Wayland)
- Optimized for Intel iGPU (i5-8250U)
- Low idle RAM usage (~300–400 MB)
- Stable frametimes for lightweight gaming (e.g. WoW Classic)
- Simple, documented and reproducible setup
- No visual bloat, no unnecessary background services

---

## 🧠 System Philosophy / System-Philosophie

🇩🇪  
Dieses Repository ist **keine allgemeine Dotfiles-Sammlung**.  
Es bildet einen **konkreten Systemzustand** ab:

- Installationsskripte
- Hardware-spezifische Entscheidungen
- Minimale, bewusste Konfiguration

Ziel ist es, das System auf derselben Hardware **jederzeit reproduzierbar** neu aufzusetzen.

🇬🇧  
This repository is **not a generic dotfiles collection**.  
It represents a **documented system state**:

- installation scripts
- hardware-specific decisions
- minimal and intentional configuration

The goal is to **rebuild the system reliably** on the same hardware at any time.

---
# XMonad Keybindings — Gruvnode

🇩🇪 Kurzübersicht der wichtigsten Tastenkombinationen  
🇬🇧 Quick overview of the most important keybindings

**Mod key:** `Super` (Windows key)

---

## 🚀 Applications / Anwendungen

| Key | Action |
|----|--------|
| `Super + Enter` | Terminal (Kitty) |
| `Super + D` | dmenu (Application launcher) |
| `Super + B` | Google Chrome |

---

## 🪟 Window Management / Fensterverwaltung

| Key | Action |
|----|--------|
| `Super + Q` | Close focused window |
| `Super + Shift + Q` | Exit XMonad (logout) |

---

## 🧭 Focus / Fokus

| Key | Action |
|----|--------|
| `Super + J` | Focus next window |
| `Super + K` | Focus previous window |
| `Super + M` | Focus master window |

---

## 🔀 Swap Windows / Fenster tauschen

| Key | Action |
|----|--------|
| `Super + Shift + J` | Swap window down |
| `Super + Shift + K` | Swap window up |

---

## 📐 Layout & Resize / Layout & Größe

| Key | Action |
|----|--------|
| `Super + Space` | Cycle layouts |
| `Super + H` | Shrink window |
| `Super + L` | Expand window |

---

## 🔄 XMonad Control

| Key | Action |
|----|--------|
| `Super + Shift + R` | Recompile & restart XMonad |

---

## 📸 Screenshots

| Key | Action |
|----|--------|
| `Print` | Screenshot → `~/Pictures/screenshots/` |

---

## 🔊 Volume / Lautstärke

| Key | Action |
|----|--------|
| `XF86AudioRaiseVolume` | Volume +5 % |
| `XF86AudioLowerVolume` | Volume −5 % |
| `XF86AudioMute` | Toggle mute |

---

## 🔆 Brightness / Helligkeit

| Key | Action |
|----|--------|
| `XF86MonBrightnessUp` | Brightness +5 % |
| `XF86MonBrightnessDown` | Brightness −5 % |

---

## 📝 Notes / Hinweise

🇩🇪  
- Klassische, minimalistische XMonad-Keybindings  
- Keine Wayland- oder Compositor-Abhängigkeiten  
- Optimiert für Stabilität, geringe Latenz und lange Sessions  

🇬🇧  
- Classic, minimal XMonad keybindings  
- No Wayland or compositor dependencies  
- Optimized for stability, low latency and long sessions  

---

**System:** Debian 13 + XMonad  
**Hardware target:** ThinkPad T480 (Gruvnode)



🖥️ Hardware Target / Zielhardware

Lenovo ThinkPad T480

Intel i5-8250U

Intel UHD Graphics 620

32 GB RAM

Improved cooling (dGPU heatsink + graphite thermal pad)

⚠️ Disclaimer

🇩🇪
Dieses Setup ist hardware-spezifisch und primär als persönliche Referenz gedacht.
Es gibt keinen Anspruch auf universelle Einsetzbarkeit.

🇬🇧
This setup is hardware-specific and primarily intended as a personal reference.
There is no guarantee of suitability for other systems.


## 🧱 Repository Structure / Struktur

```text
gruvnode/
├── README.md
│
├── install.sh
│
├── xmonad/
│   ├── xmonad.hs
│   └── README.md
│
├── kitty/
│   └── kitty.conf
│
├── assets/
│   └── wallpaper/
│       └── 1.png
│
├── docs/
│   ├── hardware.md
│   ├── tweaks.md
│   └── troubleshooting.md
│
└── wow-classic/
    ├── settings.md
    └── addons.md

