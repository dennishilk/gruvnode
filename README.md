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
├── debian/
│   └── debian-xmonad-gaming.sh
├── wow-classic/
│   ├── settings.md
│   └── addons.md
├── docs/
│   └── thermals-and-power.md
└── README.md

