from __future__ import annotations
from typing import Callable, Any

import os
import time
import logging
import psutil
import subprocess
import dbus
import docker

docker_client = docker.from_env()

from newm.layout import Layout
from newm.helper import BacklightManager, WobRunner, PaCtl


logger = logging.getLogger(__name__)

debug_windows = False


def run_shell(command: str):
    """Run a shell command asynchronously
    If the shell command doesn't already end with "&", add it.
    """
    if not command.endswith("&"):
        command = command + " &"
    os.system(command)


def command_is_running(command: str):
    for proc in psutil.process_iter():
        try:
            cmdline = proc.cmdline()
        except psutil.NoSuchProcess:
            continue
        if command in cmdline[0]:
            return True
    return False


def on_reconfigure():
    run_shell('notify-send newm "Reloaded config"')


def on_startup():
    run_shell("mpc stop")
    run_shell("dunst")
    run_shell("pactl load-module module-switch-on-connect")
    run_shell("xfce-polkit")
    run_shell("kdeconnectd")
    os.system(
        "systemctl --user import-environment DISPLAY \
        WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
    )
    os.system(
        "dbus-update-activation-environment && \
    dbus-update-activation-environment --systemd DISPLAY \
    WAYLAND_DISPLAY XDG_CURRENT_DESKTOP"
    )


with open("{}/.cache/wallpaper".format(os.environ["HOME"])) as wp_cache:
    background = {
        "path": wp_cache.read().strip(),
        "anim": True,
    }

pywm = {
    "xkb_layout": "fr",
    "xkb_variant": "bepo_afnor",
    "xkb_options": "caps:ctrl_modifier",
    "enable_xwayland": True,
    "natural_scroll": False,
    "focus_follows_mouse": True,
    "encourage_csd": False,
}

outputs = [
    {"name": "eDP-1"},
    {"name": "HDMI-A-1", "pos_x": -2560, "pos_y": 0, "width": 2560, "height": 1080},
]

wob_runner = WobRunner("wob -a bottom -M 100")
backlight_manager = BacklightManager(anim_time=1.0, bar_display=wob_runner)
kbdlight_manager = BacklightManager(
    args="--device='*::kbd_backlight'", anim_time=1.0, bar_display=wob_runner
)


def synchronous_update() -> None:
    backlight_manager.update()
    kbdlight_manager.update()


def rules(m_view):
    blur_apps = ("kitty", "wofi", "emacsclient", "emacs")
    float_apps = ("Rofi", "xfce-polkit")
    nonfloat_apps = ("discord",)
    if debug_windows:
        with open("/tmp/newm_windows.txt", "a", encoding="utf-8") as file:
            file.write(str(m_view.app_id))
            file.write("\n")
    m_rules = {}
    if m_view.app_id in blur_apps:
        m_rules.update({"blur": {"radius": 6, "passes": 2}})
    if m_view.app_id in float_apps:
        m_rules.update({"float": True})
    if m_view.app_id in nonfloat_apps:
        m_rules.update({"float": False})
    return m_rules


pactl = PaCtl(0, wob_runner)

focus = {
    "animate_on_change": True,
    "distance": 4,
    "width": 2,
    "color": "#5E81ACDD",
    "anim_time": 0.1,
}

view = {
    "corner_radius": 8,
    "padding": 10,
    "rules": rules,
}


leader: str = "L-Spc "


def key_bindings(layout: Layout) -> list[tuple[str, Callable[[], Any]]]:
    return [
        ("L-Return", lambda: os.system("kitty &")),
        (leader + "a r", lambda: run_shell("wofi --show drun")),
        (leader + "a b", lambda: run_shell("firefox")),
        (leader + "a d", lambda: run_shell("discord")),
        (leader + "a e", lambda: run_shell("emacsclient -c")),
        (leader + "l", layout.ensure_locked),
        (leader + "w f", layout.toggle_fullscreen),
        (leader + "w v", layout.toggle_focused_view_floating),
        (leader + "w +", lambda: layout.basic_scale(1)),
        (leader + "w -", lambda: layout.basic_scale(-1)),
        ("L-o", layout.move_workspace),
        ("L-O", layout.move_workspace),
        (leader + "w c", lambda: layout.move(-1, 0)),
        (leader + "w t", lambda: layout.move(0, 1)),
        (leader + "w s", lambda: layout.move(0, -1)),
        (leader + "w r", lambda: layout.move(1, 0)),
        (leader + "w n", lambda: layout.move_in_stack(1)),
        ("L-Tab", lambda: layout.move_in_stack(1)),
        (leader + "w p", lambda: layout.move_in_stack(-1)),
        (leader + "w C", lambda: layout.move_focused_view(-1, 0)),
        (leader + "w T", lambda: layout.move_focused_view(0, 1)),
        (leader + "w S", lambda: layout.move_focused_view(0, -1)),
        (leader + "w R", lambda: layout.move_focused_view(1, 0)),
        (leader + "b d", layout.close_focused_view),
        (leader + "q l", lambda: layout.ensure_locked(dim=False)),
        (leader + "q q", layout.terminate),
        (leader + "u", layout.update_config),
        ("L-c", lambda: layout.move(-1, 0)),
        ("L-t", lambda: layout.move(0, 1)),
        ("L-s", lambda: layout.move(0, -1)),
        ("L-r", lambda: layout.move(1, 0)),
        ("L-plus", lambda: layout.basic_scale(-1)),
        ("L-minus", lambda: layout.basic_scale(1)),
        ("L-C", lambda: layout.move_focused_view(-1, 0)),
        ("L-T", lambda: layout.move_focused_view(0, 1)),
        ("L-S", lambda: layout.move_focused_view(0, -1)),
        ("L-R", lambda: layout.move_focused_view(1, 0)),
        (leader + "w r c", lambda: layout.resize_focused_view(-1, 0)),
        (leader + "w r t", lambda: layout.resize_focused_view(0, 1)),
        (leader + "w r s", lambda: layout.resize_focused_view(0, -1)),
        (leader + "w r r", lambda: layout.resize_focused_view(1, 0)),
        ("L-", lambda: layout.toggle_overview(only_active_workspace=True)),
        (
            "XF86MonBrightnessUp",
            lambda: backlight_manager.set(backlight_manager.get() + 0.1),
        ),
        (
            "XF86MonBrightnessDown",
            lambda: backlight_manager.set(backlight_manager.get() - 0.1),
        ),
        (
            "XF86KbdBrightnessUp",
            lambda: kbdlight_manager.set(kbdlight_manager.get() + 0.1),
        ),
        (
            "XF86KbdBrightnessDown",
            lambda: kbdlight_manager.set(kbdlight_manager.get() - 0.1),
        ),
        ("XF86AudioPlay", lambda: run_shell("playerctl play-pause")),
        ("XF86AudioPause", lambda: run_shell("playerctl pause")),
        ("XF86AudioStop", lambda: run_shell("playerctl stop")),
        ("XF86AudioPrev", lambda: run_shell("playerctl previous")),
        ("XF86AudioNext", lambda: run_shell("playerctl next")),
        ("XF86AudioForward", lambda: run_shell("playerctl position +1")),
        ("XF86AudioRewind", lambda: run_shell("playerctl position -1")),
        ("XF86AudioRaiseVolume", lambda: pactl.volume_adj(5)),
        ("XF86AudioLowerVolume", lambda: pactl.volume_adj(-5)),
        ("XF86AudioMute", pactl.mute),
        ("Print", lambda: run_shell("env XDG_CURRENT_DESKTOP=Sway flameshot gui")),
    ]


battery_icons = {
    100: {True: "", False: ""},
    90: {True: "", False: ""},
    80: {True: "", False: ""},
    70: {True: "", False: ""},
    60: {True: "", False: ""},
    50: {True: "", False: ""},
    40: {True: "", False: ""},
    30: {True: "", False: ""},
    20: {True: "", False: ""},
    10: {True: "", False: ""},
    0: {True: "", False: ""},
}


def battery_status() -> str:
    battery = psutil.sensors_battery()
    percent = format(battery.percent, ".1f")
    minutes = battery.secsleft // 60
    remaining = "{0:0>2}:{1:0>2}".format(minutes // 60, minutes % 60)
    icon = battery_icons[(int(float(percent)) // 10) * 10][battery.power_plugged]
    return f"{icon} {percent}% ({remaining})"


def unread_emails() -> str:
    unread = subprocess.run(
        ["mu", "find", "flag:unread AND (maildir:/Inbox OR maildir:/Junk)"],
        capture_output=True,
        text=True,
        check=True,
    ).stdout
    nbr_unread: int = len(str(unread).strip().split("\n"))
    return f" {nbr_unread}"


def cpu_usage() -> str:
    cpu: str = format(psutil.cpu_percent(interval=1), ".1f")
    return f" {cpu}%"


def mem_usage() -> str:
    mem: str = format(psutil.virtual_memory().percent, ".1f")
    return f" {mem}%"


def get_bluetooth_devices() -> str:
    import xml.etree.ElementTree as ET

    bus = dbus.SystemBus()
    service_name = "org.bluez"

    # Verify if bluetooth is turned on
    proxy = bus.get_object(service_name, "/org/bluez/hci0")
    props = dbus.Interface(proxy, "org.freedesktop.DBus.Properties")
    if not props.Get("org.bluez.Adapter1", "Powered"):
        return ""

    # Grab all known devices
    bt_intro_iface = dbus.Interface(proxy, "org.freedesktop.DBus.Introspectable")
    bt_intro = str(bt_intro_iface.Introspect())
    root_node = ET.fromstring(bt_intro)
    known_devices = [n.get("name") for n in root_node.findall("node")]

    # Check if all devices are connected
    counter = 0
    for device in known_devices:
        object_path = f"/org/bluez/hci0/{device}"
        proxy = bus.get_object(service_name, object_path)
        props = dbus.Interface(proxy, "org.freedesktop.DBus.Properties")
        if props.Get("org.bluez.Device1", "Connected"):
            counter = counter + 1
    return f" {counter}"


def get_wifi_name(iface) -> str:
    bus = dbus.SystemBus()
    service_name = "org.freedesktop.NetworkManager"
    proxy = bus.get_object(service_name, "/org/freedesktop/NetworkManager/Settings")

    # Get the device object path based on interface name
    proxy = bus.get_object(service_name, "/org/freedesktop/NetworkManager")
    nm = dbus.Interface(proxy, "org.freedesktop.NetworkManager")
    devpath = nm.GetDeviceByIpIface(iface)

    # Get a proxy to the wifi device and get the active access point's object path
    proxy = bus.get_object(service_name, devpath)
    props = dbus.Interface(proxy, "org.freedesktop.DBus.Properties")
    active_ap_path = props.Get(
        "org.freedesktop.NetworkManager.Device.Wireless", "ActiveAccessPoint"
    )
    if active_ap_path == "/":
        return ""

    # Get the active access point's SSID and BSSID
    ap_proxy = bus.get_object(service_name, active_ap_path)
    ap_props = dbus.Interface(ap_proxy, "org.freedesktop.DBus.Properties")
    raw_ssid = ap_props.Get("org.freedesktop.NetworkManager.AccessPoint", "Ssid")
    ssid = b"".join([bytes([v]) for v in raw_ssid]).decode("utf-8")
    return ssid


def get_local_address(wifi_iface, eth_iface) -> str:
    if_addrs = psutil.net_if_addrs()
    wifi = if_addrs.get(wifi_iface)
    eth = if_addrs.get(eth_iface)
    wifi_addr = wifi[0].address if wifi else ""
    eth_addr = eth[0].address if eth else ""
    wifi_addr = wifi_addr if not ":" in wifi_addr else ""
    eth_addr = eth_addr if not ":" in eth_addr else ""
    return wifi_addr or eth_addr


def get_network() -> str:
    wifi_interface = "wlp8s0"
    eth_interface = "enp9s0f1"
    network_name = get_wifi_name(wifi_interface)
    network_name = f"  {network_name}" if network_name else "  eth"
    addr = get_local_address(wifi_interface, eth_interface)
    return f"{network_name} ({addr})" if addr else "睊 disconnected"


def get_playerctl_bus():
    bus = dbus.SessionBus()
    service_name = "org.mpris.MediaPlayer2.playerctld"
    service_props = "org.mpris.MediaPlayer2.Player"
    proxy = bus.get_object(service_name, "/org/mpris/MediaPlayer2")
    return (service_props, proxy)


def get_currently_playing():
    (service_props, proxy) = get_playerctl_bus()
    props = dbus.Interface(proxy, "org.freedesktop.DBus.Properties")
    metadata = props.Get(service_props, "Metadata")
    status = str(props.Get(service_props, "PlaybackStatus"))
    if status != "Playing":
        return ""
    artist = ", ".join(metadata.get("xesam:artist"))
    title = metadata.get("xesam:title")
    return f" {artist} — {title}"


def display_docker() -> str:
    containers = docker_client.containers.list(sparse=True)
    return f"  {len(containers)}"


def get_time() -> str:
    return time.strftime("%a %Y-%m-%d %X")


def bar_text() -> str:
    return " | ".join(
        [
            f
            for f in [
                get_currently_playing(),
                # get_network(),
                display_docker(),
                # get_bluetooth_devices(),
                unread_emails(),
                cpu_usage(),
                mem_usage(),
                get_time(),
                battery_status(),
            ]
            if f
        ]
    )


def max_width(strings: list[str]) -> int:
    r_max_width: int = 0
    for s in strings:
        if len(s) > r_max_width:
            r_max_width = len(s)
    return r_max_width


panels = {
    "lock": {
        "cmd": "kitty -e newm-panel-basic lock",
    },
    "launcher": {"cmd": "kitty -e newm-panel-basic launcher"},
    "top_bar": {
        "native": {
            "font": "JetBrainsMono Nerd Font",
            "enabled": True,
            "texts": lambda: [
                bar_text(),
            ],
        },
    },
    "bottom_bar": {
        "native": {
            "enabled": False,
            "texts": lambda: ["newm", "powered by pywm"],
            "color": (0.5, 0.5, 0.5, 0.1),
        }
    },
}

energy = {
    "idle_callback": backlight_manager.callback,
    "idle_times": [5 * 60, 30 * 60, 24 * 60 * 60],
}
