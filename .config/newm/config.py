from __future__ import annotations
from typing import Callable, Any

import os
import pwd
import time
import logging
import psutil
import subprocess

from newm.layout import Layout
from newm.helper import BacklightManager, WobRunner, PaCtl

from pywm import PYWM_MOD_LOGO, PYWM_MOD_ALT

logger = logging.getLogger(__name__)

debug_windows = False


def run_shell(command: str):
    """Run a shell command asynchronously
    If the shell command doesn’t already end with "&", add it.
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


background = {
    "path": os.environ["HOME"] + "/Pictures/Wallpapers/1561879941868.jpg",
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
    float_apps = ("Rofi",)
    if debug_windows:
        with open("/tmp/newm_windows.txt", "a", encoding="utf-8") as file:
            file.write(str(m_view.app_id))
            file.write("\n")
    m_rules = {}
    if m_view.app_id in blur_apps:
        m_rules.update({"blur": {"radius": 6, "passes": 2}})
    if m_view.app_id in float_apps:
        m_rules.update({"float": True})
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


energy = {"idle_times": [5 * 60, 30 * 60, 24 * 60 * 60]}


leader: str = "L-Spc "


def key_bindings(layout: Layout) -> list[tuple[str, Callable[[], Any]]]:
    return [
        ("L-Return", lambda: os.system("kitty &")),
        (leader + "a r", lambda: run_shell("wofi --show drun")),
        (leader + "a b", lambda: run_shell("firefox")),
        (leader + "a d", lambda: run_shell("discord")),
        (leader + "a e", lambda: run_shell("emacsclient -c")),
        (leader + "w f", layout.toggle_fullscreen),
        (leader + "w v", layout.toggle_focused_view_floating),
        (leader + "w +", lambda: layout.basic_scale(1)),
        (leader + "w -", lambda: layout.basic_scale(-1)),
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
        (leader + "w C-c", lambda: layout.resize_focused_view(-1, 0)),
        (leader + "w C-t", lambda: layout.resize_focused_view(0, 1)),
        (leader + "w C-s", lambda: layout.resize_focused_view(0, -1)),
        (leader + "w C-r", lambda: layout.resize_focused_view(1, 0)),
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
        ("L-", layout.toggle_overview),
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
        ("XF86AudioRaiseVolume", lambda: pactl.volume_adj(5)),
        ("XF86AudioLowerVolume", lambda: pactl.volume_adj(-5)),
        ("XF86AudioMute", pactl.mute),
    ]


def battery_status() -> str:
    battery = psutil.sensors_battery()
    plugged = "+" if battery.power_plugged else "-"
    percent = format(battery.percent, ".1f")
    minutes = battery.secsleft // 60
    remaining = "{0:0>2}:{1:0>2}".format(minutes // 60, minutes % 60)
    return f"{percent}%{plugged} ({remaining})"


def unread_emails() -> str:
    unread = subprocess.run(
        ["mu", "find", "flag:unread AND (maildir:/Inbox OR maildir:/Junk)"],
        capture_output=True,
        text=True,
        check=True,
    ).stdout
    nbr_unread: int = len(str(unread).split("\n"))
    return f"Emails: {nbr_unread}"


def cpu_usage() -> str:
    cpu: str = format(psutil.cpu_percent(interval=None), ".1f")
    return f"CPU: {cpu}%"


def mem_usage() -> str:
    mem: str = format(psutil.virtual_memory().percent, ".1f")
    return f"RAM: {mem}%"


def right_text() -> str:
    return " | ".join([unread_emails(), cpu_usage(), mem_usage(), battery_status()])


def get_time() -> str:
    return time.strftime("%a %Y-%m-%d %X")


def center_text() -> str:
    return f"{get_time()}"


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
            "enabled": True,
            "texts": lambda: [
                "",
                center_text(),
                right_text(),
            ],
        }
    },
    "bottom_bar": {
        "native": {
            "enabled": False,
            "texts": lambda: ["newm", "powered by pywm"],
            "color": (0.5, 0.5, 0.5, 0.1),
        }
    },
}

energy = {"idle_callback": backlight_manager.callback}
