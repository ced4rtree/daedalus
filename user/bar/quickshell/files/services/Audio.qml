pragma Singleton

import Quickshell
import Quickshell.Hyprland
import Quickshell.Services.Pipewire

Singleton {
    id: root

    readonly property PwNode sink: Pipewire.defaultAudioSink
    readonly property PwNode source: Pipewire.defaultAudioSource

    readonly property bool muted: sink?.audio?.muted ?? false
    readonly property real volume: sink?.audio?.volume ?? 0

    property bool holdActive: false

    GlobalShortcut {
        name: "volumeUp"
        onPressed: increaseVolume()
    }

    GlobalShortcut {
        name: "volumeDown"
        onPressed: decreaseVolume()
    }

    ElapsedTimer {
        id: volumeHoldTimer
    }

    function increaseVolume() {
        root.setVolume(root.addWithLimit(root.volume, 0.05, 1));
    }

    function decreaseVolume() {
        root.setVolume(root.subtractWithLimit(root.volume, 0.05, 0));
    }

    function addWithLimit(lhs: real, rhs: real, limit: real): real {
        if (lhs + rhs > limit) {
            return limit
        } else {
            return lhs + rhs
        }
    }

    function subtractWithLimit(lhs: real, rhs: real, limit: real): real {
        if (lhs - rhs < limit) {
            return limit
        } else {
            return lhs - rhs
        }
    }

    function setVolume(volume: real): void {
        if (sink?.ready && sink?.audio) {
            sink.audio.muted = false;
            sink.audio.volume = volume;
        }
    }

    PwObjectTracker {
        objects: [Pipewire.defaultAudioSink, Pipewire.defaultAudioSource]
    }
}
