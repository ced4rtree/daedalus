import "root:/widgets"
import "root:/services"
import Quickshell
import Quickshell.Hyprland
import Quickshell.Io

Scope {
    id: root

    property bool launcherInterrupted

    GlobalShortcut {
        name: "session"
        description: "Toggle session menu"
        onPressed: {
            const visibilities = Visibilities.getForActive();
            visibilities.session = !visibilities.session;
        }
    }

    GlobalShortcut {
        name: "launcher"
        description: "Toggle launcher"
        onPressed: root.launcherInterrupted = false
        onReleased: {
            if (!root.launcherInterrupted) {
                const visibilities = Visibilities.getForActive();
                visibilities.launcher = !visibilities.launcher;
            }
            root.launcherInterrupted = false;
        }
    }

    GlobalShortcut {
        name: "launcherInterrupt"
        description: "Interrupt launcher keybind"
        onPressed: root.launcherInterrupted = true
    }

    IpcHandler {
        target: "drawers"

        function toggle(drawer: string): void {
            if (list().split("\n").includes(drawer)) {
                const visibilities = Visibilities.getForActive();
                visibilities[drawer] = !visibilities[drawer];
            } else {
                console.warn(`[IPC] Drawer "${drawer}" does not exist`);
            }
        }

        function list(): string {
            const visibilities = Visibilities.getForActive();
            return Object.keys(visibilities).filter(k => typeof visibilities[k] === "boolean").join("\n");
        }
    }
}
