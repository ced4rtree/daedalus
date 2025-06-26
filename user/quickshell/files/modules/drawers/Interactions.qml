import "root:/services"
import "root:/config"
import "root:/modules/bar"
import "root:/modules/bar/popouts" as BarPopouts
import "root:/modules/osd" as Osd
import Quickshell
import QtQuick

MouseArea {
    id: root

    required property ShellScreen screen
    required property BarPopouts.Wrapper popouts
    required property PersistentProperties visibilities
    required property Panels panels
    required property Bar bar

    property bool osdHovered
    property point dragStart
    property bool osdShortcutActive

    function withinPanelHeight(panel: Item, x: real, y: real): bool {
        const panelY = Config.border.thickness + panel.y;
        return y >= panelY - Config.border.rounding && y <= panelY + panel.height + Config.border.rounding;
    }

    function withinPanelWidth(panel: Item, x: real, y: real): bool {
        const panelX = Config.border.thickness + panel.x;
        return x >= panelX - Config.border.rounding && x <= panelX + panel.width + Config.border.rounding;
    }

    function inRightPanel(panel: Item, x: real, y: real): bool {
        return x > panel.x && withinPanelHeight(panel, x, y);
    }

    function inBottomPanel(panel: Item, x: real, y: real): bool {
        const topY = panel.y + Config.border.thickness;
        const bottomY = topY + bar.height + panel.height;
        
        return y > topY && y < bottomY
    }

    anchors.fill: parent
    hoverEnabled: true

    onPressed: event => dragStart = Qt.point(event.x, event.y)
    onContainsMouseChanged: {
        if (!containsMouse) {
            // Only hide if not activated by shortcut
            if (!osdShortcutActive) {
                visibilities.osd = false;
                osdHovered = false;
            }
            popouts.hasCurrent = false;
        }
    }

    onPositionChanged: ({
            x,
            y
        }) => {
        // Show osd on hover
        const showOsd = inRightPanel(panels.osd, x, y);

        // Always update visibility based on hover if not in shortcut mode
        if (!osdShortcutActive) {
            visibilities.osd = showOsd;
            osdHovered = showOsd;
        } else if (showOsd) {
            // If hovering over OSD area while in shortcut mode, transition to hover control
            osdShortcutActive = false;
            osdHovered = true;
        }

        // Show/hide session on drag
        if (pressed && withinPanelHeight(panels.session, x, y)) {
            const dragX = x - dragStart.x;
            if (dragX < -Config.session.dragThreshold)
                visibilities.session = true;
            else if (dragX > Config.session.dragThreshold)
                visibilities.session = false;
        }

        // Show popouts on hover
        const popout = panels.popouts;
        if (y > bar.y + Config.border.thickness - popout.height) {
            if (y > bar.y + Config.border.thickness) {
                // Handle like part of bar
                bar.checkPopout(x);
            } else {
                // Keep on hover
                popouts.hasCurrent = withinPanelWidth(popout, x, y);
            }
        } else {
            popouts.hasCurrent = false;
        }
    }

    // Monitor individual visibility changes
    Connections {
        target: root.visibilities

        function onLauncherChanged() {
            // If launcher is hidden, clear shortcut flags for OSD
            if (!root.visibilities.launcher) {
                root.osdShortcutActive = false;

                // Also hide OSD if they're not being hovered
                const inOsdArea = root.inRightPanel(root.panels.osd, root.mouseX, root.mouseY);

                if (!inOsdArea) {
                    root.visibilities.osd = false;
                    root.osdHovered = false;
                }
            }
        }

        function onOsdChanged() {
            if (root.visibilities.osd) {
                // OSD became visible, immediately check if this should be shortcut mode
                const inOsdArea = root.inRightPanel(root.panels.osd, root.mouseX, root.mouseY);
                if (!inOsdArea) {
                    root.osdShortcutActive = true;
                }
            } else {
                // OSD hidden, clear shortcut flag
                root.osdShortcutActive = false;
            }
        }
    }

    Osd.Interactions {
        screen: root.screen
        visibilities: root.visibilities
        hovered: root.osdHovered
    }
}
