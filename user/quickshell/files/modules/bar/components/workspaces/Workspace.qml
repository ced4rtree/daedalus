import "root:/widgets"
import "root:/services"
import "root:/utils"
import "root:/config"
import Quickshell
import Quickshell.Widgets
import QtQuick
import QtQuick.Layouts

Item {
    id: root

    required property int index
    required property var occupied
    required property int groupOffset

    readonly property bool isWorkspace: true // Flag for finding workspace children
    // Unanimated prop for others to use as reference
    readonly property real size: childrenRect.height + (hasWindows ? Appearance.padding.small : 0)

    readonly property int ws: groupOffset + index + 1
    readonly property bool isOccupied: occupied[ws] ?? false
    readonly property bool hasWindows: isOccupied && Config.bar.workspaces.showWindows
    readonly property int currentWsIdx: Hyprland.activeWsId - 1 - groupOffset
    readonly property bool isSelected: currentWsIdx == index

    Layout.preferredWidth: childrenRect.width
    Layout.preferredHeight: size

    StyledRect {
        id: background

        clip: true
        x: 1
        y: hasWindows ? Appearance.padding.small/2 : 1
        implicitWidth: Config.bar.sizes.innerHeight - 2
        implicitHeight: (indicator.visible ? indicator.implicitHeight : 0)
            + windows.implicitHeight
            + (hasWindows ? Appearance.padding.small : Appearance.padding.normal)
        radius: Config.bar.workspaces.rounded ? Appearance.rounding.full : 0
        color: isSelected ? "transparent" : isOccupied
            ? Colours.palette.m3surfaceBright
            : Colours.palette.m3background
    }

    StyledText {
        id: indicator

        readonly property string label: Config.bar.workspaces.label || root.ws
        readonly property string occupiedLabel: Config.bar.workspaces.occupiedLabel || label
        readonly property string activeLabel: Config.bar.workspaces.activeLabel || (root.isOccupied ? occupiedLabel : label)

        animate: true
        text: label
        color: Config.bar.workspaces.occupiedBg || root.isOccupied || Hyprland.activeWsId === root.ws ? Colours.palette.m3onSurface : Colours.palette.m3outlineVariant
        horizontalAlignment: StyledText.AlignHCenter
        verticalAlignment: StyledText.AlignVCenter

        visible: !root.isOccupied

        width: Config.bar.sizes.innerHeight
        height: root.isOccupied ? Appearance.padding.small : Config.bar.sizes.innerHeight
    }

    Loader {
        id: windows

        active: Config.bar.workspaces.showWindows
        asynchronous: true

        anchors.horizontalCenter: indicator.horizontalCenter
        anchors.top: indicator.bottom

        sourceComponent: Column {
            spacing: 0

            add: Transition {
                Anim {
                    properties: "scale"
                    from: 0
                    to: 1
                    easing.bezierCurve: Appearance.anim.curves.standardDecel
                }
            }

            Repeater {
                model: ScriptModel {
                    values: Hyprland.clients.filter(c => c.workspace?.id === root.ws)
                }

                delegate: IconImage {
                    implicitSize: 24
                    source: Icons.getAppIcon(modelData.wmClass ?? "", "image-missing")
                }
            }
        }
    }

    Behavior on Layout.preferredWidth {
        Anim {}
    }

    Behavior on Layout.preferredHeight {
        Anim {}
    }

    component Anim: NumberAnimation {
        duration: Appearance.anim.durations.normal
        easing.type: Easing.BezierSpline
        easing.bezierCurve: Appearance.anim.curves.standard
    }
}
