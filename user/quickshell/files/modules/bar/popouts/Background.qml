import "root:/services"
import "root:/config"
import QtQuick
import QtQuick.Shapes

ShapePath {
    id: root

    required property Wrapper wrapper
    required property bool invertRightRounding
    readonly property real rounding: Config.border.rounding
    readonly property bool flatten: wrapper.height < rounding * 2
    readonly property real roundingY: flatten ? wrapper.height / 2 : rounding
    property real irr: invertRightRounding ? -1 : 1

    strokeWidth: 0
    fillColor: Config.border.colour

    PathArc {
        relativeX: -root.rounding
        relativeY: root.roundingY
        radiusX: root.rounding
        radiusY: Math.min(root.rounding, root.wrapper.height)
        direction: PathArc.Counterclockwise
    }
    PathLine {
        relativeX: 0
        relativeY: root.wrapper.height - root.roundingY * 2
    }
    PathArc {
        relativeX: -root.rounding
        relativeY: root.roundingY
        radiusX: root.rounding
        radiusY: Math.min(root.rounding, root.wrapper.height)
        direction: PathArc.Clockwise
    }
    PathLine {
        relativeX: root.wrapper.width + root.rounding * 2
            + (root.invertRightRounding ? -root.rounding : 0)
        relativeY: 0
    }
    PathArc {
        relativeX: root.invertRightRounding ? 0 : -root.rounding
        relativeY: -root.roundingY
        radiusX: root.rounding
        radiusY: Math.min(root.rounding, root.wrapper.height)
        direction: root.irr < 0 ? PathArc.Counterclockwise : PathArc.Clockwise
    }
    PathLine {
        relativeX: 0
        /* relativeY: -root.wrapper.height + root.roundingY */
        relativeY: -(root.wrapper.height - root.roundingY - root.roundingY * root.irr)
    }
    PathArc {
        relativeX: -root.rounding
        relativeY: -root.irr * root.roundingY
        radiusX: root.rounding
        radiusY: Math.min(root.rounding, root.wrapper.height)
        direction: root.irr == -1 ? PathArc.Clockwise : PathArc.Counterclockwise
    }

    Behavior on fillColor {
        ColorAnimation {
            duration: Appearance.anim.durations.normal
            easing.type: Easing.BezierSpline
            easing.bezierCurve: Appearance.anim.curves.standard
        }
    }

    Behavior on irr {
        NumberAnimation {
            duration: Appearance.anim.durations.normal
            easing.type: Easing.BezierSpline
            easing.bezierCurve: Appearance.anim.curves.standard
        }
    }
}
