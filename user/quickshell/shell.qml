import Quickshell
import QtQuick

PanelWindow {
    anchors {
        // anchor panel to the bottom
        top: true
        left: true
        right: true
    }

    implicitHeight: 30

    Text {
        // center text
        anchors.centerIn: parent
        
        text: "Hello, World!"
    }
}
