package gui

import (
	"github.com/zacanger/term/config"
)

var actionMap = map[config.UserAction]func(gui *GUI){
	config.ActionToggleDebug: actionToggleDebug,
}

func actionToggleDebug(gui *GUI) {
	gui.showDebugInfo = !gui.showDebugInfo
	gui.terminal.SetDirty()
}
