# TODO
* implement ConfigFile read/write in YAML
* write test suite of library
* Remove Relude from MyPrelude and write explicit

## module structure?
* Config.Lens 
  - Table     . Internal
  - Parser    . Internal
  - Monad     . Internal
  - Internal

## GUI
* recursive commands for GUI: CmdGUI :: CmdDataGUI -> Cmd -> Cmd 
  recursive commands in general, not only GUI?
* GUIwidget for each command and populate window with each command?
