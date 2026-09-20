# Revision history for ned

## 0.1.0.0 -- 2026-09-19

* First version: a source code editor on nano-rope and nano-ui, with syntax
  highlighting, undo and redo, find, go to line, and LF and CRLF files.
* A file tree beside the editor, on the folder the open file is in: its
  folders open and close, a click on a file opens it, the bar between it and
  the text resizes it, and Ctrl+B puts it away.
* The file tree draws a rule down each level it is deep, a folder or a page
  on every row, and the page in the colour of the language the file would
  open as; the file the editor has is marked in the caret's colour.
* The status bar sets the file and the caret's place apart from the settings
  behind them, and leaves a setting off while it is at its default.
* Typing that comes to nothing (a Ctrl+Alt chord, an empty paste) no longer
  deletes the selection; the go-to-line field takes the keyboard back when it
  is pressed; and a press on the file tree takes it from the find bar's field.
