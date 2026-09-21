# Revision history for ned

## 0.1.0.0 -- 2026-09-19

* First version: a source code editor on nano-rope and nano-ui, with syntax
  highlighting, undo and redo, find, go to line, and LF and CRLF files.
* A file tree beside the editor, on the folder the open file is in: its
  folders open and close, a click on a file opens it, the bar between it and
  the text resizes it, and Ctrl+B puts it away. A resized window resizes the
  text: the tree is the pane grid's pinned pane and keeps the width it was
  left at.
* The window has no border of its own. The bar along the top is its title
  bar: the menus at the left, the file's name in the middle, and the buttons
  that put the window away, fill the screen with it and close it at the
  right. What is left of the bar between the menus and those buttons drags
  the window, and its edges still resize it. The window draws its own line
  around itself, since it has no frame to be told apart from the desktop by,
  and keeps the desktop's shadow under it; a window filling the screen draws
  neither.
* The file tree draws a rule down each level it is deep, a folder or a page
  on every row, and the page in the colour of the language the file would
  open as; the file the editor has is marked in the caret's colour.
* The status bar sets the file and the caret's place apart from the settings
  behind them, and leaves a setting off while it is at its default.
* The code is in layers that only reach downward: the text and the lexer
  know nothing of a window, the editor and the file tree know nothing of the
  application, and what colour anything is is said in one place. Everything
  that draws is one module over the lot of them, so nothing that keeps state
  or answers a key is written in the same place as the ops that paint it.
* Typing that comes to nothing (a Ctrl+Alt chord, an empty paste) no longer
  deletes the selection; the go-to-line field takes the keyboard back when it
  is pressed; and a press on the file tree takes it from the find bar's field.
