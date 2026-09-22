# Revision history for ned

## 0.1.0.0 -- 2026-09-19

* First version: a source code editor on nano-rope and nano-ui, with syntax
  highlighting, undo and redo, find, go to line, and LF and CRLF files.
* The editor scrolls two ways: a line wider than the view puts a scrollbar
  along its foot, under the upright one, and its thumb is taken and dragged
  the same way. The view is held to the widest line in the file and a few
  cells past it, so the bar stays while the wide line is out of sight; the
  wheel walks along the line with Shift, and the caret is followed when it
  moves.
* A fuzzy file finder on Ctrl+P: a prompt over the files under the tree's
  root, ranked by fzf's own matching algorithm (vendored as C in
  `ned-fzf`), each row a file's name with its folder in a column beside it
  and the characters that answered the query coloured. The file the keyboard
  is on is previewed beside the rows in the colours the editor would open it
  in, with its length and language; the keys that work in the finder are
  listed along its foot. The files are walked on a thread of its own and shown
  as they are found, so the window never waits on a disk, and what a build or
  a version control system leaves behind is walked past.
* A live grep on Ctrl+Shift+F, in the same finder: ripgrep is run on the
  query once typing has paused, each line it finds is a row with what it
  matched coloured, the preview opens on the line, and Enter opens the file
  with the caret there. It passes over what the file finder does.
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
* The rope keeps the widest line of the text at every node, so an edit costs
  the sideways scroll nothing: a keystroke in a file of 200,000 lines takes a
  seventh of the frame it did, when every edit set a scan of the file going
  again. The view's bound is exact after any edit, where a line deleted used
  to leave it too wide until the scan came round, and the sideways thumb
  stays under the pointer to the end of its lane.
* The finder's preview draws its lines with the editor's own code, so a tab,
  a wide character or a semibold name stands on the cells it would in the
  editor, and a line is coloured as the editor colours it.
