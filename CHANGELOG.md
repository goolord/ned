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
  and the characters that answered the query coloured. The count of what
  answered sits inside the prompt, at its end. The file the keyboard is on is
  previewed beside the rows in the colours the editor would open it in, under
  a heading with its length and language that lines up with the prompt; the
  finder takes the whole window and leaves no room around its parts. The files are walked on a thread of its own and shown
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
* Tabs: every open file has a tab over the text, in nano-ui's contained tab
  style; with one file open there is no strip. A click (or Enter) on a file
  in the tree opens it in the tab in front, asking first if that tab has
  unsaved changes, and Shift opens it in a new tab. The finder, the open
  dialog and a drop open files in a new tab after the one in front (or in
  place of an empty untitled tab), and opening one that is already open
  brings its tab to the front. Ctrl+N opens an untitled tab, as does the + after the
  tabs; Ctrl+W, a tab's cross or a middle click closes one, asking first if
  it has unsaved changes; Ctrl+Tab and Ctrl+Shift+Tab, or Ctrl+PageDown and
  Ctrl+PageUp, go to the next and previous tab. A tab with unsaved changes
  has a dot after its name. Quitting asks about every tab with unsaved
  changes. Zoom and indentation marks are the same in every tab. Every file
  named on the command line opens in its own tab.
* Page Up and Page Down move the caret a page.
* Built against nano-ui 0.2: views are `NanoUI` actions, and chords are read
  as key presses, since 0.2 puts no characters in the input for a chord. The
  text asks for the input method while it has the keyboard, since 0.2 hands
  over typed text only then, and puts the candidate window at the caret.
  The application's chords go through `shortcut`, so a menu row bound to
  the same chord does not act on a press a second time.
* Vim keys, on by default and turned off from the View menu: normal, insert,
  visual and visual line modes, the common motions and operators with counts,
  text objects, a command line for `:w`, `:q` and a line number, and a leader
  on Space for the finder (`SPC f f`), the grep (`SPC f g`) and the tree
  (`SPC d`). Normal and visual mode draw a block caret, the status bar names
  the mode, and a selection made with the pointer is visual mode. With vim's
  keys a find puts the caret on the match rather than selecting it.
* With vim's keys the caret does not blink, `j` and `k` (and the up and down
  arrows) go further with each repeat while held, as accelerated-jk has them,
  and Ctrl+H, Ctrl+J, Ctrl+K and Ctrl+L move the keyboard between the file
  tree, the text and the find bar.
* Settings in `~/.config/ned/config.dhall`, a Dhall record laid over the
  defaults: the size of the chrome's font and of the text's, the scale of the
  whole window, both fonts by family or by file, the window's size, and
  whether vim's keys, the file tree and the indentation marks start on. Ctrl+0
  resets the zoom to the text size set there. A misspelt field or one of the
  wrong type is printed and the defaults are used; `ned --default-config`
  prints every setting with what it does.
* The settings file is watched while the window is open, and a save takes
  effect without a restart: what the edit changed is applied, and what it
  left alone stays as the View menu or the zoom left it. The chrome's text
  size and the text's font still wait for a restart, which the status bar
  says. A file that does not read changes nothing and says why.
* Tab completes the word before the caret. The first word that starts with it
  goes in, and a menu under it offers the rest, each with where it is from:
  the file itself, nearest the caret first; the other tabs; the names in a
  ctags `tags` file in the tree's folder or above it, read on a thread of its
  own and again when it changes; and the language's keywords and types. Tab,
  Down and Ctrl+N step down the menu and Shift+Tab, Up and Ctrl+P up it, Enter
  takes the word and Ctrl+E puts back what was typed; typing on narrows it.
  The words tried undo as one step. Tab still indents where there is no word
  to complete, and works the same in vim's insert mode.
* With vim's keys, Ctrl+N, Ctrl+P and Ctrl+W are vim's and not the
  application's: in insert mode Ctrl+N and Ctrl+P complete the word before
  the caret and step through the menu, and Ctrl+W deletes the word before the
  caret; in normal and visual mode Ctrl+N and Ctrl+P move down and up a line.
  New tab, find file and close tab stay on the File menu, which shows no chord
  for them while vim's keys are on.
* Vim's command line takes a range and more commands: `:e file` opens a file
  in the tab in front, asking about its changes (or not, with `:e!`), and
  `:e` and `:e!` read the file in front again, as a step undo takes back;
  `:enew` puts an untitled file in the tab and `:tabnew`, with or without a
  file, and `:tabe` open one in a new tab; `:tabn` and `:tabp` go through the
  tabs; `:noh` stops marking what find found. `:s/pattern/replacement/gi`,
  `:d` and `:y` work on the lines a range names: `%`, a number, `.`, `$`,
  `'<` and `'>`, each with `+n` or `-n`, and two of them apart by a comma.
  `:` in visual mode starts a command over the lines selected. The pattern
  is a POSIX extended regular expression, read by regex-tdfa, with vim's
  `\s`, `\d` and `\w`; in the replacement `&` and `\1` to `\9` are what
  matched and `\r` breaks the line. A substitution undoes in one step.
* Language servers: Ctrl+] (and `gd` with vim's keys) goes to where what is
  under the caret is defined, and `K` puts what the server says of it on the
  status bar. A server is a command per language in `languageServers`, run
  through `languageServerShell` in the tree's folder, and started the first
  time a file of its language asks. `projects` gives the files under a
  folder servers of their own, run in that folder. The server is told the
  whole file each time it is asked, and waited for on a thread of its own.
* Diagnostics: a file's server is started when the file is opened, and told
  its text on a thread of its own each time it changes, only the latest
  being sent when the typing is ahead of it. What it finds wrong is
  underlined, red for an error, yellow for a warning and blue for the rest,
  and with vim's keys `g]` and `g[` go to the next and the one before and put
  what it is on the status bar. A server that would not start is not tried
  again on each key, only when asked something outright.
* `K` shows the server's hover in a popup under the caret, drawn as markdown
  by nano-ui-markdown, until the caret or the text moves.
* Projects are general: `projects` is a record of projects, each a `root`
  and any of the settings that are a file's rather than the window's (for
  now `languageServerShell` and `languageServers`). A project is laid over
  the settings as the file is laid over the defaults, and a project inside
  another over the outer one's. A file takes the deepest project that holds
  it, and its servers run in that project's root. A root is absolute, under
  `~`, or from the settings file's folder. A window setting in a project is
  an error that says so.
* Vim's `*` and `#` find the word under the caret, as a whole word, forward
  and backward; in visual mode they find the selected text. The find bar
  comes up to show the matches but leaves the keyboard with the text, so `n`
  and `N` go on from there. With vim's keys, Enter in the find bar ends the
  search: the caret stays on the match and the keyboard goes back to the
  text. The find bar has a Whole word box, which `/` clears.
