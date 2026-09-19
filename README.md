# ned

A small, fast source code editor. The text lives in a
[nano-rope](https://github.com/goolord/nano-rope) and the window is
[nano-ui](https://github.com/goolord/nano-ui) on SDL3.

```sh
cabal run ned -- path/to/file.hs
```

## What it does

- Opens, edits and saves UTF-8 files, keeping the line endings (LF or CRLF)
  and the byte order mark the file came with, and says so when a file is not
  UTF-8. A file dropped on the window opens.
- Syntax highlighting for Haskell, Cabal, C, C++, C#, Rust, Go, Java,
  JavaScript, TypeScript, JSON, Python, Lua, Zig, Nix, shell, TOML, YAML, SQL,
  CSS, HTML and Markdown, chosen by file name.
- Selection with the keyboard and the mouse (double click for a word, triple
  click for a line, a press or a drag on the line numbers for whole lines,
  drag past an edge to scroll), clipboard, undo and redo that go a word at a
  time, and a context menu on the right button.
- Auto indent, Tab and Shift+Tab over a selection, tabs or spaces as the
  file already has them. Indentation is drawn, a dot a space and a rule a
  tab; View > Hide Indentation Marks turns that off.
- Incremental find with matches marked in the view, and go to line.
- Zoom, a line number gutter, and a status bar with the position, language,
  indentation and line endings.

## Keys

| Key | Does |
| --- | --- |
| Ctrl+N, Ctrl+O, Ctrl+S, Ctrl+Shift+S | New, open, save, save as |
| Ctrl+Q | Quit |
| Ctrl+Z, Ctrl+Y or Ctrl+Shift+Z | Undo, redo |
| Ctrl+X, Ctrl+C, Ctrl+V | Cut, copy, paste; with nothing selected, cut and copy take the line |
| Ctrl+A | Select all |
| Ctrl+F | Find; Enter and Shift+Enter go to the next and previous match, Esc closes |
| Ctrl+G | Go to line |
| Arrows, Home, End | Move; with Shift, select. Home goes to the indentation first |
| Ctrl+Left, Ctrl+Right | Move by words |
| Ctrl+Home, Ctrl+End | Start and end of the file |
| Alt+Up, Alt+Down | Move by pages (nano-ui at this commit reports no Page Up or Page Down) |
| Ctrl+Backspace, Ctrl+Delete | Delete a word |
| Tab, Shift+Tab | Indent, unindent |
| Ctrl+=, Ctrl+-, Ctrl+0 | Zoom in, out, reset |
| Wheel, Shift+wheel | Scroll, scroll sideways |

## Why it is fast

- Every location is a code point offset into the rope, which converts
  between offsets, lines and columns in O(log n). Nothing the editor does on
  a keystroke or a frame reads more than the lines on screen: a 100 MB file
  of 2.4 million lines opens in about 80 ms here and draws a frame in the
  same 0.7 ms as a file of 600 lines.
- An edit copies one chunk of the rope and the path to it. Ropes are
  persistent, so the undo history keeps whole ropes and they share
  everything an edit did not touch.
- The editor is three nano-ui custom widgets (line numbers, text, scrollbar)
  that scroll by themselves. Their draw ops are keyed on everything they read (the text's version, caret,
  selection, scroll, zoom), so a frame where none of that changed builds
  nothing and repaints nothing, and the window blocks in the event wait
  between caret blinks.
- The lexer works a line at a time and carries only the state a line starts
  in (a block comment, a string over lines), so it runs on the visible lines.
  The state at the top of the view is cached and survives edits below it.
- Lines longer than 4096 characters are never read whole: they are drawn a
  window at a time, so a minified file is no slower than any other.
- Files are saved a chunk at a time, straight from the rope.

## Layout

| Module | Holds |
| --- | --- |
| `Ned.Buffer` | The rope, caret, selection, movement, editing, history and search. Pure |
| `Ned.Highlight` | Languages and the line lexer. Pure |
| `Ned.View` | The editor widget: input, scrolling and drawing |
| `Ned.App` | Menus, files, the find bar, the status bar |
| `Ned.Selftest` | Drives the application in a hidden window |

## Building

You need GHC 9.14, Cabal, SDL3, SDL3_ttf and pkg-config. `cabal.project` pins
nano-ui and nano-rope to the commits ned is written against.

On Windows install the libraries with MSYS2 (UCRT64) and point Cabal at them:

```sh
pacman -S mingw-w64-ucrt-x86_64-sdl3 mingw-w64-ucrt-x86_64-sdl3-ttf mingw-w64-ucrt-x86_64-pkg-config
```

```powershell
$env:PKG_CONFIG_PATH = "C:\msys64\ucrt64\lib\pkgconfig"
$env:PATH = "C:\msys64\ucrt64\bin;" + $env:PATH
cabal build -j1 ned
```

`ned.exe` needs `C:\msys64\ucrt64\bin` on the `PATH` when it runs, for
`SDL3.dll` and `SDL3_ttf.dll`.

## Testing

```sh
cabal test ned-test
cabal run ned -- --selftest out [FILE]
```

`ned-test` checks the buffer (against a model kept as one `Text`, over
twenty thousand random edits), the history, search, the lexer and file round
trips. `--selftest` drives the whole application in a hidden window on
scripted input, checks what the editing did, times frames when given a file,
and writes screenshots and `selftest.log` to the directory.

`NED_TRACE=file` logs a line a frame (time, window size, milliseconds spent
building the view), and `NED_BLANK=1` swaps the application for one label, to
tell what a frame costs nano-ui from what it costs the editor.

## Limits

- One file at a time, no replace, no soft wrap.
- Closing the window does not ask about unsaved changes; Ctrl+Q and the File
  menu do.
- Highlighting is lexical. A view that jumps into the middle of a file
  guesses the lexer state from the 500 lines above it.
