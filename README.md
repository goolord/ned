# ned

A small, fast source code editor built on
[nano-rope](https://github.com/goolord/nano-rope) and
[nano-ui](https://github.com/goolord/nano-ui), using SDL3.

<img width="1102" height="760" alt="ned editor" src="https://github.com/user-attachments/assets/5c986cf4-0e5b-49fb-a991-a42d5e1966fe" />

```sh
cabal run ned -- path/to/file.hs [more files...]
```

## Features

- Syntax highlighting for Haskell, Rust, C/C++, JavaScript, Python and more.
- Tabs for open files, with a dot on each one that has unsaved changes. The
  strip is hidden while only one file is open. A click in the file tree opens
  the file in the current tab; Shift+click opens it in a new one.
- Resizable file tree and drag-and-drop file opening.
- Fuzzy file finder with a live preview, matching with fzf's own algorithm.
- Live grep across the tree's folder, through ripgrep (`rg` on the `PATH`).
- Keyboard and mouse selection, clipboard, undo/redo and auto-indent.
- Incremental find, go to line, zoom and indentation guides.
- UTF-8 editing with line endings and byte order marks preserved.
- Vim keys: normal, insert, visual and visual line modes, on by default and
  turned off from the View menu.

## Building

Requires GHC 9.14, Cabal, SDL3, SDL3_ttf and pkg-config. Dependency versions
for nano-rope and nano-ui are pinned in `cabal.project`.

```sh
cabal build -j1 ned
```

On Windows, install the libraries in MSYS2 (UCRT64):

```sh
pacman -S mingw-w64-ucrt-x86_64-sdl3 mingw-w64-ucrt-x86_64-sdl3-ttf mingw-w64-ucrt-x86_64-pkg-config
```

Then build from PowerShell:

```powershell
$env:PKG_CONFIG_PATH = "C:\msys64\ucrt64\lib\pkgconfig"
$env:PATH = "C:\msys64\ucrt64\bin;" + $env:PATH
cabal build -j1 ned
```

Keep `C:\msys64\ucrt64\bin` on `PATH` when running `ned.exe`.

## Shortcuts

| Key | Action |
| --- | --- |
| Ctrl+N / Ctrl+O / Ctrl+S / Ctrl+Shift+S | New tab / open / save / save as |
| Ctrl+W | Close tab (middle click also closes one) |
| Ctrl+Tab / Ctrl+Shift+Tab | Next / previous tab (also Ctrl+PageDown / Ctrl+PageUp) |
| Ctrl+Z / Ctrl+Y | Undo / redo |
| Ctrl+B | Toggle file tree |
| Ctrl+P | Find a file; Up / Down or Ctrl+P / Ctrl+N walk the rows, Ctrl+U / Ctrl+D scroll the preview, Enter opens |
| Ctrl+Shift+F | Search in files; the same keys as the file finder, and Enter opens the file at the line |
| Ctrl+F | Find; Enter / Shift+Enter for next / previous match |
| Ctrl+G | Go to line |
| Tab / Shift+Tab | Indent / unindent |
| Page Up / Page Down, Alt+Up / Alt+Down | Page up / down |
| Ctrl+= / Ctrl+- / Ctrl+0 | Zoom in / out / reset |
| Ctrl+Q | Quit |

## Vim keys

The mode is on the left of the status bar, with a command's keys as they are
typed. The caret does not blink, and is a block in normal and visual mode; insert mode is the editor's
own keys, so every shortcut above still works there. Yanks and puts go through
the system clipboard, and what ends in a line break is put as whole lines.

| Keys | |
| --- | --- |
| Motions | `h j k l`, `w b e`, `0 ^ $`, `gg G`, `{ }`, `f F t T` and `; ,`, `+ -`, Ctrl+D / Ctrl+U, all with counts; `j` and `k` speed up while held |
| Operators | `d c y > <` with a motion, a text object or doubled (`dd`, `cc`, `yy`, `>>`, `<<`) |
| Text objects | `iw aw`, `i( a(` (also `b`), `i{ a{` (also `B`), `i[ a[`, `i< a<`, `i" a"`, `i' a'` |
| Edits | `i a I A o O`, `x X s S D C Y`, `p P`, `J`, `r`, `~`, `u` / Ctrl+R |
| Visual | `v V`, then motions, text objects, `o`, `d y c > < p J ~ u U` |
| Search | `/` opens find, `n N` go to the next and previous match |
| Tabs | `gt gT` |
| Panes | Ctrl+H / Ctrl+L to the file tree and back, Ctrl+J / Ctrl+K down to the find bar and back |
| Commands | `:w`, `:q`, `:q!`, `:wq`, `:x`, `:qa`, `:<line>`, `ZZ`, `ZQ` |
| Leader (Space) | `SPC f f` find a file, `SPC f g` search in files, `SPC d` toggle the file tree |

## Testing

```sh
cabal test ned-test
cabal run ned -- --selftest out [FILE]
```

The self-test runs in a hidden window and writes screenshots and a log to `out`.

## Limitations

- No replace or soft wrap.
- Closing the window does not prompt to save; use Ctrl+Q or the File menu.
- The file tree requires a manual refresh for external changes.
- Highlighting is lexical and may be inaccurate after jumping into a file.
