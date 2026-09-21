# ned

A small, fast source code editor built on
[nano-rope](https://github.com/goolord/nano-rope) and
[nano-ui](https://github.com/goolord/nano-ui), using SDL3.

<img width="1101" height="800" alt="ned editor" src="https://github.com/user-attachments/assets/0464941a-5166-4599-9951-72abdee6889d" />

```sh
cabal run ned -- path/to/file.hs
```

## Features

- Syntax highlighting for Haskell, Rust, C/C++, JavaScript, Python and more.
- Resizable file tree and drag-and-drop file opening.
- Fuzzy file finder with a live preview, matching with fzf's own algorithm.
- Keyboard and mouse selection, clipboard, undo/redo and auto-indent.
- Incremental find, go to line, zoom and indentation guides.
- UTF-8 editing with line endings and byte order marks preserved.

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
| Ctrl+N / Ctrl+O / Ctrl+S / Ctrl+Shift+S | New / open / save / save as |
| Ctrl+Z / Ctrl+Y | Undo / redo |
| Ctrl+B | Toggle file tree |
| Ctrl+P | Find a file; Up / Down or Ctrl+P / Ctrl+N walk the rows, Ctrl+U / Ctrl+D scroll the preview, Enter opens |
| Ctrl+F | Find; Enter / Shift+Enter for next / previous match |
| Ctrl+G | Go to line |
| Tab / Shift+Tab | Indent / unindent |
| Alt+Up / Alt+Down | Page up / down |
| Ctrl+= / Ctrl+- / Ctrl+0 | Zoom in / out / reset |
| Ctrl+Q | Quit |

## Testing

```sh
cabal test ned-test
cabal run ned -- --selftest out [FILE]
```

The self-test runs in a hidden window and writes screenshots and a log to `out`.

## Limitations

- One file at a time; no replace or soft wrap.
- Closing the window does not prompt to save; use Ctrl+Q or the File menu.
- The file tree requires a manual refresh for external changes.
- Highlighting is lexical and may be inaccurate after jumping into a file.
