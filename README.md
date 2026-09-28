# ned

A small, fast source code editor built on
[nano-rope](https://github.com/goolord/nano-rope) and
[nano-ui](https://github.com/goolord/nano-ui), using SDL3.

<img width="2205" height="1523" alt="ned editor" src="https://github.com/user-attachments/assets/63baafff-c79f-4f67-9668-7fcfdfab42cd" />

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
- Tab completion of the word before the caret, from the file itself (nearest
  words first), the other open tabs, a ctags `tags` file, and the language's
  keywords and types.

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
| Ctrl+] | Go to definition, with a language server (see [Language servers](#language-servers)) |
| Tab / Shift+Tab | Complete the word before the caret; indent / unindent where there is none |
| Page Up / Page Down, Alt+Up / Alt+Down | Page up / down |
| Ctrl+= / Ctrl+- / Ctrl+0 | Zoom in / out / reset |
| Ctrl+Q | Quit |

## Vim keys

The mode is on the left of the status bar, with a command's keys as they are
typed. The caret does not blink, and is a block in normal and visual mode; insert mode is the editor's
own keys, so the shortcuts above work there too, except Ctrl+N, Ctrl+P and
Ctrl+W, which are vim's (below). New tab, find file and close tab are still
on the File menu, and on `SPC f f`, `:tabnew` and `:q`. Yanks and puts go through
the system clipboard, and what ends in a line break is put as whole lines.

| Keys | |
| --- | --- |
| Motions | `h j k l`, `w b e`, `0 ^ $`, `gg G`, `{ }`, `f F t T` and `; ,`, `+ -`, Ctrl+N / Ctrl+P, Ctrl+D / Ctrl+U, all with counts; `j` and `k` speed up while held |
| Operators | `d c y > <` with a motion, a text object or doubled (`dd`, `cc`, `yy`, `>>`, `<<`) |
| Text objects | `iw aw`, `i( a(` (also `b`), `i{ a{` (also `B`), `i[ a[`, `i< a<`, `i" a"`, `i' a'` |
| Edits | `i a I A o O`, `x X s S D C Y`, `p P`, `J`, `r`, `~`, `u` / Ctrl+R |
| Insert mode | Tab, Ctrl+N and Ctrl+P complete the word before the caret; Ctrl+W deletes the word before it |
| Visual | `v V`, then motions, text objects, `o`, `d y c > < p J ~ u U`, and `:` for a command over the lines selected |
| Search | `/` opens find, `n N` go to the next and previous match |
| Tabs | `gt gT`, `:tabn`, `:tabp` |
| Language server | `gd` / Ctrl+] go to definition, `K` shows what is under the caret, `g]` / `g[` go to the next / previous diagnostic |
| Panes | Ctrl+H / Ctrl+L to the file tree and back, Ctrl+J / Ctrl+K down to the find bar and back |
| Commands | `:w`, `:q`, `:q!`, `:wq`, `:x`, `:qa`, `:<line>`, `ZZ`, `ZQ` |
| Files | `:e file` in this tab, `:e` / `:e!` read this file again, `:enew` an untitled file in this tab, `:tabnew [file]` / `:tabe file` in a new tab |
| Lines | `:s`, `:d`, `:y` over a range: `%`, `5`, `.`, `$`, `'<`, `'>`, `+n`, `-n`, two of them between a comma |
| Other | `:noh` stops marking what find found |
| Leader (Space) | `SPC f f` find a file, `SPC f g` search in files, `SPC d` toggle the file tree |

`:s/pattern/replacement/flags` (as `:%s/.../.../g` for the whole file) reads
its pattern as a POSIX extended regular expression, through
[regex-tdfa](https://hackage.haskell.org/package/regex-tdfa): groups are
`( )` and alternatives `|` without a backslash, as in vim's `\v` mode, and
`\s`, `\d`, `\w` and their capitals work as in vim. In the replacement `&`
is the match, `\1` to `\9` its groups and `\r` a line break. The flags are
`g`, every match on a line, and `i`, any case. Any character but a letter, a
digit or a backslash can stand in for `/`. The whole substitution undoes in
one step.

## Completion

Tab after a word puts in the first word that starts with it and opens a menu
of the rest. What is typed in lower case matches any case; a capital matches
its own case. Words come from, in this order:

1. The file being edited, nearest the caret first.
2. The other open tabs, in the order the tab strip shows them.
3. The `tags` file in the file tree's folder or the nearest folder above it,
   as written by `ctags -R`. It is read again when it changes.
4. The language's keywords and built-in types.

When only one word answers, it goes in with no menu. Where there is no
word before the caret, or nothing answers it, Tab indents as before.

| Key | While the menu is open |
| --- | --- |
| Tab / Down / Ctrl+N | Next word |
| Shift+Tab / Up / Ctrl+P | Previous word; past either end is what was typed |
| Enter | Take the word |
| Ctrl+E | Put back what was typed |
| Escape | Close the menu, keeping the word (and leave insert mode, with vim keys) |

Typing on narrows the menu; anything else closes it with the word in place.
The words tried undo as one step. With vim keys, completion works in insert
mode, and Ctrl+N and Ctrl+P open the menu as Tab does.

## Configuration

Settings are read from `config.dhall` in `~/.config/ned` (`%APPDATA%\ned` on
Windows). The file is [Dhall](https://dhall-lang.org), laid over the defaults,
so it only needs the fields it changes:

```dhall
{ uiFontSize = 17.0
, bufferFontSize = 16.0
, bufferFont = Some "JetBrains Mono"
, vimKeys = False
}
```

Print every setting, with its default and what it does:

```sh
cabal run ned -- --default-config > ~/.config/ned/config.dhall
```

| Setting | Default | |
| --- | --- | --- |
| `uiFontSize` | `16.0` | Menus, bars, file tree and status bar, in points |
| `bufferFontSize` | `15.0` | The text and the finder, in points; Ctrl+0 resets the zoom to it |
| `scale` | `1.0` | Zoom of the whole window over the display's pixel density; `0.0` follows the display's scale |
| `uiFont`, `bufferFont` | `None Text` | A font family (`Some "Inter"`) or a path to a `.ttf`/`.otf` file |
| `windowWidth`, `windowHeight` | `1100`, `760` | Size of the window when it opens |
| `vimKeys` | `True` | Start with vim keys on |
| `showFileTree` | `True` | Start with the file tree shown |
| `showIndentation` | `True` | Mark indentation with dots and rules |
| `shell` | `["sh", "-c"]` (`["cmd", "/c"]` on Windows) | The program a language server's command runs in, and its arguments before the command |
| `languageServers` | `[]` | A language server command per language |
| `projects` | `[]` | Language servers for the files under a folder, in place of `languageServers` |

A misspelled field or a value of the wrong type is an error: ned prints it and
starts with the defaults. Sizes are Doubles, so write `18.0`, not `18`.

The file is reloaded when it is saved while ned is open. Only the settings
the edit changed are applied, so a toggle from the View menu stays unless the
file changes that setting. `uiFontSize` and `bufferFont` take effect on
restart, and the window size only applies when the window opens. A file that
does not read while ned is open leaves the current settings in place.

### Language servers

A language server is started when a file of its language is opened, and is
sent the file's text as it changes. Its diagnostics are underlined: red for
errors, yellow for warnings, blue for the rest. With vim keys, `g]` and `g[`
jump to the next and previous one and show its message on the status bar.
Ctrl+] (or `gd`) goes to a definition, and `K` shows the hover documentation,
rendered as markdown, under the caret until it moves.

The language is the name the status bar shows for the file, compared without
case. The command runs through `shell`, in the folder the file tree is on:

```dhall
{ languageServers =
  [ { language = "Haskell", command = "haskell-language-server-wrapper --lsp" }
  , { language = "C", command = "clangd" }
  ]
, projects =
  [ { root = "~/src/app"
    , languageServers =
      [ { language = "Haskell", command = "nix develop -c haskell-language-server-wrapper --lsp" } ]
    }
  ]
}
```

A file under a project's `root` uses that project's server for its language,
run in `root`. When several projects hold the file, the deepest one wins, and
a language the project does not list falls back to `languageServers`. The
server's stderr goes to the terminal ned was started from. A server that fails
to start is not tried again while you type; Ctrl+], `gd` or `K` tries it again.

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
