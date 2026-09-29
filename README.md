# ned

A small, fast source code editor built on [nano-rope](https://github.com/goolord/nano-rope) and [nano-ui](https://github.com/goolord/nano-ui), using SDL3.

<img width="2205" height="1523" alt="ned editor" src="https://github.com/user-attachments/assets/63baafff-c79f-4f67-9668-7fcfdfab42cd" />

## Usage

```sh
cabal run ned -- path/to/file.hs [more files...]
```

## Features

- Syntax highlighting for Haskell, Rust, C/C++, JavaScript, Python, and more.
- Tabs and a resizable file tree (Shift+click opens a file in a new tab).
  Tabs drag: between two tabs to reorder them, onto another pane to move it
  there, or against an edge of a pane to split the row and open it there.
- Drag-and-drop file opening.
- Fuzzy file finder with live preview, powered by fzf's matching algorithm.
- Searchable command palette, opened with `Ctrl+Shift+P`.
- Live Markdown preview beside the source. `Ctrl+Shift+M` toggles it, links open with the system's default handler, and rendered text can be selected and copied.
- Live grep across the project folder using ripgrep (`rg` must be on your `PATH`).
- Find, go to line, undo/redo, auto-indent, zoom, and indentation guides.
- UTF-8 editing that preserves line endings and byte order marks.
- Vim keys, enabled by default.
- Tab completion from open files, a ctags `tags` file, and language keywords.
- Language server support: diagnostics, go to definition, and hover.

## Building

You need GHC 9.14, Cabal, SDL3, SDL3_ttf, and pkg-config. Dependency versions for nano-rope and nano-ui are pinned in `cabal.project`.

```sh
cabal build -j1 ned
```

On Windows, install the required libraries in MSYS2 (UCRT64):

```sh
pacman -S mingw-w64-ucrt-x86_64-sdl3 \
          mingw-w64-ucrt-x86_64-sdl3-ttf \
          mingw-w64-ucrt-x86_64-pkg-config
```

Then build from PowerShell:

```powershell
$env:PKG_CONFIG_PATH = "C:\msys64\ucrt64\lib\pkgconfig"
$env:PATH = "C:\msys64\ucrt64\bin;" + $env:PATH
cabal build -j1 ned
```

Keep `C:\msys64\ucrt64\bin` on your `PATH` when running `ned.exe`.

## Shortcuts

| Key | Action |
|-----|--------|
| `Ctrl+N` / `Ctrl+O` / `Ctrl+S` / `Ctrl+Shift+S` | New tab / open / save / save as |
| `Ctrl+W`, middle click | Close tab |
| `Ctrl+Tab` / `Ctrl+Shift+Tab` | Next / previous tab |
| `Ctrl+Z` / `Ctrl+Y` | Undo / redo |
| `Ctrl+B` | Toggle file tree |
| `Ctrl+P` | Find a file; `Ctrl+U` / `Ctrl+D` scroll the preview |
| `Ctrl+Shift+F` | Search in files |
| `Ctrl+F` | Find; `Enter` / `Shift+Enter` for next / previous (with Vim keys, `Enter` returns to the text) |
| `Ctrl+G` | Go to line |
| `Ctrl+Shift+P` | Open command palette |
| `Ctrl+Shift+M` | Toggle Markdown preview |
| `Ctrl+]` | Go to definition |
| `Tab` / `Shift+Tab` | Complete the word before the caret, or indent / unindent |
| `Ctrl+=` / `Ctrl+-` / `Ctrl+0` | Zoom in / out / reset |
| `Ctrl+Q` | Quit |

## Vim keys

Turn Vim keys off from the View menu. Insert mode keeps the shortcuts above, except `Ctrl+N`, `Ctrl+P`, and `Ctrl+W`, which behave as in Vim; new tab, find file, and close tab remain on the File menu. Yanks and puts use the system clipboard.

| Category | Keys |
|----------|------|
| Motions | `h j k l`, `w b e`, `0 ^ $`, `gg G`, `{ }`, `f F t T`, `; ,`, `+ -`, `Ctrl+D` / `Ctrl+U`, with counts |
| Operators | `d c y > <` with a motion, a text object, or doubled |
| Text objects | `iw aw`, `i( a(` / `ib ab`, `i{ a{` / `iB aB`, `i[ a[`, `i< a<`, `i" a"`, `i' a'` |
| Edits | `i a I A o O`, `x X s S D C Y`, `p P`, `J`, `r`, `~`, `u` / `Ctrl+R` |
| Visual | `v V`, with motions, text objects, `o`, `d y c > < p J ~ u U`, and `:` over the selected lines |
| Search | `/` then `Enter`, `n N`, `*` `#` for the word under the caret or the selection, `:noh` |
| Language server | `gd`, `K` for hover, `g]` / `g[` for the next / previous diagnostic |
| Panes | `Ctrl+H` / `Ctrl+L` to the file tree and back, `Ctrl+J` / `Ctrl+K` to the find bar and back |
| File tree | `j k` with counts, `h` / `l` to close / open a folder or step out / in, `l` or `o` opens a file (`O` in a new tab), `gg G`, `Ctrl+D` / `Ctrl+U`, `-` to the parent folder |
| Files and tabs | `:w`, `:q`, `:q!`, `:wq`, `:x`, `:qa`, `ZZ`, `ZQ`, `:e[!] [file]`, `:enew`, `:tabnew [file]`, `:tabe file`, `gt gT`, `:tabn`, `:tabp` |
| Lines | `:`; `:s`, `:d`, `:y` over a range (`%`, `5`, `.`, `$`, `'<,'>`, `+n`, `-n`) |
| Leader (Space) | `SPC f f` find a file, `SPC f g` search in files, `SPC d` toggle the file tree |

`:s` patterns are POSIX extended regular expressions ([regex-tdfa](https://hackage.haskell.org/package/regex-tdfa)), so groups and `|` need no backslash, as in Vim's `\v` mode. `\s`, `\d`, and `\w` work. In the replacement, `&` is the match, `\1` to `\9` its groups, and `\r` a line break. The flags are `g` and `i`.

## Completion

Pressing Tab after a word inserts the first match and opens a menu of the rest. Lower case matches any case. Words come from, in order:

1. The current file (nearest the caret first)
2. Other open tabs
3. The `tags` file from `ctags -R` in the file tree's folder or above it
4. The language's keywords and types

`Tab` / `Ctrl+N` and `Shift+Tab` / `Ctrl+P` move through the menu, `Enter` takes the word, and `Ctrl+E` restores what you typed.

## Configuration

ned reads `config.dhall` from `~/.config/ned` (`%APPDATA%\ned` on Windows). The file is [Dhall](https://dhall-lang.org) and only needs the fields it changes:

```dhall
{ uiFontSize = 17.0
, bufferFont = Some "JetBrains Mono"
, vimKeys = False
}
```

To write out every setting with its default and a description:

```sh
cabal run ned -- --default-config > ~/.config/ned/config.dhall
```

ned remembers the window's size, whether it was maximized, and the file tree's width, visibility, and position relative to the buffers in `layout.json` in `~/.local/state/ned` (`%LOCALAPPDATA%\ned` on Windows), so `windowWidth`, `windowHeight`, and `showFileTree` only set the first run. Delete the file to start from them again.

Sizes are Doubles, so write `18.0`, not `18`. A field that is misspelled or has the wrong type is an error, and ned starts with the defaults. ned applies changes when you save the file, except `uiFontSize`, `bufferFont`, and the window size, which need a restart.

### Language servers

A server is a command per language, where the language is the name the status bar shows (case does not matter). It runs through `languageServerShell` (`sh -c`, or `cmd /c` on Windows) in the file tree's folder:

```dhall
{ languageServers =
  [ { language = "Haskell", command = "haskell-language-server-wrapper --lsp" }
  , { language = "C", command = "clangd" }
  ]
}
```

The default is the Haskell entry above. Server stderr goes to the terminal ned was started from. If a server fails to start, `Ctrl+]`, `gd`, or `K` retries it.

### Projects

A project overrides `languageServerShell` and `languageServers` for the files under its `root`, and its servers run in that root:

```dhall
{ projects =
  { app =
    { root = "~/src/app"
    , languageServerShell = [ "nix", "develop", "-c", "sh", "-c" ]
    }
  , web =
    { root = "~/src/app/web"
    , languageServers =
      [ { language = "TypeScript", command = "typescript-language-server --stdio" }
      ]
    }
  }
}
```

Nested projects stack, so files under `~/src/app/web` get the `nix develop` shell and the TypeScript server. A list replaces the outer list rather than adding to it. A root is absolute, starts with `~`, or is relative to `config.dhall`'s folder.

## Testing

```sh
cabal test ned-test
cabal run ned -- --selftest out [FILE]
```

The self-test runs in a hidden window and writes screenshots and a log to `out`.

## Limitations

- No soft wrap, and no replace outside Vim's `:s`.
- Closing the window does not prompt to save; use `Ctrl+Q` or the File menu.
- The file tree requires a manual refresh for external changes.
- Highlighting is lexical and may be inaccurate after jumping into a file.
