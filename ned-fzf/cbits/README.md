# Vendored C sources

`fzf.c` and `fzf.h` are copied verbatim from
[telescope-fzf-native.nvim](https://github.com/nvim-telescope/telescope-fzf-native.nvim),
which is a C port of the matching algorithm from
[fzf](https://github.com/junegunn/fzf).

    upstream: https://github.com/nvim-telescope/telescope-fzf-native.nvim
    path:     src/fzf.c, src/fzf.h
    commit:   b25b749b9db64d375d782094e2b9dce53ad53a40
    date:     2026-05-06

Both files are MIT licensed; the upstream licence is kept beside them as
`LICENSE.fzf`. Keep them unmodified so a newer upstream can be dropped in:
everything this package adds lives in `ned_fzf.c`.

`ned_fzf.c` is ned's own code. It scans a whole candidate arena in one
call, so matching a query against a hundred thousand paths costs one FFI call
instead of one per path.
