# ned-fzf

Haskell bindings to the C port of [fzf](https://github.com/junegunn/fzf)'s
matching algorithm that
[telescope-fzf-native](https://github.com/nvim-telescope/telescope-fzf-native.nvim)
maintains. The C source is bundled and compiled with the package; see
[`cbits/README.md`](cbits/README.md) for its provenance.

`Ned.Fuzzy` is the wrapped API. `Ned.Fuzzy.Raw` is the direct FFI layer,
including the bundled `ned_fzf_match_many`, which scores a whole candidate
list in one call.

## Filtering a list

```haskell
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Ned.Fuzzy

main :: IO ()
main = do
  slab <- newSlab
  let files = candidates (V.fromList ["src/Main.hs", "src/Widgets/Text.hs", "README.md"])
  pat <- compile (defaultQuery "wte")
  hits <- matchCandidates slab pat files 200
  mapM_ (print . candidateText files) (U.toList (matchedIndices hits))
```

`matchCandidates` returns the best matches first, along with `matchedTotal`,
the number that matched before the limit applied. `matchPositions` reports
which characters of one candidate the query matched, for a picker to
highlight; ask for it only for the rows on screen.

Queries use fzf's extended syntax: `foo bar` requires both terms, `^src` and
`.hs$` anchor a term, `'exact` matches literally, `!test` excludes, and `|`
separates alternatives inside a term.

## Threads

A `Slab` is mutable scratch space the matcher writes during a match, so give
each matching thread its own. `compile` is not thread-safe either, because the
C query parser splits on `strtok`.
