-- | Specs for the fzf bindings: ranking, limits, query syntax, and the
-- character offsets a picker highlights.
module Main (main) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Ned.Fuzzy
import Test.Hspec

paths :: [Text]
paths =
  [ "src/Ned/View/Editor.hs"
  , "src/Ned/View/Tree.hs"
  , "src/Ned/Highlight/Lex.hs"
  , "src/Ned/Buffer.hs"
  , "README.md"
  , "cabal.project"
  ]

files :: Candidates
files = candidates (V.fromList paths)

-- | The candidates a query keeps, best first.
ranked :: Slab -> Text -> IO [Text]
ranked slab q = do
  pat <- compile (defaultQuery q)
  hits <- matchCandidates slab pat files (candidatesCount files)
  pure [candidateText files i | i <- U.toList (matchedIndices hits)]

main :: IO ()
main = do
  slab <- newSlab
  hspec $ do
    describe "matchCandidates" $ do
      it "keeps every candidate, in order, for an empty query" $ do
        ranked slab "" `shouldReturn` paths

      it "ranks a tighter match first" $ do
        top <- take 1 <$> ranked slab "editor"
        top `shouldBe` ["src/Ned/View/Editor.hs"]

      it "matches characters spread across the candidate" $ do
        hit <- ranked slab "nvedit"
        hit `shouldSatisfy` elem "src/Ned/View/Editor.hs"

      it "drops candidates that do not match" $ do
        ranked slab "zzz" `shouldReturn` []

      it "counts every match but returns only the limit" $ do
        pat <- compile (defaultQuery "hs")
        hits <- matchCandidates slab pat files 2
        matchedTotal hits `shouldBe` 4
        U.length (matchedIndices hits) `shouldBe` 2

      it "reports a score for every index it returns" $ do
        pat <- compile (defaultQuery "buffer")
        hits <- matchCandidates slab pat files 10
        U.length (matchedScores hits) `shouldBe` U.length (matchedIndices hits)
        U.all (> 0) (matchedScores hits) `shouldBe` True

      it "requires every space-separated term" $ do
        both <- ranked slab "view tree"
        both `shouldBe` ["src/Ned/View/Tree.hs"]

      it "excludes a term marked with !" $ do
        kept <- ranked slab "hs !lex"
        kept `shouldSatisfy` all (not . T.isInfixOf "Lex")
        kept `shouldSatisfy` (not . null)

      it "anchors a term with ^ and $" $ do
        ranked slab "^README" `shouldReturn` ["README.md"]
        ranked slab ".project$" `shouldReturn` ["cabal.project"]

      it "ignores case until the query has some" $ do
        upper <- ranked slab "readme"
        upper `shouldBe` ["README.md"]
        ranked slab "REAdme" `shouldReturn` []

    describe "score" $ do
      it "is zero when the candidate does not match" $ do
        pat <- compile (defaultQuery "xyzzy")
        score slab pat "README.md" `shouldReturn` 0

      it "prefers a match on a word boundary" $ do
        pat <- compile (defaultQuery "bar")
        onBoundary <- score slab pat "foo/bar.hs"
        inWord <- score slab pat "foobar.hs"
        onBoundary `shouldSatisfy` (> inWord)

    describe "matchPositions" $ do
      it "reports the characters the query matched" $ do
        pat <- compile (defaultQuery "ru")
        pos <- matchPositions slab pat "runner"
        pos `shouldBe` U.fromList [0, 1]

      it "counts characters, not bytes, through multi-byte text" $ do
        pat <- compile (defaultQuery "cafe")
        -- "héllo/cafe.hs": the é is two bytes, so a byte offset would be one
        -- past every character offset after it.
        pos <- matchPositions slab pat "h\233llo/cafe.hs"
        pos `shouldBe` U.fromList [6, 7, 8, 9]

      it "is empty for a candidate that does not match" $ do
        pat <- compile (defaultQuery "zz")
        matchPositions slab pat "README.md" `shouldReturn` U.empty

      it "is empty for an empty query" $ do
        pat <- compile (defaultQuery "")
        matchPositions slab pat "README.md" `shouldReturn` U.empty

    describe "candidates" $ do
      it "keeps the texts it was given" $ do
        candidatesCount files `shouldBe` length paths
        candidateText files 4 `shouldBe` "README.md"
        V.toList (candidateTexts files) `shouldBe` paths

      it "matches nothing when there is nothing to match" $ do
        pat <- compile (defaultQuery "x")
        hits <- matchCandidates slab pat (candidates V.empty) 10
        matchedTotal hits `shouldBe` 0
