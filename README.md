# Programmiersprachen und Konzepte - Projekt

## Haskell-Snake

### Vorraussetzungen:

* GHC (https://www.haskell.org/ghc/)
* cabal (https://www.haskell.org/cabal/)


### Installation:

Fehlende Module installieren:
```bash
$ cabal update
$ cabal install --lib random
$ cabal install --lib pipes
$ cabal install --lib pipes-concurrency
$ cabal install --lib ansi-terminal
```