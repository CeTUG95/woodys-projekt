# Programmiersprachen und Konzepte - Projekt

## Haskell-Snake

### Vorraussetzungen:

* Funktioniert nur auf Linux Systemem (wegen dem Package System.Console.ANSI)
* GHC (https://www.haskell.org/ghc/)
* cabal (https://www.haskell.org/cabal/)

### Installation Linux:

Fehlende Module installieren:
```bash
$ cabal update
$ cabal install random
$ cabal install pipes
$ cabal install pipes-concurrency
$ cabal install ansi-terminal
```
