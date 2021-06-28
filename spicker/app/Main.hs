module Main where

import qualified System.Random as R
import qualified Control.Monad as Data.Foldable
import qualified Pipes.Prelude as P
import System.IO (hSetBuffering, hSetEcho, stdin, stdout, BufferMode(NoBuffering))
import System.Console.ANSI (clearScreen, cursorBackward, setCursorPosition)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async ( async, waitAny, Async )
import Pipes ((>->), await, for, yield, runEffect, Pipe, Producer, Proxy, MonadTrans(lift))
import Pipes.Concurrent (performGC, fromInput, latest, spawn, toOutput, unbounded)
import Data.Monoid ((<>))

data Richtung = Hoch
               | Runter
               | Rechts
               | Links
               deriving (Show, Eq)

data Command = Verlassen | Laufen Richtung deriving (Show, Eq)

type Position = (Int, Int)
type Snake = [Position]

data Welt = Welt { snake :: Snake
                   , essen :: Position
                   , richtung :: Richtung
                   , random :: R.StdGen
                   , grenzen :: (Int, Int)
                   , punkte :: Int
                   } deriving (Show)

data Status = Spielt Welt
               | Ende
               deriving (Show)

takeUntilAfter :: Monad m => (a -> Bool) -> Pipe a a m ()
takeUntilAfter p = do
    v <- await
    yield v
    if p v then return () else takeUntilAfter p

gegenteil :: Richtung -> Richtung
gegenteil richtung = case richtung of
    Hoch -> Runter
    Runter -> Hoch
    Rechts -> Links
    Links -> Rechts

bewege :: Richtung -> Position -> Position
bewege richtung (r, c) = case richtung of
    Hoch -> (r - 1, c)
    Runter -> (r + 1, c)
    Rechts -> (r, c + 1)
    Links -> (r, c - 1)

laufe :: Snake -> Richtung -> Snake
laufe snake richtung = bewege richtung (head snake) : init snake

esse :: Snake -> Richtung -> Snake
esse snake richtung = bewege richtung (head snake) : snake

zufallsPosition :: R.RandomGen g => (Int, Int) -> g -> (Position, g)
zufallsPosition (maxr, maxc) g =
    let (r, g1) = R.randomR (1, maxr) g
        (c, g2) = R.randomR (1, maxc) g1
    in ((r, c), g2)

freiePosition :: R.RandomGen g => (Int, Int) -> g -> Snake -> (Position, g)
freiePosition lim g snake =
    head $ dropWhile imSnake (freiePositionen g)
    where imSnake (x, _) = x `elem` snake
          freiePositionen h = r:freiePositionen g'
              where r@(_, g') = zufallsPosition lim h

update :: Welt -> Richtung -> Welt
update welt neueRichtung
    | neueRichtung == gegenteil (richtung welt) = welt
    | bewege neueRichtung (head $ snake welt) == essen welt = gefressen
    | otherwise = continue
    where continue = welt { snake = laufe (snake welt) neueRichtung
                        , richtung = neueRichtung
                        }
          gefressen = welt { snake = esse (snake welt) neueRichtung
                    , richtung = neueRichtung
                    , essen = neuesEssen
                    , random = neuRandom
                    , punkte = punkte welt + schwierigkeitsgrad}
          (neuesEssen, neuRandom) = freiePosition (grenzen welt) (random welt) $ snake gefressen

wechselStatus :: Welt -> Status
wechselStatus welt
    | kollision $ snake welt = Ende
    | any (aussen $ grenzen welt) (snake welt) = Ende
    | otherwise = Spielt welt
    where
        kollision (x:xs) = x `elem` tail xs
        aussen (maxr, maxc) (r, c) =
            r < 1 || r > maxr || c < 1 || c > maxc

deltas :: Monad m => Pipe a (a,a) m ()
deltas = do
    first <- await
    P.scan remember (first, first) id
    where
        remember (_, a) b = (a, b)

transitions :: Monad m => Welt -> Proxy () Richtung () (Status, Status) m ()
transitions welt =
    P.scan update welt id
    >-> P.map wechselStatus
    >-> takeUntilAfter checkEnde
    >-> deltas
    where
        checkEnde Ende = True
        checkEnde (Spielt _) = False

eingabeVerarbeiten :: Char -> Maybe Command
eingabeVerarbeiten c = case c of
    'q' -> Just Verlassen
    'w' -> Just $ Laufen Hoch
    'a' -> Just $ Laufen Links
    's' -> Just $ Laufen Runter
    'd' -> Just $ Laufen Rechts
    _   -> Nothing

eingabeHolen :: Producer Command IO ()
eingabeHolen = forever $ do
    c <- lift getChar
    Data.Foldable.forM_ (eingabeVerarbeiten c) yield

aktuelleRichtung :: Producer Richtung IO ()
aktuelleRichtung =
    eingabeHolen
    >-> P.takeWhile (/= Verlassen)
    >-> P.map fortfahren
    where fortfahren (Laufen x) = x

start :: IO ()
start = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen

draw :: Char -> Position -> IO ()
draw char (row, col) = do
    setCursorPosition row col
    putChar char

grenzenZeichen :: Welt -> IO ()
grenzenZeichen welt = do
    let (r, c) = grenzen welt
    mapM_ (draw '▓') [(0, x) | x <- [0..c+1]]
    mapM_ (draw '▓') [(r+1, x) | x <- [0..c+1]]
    mapM_ (draw '▓') [(x, 0) | x <- [0..r+1]]
    mapM_ (draw '▓') [(x, c+1) | x <- [0..r+1]]

weltRendern :: Char -> Char -> Welt -> IO ()
weltRendern snakeChar essenChar welt = do
    draw essenChar (essen welt)
    mapM_ (draw snakeChar) (reverse $ snake welt)
    cursorBackward 1

weltZeichnen :: Welt -> IO ()
weltZeichnen = weltRendern '#' '■'

weltLoeschen :: Welt -> IO ()
weltLoeschen = weltRendern ' ' ' '

zeichnen :: (Status, Status) -> IO ()
zeichnen (Spielt alt, Spielt neu) = weltLoeschen alt >> weltZeichnen neu
zeichnen (Spielt welt, Ende) = do
    let text = "GAME OVER"
        (r, c) = grenzen welt
    clearScreen
    setCursorPosition (r`div`2) ((c - length text)`div`2)
    putStrLn text
    setCursorPosition (r`div`2 + 1) ((c - length text)`div`2 + 1)
    putStr "Punkte: "
    let endPunkte = punkte welt
    print endPunkte
    setCursorPosition r 0

anfangsWelt :: Welt
anfangsWelt = Welt { snake = [(5, x)| x <- [10..13]]
                     , essen = (5, 5)
                     , richtung = Links
                     , random = R.mkStdGen 0
                     , grenzen = grenzenStart
                     , punkte = 0
                     }

fps :: Int -> Proxy () y () y IO b
fps frames = forever $ do
    lift $ threadDelay (quot 500000 frames*2)
    await >>= yield

--von 1 bis 100, je größer desto schwieriger
-- 1 sehr leicht 
--10 einfach
--15 mittel
--25 schwieriger
--ab 30 flinke finger
schwierigkeitsgrad :: Int
schwierigkeitsgrad = 1

grenzenStart :: (Int, Int)
grenzenStart = (20,40)

main :: IO (Async (), ())
main = do
    start
    grenzenZeichen anfangsWelt
    weltZeichnen anfangsWelt

    let anfangsRichtung = richtung anfangsWelt
        run p = async $ runEffect p >> performGC
        from = fromInput
        to = toOutput

    (mO, mI) <- spawn unbounded
    (dO, dI) <- spawn $ latest anfangsRichtung

    eingabeTask <- run $ aktuelleRichtung >-> to (mO <> dO)
    delayedTask <- run $ from dI >-> fps schwierigkeitsgrad >-> to mO
    zeichnenTask <- run $ for
        (from mI >-> transitions anfangsWelt)
        (lift . zeichnen)

    waitAny [eingabeTask, zeichnenTask]