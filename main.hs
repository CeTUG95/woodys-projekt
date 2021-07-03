import System.IO
import System.Console.ANSI
import System.Random as R
----------------------------- HELPER FUNCTIONS -----------------------------

data Facing = North
    | South
    | East
    | West

data World = World {
    borders :: (Int, Int),
    snake :: [(Int, Int)],
    facing :: Facing,
    snack :: (Int, Int)
}

world :: World
world = World {
    borders = (25, 60),
    snake = [(5, x) | x <- [18..22]],
    facing = West,
    snack = (10, 10)
}

getRandom :: Int -> Int -> IO Int
getRandom x y = getStdRandom (randomR (x,y))

draw :: Char -> (Int, Int) -> IO ()
draw char (row, col) = do
    setCursorPosition row col           -- funktion aus System.Console.ANSI
    putChar char

write :: String -> (Int, Int) -> IO()
write string (row, col) = do
        setCursorPosition row col
        putStr string

writeCenter :: String -> (Int, Int) -> IO()
writeCenter string (row, cols) = do
    write string (row, ((cols `div` 2) - (length string `div` 2)) + 1)

-- Border Types:
--  ┌──┬──┐  ╔══╦══╗
--  │  │  │  ║  ║  ║
--  ├──┼──┤  ╠══╬══╣
--  │  │  │  ║  ║  ║
--  └──┴──┘  ╚══╩══╝
--     1        2

drawBorders :: (Int, Int) -> IO()
drawBorders (rows, cols) = do
    mapM_ (draw '═') [(0, x) | x <- [0..cols+1]]        -- oberer Rand
    mapM_ (draw '═') [(2, x) | x <- [0..cols+1]]        -- mittlere Linie
    mapM_ (draw '═') [(rows+3, x) | x <- [0..cols+1]]   -- unterer Rand
    mapM_ (draw '║') [(x, 0) | x <- [0..rows+3]]        -- linker Rand
    mapM_ (draw '║') [(x, cols+1) | x <- [0..rows+3]]   -- rechter Rand
    draw '╔' (0, 0)
    draw '╗' (0, cols+1)
    draw '╠' (2, 0)
    draw '╣' (2, cols+1)
    draw '╚' (rows+3, 0)
    draw '╝' (rows+3, cols+1)

clearField :: Int -> Int -> IO()
clearField 0 cols = do
    mapM_ (draw ' ') [(0, x) | x <- [0..cols]]
clearField count cols = do
    mapM_ (draw ' ') [(count, x) | x <- [0..cols]]
    let rest = (count - 1)
    clearField rest cols

drawSingleplayerScore :: (Int, Int) -> IO()
drawSingleplayerScore (rows, cols) = do
    write " Score: " (1, 1)
    write "0" (1, (length(" Score: ") + 1))    -- Punkte hinter Score anzeigen
    draw '╦' (0, (length(" Score: ") + 6))
    draw '║' (1, (length(" Score: ") + 6))
    draw '╩' (2, (length(" Score: ") + 6))

drawMultiplayerScore :: (Int, Int) -> IO()
drawMultiplayerScore (rows, cols) = do
    -- spieler 1 punktzahl interface
    write " Player 1: " (1, 1)
    let lengthPlayer = length " Player 1: "
    write "0" (1, (lengthPlayer + 1))    -- Punkte hinter Score anzeigen
    draw '╦' (0, (lengthPlayer + 6))
    draw '║' (1, (lengthPlayer + 6))
    draw '╩' (2, (lengthPlayer + 6))

    -- spieler 2 punktzahl interface
    write "Player 2: " (1, cols - lengthPlayer - 4)
    let lengthPlayer = length "Player 2: "
    draw '╦' (0, (cols - lengthPlayer - 7))
    draw '║' (1, (cols - lengthPlayer - 7))
    draw '╩' (2, (cols - lengthPlayer - 7))
    write "0" (1, ( cols - 4))    -- Punkte hinter Score anzeigen

---------------------------- GAME FUNCTIONALITY ----------------------------

-- spawnSnack :: World -> IO()
-- spawnSnack world = do
--     let (rows, cols) = (borders world)
--     randX <- uniformR (3, (rows+2))
--     randY <- uniformR (1, cols)
--     world { snack = (randX, randY)}
--     let (snackx, snacky) = snack world
--     write 'O' (snackx, snacky)

spawnSnake :: World -> IO()
spawnSnake world = do
    mapM_ (write "█") (reverse $ (snake world))
    let (rows, cols) = (borders world)
    setCursorPosition (rows+3) (cols+2)

move :: Facing -> (Int, Int) -> (Int, Int)
move direction (posx, posy) = do
    case direction of
        North -> (posx, (posy+1))
        West -> ((posx-1), posy)
        South -> (posx, (posy-1))
        East -> ((posx+1), posy)

getDirection :: Char -> World -> World
getDirection input world = case input of
        'w' -> world { facing = North }
        'a' -> world { facing = West }
        's' -> world { facing = South }
        'd' -> world { facing = East }

moveSnake :: World -> World IO ()
moveSnake world = do
    let (rows, cols) = (borders world)
    let body = (snake world)
    let newHead = move (facing world) (head body)
    let newBody = [newHead] ++ init body
    world = { snake = newBody } -- Problem hier, weil World und IO geändert wird.. was tun?
    clearField (rows+3) (cols+1)
    spawnSnake world

singleplayer :: World -> IO()
singleplayer world = do
    let (rows, cols) = (borders world)
    clearField (rows+3) (cols+1)
    drawBorders (rows, cols)
    writeCenter "Singleplayer" (1, cols)
    drawSingleplayerScore (rows, cols)
    spawnSnake world
    setCursorPosition (rows+3) (cols+2)
    input <- getChar
    getDirection input world
    moveSnake world

multiplayer :: World -> IO()
multiplayer world = do
    let (rows, cols) = (borders world)
    clearField (rows+3) (cols+1)
    drawBorders (rows, cols)
    writeCenter "Multiplayer" (1, cols)
    writeCenter "Sorry ... not implemented yet :(" (15, cols)
    drawMultiplayerScore (rows, cols)
    setCursorPosition (rows+3) (cols+2)

credits :: World -> IO()
credits world = do
    let (rows, cols) = (borders world)
    clearField (rows+3) (cols+1)
    drawBorders (rows, cols)
    writeCenter "Credits" (1, cols)
    writeCenter "Written by" ((rows `div` 2), cols)
    writeCenter "Tugay" ((rows `div` 2) + 2, cols)
    writeCenter "Niaz" ((rows `div` 2) + 3, cols)
    writeCenter "Basti" ((rows `div` 2) + 4, cols)
    setCursorPosition (rows+3) (cols+2)

mainMenu :: World -> IO()
mainMenu world = do
    let (rows, cols) = (borders world)
    writeCenter "Main Menu" (1, cols)
    writeCenter "1 Singleplayer" ((rows `div` 2), cols)
    writeCenter "2 Multiplayer" ((rows `div` 2) + 1, cols)
    writeCenter "3 Credits" ((rows `div` 2) + 2, cols)
    writeCenter "Select mode..." ((rows `div` 2) + 4, cols)
    setCursorPosition (rows+3) (cols+2)
    mode <- getChar
    case mode of
        '1' -> singleplayer world
        '2' -> multiplayer world
        '3' -> credits world
        _ -> writeCenter "Invalid selection! Try again..." ((rows `div` 2) + 4, cols)

-- funktion um alle einstellungen zum start vorzunehmen
setup :: IO()
setup = do
    hSetBuffering stdin NoBuffering     -- bei input nicht mehr auf newline warten
    hSetBuffering stdout NoBuffering    -- bei output  nicht mehr auf newline warten
    hSetEcho stdin False                -- verhindert das printen der gedrückten tasten

main :: IO ()
main = do
    let rows = 25
    let cols = 60
    setup                       -- einstellungen zum start vornehmen
    clearScreen                 -- funktion aus System.Console.ANSI
    drawBorders(rows, cols)     -- Weltgrenzen zeichnen mit anzahl zeilen und spalten
    mainMenu world        -- Hauptmenü erzeugen