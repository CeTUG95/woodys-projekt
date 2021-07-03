import System.IO
import System.Console.ANSI

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


drawSingleplayerScore :: (Int, Int) -> IO()
drawSingleplayerScore (rows, cols) = do
    write "Score: " (1, 1)
    write "0" (1, (length("Score: ") + 1))    -- Punkte hinter Score anzeigen
    draw '╦' (0, (length("Score: ") + 6))
    draw '║' (1, (length("Score: ") + 6))
    draw '╩' (2, (length("Score: ") + 6))

drawMultiplayerScore :: (Int, Int) -> IO()
drawMultiplayerScore (rows, cols) = do
    -- spieler 1 punktzahl interface
    write "Player 1: " (1, 1)
    let lengthPlayer = length "Player 1: "
    write "0" (1, (lengthPlayer + 1))    -- Punkte hinter Score anzeigen
    draw '╦' (0, (lengthPlayer + 6))
    draw '║' (1, (lengthPlayer + 6))
    draw '╩' (2, (lengthPlayer + 6))

    -- spieler 2 punktzahl interface
    write "Player 2: " (1, cols - lengthPlayer - 4)
    let lengthPlayer = length "Player 2: "
    draw '╦' (0, (cols - lengthPlayer - 6))
    draw '║' (1, (cols - lengthPlayer - 6))
    draw '╩' (2, (cols - lengthPlayer - 6))
    write "0" (1, ( cols - 4))    -- Punkte hinter Score anzeigen

spawnSnake :: (Int, Int) -> IO()


singleplayer :: (Int, Int) -> IO()
singleplayer (rows, cols) = do
    clearScreen
    drawBorders (rows, cols)
    writeCenter "Singleplayer" (1, cols)
    drawSingleplayerScore (rows, cols)
    setCursorPosition (rows+3) (cols+2)
    spawnSnake

multiplayer :: (Int, Int) -> IO()
multiplayer (rows, cols) = do
    clearScreen
    drawBorders (rows, cols)
    writeCenter "Multiplayer" (1, cols)
    drawMultiplayerScore (rows, cols)
    setCursorPosition (rows+3) (cols+2)


credits :: (Int, Int) -> IO()
credits (rows, cols) = do
    clearScreen
    drawBorders(rows, cols)
    writeCenter "Credits" (1, cols)
    writeCenter "Written by" ((rows `div` 2), cols)
    writeCenter "Tugay" ((rows `div` 2) + 2, cols)
    writeCenter "Niaz" ((rows `div` 2) + 3, cols)
    writeCenter "Basti" ((rows `div` 2) + 4, cols)
    setCursorPosition (rows+3) (cols+2)


mainMenu :: (Int, Int) -> IO()
mainMenu (rows, cols) = do
    writeCenter "Main Menu" (1, cols)
    writeCenter "1 Singleplayer" ((rows `div` 2), cols)
    writeCenter "2 Multiplayer" ((rows `div` 2) + 1, cols)
    writeCenter "3 Credits" ((rows `div` 2) + 2, cols)
    writeCenter "Select mode..." ((rows `div` 2) + 4, cols)
    setCursorPosition (rows+3) (cols+2)
    mode <- getChar
    case mode of
        '1' -> singleplayer (rows, cols)
        '2' -> multiplayer (rows, cols)
        '3' -> credits (rows, cols)
        _ -> writeCenter "Invalid selection! Try again..." ((rows `div` 2) + 4, cols)

-- funktion um alle einstellungen zum start vorzunehmen
setup :: IO()
setup = do
    hSetBuffering stdin NoBuffering     -- bei input nicht mehr auf newline warten
    hSetBuffering stdout NoBuffering    -- bei output  nicht mehr auf newline warten
    hSetEcho stdin False                -- verhindert das printen der gedrückten tasten


main :: IO ()
main = do
    setup                   -- einstellungen zum start vornehmen
    clearScreen             -- funktion aus System.Console.ANSI
    drawBorders(25, 60)     -- Weltgrenzen zeichnen mit anzahl zeilen und spalten
    mainMenu(25, 60)        -- Hauptmenü erzeugen