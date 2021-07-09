import System.IO
import System.Console.ANSI
import System.Random as R
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Pipes
import Pipes.Concurrent
----------------------------- HELPER FUNCTIONS -----------------------------

data Facing = North
    | South
    | East
    | West
    deriving (Show, Eq)

data World = World {
    borders :: (Int, Int),
    snake :: [(Int, Int)],
    snakeLast :: [(Int, Int)],
    facing :: Facing,
    snack :: (Int, Int),
    randomSeed :: R.StdGen,
    score :: Int
}

data Event = ChangeDirection Facing | NotDoingAThing | UpdateGame deriving (Show)

world :: World
world = World {
    borders = (25, 60),
    snake = [(5, x) | x <- [18..22]],
    snakeLast = [(5, x) | x <- [18..22]],
    facing = West,
    snack = (10, 10),
    randomSeed = R.mkStdGen 0,
    score = 0
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

drawSingleplayerScore :: World -> (Int, Int) -> IO()
drawSingleplayerScore world (rows, cols) = do
    write " Score: " (1, 1)
    write (show (score world)) (1, (length(" Score: ") + 1))    -- Punkte hinter Score anzeigen
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

drawSnake :: [(Int, Int)] -> (Int, Int) -> String -> IO()
drawSnake snake borders c = do
    mapM_ (write c) (reverse $ snake)
    let (rows, cols) = borders
    setCursorPosition (rows+3) (cols+2)

move :: Facing -> (Int, Int) -> (Int, Int)
move direction (posx, posy) = do
    case direction of
        North -> ((posx-1), posy)
        West -> (posx, (posy-1))
        South -> ((posx+1), posy)
        East -> (posx, (posy+1))

moveSnake :: World -> World
moveSnake world = do
    let (rows, cols) = (borders world)
    let body = (snake world)
    let newHead = move (facing world) (head body)
    let newBody = [newHead] ++ init body
    let newWorld = world { snake = newBody, snakeLast = body }
    newWorld

checkFacing :: World -> Facing -> Bool
checkFacing world direction | direction == North && (facing world) == South = False
                            | direction == South && (facing world) == North = False
                            | direction == East && (facing world) == West = False
                            | direction == West && (facing world) == East = False
                            | otherwise = True


user :: Producer Event IO ()
user = forever $ do
    input <- lift getChar
    case input of
        'w' -> yield(ChangeDirection North)
        'a' -> yield(ChangeDirection West)
        's' -> yield(ChangeDirection South)
        'd' -> yield(ChangeDirection East)
        _ -> yield(NotDoingAThing)

drawSnack :: World -> IO()
drawSnack world = do
    let (snackx, snacky) = (snack world)
    write "O" (snackx, snacky)

snackSpawn :: World -> World
snackSpawn world = do
    let (rows, cols) = (borders world)
    let g = (randomSeed world)
    let (randX, seed) = R.randomR (0, rows) g
    let (randY, seed1) = R.randomR (3, cols+2) seed
    let newWorld = world { snack = (randX, randY), randomSeed = seed1 }
    newWorld

riseScore :: World -> World
riseScore world = do
    let newScore = (score world) + 1
    let newWorld = world { score = newScore }
    let newSnakeWorld = newWorld { snake = (snake newWorld) ++ [last (snake newWorld)] }
    newSnakeWorld

detectCollisionSnake :: (Int, Int) -> (Int, Int) -> Bool
detectCollisionSnake snakeHead snakePart = snakeHead == snakePart

detectCollision :: World -> Bool
detectCollision world = do
    let (rows, cols) = (borders world)
    let (y, x) = head (snake world)
    --  links    rechts      oben       unten         schlange
    if (x < 2 || x >= cols || y <= 3 || y > rows+1 || (elem True $ map (\snakePart -> detectCollisionSnake (head (snake world)) snakePart) (tail (snake world)))) then
        False
    else
        True

update :: Producer Event IO r
update = forever $ do
    lift $ threadDelay 250000 -- Wait 250 ms
    yield (UpdateGame)

gameOver :: World -> IO()
gameOver world = do
    let (rows, cols) = (borders world)
    clearField (rows+3) (cols+1)
    drawBorders (rows, cols)
    writeCenter "GAME OVER" (((rows `div` 2)+2), cols)
    writeCenter ("Score " ++ show (score world)) (((rows `div` 2)+4), cols)
    writeCenter "Press any key to continue." (((rows `div` 2)+10), cols)
    setCursorPosition (rows+3) (cols+2)


singleplayer :: Consumer Event IO ()
singleplayer = loop world
  where
    loop world = do
        let (rows, cols) = (borders world)
        lift $ drawSnake (snakeLast world) (borders world) " "
        -- lift $ clearField (rows+3) (cols+1)
        lift $ drawBorders (rows, cols)
        lift $ writeCenter "Singleplayer" (1, cols)
        lift $ drawSingleplayerScore world (rows, cols)
        lift $ drawSnack world
        lift $ drawSnake (snake world) (borders world) "█"
        let newWorld =  if head (snake world) == (snack world)
                        then snackSpawn (riseScore world)
                        else world
        event <- await
        case event of
            ChangeDirection facing -> if checkFacing newWorld facing
                                      then loop (newWorld {facing = facing})
                                      else loop newWorld
            UpdateGame ->   if detectCollision world then
                                loop (moveSnake newWorld)
                            else
                                lift $ gameOver newWorld
            _ -> loop newWorld

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
    clearField (rows+3) (cols+1)
    drawBorders (rows, cols)
    writeCenter "Main Menu" (1, cols)
    writeCenter "1 Singleplayer" ((rows `div` 2), cols)
    writeCenter "2 Multiplayer" ((rows `div` 2) + 1, cols)
    writeCenter "3 Credits" ((rows `div` 2) + 2, cols)
    writeCenter "Select mode..." ((rows `div` 2) + 4, cols)
    setCursorPosition (rows+3) (cols+2)
    mode <- getChar
    case mode of
        '1' -> do
            let (rows, cols) = (borders world)
            clearField (rows+3) (cols+1)
            drawBorders (rows, cols)
            writeCenter "Singleplayer" (1, cols)
            drawSingleplayerScore world (rows, cols)
            (output, input) <- spawn unbounded
            forkIO $ do runEffect $ user >-> toOutput output
                        performGC -- Garbage collection for pipes
            forkIO $ do runEffect $ update  >-> toOutput output
                        performGC
            runEffect $ fromInput input >-> singleplayer
            performGC
            getChar
            main
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