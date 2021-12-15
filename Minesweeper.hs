----------------------------
-- Minesweeper in Haskell --
--      Evan Nichols      --
----------------------------
import Data.Char
import Data.List
import System.Random
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)
--import System.Console.ANSI
--import Control.Monad

-- TYPES                -- Mainly for more clear/compact reading.
type Size = Int         -- Board Size == Difficulty (increases MY readability, probably decreases others')
type BoolGG = Bool      -- Game is live/over
type Board = [[Int]]    -- Simply for my own sanity
type Cell = (Int, Int)  -- Coordinates of a spot on the board
type Command = (Bool, Int, Int) -- Workable format of user input
type StateR = (Int, Board, BoolGG, Size)        -- Probably not helpful for others, but makes reading the code easier for me (program is small)
type StateC = (Int, Int, Board, BoolGG, Size)   -- see above ^
-- DIFFICULTY VALUES
val_beg = 1     -- Beginner difficulty = 8x8
val_int = 2     -- Intermediate difficulty = 16x16
val_exp = 3     -- Expert difficulty = 24x24
-- CELL VALUES
val_empty = 0    -- Empty
val_emptyF = 1   -- Empty w/ Flag
val_emptyClr = 2 -- Empty & Cleared
val_mine = 3     -- Mine
val_mineF = 4    -- Mine w/ Flag
-- BOARD PRINTING LISTS
dis_Col = ["    1  2  3  4  5  6  7  8",
           "    1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 ",
           "    1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 "]
dis_BorTop = ["  ┌────────────────────────┐",
               "  ┌────────────────────────────────────────────────┐ ",
               "  ┌────────────────────────────────────────────────────────────────────────┐ "]
dis_BorBot = ["  └────────────────────────┘",
               "  └────────────────────────────────────────────────┘ ",
               "  └────────────────────────────────────────────────────────────────────────┘ "]


{- MAIN -}
main = do
    msg_intro           -- Intro Message
    d <- difficulty     -- Choose difficulty function
    if (d == "1") then main_setup 8         -- Beginner (8x8) diff. selected
    else if (d == "2") then main_setup 16   -- Intermediate (16x16) diff. selected
    else do main_setup 24                   -- Expert (24x24) diff. selected

main_setup d = do
    rng <- newStdGen            -- Random #, should be new each run of 'main' (getStdGen does not)
    let board = creation d rng  -- Build board
    main_driver board d         -- Pass board & seed to main game loop

main_driver mat d = do
    display_B mat True d    -- Show the board upon new game state
    cmd <- cmd_read d       -- Get command from player
    let lose = explode mat d cmd    -- Needs "let" because 'Bool -> IO Bool'
    if lose then do             -- Game over
        display_B mat False d   -- Reveal all mines
        loser
    else do                             -- Game is still live
        let update = refresh mat d cmd  -- Get new board after command is played
        if winner update then do        -- Game won!
            display_B update True d
            msg_win
        else do
            main_driver update d    -- Move to next game state

exit = do
    putStrLn "'Til next time."
    exitSuccess


{- GAME MOMENTS -}
difficulty :: IO (String)
difficulty = do
    msg_diff
    xs <- getLine   -- read input
    putStrLn " "
    if (xs == "quit") then exit
    else if elem xs ["1", "2", "3"] then return xs   -- valid diff options
    else do -- Loop until 1,2, or 3
        msg_diff_invalid
        difficulty

creation :: RandomGen g => Size -> g -> Board   -- Make fresh board w/ random seed
creation diff seed = matrix_make diff (getRandoms diff seed)

getRandoms :: RandomGen g => Size -> g -> [Int]
getRandoms d seed = map (\x -> if x == 1 then val_mine else val_empty) (take (d * d) rs)
    where rs = randomRs (0::Int, 4::Int) seed

refresh :: Board -> Size -> Command -> Board
refresh mat d (f,x,y)
    | not (cell_valid d (x, y)) = mat   -- Nothing changes w/ invalid coords
    | not f && (mat!!x!!y /= val_emptyClr) && (cell_adjMine mat (x, y) d == 0) =
        ref_fold newmat nhbrs
    | otherwise = newmat
    where nhbrs = [(f, x + i, y + j) | i <- [-1 .. 1], j <- [-1 .. 1], i /= 0 || j /= 0]    -- List of Command
          newmat = matrix_update (x, y) mat (cell_updateVal (mat!!x!!y) f)                  -- Board
          ref_fold mat' [] = mat'
          ref_fold mat' (n:ns) = ref_fold (refresh mat' d n) ns

explode :: Board -> Size -> Command -> Bool
explode mat d (True, _, _) = False   -- Can't hit a mine if you're just flagging it
explode mat d (False, x, y) = cell_isMine mat d (x, y)    -- Check for actual mine

winner :: Board -> Bool
winner mat = length unfin == 0  -- # of rows in Board with unfinished cells is 0
    where unfin = [x | x <- filter (\c -> (length (filter (\c' -> c' <= val_emptyF) c)) /= 0) mat]
    -- OR '\c' -> c' /= val_emptyClr && c' /= val_mineF', not sure which is better yet

loser :: IO ()
loser = do
    msg_lose
    replay

replay :: IO ()
replay = do
    msg_replay
    rs <- getLine
    if (elem rs ["Y", "y", "Yes", "yes"]) then main
    else if (elem rs ["N", "n", "No", "no"]) then exit
    else do
        replay
    

{- CELL OPERATIONS -}
cell_valid :: Size -> Cell -> Bool   -- Cell coords are w/in bounds of Board
cell_valid d (x, y) = (x >= 0 && x < d) && (y >= 0 && y < d)

cell_nhbrs :: Size -> Cell -> [Cell] -- Return list of Coords for neighbring cells
cell_nhbrs d (x, y) =
    filter (cell_valid d) [(x + i, y + j) | i <- [-1 .. 1], j <- [-1 .. 1], i /= 0 || j /= 0]   -- Courtesy of Prof.

cell_isMine :: Board -> Size -> Cell -> Bool
cell_isMine mat d (x, y) | cell_valid d (x, y) = (mat!!x!!y) >= val_mine  -- is mine or flagged mine
                         | otherwise = False

cell_adjMine :: Board -> Cell -> Size -> Int
cell_adjMine mat (x, y) d =
    length [p | p <- cell_nhbrs d (x, y), cell_isMine mat d p]

cell_updateVal :: Int -> Bool -> Int
cell_updateVal v f | f && (elem v [1, 4]) = (v - 1)         -- Remove flage (must be 1st)
                   | f && (v == val_empty) = val_emptyF     -- Flag, empty cell
                   | f && (v >= val_mine) = val_mineF       -- Flag, mine
                   | otherwise = val_emptyClr               -- Clear space


{- PLAYER COMMANDS -}
cmd_read :: Size -> IO (Command)
cmd_read d = do
    cmd <- getLine-- Read input
    let cmdConv = cmd_parse cmd -- Get formatted command from input
    if (cmd == "quit") then exit
    else if (cmd_inputCheck cmd) && (cmd_valid d cmdConv) then return cmdConv  -- Return valid command
    else do
        msg_cmd_invalid
        cmd_read d          -- Loop back until command input is valid

cmd_inputCheck :: String -> Bool     -- Check if the input command is a set of valid characters
cmd_inputCheck str = (all isAlphaOrDigit str) && not (all isAlpha str) && not (all isDigit str) -- **Not very elegant

cmd_parse :: String -> Command     -- Returns (Flag Bool, X coord, Y coord)
cmd_parse cmd | (cmd!!0) == 'F' = (True, (ord (cmd!!1) - (ord 'a')), (col1 - 1))    -- Flag on space
              | otherwise = (False, (ord (cmd!!0) - (ord 'a')), (col2 - 1))         -- Step on space
    where col1 = read (drop 2 cmd) :: Int   -- Gets column from "F_#"
          col2 = read (drop 1 cmd) :: Int   -- Gets column from "_#"

cmd_valid :: Size -> Command -> Bool
cmd_valid d (_, x, y) = cell_valid d (x, y)   -- The input coords (x, y) are in the bounds of the game board 'd x d'


{- DISPLAY BOARD -}
{- ASCII Symbols
218 ┌ ┐ 191     201 ╔ ╗ 187     254 ■
179 │ ─ 196     186 ║ ═ 205     207 ¤ need to fix utf-8 or something
192 └ ┘ 217     200 ╚ ╝ 188

30 - Black, 31 - Red, 32 - Green, 33 - Yellow
34 - Blue, 35 - Magenta, 36 - Cyan, 37 - White
putStrLn $ "\x1b[32m"   -- highlight all after this (ansi) esc. seq.
putStrLn $ "\x1b[32m" ++ "highlight me" ++ "\x1b[0m" ++ " but not me"     -- pass 0 to stop highlighting
-}
display_B :: Board -> BoolGG -> Int -> IO ()
display_B mat live d = do     -- Looks like Battleship
    let d' = (div d 8) - 1
    putStrLn $ dis_Col!!d'      -- " 1 2 3 ... "
    putStrLn $ dis_BorTop!!d'   -- " ┌─...─┐ "
    sequence (map_R display_R mat mat live d)   -- Combine each row printing to one event
    putStrLn $ dis_BorBot!!d'   -- " └─...─┘ "
    putStrLn $ dis_Col!!d'      -- " 1 2 3 ... "

display_R :: [Int] -> StateR -> IO ()
display_R row (i, mat, live, d) = do
    putStr ([chr (ord 'a' + i)] ++ " │")            -- Print "row letter" on left of board
    sequence (map_C display_C row i mat live d)     -- Combine each cell printing to one event
    putStrLn ("│ " ++ [chr (ord 'a' + i)])          -- Print "row letter" on right of board

display_C :: Int -> StateC -> IO ()
display_C val (col, row, mat, live, d)
    | not live && (val >= val_mine) = putStr $ "\x1b[31m" ++ " * " ++ "\x1b[0m" -- Game Over => Reveal all mines (in red)
    | val == val_emptyClr && clue == 0 = putStr "   "   -- Empty and cleared space, no adj. mines
    | elem val [0, 1, 3, 4] = putStr $ colors!!val ++ symbols!!val ++ colors!!0     -- Empty, EmptyFlag, Mine, MineFlag
    | otherwise = putStr $ " " ++ show clue ++ " "      -- Show # adj. mines
    where clue = cell_adjMine mat (row, col) d          -- Get # of adj. mines
          symbols = [" ■ ", " F ", "   ", " ■ ", " F "] -- 0 Empty, 1 EmptyFlag, 2 EmptyClr, 3 Mine, 4 MineFlag
          colors = ["\x1b[0m", "\x1b[36m", " ", "\x1b[0m", "\x1b[36m"]  -- Reset, Cyan, -, Reset, Cyan


{- UTILITIES -}
matrix_make :: Size -> [Int] -> [[Int]]
matrix_make s rs = [[rs!!((s * i') + i) | i <- [0..s-1]] | i' <- [0..s-1]]

matrix_update :: Cell -> [[a]] -> a -> [[a]]
matrix_update (x, y) ms e = list_update x ms $ list_update y (ms !! x) e

list_update :: Int -> [a] -> a -> [a]
list_update _ [] _ = []  -- Base
list_update i (x:xs) e | i == 0 = e:xs
                       | otherwise = x : list_update (i - 1) xs e

--map :: (a -> b) -> [a] -> [b]    (for my own reference)
--map _ []     = []
--map f (x:xs) = f x : map f xs
map_matrix :: (a -> b) -> [[a]] -> [[b]]
map_matrix f ms = map (map f) ms

map_R :: (a -> StateR -> b) -> [a] -> Board -> BoolGG -> Size -> [b]
map_R f i mat live d = zipWith f i [(i', mat, live, d) | i' <- [0..]]       -- map over each row at index i'

map_C :: (a -> StateC -> b) -> [a] -> Int -> Board -> BoolGG -> Size -> [b]
map_C f i x mat live d = zipWith f i [(i', x, mat, live, d) | i' <- [0..]]  -- map over each cell at index i' on row 'x'

isAlphaOrDigit :: Char -> Bool
isAlphaOrDigit c = isAlpha c || isDigit c

strToInt0 :: [Char] -> Int
strToInt0 xs | all isDigit xs = d
             | otherwise = 0
    where d = read xs :: Int


{- OUTPUT MESSAGES -}
putStrLn_slow :: [Char] -> IO ()
putStrLn_slow "" = do
    putStrLn ""
    return ()
putStrLn_slow (x:xs) = do
    putChar x
    threadDelay 10000       -- Pause, 50000 was good but too slow for testing
    putStrLn_slow xs

msg_intro :: IO ()
msg_intro = do
    putStrLn " "
    putStrLn_slow "Welcome, Minesweeper."
    putStrLn_slow "The goal of the game is to mark each mine with a flag."
    putStrLn_slow "To uncover a spot, enter coordinates [row][col], e.g. 'b6'."
    putStrLn_slow "To flag a spot, add the prefix 'F', e.g. 'Fe4'."
    putStrLn_slow "A number on a spot represents the number of mines adjacent to that spot."
    putStrLn_slow "If you wish to quit at any time, enter 'quit'."

msg_diff :: IO ()
msg_diff = do
    putStrLn " "
    putStrLn_slow "Choose which size board to play."
    putStrLn_slow "[1] Beginner difficulty is 8x8."
    putStrLn_slow "[2] Intermediate is 16x16."
    putStrLn_slow "[3] Expert is 24x24."
    putStr "Enter 1, 2, or 3: "

msg_diff_invalid :: IO ()
msg_diff_invalid = do
    putStrLn_slow "You must enter '1', '2', or '3'."

msg_cmd_invalid :: IO ()
msg_cmd_invalid = do
    putStrLn "Invalid command."
    putStrLn "To uncover a spot, enter coordinates [row][col], e.g. 'b6'."
    putStrLn "To flag a spot, add the prefix 'F', e.g. 'Fe4'."

msg_lose :: IO ()
msg_lose = putStrLn_slow "You hit a mine! GG"   -- D:

msg_replay :: IO ()
msg_replay = do
    putStrLn " "
    putStrLn_slow "Would you like to play again?"
    putStrLn_slow "Enter [Y] yes, [N] no..."

msg_win :: IO ()
msg_win = putStrLn_slow "You've successfully secured the field!"    -- Congrat.
