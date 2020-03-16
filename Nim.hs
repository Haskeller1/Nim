-- Havar Andre Melheim Salbu

module Nim where 

import Data.Char
import System.Random

type Board = [Int]
type BoardSize = Int
type Move = (Int,Int)

data Instruction = Quit | Game Game | Info | Move Move | Invalid deriving(Eq)
data Game = Nim BoardSize | Chomp BoardSize deriving(Eq)
data Player = User | AI deriving(Eq)

-- Diverse meldingar:
mainMenu = putStr "n(im) x / c(homp) x / q > "
nimMenu = putStr "Nim: r a / ? / q > "
chompMenu = putStr "Chomp: r k / ? / q > "
nimInfo = putStrLn "Fjern a brikker fra rad r. \nVinner er den som fjerner siste brikke.\n"
chompInfo = putStrLn ("r angir radnummer og k kolonnenummer, \
                      \Eit trekk forer til at brikker fjernes fra alle posisjoner ri ki \
                      \med ri <= r og ki >= k. Vinner er den som ikkje fjerner siste brikke.")
ugyldigKommando = putStrLn "Ugyldig kommando prov igjen!\n"
ugyldigBrettstorrelse = putStrLn "Ugyldig brettstorrelse, brettstorrelsen ma vere >=1!"
ugyldigTrekk = putStrLn "Ugyldig trekk, prov igjen!\n"

spill :: IO()    -- Hovudfunksjonen.
spill = do mainMenu
           xs <- getLine
           let game = parse (tokenize xs)
           case game of 
             (Game (Nim i)) -> if i < 1 then do {ugyldigBrettstorrelse; spill} else do {let {board = [1..i]}; putBoard i board; generiskSpill (Nim i) User board}
             (Game (Chomp i)) -> if i < 1 then do {ugyldigBrettstorrelse; spill} else do {let {board = take i (repeat i)}; putBoard i board; generiskSpill (Chomp i) User board}
             (Quit) -> return()
             (otherwise) -> do {ugyldigKommando; spill}

generiskSpill :: Game -> Player -> Board -> IO() 
generiskSpill g p b = case (g,p) of 
                       ((Nim l),User) -> do nimMenu
                                            ys <- getLine
                                            let action = parse (tokenize ys)
                                            case action of 
                                              (Move (i,j)) -> do let {game = (Nim l); move = (i,j)}; 
                                                                 if (validateMove game move b) 
                                                                        then do let updatedBoard = (performMove game move b)
                                                                                (putBoard l updatedBoard)
                                                                                if checkEmptyBoard updatedBoard 
                                                                                        then do {putStrLn "Du vant!"; spill}  
                                                                                        else generiskSpill g AI updatedBoard 
                                                                        else do {ugyldigTrekk; generiskSpill g User b}
                                              (Info) -> do {nimInfo ; generiskSpill g User b}
                                              (Quit) -> spill
                                              (otherwise) -> do {ugyldigKommando; generiskSpill g User b}
                       ((Nim l),AI) -> do putStrLn "Maskinens trekk."
                                          let game = (Nim l)
                                          let convertedRows = convertToBase2 $ sortRows $ zipRows b
                                          let possibleRows = getPossibleRows (getPowerCount convertedRows) convertedRows 
                                          if possibleRows == [] 
                                                then do let possibleMoves = getListOfAllPossibleMoves game 1 b b 
                                                        choice <- (randomRIO(0, length(possibleMoves)-1))
                                                        let move = possibleMoves!!choice
                                                        let updatedBoard = (performMove game move b)              -- Strategien til AI i NIM er a passe  
                                                        putBoard l updatedBoard                                      -- pa a holde Nim summen balansert  
                                                        if checkEmptyBoard updatedBoard                              -- dvs eit jamt antall av kvar 2ar
                                                                then do {putStrLn "Du tapte!"; spill}                -- potens nar turen gar vidare.  
                                                                else generiskSpill g User updatedBoard               -- Om maskina mottek eit ubalansert brett      
                                                else do choice <- (randomRIO(0, length(possibleRows)-1))             -- er maskina garantert a vinne.
                                                        let removedRow = getRow choice possibleRows                  -- Er Nim summen balansert nar AI mottar
                                                        let remainingRows = removeRow (fst removedRow) convertedRows -- brettet vil AI utfore eit tilfeldig trekk.
                                                        let y = map (`mod`2) (getPowerCount remainingRows)
                                                        let solution = b!!(fst removedRow) - binaryToBase10 0 y
                                                        let move = ((fst removedRow)+1,solution)
                                                        let updatedBoard = performMove game move b
                                                        putBoard l updatedBoard
                                                        if checkEmptyBoard updatedBoard 
                                                                then do {putStrLn "Du tapte!"; spill}  
                                                                else generiskSpill g User updatedBoard
                       ((Chomp l),User) -> do chompMenu
                                              ys <- getLine
                                              let action = parse (tokenize ys)
                                              case action of 
                                                (Move (i,j)) -> do let game = (Chomp l)
                                                                   let move = (i,j) 
                                                                   if (validateMove game move b) 
                                                                        then do let updatedBoard = (performMove game move b)
                                                                                (putBoard l updatedBoard)
                                                                                if checkEmptyBoard updatedBoard
                                                                                        then do {putStrLn "Du tapte!"; spill}
                                                                                        else generiskSpill g AI updatedBoard 
                                                                        else do {ugyldigTrekk; generiskSpill g User b}
                                                (Info) -> do {chompInfo; generiskSpill g User b}
                                                (Quit) -> spill
                                                (otherwise) -> do {ugyldigKommando; generiskSpill g User b}
                       ((Chomp l),AI) -> do putStrLn "Maskinens trekk."
                                            let game = (Chomp l) 
                                            let possibleMoves = getListOfAllPossibleMoves game 1 b b  -- Strategien til AI for Chomp er 
                                            choice <- (randomRIO(0, length(possibleMoves)-1))         -- a utfore eit 
                                            let move = possibleMoves!!choice                          -- tilfeldig gyldig trekk.
                                            let updatedBoard = (performMove game move b)
                                            putBoard l updatedBoard
                                            if checkEmptyBoard updatedBoard
                                                then do {putStrLn "Du vant!"; spill}
                                                else generiskSpill g User updatedBoard

validateMove :: Game -> Move -> Board -> Bool       -- Funksjon som validerer eit trekk.
validateMove game (r,a) br = case game of 
                              (Nim l) -> if r <= l && r >= 1 && a >= 1 && a <= (br!!(r-1)) then True else False
                              (Chomp l) -> if r <= l && r >= 1 && a >= 1 && or (map (>=a) (fst(splitAt r br))) then True else False

performMove :: Game -> Move -> Board -> Board       -- Funksjon som tar eit spel, eit trekk og brettet og returner brettet med trekket utfort.
performMove game (r,a) br = case game of 
                                    (Nim _) -> let (x,y:ys) = splitAt (r-1) br in (x++[y-a]++ys)
                                    (Chomp _) -> (map (\t -> if t >= a then (a-1) else t) (fst(splitAt r br))) ++ (snd(splitAt r br))

putBoard :: BoardSize -> Board -> IO()                             -- Ein funksjon for å printe brettet til terminalen
putBoard n [] = putStrLn ("   " ++ (lastLine 1 n))
putBoard n (b:br) = do putRow (n-length(br)) b 
                       putBoard n br

putRow :: Int -> Int -> IO()                                       -- Hjelpefunksjon til putBoard
putRow row num = do putStr (show row)
                    putStr "  "
                    putStrLn (concat (replicate num "*  "))

lastLine :: Int -> BoardSize -> String                             -- Hjelpefunksjon til putBoard
lastLine c n | c /= n = (show c ++ "  " ++ (lastLine (c+1) n)) 
             | otherwise = show c ++ "\n"  

parse :: [String] -> Instruction                                   -- Funksjon som parser ein kommando gitt av bruker.
parse (x:y:z:xs) = Invalid
parse (x:y:[]) | x == "n" && and (map isDigit y) = Game (Nim (read y))
               | x == "c" && and (map isDigit y) = Game (Chomp (read y))
               | and (map isDigit x ++ map isDigit y) = Move(read x,read y)
               | otherwise = Invalid
parse (x:[])   | x == "?" = Info
               | x == "q" = Quit
               | otherwise = Invalid 
parse [] = Invalid

tokenize :: String -> [String]                   -- Funksjon som tokeniserer kommando strengen fra bruker. 
tokenize "" = []
tokenize (s:ss) = if s == ' ' then tokenize ss else takeWhile (/=' ') (s:ss) : tokenize (dropWhile (/=' ') (s:ss))

checkEmptyBoard :: Board -> Bool              -- Funksjon som sjekker om brettet er tomt.
checkEmptyBoard board = if sum board == 0 
                                then True 
                                else False

--------------------------------------------------------------------------------------------
--- Diverse funksjonar for Nim og Chomp AI
---------------------------------------------------------------------------------------------

zipRows :: [Int] -> [(Int,Int)]            -- Funksjon som zipper radnummer med antall elementer i rad.
zipRows xs = zip [0..] xs

sortRows :: [(Int,Int)] -> [(Int,Int)]     -- Sorterar rader etter antall element.
sortRows [] = []
sortRows (x:xs) = insertRow x (sortRows xs)

insertRow :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]  -- "Sett inn sortering"
insertRow e [] = [e]
insertRow e (x:xs) | snd e <= snd x = e : x : xs
                   | otherwise = x : insertRow e xs

convertToBase2 :: [(Int, Int)] -> [(Int, [Int])]    -- Tar liste av sorterte rader og Konverterar fra base10 til biner.
convertToBase2 xs = map (base10ToBinary) xs 

base10ToBinary :: (Int, Int) -> (Int, [Int]) -- Konverterar ei rad fra base10 til biner.       
base10ToBinary (a,0) = (a,[])
base10ToBinary (a,i) = (a, (i `mod` 2) : snd(base10ToBinary (a,i `div` 2)))  

binaryToBase10 :: Int -> [Int] -> Int       -- Konverterar eit binert tall til desimal.     
binaryToBase10 p [] = 0
binaryToBase10 p (x:xs) | x == 1 = 2^p + binaryToBase10 (p+1) xs
                        | otherwise = binaryToBase10 (p+1) xs

getPowerCount :: [(Int, [Int])] -> [Int]    -- Sumerar 2ar potenser.
getPowerCount xs = countPowers (map (\(a,b) -> b) xs)

countPowers :: [[Int]] -> [Int]    -- Teller antallet av kvar 2ar potens fra 0..n
countPowers [] = []
countPowers xs = length (filter (==1) (map (safeHead) xs)) : countPowers (filter (/=[]) (map safeTail xs))  

getMinimumLength :: [Int] -> Int  -- Funksjon som finner minimum antall element ei rad ma ha for a kunne balansere ein gitt Nim sum.
getMinimumLength [] = 0
getMinimumLength (p:ps) = if odd p then length (p:ps) else getMinimumLength ps 

getPossibleRows :: [Int] -> [(Int, [Int])] -> [(Int, [Int])]  -- Funksjon som hentar ut alle mulige rader som ved reduksjon kan balansere NIM summen.
getPossibleRows _ [] = [] 
getPossibleRows powers (r:rs) = if length (snd r) >= getMinimumLength (reverse powers) && binaryToBase10 0 (map (`mod` 2) powers) /= 0
                                        then if odd ((snd r)!!(getMinimumLength (reverse powers)-1)) 
                                                then r : getPossibleRows powers rs 
                                                else getPossibleRows powers rs
                                else getPossibleRows powers rs

removeRow :: Int -> [(Int, [Int])] -> [(Int, [Int])]   -- Fjernar ei rad fra lista.
removeRow n [] = []
removeRow n (x:xs) | fst x == n = xs
                   | otherwise = x : removeRow n xs

getRow :: Int -> [(Int, [Int])] -> (Int, [Int])   -- Hentar ut ei rad fra lista.
getRow n xs = xs!!n

getListOfAllPossibleMoves :: Game -> Int -> Board -> Board -> [(Int,Int)]  -- Generar ei liste av alle mulige trekk, uavhengig om dei er optimale eller ikkje.
getListOfAllPossibleMoves game r [] board = []
getListOfAllPossibleMoves game r (a:as) board = case game of 
                                                   (Nim l) | validateMove game (r,a) board -> (r,a) : getListOfAllPossibleMoves game r ((a-1):as) board
                                                           | otherwise -> getListOfAllPossibleMoves game (r+1) as board
                                                   (Chomp l) | validateMove game (r,a) board && r <= length board -> (r,a) : getListOfAllPossibleMoves game (r+1) (a:as) board
                                                             | not (validateMove game (r,a) board) && r <= length board -> getListOfAllPossibleMoves game (r+1) (a:as) board
                                                             | otherwise -> getListOfAllPossibleMoves game 1 as board                                                               

safeHead :: [Int] -> Int
safeHead [] = 0
safeHead (x:xs) = x

safeTail :: [Int] -> [Int]
safeTail [] = []
safeTail (x:xs) = xs
