import System.IO
data Move = Move AppDir Card Int | Comment String

data AppDir = L | R

data Card = Get | Succ | Double | Sc | Kc | Ic | Zero | Put

data SKI = Ap SKI SKI | S | K | I

apply :: Int -> Int -> Int -> [Move]
apply dest l r = concat [num dest l, [Move L Get dest], inapp dest (Get : cardnum r), [Move R Zero dest]]
applyInPlace :: Int -> Int -> [Move]
applyInPlace dest r = inapp dest (Get : cardnum r)
num :: Int -> Int -> [Move]
num dest val = (Move L Put dest : Move R Zero dest : (map (\x -> Move L x dest) $ cardnum val))

cardnum :: Int -> [Card]
cardnum 0 = []
cardnum n = if odd n then (Succ : Double : cardnum ((n - 1) `div` 2)) else (Double : cardnum (n `div` 2))

inapp :: Int -> [Card] -> [Move]
inapp l = concatMap fuckapp
   where fuckapp c = [Move L Kc l, Move L Sc l, Move R c l]

instance Show Card where
   show Get  = "get"
   show Succ = "succ"
   show Double = "dbl"
   show Sc = "S"
   show Kc = "K"
   show Ic = "I"
   show Zero = "zero"
   show Put = "put"

instance Show AppDir where
   show L = "1"
   show R = "2"

instance Show Move where
   show (Move dir@L card n) = unlines $ [show dir, show card, show n]
   show (Move dir@R card n) = unlines $ [show dir, show n, show card]
   show (Comment str) = "Comment: " ++ str
comms :: [Move] -> IO ()
comms [] = return ()
comms ((Comment str) : xs) = do hPutStrLn stderr str; comms xs
comms (m@(Move _ _ _) : xs) = do putStr (show m); comms xs

sizeofXBOX = 100

xbox = bomb 1
--xbox = concat $ (bomb 1) : (apply 0 1 1) : (concat $ replicate sizeofXBOX $ applyInPlace 0 1) : []

bomb :: Int -> [Move]
bomb target = skiComp target bombski
   where bombski :: SKI
         --bombski = (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap K K)) I))
         bombski = (Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap K K)) I))) (Ap (Ap S (Ap (Ap S (Ap K S)) (Ap (Ap S (Ap K K)) I))) (Ap K I)))

skiComp :: Int -> SKI -> [Move]
skiComp target z = (Move L Put target) : (skiComp' z)
   where skiComp' (Ap a b) = (skiComp' a) ++ (shift b)
         skiComp'  v = [Move R (skiToCard v) target]
         shift (Ap a b) = [Move L Kc target, Move L Sc target] ++ (shift a) ++ (shift b)
         shift v = [Move R (skiToCard v) target]

skiToCard S = Sc
skiToCard K = Kc
skiToCard I = Ic
skiToCard _ = error "FYAD"

commsLTG moves = unlines
   ("CM.make \"sources.cm\";" :
    "open Evaluator;" :
    "open LTG;" :
    "Control.Print.printDepth := 9000;" :
    "val board = LTG.build_board ();" : map ltgShow moves)
   where ltgShow (Move dir card dest) = "run_move board " ++ (ltgDir dir) ++ " " ++ (show dest) ++ " (%" ++ (ltgCard card) ++ ");\nrun_zombies board;board;"
         ltgDir L = "LeftApp"
         ltgDir R = "RightApp"
         ltgCard Succ = "CSucc"
         ltgCard Double = "CDbl"
         ltgCard Get = "CGet"
         ltgCard Put = "CPut"
         ltgCard Sc  = "CS"
         ltgCard Kc  = "CK"
         ltgCard Ic  = "CI"
         ltgCard Zero = "(CVal 0)"

main = do hSetBuffering stderr LineBuffering; hSetBuffering stdout LineBuffering; hSetBuffering stdin LineBuffering; comms xbox; hFlush stdout; interact id
