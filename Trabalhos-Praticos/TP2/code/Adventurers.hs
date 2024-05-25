{-# LANGUAGE FlexibleInstances #-}
module Adventurers where

import DurationMonad
import Control.Monad (when) -- usado nos testes

-- The list of adventurers
data Adventurer = P1 | P2 | P5 | P10 deriving (Show,Eq)
-- Adventurers + the lantern
type Objects = Either Adventurer ()

-- The time that each adventurer needs to cross the bridge
-- To implement 
getTimeAdv :: Adventurer -> Int
getTimeAdv P1  = 1
getTimeAdv P2  = 2
getTimeAdv P5  = 5
getTimeAdv P10 = 10


{--#############################################################################################################################--}


-- igual a getTimeAdv só que para os objetos
getTimeOAdv :: Objects -> Int
getTimeOAdv (Left a) = getTimeAdv a

-- Utilities
-- lanterna
lamp :: Objects
lamp = Right ()

-- mover a lanterna
moveLamp :: State -> State
moveLamp = changeState lamp

-- lista dos aventureiros como objetos
-- aplica a função Left a cada elemento da lista [P1, P2, P5, P10]. 
-- isto cria uma lista de valores do tipo Either Adventurer (), onde cada elemento é do tipo Left Adventurer.
adventurers :: [Objects]
adventurers = map Left [P1, P2, P5, P10]


{--#############################################################################################################################--}


{-- The state of the game, i.e. the current position of each adventurer
+ the lantern. The function (const False) represents the initial state of the
game, with all adventurers and the lantern on the left side of the bridge.
Similarly, the function (const True) represents the end state of the game, with
all adventurers and the lantern on the right side of the bridge.  
--}
type State = Objects -> Bool

instance Show State where
  show s = (show . (fmap show)) [s (Left P1),
                                 s (Left P2),
                                 s (Left P5),
                                 s (Left P10),
                                 s (Right ())]

instance Eq State where
  (==) s1 s2 = and [s1 (Left P1) == s2 (Left P1),
                    s1 (Left P2) == s2 (Left P2),
                    s1 (Left P5) == s2 (Left P5),
                    s1 (Left P10) == s2 (Left P10),
                    s1 (Right ()) == s2 (Right ())]


-- The initial state of the game
gInit :: State
gInit = const False

-- Changes the state of the game for a given object
changeState :: Objects -> State -> State
changeState a s = let v = s a in (\x -> if x == a then not v else s x)

-- Changes the state of the game for a list of objects 
mChangeState :: [Objects] -> State -> State
mChangeState os s = foldr changeState s os
                               

{-- For a given state of the game, the function presents all the
possible moves that the adventurers can make.  --}
-- To implement
allValidPlays :: State -> ListDur State
allValidPlays s = manyChoice [ moveOne s, moveTwo s ]

moveOne :: State -> ListDur State
moveOne s = LD (map f canCross) 
               where
                  -- filtar os aventureiros que podem atravessar a ponte, ou seja estão no mesmo lado que a lanterna (têm o mesmo estado)
                  -- canCross :: [Objects]
                  canCross = filter ((== s lamp) . s) adventurers
                  -- f :: State -> Duration State
                  f x = wait (getTimeOAdv x) (return $ changeState x $ moveLamp s)


moveTwo :: State -> ListDur State
moveTwo s = LD (map f po)
               where 
                  -- pares dos aventureiros que podem atravessar a ponte
                  -- po :: [(Objects, Objects)]
                  po = makePairs $ filter ((== s lamp) . s) adventurers
                  -- durações dos pares após atravessarem
                  -- f :: (Objects, Objects) -> State
                  f (x, y) = wait (m x y) (c x y)
                  -- estado após dois aventureiros e lanterna atravessarem
                  -- c :: Objects -> Objects -> m State
                  c x y = return $ changeState x $ changeState y $ moveLamp s
                  -- tempo que demoram os dois aventueiros a atravessar
                  -- m :: Objects -> Objects -> Int
                  m x y = max (getTimeOAdv x) (getTimeOAdv y)

{-- For a given number n and initial state, the function calculates
all possible n-sequences of moves that the adventures can make --}
-- To implement 
exec :: Int -> State -> ListDur State
exec 0 s = return s
exec n s = do s' <- allValidPlays s
              exec (n-1) s'

{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}
-- To implement
leq17 :: Bool
leq17 = any (\(Duration (x, s)) -> x<=17 && all s adventurers) (remLD (exec 5 gInit))

{-- Is it possible for all adventurers to be on the other side
in < 17 min ? --}
-- To implement
l17 :: Bool
l17 = any id [any f (remLD (exec i gInit)) | i <- [1..8]]
       where
          -- duração menor que 17 e todos atravessaram
          -- f :: Duration State -> Bool
          f (Duration (x, s)) = x<17 && all s adventurers


{--#############################################################################################################################--}


{-- Implementation of the monad used for the problem of the adventurers.
Recall the Knight's quest --}

data ListDur a = LD [Duration a] deriving Show

remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

instance Functor ListDur where
   fmap f = let f' = (fmap f) in
     LD . (map f') . remLD

instance Applicative ListDur where
   pure x = LD [Duration (0,x)]
   l1 <*> l2 = LD $ do x <- remLD l1
                       y <- remLD l2
                       return $ do f <- x; a <- y; return (f a)

instance Monad ListDur where
   return = pure
   l >>= k = LD $ do x <- remLD l
                     g x where
                       g(Duration (i,x)) = let u = (remLD (k x))
                          in map (\(Duration (i',x)) -> Duration (i + i', x)) u

manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)

--------- List Utils ----------
makePairs :: (Eq a) => [a] -> [(a,a)]
makePairs as = normalize $ do a1 <- as; a2 <- as; [(a1,a2)]
                                
normalize :: (Eq a) => [(a,a)] -> [(a,a)]
normalize l = removeSw $ filter p1 l where
  p1 (x,y) = if x /= y then True else False

removeSw :: (Eq a) => [(a,a)] -> [(a,a)]
removeSw [] = []
removeSw ((a,b):xs) = if elem (b,a) xs then removeSw xs else (a,b):(removeSw xs)



{--#############################################################################################################################--}


--------- Testes ----------
printSolution :: ListDur State -> IO ()
printSolution (LD durations) = mapM_ printDuration durations
  where
    printDuration (Duration (time, state)) = do
      putStrLn $ "Time: " ++ show time
      print state

testPrintSolution :: IO ()
testPrintSolution = do
  let solutions = exec 5 gInit
  putStrLn "Possible solutions within 5 steps:"
  printSolution solutions

testLeq17 :: IO ()
testLeq17 = do
  putStrLn $ "Is it possible for all adventurers to be on the other side in <=17 min and not exceeding 5 moves? " ++ show leq17
  when leq17 $ do
    putStrLn "Valid solution(s) for leq17 found:"
    let solutions = exec 5 gInit
    printSolution solutions
  when (not leq17) $ do
    putStrLn "No valid solution(s) for leq17 found within the step limit."
    
testL17 :: IO ()
testL17 = do
  putStrLn $ "Is it possible for all adventurers to be on the other side in < 17 min? " ++ show l17
  when l17 $ do
    putStrLn "Valid solution(s) for l17 found:"
    let solutions = exec 5 gInit
    printSolution solutions
  when (not l17) $ do
    putStrLn "No valid solution(s) for l17 found within the step limit."
    
-- Função para verificar se um estado é final (todos os aventureiros estão à direita)
isFinalState :: State -> Bool
isFinalState s = all (s . Left) [P1, P2, P5, P10] && s (Right ())

-- Função para imprimir apenas os estados finais com tempo <= 17
printFinalStates :: ListDur State -> IO ()
printFinalStates (LD durations) = mapM_ printIfFinal durations
  where
    printIfFinal (Duration (time, state))
      | isFinalState state && time == 17 = do
          putStrLn $ "Time: " ++ show time
          print state
      | otherwise = return ()

testPrintFinalStates :: IO ()
testPrintFinalStates = do
  let solutions = exec 5 gInit
  putStrLn "Final solutions within 5 steps and exactly 17 minutes:"
  printFinalStates solutions

main :: IO ()
main = do
  putStrLn "\nTesting leq17:"
  testLeq17
  putStrLn "\nTesting l17:"
  testL17
  putStrLn "\nPrinting final solution(s):"
  testPrintFinalStates


{--#############################################################################################################################--}

{-- 

RESOLUÇÃO ALTERNATIVA

-- Gera as possibilidades de um aventureiro atravessar a ponte
moveOne :: State -> ListDur State
moveOne s = LD (map f lo)
  where
    lo = filter ((== s lamp) . s) advo
    f x = wait (getTimeOAdv x) (return (changeState x (moveLamp s)))

-- Gera as possibilidades em que dois aventureiros atravessam a ponte
moveTwo :: State -> ListDur State
moveTwo s = LD (map f po)
  where
    po = makePairs $ filter ((== s lamp) . s) advo
    f (x, y) = wait (max (getTimeOAdv x) (getTimeOAdv y)) (return (changeState y (changeState x (moveLamp s))))

{-- For a given state of the game, the function presents all the
possible moves that the adventurers can make.  --}
-- To implement
allValidPlays :: State -> ListDur State
allValidPlays s = manyChoice [moveOne s, moveTwo s]


{-- For a given number n and initial state, the function calculates
all possible n-sequences of moves that the adventures can make --}
-- To implement 
exec :: Int -> State -> ListDur State
exec 0 s = return s
exec n s = do
  s1 <- allValidPlays s
  exec (n-1) s1

{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}
-- To implement
leq17 :: Bool
leq17 = any (\(Duration (x, s)) -> x <= 17 && all (s . Left) [P1, P2, P5, P10]) (remLD (exec 5 gInit))


{-- Is it possible for all adventurers to be on the other side
in < 17 min ? --}
-- To implement
l17 :: Bool
l17 = any id [any (\(Duration (x, s)) -> x < 17 && all (s . Left) [P1, P2, P5, P10]) (remLD (exec i gInit)) | i <- [1..6]]

--}
