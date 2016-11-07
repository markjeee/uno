module Game where

import Common
import Shuffler

import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))
import Data.List

initialCardCount :: Int
initialCardCount = 7

initGame :: Int -> State

-- TODO: Implement a method to initialize a new game given n players
initGame n = State { players = generateHumanPlayers n,
                     deck = fullDeck,
                     d_stack = [ ] }

generateHumanPlayers :: Int -> [Player]
generateHumanPlayers n | (n > 0) = [HPlayer {name = "Human " ++ show n, hand = [ ]} ] ++ generateHumanPlayers (n-1)
                       | otherwise = [ ]

-- TODO: Implement a method to setup the game
setupGame :: State -> IO State
setupGame gs = do
  gs' <- shuffleDeck gs
  gs'' <- dealToAllPlayers initialCardCount gs' $ players gs
  return (gs { players = (players gs''),
               deck = (deck gs'') })

dealToAllPlayers :: Int -> State -> [ Player ] -> IO State
dealToAllPlayers n gs [] = return (gs)
dealToAllPlayers n gs ps = do
  let (ps', ps'') = splitAt 1 ps
  gs' <- dealNCards n gs $ head ps'
  dealToAllPlayers n gs' ps''

dealNCards :: Int -> State -> Player -> IO State
dealNCards n gs player = do
  let (hand', deck') = splitAt n $ deck gs
  player' <- updateHand player hand'
  gs' <- updatePlayer gs player player'
  updateDeck gs' deck'

updatePlayer :: State -> Player -> Player -> IO State
updatePlayer gs p new_p = do
  return (gs { players = map (\p' -> if p' == p then new_p else p') $ players gs })

updateHand :: Player -> Hand -> IO Player
updateHand player h = return (player { hand = h })

updateDeck :: State -> Deck -> IO State
updateDeck gs deck' = return (gs { deck = deck' })
