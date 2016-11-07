module Game where

import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))
import Data.List
import Data.Maybe (fromJust, fromMaybe)

import Common
import Shuffler

initialCardCount :: Int
initialCardCount = 7

initGame :: Int -> State

-- TODO: Implement a method to initialize a new game given n players
initGame n = State { players = generateHumanPlayers n,
                     e_players = [ ],
                     deck = fullDeck,
                     d_stack = [ ],
                     cur_player = noPlayer }

initGameWithPlayers :: [ Player ] -> State
initGameWithPlayers pa = gs' { players = clearHands pa } where
  gs' = initGame (length $ pa)

generateHumanPlayers :: Int -> [ Player ]
generateHumanPlayers n
  | (n > 0) = [ HPlayer {name = "Human " ++ show n, hand = [ ]} ] ++ generateHumanPlayers (n-1)
  | otherwise = [ ]

-- TODO: Implement a method to setup the game
setupGame :: State -> IO State
setupGame gs = do
  gs' <- shuffleDeck gs
  gs' <- dealToAllPlayers initialCardCount gs' $ players gs
  gs' <- discardNCards 1 gs'
  return (gs { players = (players gs'),
               deck = (deck gs'),
               d_stack = (d_stack gs') })

startGame :: State -> IO State
startGame gs = pickNextAndPlay gs

restartGame :: State -> IO State
restartGame gs = do
  new_gs <- setupGame $ initGameWithPlayers (players gs)
  pickNextAndPlay new_gs

pickNextAndPlay :: State -> IO State
pickNextAndPlay gs = do
  gs' <- pickNextPlayer gs
  playLoop gs' NoAttack

playLoop :: State -> Attack -> IO State
playLoop gs under_attack
  | playerHasWon gs = return (gs)
  | playerIsOut gs = restartGame gs
  | deckIsEmpty gs = do
      gs' <- reloadDeck gs
      playLoop gs' under_attack
  | otherwise = do
      (next_action, gs') <- playPlayer gs under_attack
      playLoopNext gs' next_action

playLoopNext :: State -> Action -> IO State
playLoopNext gs next_action
  | next_action == EndTurn = pickNextAndPlay gs
  | next_action == AttackReverse = reverseAndPlay gs
  | next_action == AttackSkip = attackAndPlay gs Skip
  | next_action == AttackDraw2 = attackAndPlay gs Draw2
  | next_action == AttackWildDraw4 = attackAndPlay gs Draw4
  | otherwise = error "Action not allowed"

deckIsEmpty :: State -> Bool
deckIsEmpty gs = null (deck gs)

-- NOTE: This is done in purpose, so it only plays one round.
playerHasWon :: State -> Bool
playerHasWon gs = playerIsOut gs

playerIsOut :: State -> Bool
playerIsOut gs = null (curHand gs)

reverseAndPlay :: State -> IO State
reverseAndPlay gs = do
  gs' <- reversePlayers gs
  gs' <- pickNextPlayer gs'
  playLoop gs' NoAttack

attackAndPlay :: State -> Attack -> IO State
attackAndPlay gs under_attack = do
  gs' <- pickNextPlayer gs
  playLoop gs' under_attack

playPlayer :: State -> Attack -> IO (Action, State)
playPlayer gs under_attack
  | under_attack == Skip = return (EndTurn, gs)
  | under_attack == Draw2 = drawAndEnd gs 2
  | under_attack == Draw4 = drawAndEnd gs 4
  | under_attack == NoAttack = do
    (next_action, gs') <- playTurn gs
    takeNextAction next_action gs' under_attack

takeNextAction :: Action -> State -> Attack -> IO (Action, State)
takeNextAction next_action gs under_attack
  | next_action `elem` [ EndTurn, AttackReverse, AttackSkip, AttackDraw2, AttackWildDraw4 ] = return (next_action, gs)
  | otherwise = error "Action not allowed"

drawAndEnd :: State -> Int -> IO (Action, State)
drawAndEnd gs draw_count = do
  gs' <- dealNCards draw_count gs $ cur_player gs
  return (EndTurn, gs')

playTurn :: State -> IO (Action, State)
playTurn gs = playOneCard gs

playOneCard :: State -> IO (Action, State)
playOneCard gs = takeAction action card gs where
  (action, card) = playCurrentPlayer gs

takeAction :: Action -> Card -> State -> IO (Action, State)
takeAction action card gs
  | action == TakeFromDeck = takeFromDeck gs
  | action == UseCard && not (cardInCurHand card gs) = error ("Card " ++ show card ++ " not in hand: " ++ show (curHand gs))
  | action == UseCard && not (cardCanPlay card gs) = error ("Card " ++ show card ++ " not match: " ++ show (topDCard gs))
  | action == UseCard = playCard card gs
  | otherwise = error "Action not allowed"

cardCanPlay :: Card -> State -> Bool
cardCanPlay card gs
  | isWildcard card = error "Wildcard not allowed, use specific color change card"
  | isChangeColCards card = True
  | (color card) == (color $ topDCard gs) = True
  | (value card) == (value $ topDCard gs) = True
  | otherwise = False

playCard :: Card -> State -> IO (Action, State)
playCard card gs
  | color card == White = return (EndTurn, gs)
  | (value card) == ChCol = takeFromHandWithAction card EndTurn gs
  | (value card) == ChDir = takeFromHandWithAction card AttackReverse gs
  | (value card) == Stop = takeFromHandWithAction card AttackSkip gs
  | (value card) == Plus2 = takeFromHandWithAction card AttackDraw2 gs
  | (value card) == Plus4 = takeFromHandWithAction card AttackWildDraw4 gs
  | isNumberCard card = takeFromHandWithAction card EndTurn gs
  | otherwise = error "Card not allowed"

takeFromHandWithAction :: Card -> Action -> State -> IO (Action, State)
takeFromHandWithAction card next_action gs = do
  gs' <- takeFromHand card gs
  return (next_action, gs')

takeFromHand :: Card -> State -> IO State
takeFromHand card gs = do
  gs' <- removeFromCurHand card gs
  discardCards [ card ] gs'

takeFromDeck :: State -> IO (Action, State)
takeFromDeck gs = do
  gs' <- dealNCards 1 gs $ cur_player gs
  return (EndTurn, gs')

-- Simple logic:
-- [ p1, p2, _p3_, p4 ]
-- [ p1, p2 ] ++ [ p3, p4] break
-- [ p1, p2 ] ++ [ p3 ] ++ [ p4 ] drop
-- [ p3 ] ++ [ p2, p1 ] ++ [ p4 ] reverse add
reversePlayers :: State -> IO State
reversePlayers gs = return (gs { players = reverse_pa }) where
  cp = (cur_player gs)
  (pa1, pa2) = break (== cp) (players gs)
  pa3 = drop 1 pa2
  reverse_pa = [ cp ] ++ (reverse pa1) ++ (reverse pa3)

dealToAllPlayers :: Int -> State -> [ Player ] -> IO State
dealToAllPlayers n gs [] = return (gs)
dealToAllPlayers n gs ps = do
  let (ps', ps'') = splitAt 1 ps
  gs' <- dealNCards n gs $ head ps'
  dealToAllPlayers n gs' ps''

dealNCards :: Int -> State -> Player -> IO State
dealNCards n gs player = do
  let (hand', deck') = splitAt n $ deck gs
  player' <- updateHand player (hand player ++ hand')
  gs' <- updatePlayer gs player player'
  updateDeck gs' deck'

discardNCards :: Int -> State -> IO State
discardNCards n gs = do
  let first_number_card = head [ c | c <- deck gs, isNumberCard c ]
  let (d_cards', deck') = takeCards [ first_number_card ] (deck gs)
  gs' <- updateDeck gs deck'
  discardCards d_cards' gs'

updatePlayer :: State -> Player -> Player -> IO State
updatePlayer gs p new_p = do
  return (gs { players = map (\p' -> if p' == p then new_p else p') $ players gs,
               cur_player = if (cur_player gs) == p then new_p else (cur_player gs) })

discardCards :: [ Card ] -> State -> IO State
discardCards cards gs = do
  let d_stack' = (d_stack gs) ++ cards
  updateDiscardS gs d_stack'

injectCards :: [ Card ] -> State -> IO State
injectCards cards gs = updateCurHand gs hand' where
  hand' = (curHand gs) ++ cards

cardInCurHand :: Card -> State -> Bool
cardInCurHand card gs = cardInHand card (cur_player gs)

cardInHand :: Card -> Player -> Bool
cardInHand card player
  | isChangeColCards card = cardInHand (colorBlack card) player
  | otherwise = card `elem` (hand player)

removeFromCurHand :: Card -> State -> IO State
removeFromCurHand card gs
  | isChangeColCards card = removeFromCurHand (colorBlack card) gs
  | otherwise = updateCurHand gs hand' where
      (d_cards, hand') = takeCards [ card ] (curHand gs)

curHand :: State -> Hand
curHand gs = hand $ cur_player gs

updateCurHand :: State -> Hand -> IO State
updateCurHand gs h = do
  let cp = cur_player gs
  player' <- updateHand cp h
  updatePlayer gs cp player'

updateHand :: Player -> Hand -> IO Player
updateHand player h = return (player { hand = h })

updateDeck :: State -> Deck -> IO State
updateDeck gs deck' = return (gs { deck = deck' })

reloadDeck :: State -> IO State
reloadDeck gs = shuffleDeck gs' where
  (d_stack', deck') = takeCards [ (topDCard gs) ] (d_stack gs)
  gs' = gs { deck = (blackenCards deck'), d_stack = d_stack' }

topDCard :: State -> Card
topDCard gs = last (d_stack gs)

updateDiscardS :: State -> Deck -> IO State
updateDiscardS gs d_stack' = return (gs { d_stack = d_stack' })

updateCurPlayer :: State -> Player -> IO State
updateCurPlayer gs player = return (gs { cur_player = player })

getNextPlayer :: State -> Player
getNextPlayer gs
  | ((cur_player gs) == noPlayer) = head $ players gs
  | otherwise = do
      let i = fromJust $ elemIndex (cur_player gs) (players gs)
      if (i + 1) >= (length $ players gs)
        then head $ players gs
        else (players gs) !! (i + 1)

pickNextPlayer :: State -> IO State
pickNextPlayer gs = updateCurPlayer gs $ getNextPlayer gs

-- This can be overridden to use another strategy.
playCurrentPlayer :: State -> (Action, Card)
playCurrentPlayer gs = useSimpleStrategy gs (topDCard gs) (curHand gs)

useSimpleStrategy :: State -> Card -> Hand -> (Action, Card)
useSimpleStrategy gs dcard hand
  | colorInHand (color dcard) hand = do
      (UseCard, fromMaybe noCard $ getCardWithColor (color dcard) hand)
  | isNumberCard dcard && valueInHand (value dcard) hand = do
      (UseCard, fromMaybe noCard $ getCardWithValue (value dcard) hand)
  | wildcardInHand hand = do
      (UseCard, colorizeWildcard Red (fromMaybe noCard $ getWildcard hand))
  | otherwise = (TakeFromDeck, noCard)
