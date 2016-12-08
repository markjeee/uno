{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, pending, pendingWith)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))
import Data.List
import Data.Maybe (fromJust, fromMaybe)
import Data.Optional

import UnoI
import Common
import Shuffler
import Game

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "uno" $ do
  interactiveSpecs
  commonSpecs
  shufflerSpecs
  gameSpecs

interactiveSpecs :: Spec
interactiveSpecs = describe "unoInteractive" $ do
  it "Say 'Hello' if input == 1" $ do
    (unoInteractive 1) `shouldBe` "Hello"
  it "Say 'Hello World' if input == 2" $ do
    (unoInteractive 2) `shouldBe` "Hello World"

commonSpecs :: Spec
commonSpecs = describe "Common" $ do
  describe "Cards..." $ do
    it "Card with color Blue is Blue" $ do
      (color blueThree) `shouldBe` Blue
    it "Card with value Three is Three" $ do
      (value blueThree) `shouldBe` Three
    it "Johnny's first card is Blue" $ do
      (color $ head $ hand johnnyCash) `shouldBe` Blue
    it "Johnny's last card is One" $ do
      (value $ last $ hand johnnyCash) `shouldBe` One
    it "Full deck has 108 cards" $ do
      (length fullDeck) `shouldBe` 108

  describe "Utils..." $ do
    it "should take blueThree card" $ do
      let (h1, h2) = [ blueThree ] `takeCards` testDeck
      h1 `shouldBe` [ blueThree ]
    it "should ONLY take 1 blueThree card" $ do
      let (h1, h2) = [ blueThree ] `takeCards` testDeck
      h2 `shouldBe` [ redOne, yellowTwo, blueThree, whiteFour ]
    --it "should take exploding and defuse cards" $ do
    --  let (h1, h2) = [ ExplodingCard, DefuseCard ] `takeCards` defusableHand
    --  h1 `shouldBe` [ ExplodingCard, DefuseCard ]
    --it "should ONLY take exploding and defuse cards" $ do
    --  let (h1, h2) = [ ExplodingCard, DefuseCard ] `takeCards` defusableHand
    --  h2 `shouldBe` [ BeardCat, RainbowCat, AttackCard ]

shufflerSpecs :: Spec
shufflerSpecs = describe "Shuffler" $ do
  it "Perform shuffling of cards" $ do
    --pendingWith "Implement shuffleDeck function"
    let gs = testState
    gs' <- shuffleDeck gs
    (deck gs') `shouldNotBe` (deck gs)

gameSpecs :: Spec
gameSpecs = describe "Game" $ do
  describe "initGame" $ do
    it "should create 4 players" $ do
      --pendingWith "Implement the initGame function"
      let gs = initGame numberOfTestPlayers
      length (players gs) `shouldBe` 4
    it "should initialize the deck with 108 cards" $ do
      --pendingWith "Implement the initGame function"
      let gs = initGame numberOfTestPlayers
      length (deck gs) `shouldBe` 108
    it "should initialize discard pile to empty" $ do
      --pendingWith "Implement the initGame function"
      let gs = initGame numberOfTestPlayers
      length (d_stack gs) `shouldBe` 0

  describe "setupGame" $ do
    it "should shuffle the deck" $ do
      --pendingWith "Implement the setupGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- shuffleDeck gs
      (deck gs') `shouldNotBe` (deck gs)
    it "should distribute cards to players" $ do
      --pendingWith "Implement the setupGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      and (map (\p -> (length $ hand p) == initialCardCount) $ players gs') `shouldBe` True
    it "should remove cards from deck" $ do
      --pendingWith "Implement the setupGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      length (deck gs') `shouldBe` 79
    it "should put 1 card in discard" $ do
      --pendingWith "Implement the setupGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      length (d_stack gs') `shouldBe` 1

  describe "startGame" $ do
    it "should pick first player" $ do
      --pendingWith "Implement startGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs' Default
      (cur_player gs') `shouldBe` (head $ (players gs'))
    it "should double pick second player" $ do
      --pendingWith "Implement startGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs' Default
      gs' <- pickNextPlayer gs' Default
      (cur_player gs') `shouldBe` ((players gs') !! 1)
    it "should turn around and pick first player" $ do
      --pendingWith "Implement startGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- updateCurPlayer gs' $ (players gs') !! 3
      gs' <- pickNextPlayer gs' Default
      (cur_player gs') `shouldBe` (head $ (players gs'))
    it "should declare a round winner" $ do
      --pendingWith "Implement startGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs' Default
      gs' <- updateCurHand gs' [ ]
      playerIsOut gs' `shouldBe` True
    it "should declare an overall winner" $ do
      --pendingWith "Implement startGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs' Default
      gs' <- updateCurHand gs' [ ]
      playerHasWon gs' `shouldBe` True

  describe "Play..." $ do
    it "should reverse at player 0" $ do
      --pendingWith "Implement startGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      p3 <- return $ (players gs') !! 3
      gs' <- pickNextPlayer gs' Default
      gs' <- reversePlayers gs'
      gs' <- pickNextPlayer gs' Default
      p3 `shouldBe` (cur_player gs')
    it "should reverse at player 2" $ do
      --pendingWith "Implement startGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      p1 <- return $ (players gs') !! 1
      gs' <- updateCurPlayer gs' $ (players gs') !! 2
      gs' <- reversePlayers gs'
      gs' <- pickNextPlayer gs' Default
      p1 `shouldBe` (cur_player gs')
    it "should reverse at player 3" $ do
      --pendingWith "Implement startGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      p2 <- return $ (players gs') !! 2
      gs' <- updateCurPlayer gs' $ (players gs') !! 3
      gs' <- reversePlayers gs'
      gs' <- pickNextPlayer gs' Default
      p2 `shouldBe` (cur_player gs')
    it "should take card from hand and act" $ do
      --pendingWith "Implement startGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs' Default
      c1 <- return((curHand gs') !! 1)
      c1_count <- return(countCards (curHand gs') c1)
      (action, gs') <- takeFromHandWithAction c1 EndTurn gs'
      (c1_count - 1) `shouldBe` (countCards (curHand gs') c1)
    it "should put card in discard" $ do
      --pendingWith "Implement startGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs' Default
      c1 <- return(curHand gs' !! 1)
      (action, gs') <- takeFromHandWithAction c1 EndTurn gs'
      c1 `shouldBe` (last $ d_stack gs')
    it "should play reverse card" $ do
      --pendingWith "Implement startGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs' Default
      gs' <- injectCards [ redReverse ] gs'
      gs' <- discardCards [ redOne ] gs'
      (action, gs') <- takeAction UseCard redReverse gs'
      action `shouldBe` AttackReverse
    it "should play wildcard" $ do
      --pendingWith "Implement startGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs' Default
      gs' <- injectCards [ blackWild ] gs'
      (action, gs') <- takeAction UseCard redWild gs'
      action `shouldBe` EndTurn
    it "should discard color change card" $ do
      --pendingWith "Implement startGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs' Default
      gs' <- injectCards [ blackWild ] gs'
      (action, gs') <- takeAction UseCard redWild gs'
      (last $ d_stack gs') `shouldBe` redWild
    it "should remove black card" $ do
      --pendingWith "Implement startGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs' Default
      black_count <- return(countCardsByColor Black $ curHand gs')
      gs' <- injectCards [ blackWild ] gs'
      (action, gs') <- takeAction UseCard redWild gs'
      black_count `shouldBe` (countCardsByColor Black $ curHand gs')
    it "should find wildcard" $ do
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs' Default
      gs' <- updateCurHand gs' [ blackWild ]
      (cardInCurHand redWild gs') `shouldBe` True
    it "should draw 2 cards" $ do
      --pendingWith "Implement startGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs' Default
      c0_count <- return(length (hand $ (players  gs') !! 0))
      (action, gs') <- playPlayer gs' Draw2
      (c0_count + 2) `shouldBe` (length (hand $ (players  gs') !! 0))
    it "should start and end" $ do
      --pendingWith "Implement startGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- startGame gs'
      (playerHasWon gs') `shouldBe` True

  describe "Simple strategy..." $ do
    it "should return matching color" $ do
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs' Default
      let (action, card) = useSimpleStrategy gs' redOne testHand
      card `shouldBe` redThree
    it "should return matching number" $ do
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs' Default
      let (action, card) = useSimpleStrategy gs' greenOne testHand
      card `shouldBe` blueOne
    it "should return a red color change wildcard" $ do
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs' Default
      let (action, card) = useSimpleStrategy gs' greenFour testHand
      card `shouldBe` redWild
    it "should choose take from deck" $ do
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs' Default
      let (action, card) = useSimpleStrategy gs' greenSix testHand2
      action `shouldBe` TakeFromDeck

-- Test data fixtures
--
blueThree = Card { color = Blue, value = Three }
redOne = Card { color = Red, value = One }
yellowTwo = Card { color = Yellow, value = Two }
whiteFour = Card { color = White, value = Four }
redReverse = Card { color = Red, value = ChDir }
redWild = Card { color = Red, value = ChCol }
blackWild = Card { color = Black, value = ChCol }
redThree = Card { color = Red, value = Three }
greenOne = Card { color = Green, value = One }
greenTwo = Card { color = Green, value = Two }
greenFour = Card { color = Green, value = Four }
greenSix = Card { color = Green, value = Six }
blueOne = Card { color = Blue, value = One }

testDeck = [ blueThree, redOne, yellowTwo, blueThree, whiteFour ]
testHand = [ blueThree, yellowTwo, redThree, blueOne, blackWild ]
testHand2 = [ blueThree, yellowTwo, redThree, blueOne, redOne ]

johnnyCash = HPlayer { name = "Johnny", hand = [ blueThree, redOne ] }

numberOfTestPlayers = 4

testState = State { players = [ ]
                  , e_players = [ ]
                  , deck = fullDeck
                  , d_stack = [ ]
                  , cur_player = noPlayer }
