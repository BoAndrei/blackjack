import System.Random
import Data.List
import Control.Monad  
import Data.Char  

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

data Card = Ace 
          | Two 
          | Three 
          | Four 
          | Five 
          | Six 
          | Seven 
          | Eight 
          | Nine 
          | Ten 
          | Jack 
          | Queen 
          | King 
          deriving (Show, Eq, Enum)


data Outcome = Loss | Push | Win | BlackjackWin deriving (Show, Eq)


data Move = Hit | Stand | DoubleDown deriving (Show, Eq)

type Money = Integer
data GameState = PlayerPlaying | DealerPlaying


type Deck = [Card]

type Game = (Deck, Deck, Deck)



completeDeck :: Deck
completeDeck = [Ace .. King] ++ [Ace .. King] ++ [Ace .. King] ++ [Ace .. King] ++ [Ace .. King] ++ [Ace .. King]


shuffle x = if length x < 2 then return x else do
    i <- System.Random.randomRIO (0, length(x)-1)
    r <- shuffle (take i x ++ drop (i+1) x)
    return (x!!i : r)

cardValue :: Card -> Int
cardValue Ace   = 11
cardValue Two   = 2
cardValue Three = 3
cardValue Four  = 4
cardValue Five  = 5
cardValue Six   = 6
cardValue Seven = 7
cardValue Eight = 8
cardValue Nine  = 9
cardValue _     = 10

isBlackjack :: Deck -> Bool
isBlackjack hand = (sum $ map cardValue hand) == 21

busted :: Deck -> Bool
busted hand = (sum $ map cardValue hand) > 21

moneyMade :: Money -> Outcome -> Money
moneyMade bet Loss         = -1 * bet
moneyMade _   Push         = 0
moneyMade bet Win          = bet
moneyMade bet BlackjackWin = ceiling $ (1.5 :: Double) * fromIntegral bet


getMoney :: Money -> Outcome -> IO Money
getMoney bet outcome = do
  let takings = moneyMade bet outcome
  return takings




game :: Game -> IO Outcome
game (d, dealerHand, playerHand) = do
  putStrLn ("Dealer hand: [" ++ (show $ head dealerHand) ++ ",X]" )
  putStrLn ("Your hand: " ++ (show playerHand) ++ " (" ++ id (show (sum $ map cardValue playerHand)  ++ ")"))
       

  if busted playerHand
    then do
      putStrLn $ "You busted! with: " ++ id (show (sum $ map cardValue playerHand))
      return Loss
  else if isBlackjack playerHand
    then do
      putStrLn ( "Blackjack!")
      playStay (d, dealerHand, playerHand)
  else do
        
      action <- getLine
      case action of
        "hit" -> do 
          let (d1, ph1, dh1) = addCard (d, playerHand, dealerHand)
          game (d1, dh1, ph1)

        "stay" -> do
          playStay (d, dealerHand, playerHand)

        _ -> game (d, dealerHand, playerHand)



playStay :: Game -> IO Outcome
playStay (d, dealerHand, playerHand) = do
  let (d1, dh1, ph1) = justDealer (d, dealerHand, playerHand)

  if busted dh1
    then do 
      putStrLn ("Dealer busted! with: " ++ (show dh1) ++ " (" ++ id (show (sum $ map cardValue dh1)  ++ ")"))  
      return Win
  else declareWinner (d1, dh1, ph1)

justDealer :: Game -> Game
justDealer (d, dealerHand, playerHand) = if (sum $ map cardValue dealerHand) < 17
  then justDealer (addCard (d, dealerHand, playerHand))
  else (d, dealerHand, playerHand)


findOutcome :: Int -> Int -> Outcome
findOutcome playerScore dealerScore
  | playerScore == 21 && dealerScore /= 21 = BlackjackWin
  | playerScore > 21 = Loss
  | dealerScore > 21 = Win
  | playerScore > dealerScore = Win
  | playerScore == dealerScore = Push
  | otherwise = Loss



declareWinner :: Game -> IO Outcome
declareWinner (d, dealerHand, playerHand) = do

  let playerScore = (sum $ map cardValue playerHand)
  let dealerScore = (sum $ map cardValue dealerHand)


  putStrLn ("\n")
  putStrLn ("Dealer hand: " ++ (show dealerHand) ++ " (" ++ show dealerScore  ++ ")")
  putStrLn ("Your hand: " ++ (show playerHand) ++ " (" ++ show playerScore  ++ ")")
          
  if findOutcome playerScore dealerScore == Push
    then do 
      putStrLn ("Its a tie!")
      return Push
  else if findOutcome playerScore dealerScore == Loss
    then do 
      putStrLn ("Dealer wins!")
      return Loss
    else do
      putStrLn ("You win!")
      return Win

addCard :: Game -> Game
addCard ((nextCard:deck), modifiedHand, remainingHand) = (deck, modifiedHand ++ [nextCard], remainingHand)

getBet :: Money -> IO Money
getBet credits = go ""
  where go contents = do
                    putStrLn ("Your bet: ")
                    input <- getLine
                    let bet = (read input :: Money)

                    if bet <= credits
                        then return bet
                        else do 
                          putStrLn ("Bet should not be bigger then available credits!")
                          getBet credits

newGame :: Money -> IO()
newGame credits = do

    if (credits < 1)
      then  putStrLn ("No more credits!")
    else do
      putStrLn ("\n")

      putStrLn ("-------------------------------------------------------------------------")
      putStrLn ("NEW GAME")
      putStrLn ("-------------------------------------------------------------------------")
      putStrLn ("Your credits: " ++ show credits)  
      putStrLn ("\n")

      bet <- getBet credits

      putStrLn ("\n")


      deck <- shuffle completeDeck
      let (d, playerHand, dealerHand) = addCard ( addCard (deck, [], []) )
      let (d', dealerHand', playerHand' ) = addCard ( addCard (d, [], playerHand) )
             
      game <- game (d, dealerHand', playerHand')

      putStrLn ("Bet: " ++ show bet)

      money <- getMoney bet game

      putStrLn ("Game end with: " ++ show game)

      putStrLn ("Money: " ++ show (credits+money))


      newGame (credits+money)


mkBtn :: String -> IO Button
mkBtn label = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  return btn

controlBox :: Bool -> Int -> Packing -> Int -> IO HBox
controlBox homogeneous spacing packing padding = do
  box     <- hBoxNew homogeneous spacing

  button1 <- buttonNewWithLabel "Deal Card"
  boxPackStart box button1 packing padding

  button2 <- buttonNewWithLabel "Stay"
  boxPackStart box button2 packing padding

  button3 <- buttonNewWithLabel "New game"
  boxPackStart box button3 packing padding

  return box

main :: IO ()
main = do
        -- newGame 200

        initGUI
        window <- windowNew
        set window [windowTitle := "Blackjack",
                   containerBorderWidth := 10,
                   windowDefaultWidth := 450, 
                   windowDefaultHeight := 500]                 
        hb1 <- hBoxNew False 0
        containerAdd window hb1



        vb1 <- vBoxNew False 0
        boxPackStart hb1 vb1 PackNatural 0

        vbDealerHand <- vBoxNew False 0
        boxPackStart vb1 vbDealerHand PackNatural 0

        vbPlayerHand <- vBoxNew False 0
        boxPackEnd vb1 vbPlayerHand PackNatural 0

        

        vbb <- vButtonBoxNew
        boxPackStart hb1 vbb PackGrow 0

        resetb <- buttonNewWithLabel "Reset"
        containerAdd vbb resetb

        quitb <- buttonNewWithLabel "Quit"
        containerAdd vbb quitb

        dealb <- buttonNewWithMnemonic "Deal Card"
        containerAdd vbb dealb

        stayb <- buttonNewWithMnemonic "Stay"
        containerAdd vbb stayb

        playb <- buttonNewWithMnemonic "New Game"
        containerAdd vbb playb

        set vbb [buttonBoxLayoutStyle := ButtonboxStart, 
                (buttonBoxChildSecondary playb) := True,
                (buttonBoxChildSecondary dealb) := True,
                (buttonBoxChildSecondary stayb) := True ]




        img <- imageNew
        imageSetFromFile img "2Cr.png"
    
        boxPackStart vbDealerHand img PackNatural 0



        img <- imageNew
        imageSetFromFile img "2Cr.png"
      
        boxPackEnd vbPlayerHand img PackNatural 0
  


        onClicked quitb mainQuit

        onClicked playb (newGame 200)

        onDestroy window mainQuit
        widgetShowAll window
        mainGUI




