module Blackjack where

-------------Imports-------------

import Cards
import Wrapper
import Test.QuickCheck hiding (shuffle)

------------Variables------------

aCard1 :: Card
aCard1 = Card Jack Spades

aCard2 :: Card
aCard2 = Card (Numeric 7) Hearts

guestHand :: Hand
guestHand = [aCard1, aCard2, Card Ace Spades]

bankHand :: Hand
bankHand = [aCard2, Card Ace Spades, Card King Clubs]

------------Functions------------

-- A function which displays a card
displayCard :: Card -> String
displayCard (Card (Numeric n) s) = show n ++ " of " ++ show s
displayCard (Card r s)           = show r ++ " of " ++ show s


-- A function which displays a Hand
display :: Hand -> String
display []     = error "Empty hand"
display (x:[]) = displayCard x
display (x:xs) = displayCard x ++ ", " ++ display xs


-- A function which returns the value of a Rank
valueRank :: Rank -> Int
valueRank (Numeric n) = n
valueRank Ace = 11
valueRank _ = 10

-- A function which returns the value of a Card
valueCard :: Card -> Int
valueCard (Card r _) = valueRank r


-- A function which calculates the number of aces in a hand
numberOfAces :: Hand -> Int
numberOfAces [] = 0 
numberOfAces (x:xs) | (rank x) == Ace = 1 + numberOfAces xs
                    | otherwise       = numberOfAces xs

-- A function which calculates the value of a hand whitout taking into account the change of value of aces
value' :: Hand -> Int
value' [] = 0
value' (x:xs) = valueCard x + value' xs


-- A function calculating the value of a hand with the behavior of aces in mind
value :: Hand -> Int
value hand | value' hand > 21 = value' hand - 10 * numberOfAces hand
           | otherwise         = value' hand

-- A functions which decide whether or not a Hand is bust
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- A function which decide the winner of a game
winner :: Hand -> Hand -> Player
winner gHand bHand 
    | gameOver gHand                   = Bank
    | gameOver bHand                   = Guest
    | gameOver bHand && gameOver gHand = Bank
    | value gHand > value bHand        = Guest
    | otherwise                        = Bank


-- A function which return a list of all possible Ranks
allRanks :: [Rank]
allRanks = [Numeric x | x <- [2..10]] ++ [Jack, Queen, King, Ace]

-- A function which return a list of all possible Suits
allSuits :: [Suit]
allSuits = [Hearts, Spades, Diamonds, Clubs]

-- A funcion which return a full Deck
fullDeck :: Deck
fullDeck = Deck [Card x y | x <- allRanks, y <- allSuits]

-- A property function used to test whether the function "fullDeck" returns a Deck containing 52 cards.
prop_size_fullDeck :: Bool
prop_size_fullDeck = size (cards fullDeck) == 52

-- A function which draws the first card of a deck and places it in a hand
draw :: Deck -> Hand -> (Deck, Hand)
draw (Deck []) hand     = error "draw: The deck is empty."
draw (Deck (x:xs)) hand = (Deck xs, x:hand)

--  A function which returns the bank-hand
playBank :: Deck -> Hand
playBank deck = playBank' deck []

-- A function that builds the bank-hand by drawing cards from given deck
playBank' :: Deck -> Hand -> Hand
playBank' deck bankHand 
    | value bankHand >= 16 = bankHand
    | otherwise            = playBank' newDeck bankHand'
    where (newDeck, bankHand') = draw deck bankHand     

-- A function that takes a deck and uses helper-functions to return a shuffled deck
shuffle :: [Double] -> Deck -> Deck
shuffle randList deck = Deck (shuffle' randList (cards deck))

shuffle' :: [Double] -> [Card] -> [Card]
shuffle' _ []        = []
shuffle' (x:xs) deck = deck !! index : shuffle' xs (newDeck index deck)
    where index = (floor (x * fromIntegral(length deck)))

newDeck :: Int -> [Card] -> [Card]
newDeck 0 (x:xs) = xs
newDeck n (x:xs) = x : (newDeck (n-1) xs)

-- A function that checks if a card exists in a deck
belongsTo :: Card -> Deck -> Bool
c `belongsTo` (Deck [])      = False
c `belongsTo` (Deck (c':cs)) = c == c' || c `belongsTo` (Deck cs)

-- A property function that checks if a card remains in a deck after the deck has been shuffled
prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
    card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

-- A property function that checks if the size of a deck changes after the deck has been shuffled
prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) (Deck cardlist) = length cardlist == length shuffledDeck
        where shuffledDeck = (cards (shuffle randomlist (Deck cardlist)))

-- A package for the interface used to build the game
implementation = Interface
    {  iFullDeck  = fullDeck
    ,  iValue     = value
    ,  iDisplay   = display
    ,  iGameOver  = gameOver
    ,  iWinner    = winner
    ,  iDraw      = draw
    ,  iPlayBank  = playBank
    ,  iShuffle   = shuffle
    }

-- Runs the program
main :: IO ()
main = runGame implementation
