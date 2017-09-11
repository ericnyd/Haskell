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
valueRank r | r == Jack || r == Queen || r == King = 10
valueRank Ace = 11

-- A function which returns the value of a Card
valueCard :: Card -> Int
valueCard (Card r _) = valueRank r


-- A function which calculates the number of aces in a hand
numberOfAces :: Hand -> Int
numberOfAces [] = 0 
numberOfAces (x:xs) | (rank x) == Ace = 1 + numberOfAces xs
                    | otherwise       = numberOfAces xs

-- A function which calculates the value of a hand whitout taking into account the change of value of aces
valueBA :: Hand -> Int
valueBA [] = 0
valueBA (x:xs) = valueCard x + valueBA xs


-- A function calculating the value of a hand with the behavior of aces in mind
value :: Hand -> Int
value hand | valueBA hand > 21 = valueBA hand - 10 * numberOfAces hand
           | otherwise         = valueBA hand

-- A functions which decide whether or not a Hand is bust
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- A function which decide the winner of a game
winner :: Hand -> Hand -> Player
winner gHand bHand | gameOver gHand                   = Bank
                   | gameOver bHand                   = Guest
                   | gameOver bHand && gameOver gHand = Bank
                   | value gHand > value bHand        = Guest
                   | otherwise                        = Bank

allRanks :: Rank
allRanks = []



