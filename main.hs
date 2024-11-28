import System.Random
import Data.List

data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Show, Eq, Ord)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

data Card = Card {suit :: Suit, value :: Value}

instance Ord Card where
    compare (Card _ value1) (Card _ value2) = compare value1 value2

instance Eq Card where
    (==) (Card _ value1) (Card _ value2) = value1 == value2

instance Show Card where
    show (Card suit value) = show value ++ " of " ++ show suit

newtype Deck = Deck [Card] deriving (Show)

data Player = Player {name :: String, hand :: [Card], chips :: Int, dealer :: Bool} deriving Show

instance Eq Player where
    (==) (Player name1 _ _ _) (Player name2 _ _ _) = name1 == name2

data GameState = GameState {activePlayers :: [Player], deck :: Deck, pot :: Int, currentBets :: [(Player, Int)], dealerIndex :: Int, smallBlindIndex :: Int, bigBlindIndex :: Int} deriving Show

-- Shuffles the a deck of cards
-- Takes an integer seed for the random number generator
shuffleDeck :: Int -> Deck
shuffleDeck n = let
    cmp (_, n1) (_, n2) = compare n1 n2 
    (Deck cards) = Deck [Card suit value | suit <- [Spades, Hearts, Diamonds, Clubs], value <- [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]]
    in 
    Deck [ card | (card, _) <- sortBy cmp (zip cards (randoms (mkStdGen n) :: [Int]))]

createPlayers :: [Player]
createPlayers = let
    newPlayer i = Player ("Player " ++ show i) [] 100 False
    in [newPlayer i | i <- [1..6]]


dealCards :: GameState -> GameState
dealCards gs = let
    (updatedPlayers, updatedDeck) = dealCardsToPlayers (activePlayers gs) (deck gs)
    in GameState updatedPlayers updatedDeck (pot gs) (currentBets gs) (dealerIndex gs) (smallBlindIndex gs) (bigBlindIndex gs)

dealCardsToPlayers :: [Player] -> Deck -> ([Player], Deck)
dealCardsToPlayers [] deck = ([], deck)
dealCardsToPlayers (p:ps) (Deck deck) = let
    player = Player (name p) (hand p ++ take 2 deck) (chips p) (dealer p)
    (players, Deck updatedDeck) = dealCardsToPlayers ps (Deck (drop 2 deck))
    in (player : players, Deck updatedDeck)