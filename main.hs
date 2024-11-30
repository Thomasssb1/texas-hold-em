import System.Random
import Control.Monad.State
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

main :: IO ()
main = do
    -- change runstate to execstate when you don't need the result
    let (result, newState) = runState dealCards initialState
    print result

initialState :: GameState
initialState = GameState createPlayers (shuffleDeck 1) 0 [] 0 1 2

-- Shuffles the a deck of cards
-- Takes an integer seed for the random number generator
shuffleDeck :: Int -> Deck
shuffleDeck seed = let
    cmp (_, n1) (_, n2) = compare n1 n2 
    (Deck cards) = Deck [Card suit value | suit <- [Spades, Hearts, Diamonds, Clubs], value <- [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]]
    in
    Deck [ card | (card, _) <- sortBy cmp (zip cards (randoms (mkStdGen seed) :: [Int]))]

createPlayers :: [Player]
createPlayers = let
    newPlayer i = Player ("Player " ++ show i) [] 100 False
    in [newPlayer i | i <- [1..6]]

dealCards :: State GameState GameState
dealCards = do
    gs <- get
    let (updatedPlayers, updatedDeck) = dealCardsToPlayers (activePlayers gs) (deck gs)
    put gs {activePlayers = updatedPlayers, deck = updatedDeck}
    return gs
    where
    dealCardsToPlayers :: [Player] -> Deck -> ([Player], Deck)
    dealCardsToPlayers [] deck = ([], deck)
    dealCardsToPlayers (p:ps) (Deck deck) = let
        player = Player (name p) (hand p ++ take 2 deck) (chips p) (dealer p)
        (players, Deck updatedDeck) = dealCardsToPlayers ps (Deck (drop 2 deck))
        in (player : players, Deck updatedDeck)