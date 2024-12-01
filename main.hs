import System.Random
import Control.Monad.State
import Data.List
import Control.Monad.IO.Class (MonadIO(liftIO))

data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Show, Eq, Ord)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord, Enum)

data Card = Card {suit :: Suit, value :: Value}

instance Ord Card where
    compare (Card _ value1) (Card _ value2) = compare value1 value2

instance Eq Card where
    (==) (Card _ value1) (Card _ value2) = value1 == value2

instance Show Card where
    show (Card suit value) = show value ++ " of " ++ show suit

newtype Deck = Deck [Card] deriving (Show)

data HandRank = RoyalFlush | StraightFlush | FourOfAKind | FullHouse | Flush | Straight | ThreeOfAKind | TwoPair | Pair | HighCard
    deriving (Show, Eq, Ord)

data Player = Player {name :: String, hand :: [Card], chips :: Int, dealer :: Bool} deriving Show

instance Eq Player where
    (==) (Player name1 _ _ _) (Player name2 _ _ _) = name1 == name2

data GameState = GameState {activePlayers :: [Player], deck :: Deck, pot :: Int, currentBets :: [(Player, Int)], dealerIndex :: Int, smallBlindIndex :: Int, bigBlindIndex :: Int} deriving Show

main :: IO ()
main = do
    -- change runstate to execstate when you don't need the result
    let initialState = createInitialState (createPlayers 6)
    dealerIndex <- randomRIO (1, 6)
    print dealerIndex
    let (result, dealerState) = runState (assignDealer dealerIndex) initialState
    let (result, dealtState) = runState dealCards dealerState
    --print result
    let (Deck cards) = deck result
    let rflush = [Card Spades Ten, Card Spades Jack, Card Spades Queen, Card Spades King, Card Spades Ace]
    let sflush = [Card Spades Two, Card Spades Three, Card Spades Four, Card Spades Five, Card Spades Six]
    let fourofakind = [Card Spades Two, Card Hearts Two, Card Diamonds Two, Card Clubs Two, Card Spades Ace]
    let fullHouse = [Card Spades Two, Card Hearts Two, Card Diamonds Two, Card Clubs Ace, Card Spades Ace]
    let flush = [Card Spades Two, Card Spades Three, Card Spades Four, Card Spades Five, Card Spades Seven]
    let straight = [Card Spades Two, Card Hearts Three, Card Diamonds Four, Card Clubs Five, Card Spades Six]
    let threeOfAKind = [Card Spades Two, Card Hearts Two, Card Diamonds Two, Card Clubs Three, Card Spades Four]
    let twoPair = [Card Spades Two, Card Hearts Two, Card Diamonds Three, Card Clubs Three, Card Spades Four]
    let pair = [Card Spades Two, Card Hearts Two, Card Diamonds Three, Card Clubs Four, Card Spades Five]
    let highCard = [Card Spades Two, Card Hearts Three, Card Diamonds Four, Card Clubs Five, Card Spades Seven]
    print (evaluateHand rflush)
    print (evaluateHand sflush)
    print (evaluateHand fourofakind)
    print (evaluateHand fullHouse)
    print (evaluateHand flush)
    print (evaluateHand straight)
    print (evaluateHand threeOfAKind)
    print (evaluateHand twoPair)
    print (evaluateHand pair)
    print (evaluateHand highCard)
    --print (hand ((activePlayers result) !! (dealerIndex - 1)))
    --print (evaluateHand (hand ((activePlayers result) !! (dealerIndex - 1))))
     
    

createInitialState :: [Player] -> GameState
createInitialState players = GameState players (shuffleDeck 1) 0 [] 0 1 2

-- Shuffles the a deck of cards
-- Takes an integer seed for the random number generator
shuffleDeck :: Int -> Deck
shuffleDeck seed = let
    cmp (_, n1) (_, n2) = compare n1 n2 
    (Deck cards) = Deck [Card suit value | suit <- [Spades, Hearts, Diamonds, Clubs], value <- [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]]
    in
    Deck [ card | (card, _) <- sortBy cmp (zip cards (randoms (mkStdGen seed) :: [Int]))]

createPlayers :: Int -> [Player]
createPlayers count = let 
    newPlayer i = Player ("Player " ++ show i) [] 100 False
    in [newPlayer i | i <- [1..count]]

assignDealer :: Int -> State GameState GameState
assignDealer index = do
    gs <- get
    let players = map (\(i, p) -> Player (name p) (hand p) (chips p) (i == index)) (zip [1..] (activePlayers gs))
    put gs {dealerIndex = index, activePlayers = players}
    get


dealCards :: State GameState GameState
dealCards = do
    gs <- get
    -- deal two cards to each player and 5 to the dealer
    let (updatedPlayers, updatedDeck) = dealCardsToPlayers (activePlayers gs) (deck gs)
    put gs {activePlayers = updatedPlayers, deck = updatedDeck}
    get
    where
    dealCardsToPlayers :: [Player] -> Deck -> ([Player], Deck)
    dealCardsToPlayers [] deck = ([], deck)
    dealCardsToPlayers (p:ps) (Deck deck) = let
        numCards = if dealer p then 5 else 2
        player = Player (name p) (hand p ++ take numCards deck) (chips p) (dealer p)
        (players, Deck updatedDeck) = dealCardsToPlayers ps (Deck (drop numCards deck))
        in (player : players, Deck updatedDeck)

evaluateHand :: [Card] -> HandRank
evaluateHand hand = let
        isRoyalFlush = allSameSuit && allConsecutive && value (minimum hand) == Ten
        isStraightFlush = allSameSuit && allConsecutive
        isFourOfAKind = nOfAKind 4
        isFullHouse = nOfAKind 3 && nOfAKind 2
        isFlush = allSameSuit
        isStraight = allConsecutive
        isThreeOfAKind = nOfAKind 3
        isTwoPair = getNOfAKind 2 == 2
        isPair = getNOfAKind 2 == 1
        allSameSuit = all (\c -> suit c == suit (head hand)) hand
    in case () of
        _ | isRoyalFlush -> RoyalFlush
        _ | isStraightFlush -> StraightFlush
        _ | isFourOfAKind -> FourOfAKind
        _ | isFullHouse -> FullHouse
        _ | isFlush -> Flush
        _ | isStraight -> Straight
        _ | isThreeOfAKind -> ThreeOfAKind
        _ | isTwoPair -> TwoPair
        _ | isPair -> Pair
        _ -> HighCard
    where
        allConsecutive = let 
            sortedHand = sort hand
            in all (\(c1, c2) -> succ (value c1) == value c2) (zip sortedHand (tail sortedHand))
        getNOfAKind n = length (filter (\ls -> length ls == n) (group (sort hand)))
        nOfAKind n = getNOfAKind n > 0
        
