import System.Random
import Control.Monad.State
import Data.List
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.List (sortBy, subsequences)
import Data.Foldable (Foldable(maximum), maximumBy)

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

data HandRank = HighCard | Pair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush deriving (Show, Eq, Ord)

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
    let (result, dealtState) = runState  dealCards dealerState

    --let (result, _) = runState determineWinner dealtState
    print result

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
        isRoyalFlush = allSameSuit && allConsecutive && value (minimum hand) == Ten && length hand == 5
        isStraightFlush = allSameSuit && allConsecutive && length hand == 5
        isFourOfAKind = nOfAKind 4
        isFullHouse = nOfAKind 3 && nOfAKind 2
        isFlush = allSameSuit && length hand == 5
        isStraight = allConsecutive && length hand == 5
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

--
--determineWinner :: State GameState [Player]
--determineWinner = do
--   gs <- get
--    let 
--        filteredPlayers = filter (not.dealer) (activePlayers gs)
--        dealerHand = hand (activePlayers gs !! dealerIndex gs)
--        playerHands = filter (\(_, h) -> length h == 5) (map (\p -> (p, subsequences (hand p ++ dealerHand))) filteredPlayers)
--        bestPlayerHands = concat (map allMaximumHands (groupBy (\(p1, _) (p2, _) -> p1 == p2)))


--case maximumHand of
--        HighCard -> return (determineHighCardWinner orderPlayerHands)

  --  put gs
    --return topPlayers

sortCard :: [Card] -> [Card] -> Ordering
sortCard [] [] = EQ
sortCard (x:xs) (y:ys)
    | x > y = GT
    | y > x = LT
    | x == y = sortCard xs ys

findBestHand :: [[Card]] -> [Card]
findBestHand cs = let
        compareHands [] [] = EQ
        getPairs xs = filter (\hs -> length hs == 2) (groupBy (\h1 h2 -> value h1 == value h2) xs)
            -- handle different tie scenarios
        resolveHighCardTie = last (sortBy (\h1 h2 -> (sortCard h1 h2)) sortedMaxHands)
    in
        if length allMaxHands == 1 then 
            last sortedMaxHands
        else case maximumHand of
            HighCard -> resolveHighCardTie
        where
            maximumHand = evaluateHand (maximumBy (\h1 h2 -> compare (evaluateHand h1) (evaluateHand h2)) cs)
            allMaxHands = filter (\h -> evaluateHand h == maximumHand) cs
            sortedMaxHands = map (\h -> sortBy (\c1 c2 -> compare (value c1) (value c2)) h) allMaxHands