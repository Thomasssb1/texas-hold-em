import System.Random
import Control.Monad.State
import Data.List
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

data PlayerStrategy = Random | Passive | Aggressive | Smart deriving Show

data Player = Player {name :: String, hand :: [Card], chips :: Int, dealer :: Bool, strategy :: PlayerStrategy} deriving Show

instance Eq Player where
    (==) (Player name1 _ _ _ _) (Player name2 _ _ _ _) = name1 == name2

data RoundType = Preflop | Flop | Turn | River deriving (Show, Enum)

data RoundState = RoundState {highestBet :: Int, previousPlayerBets :: [(Player, Int)], minimumRaise :: Int, roundType :: RoundType} deriving Show

data GameState = GameState {activePlayers :: [Player], deck :: Deck, pot :: Int, currentBets :: [(Player, Int)], dealerIndex :: Int, smallBlindIndex :: Int, bigBlindIndex :: Int, currentRound :: RoundState} deriving Show

main :: IO ()
main = do
    -- change runstate to execstate when you don't need the result
    let initialState = createInitialState (createPlayers 6)
    dealerIndex <- randomRIO (1, 6)
    print dealerIndex
    let (result, dealerState) = runState (assignDealer dealerIndex) initialState
    let (result, preflopState) = runState bettingRound dealerState

    --print (currentRound result)
    print (activePlayers result)
    print (pot result)

createInitialState :: [Player] -> GameState
createInitialState players = let
        intitialRoundState = RoundState 0 (map (\p -> (p,0)) players) 0 Preflop
        in
        GameState players (shuffleDeck 1) 0 [] 0 1 2 intitialRoundState

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
    newPlayer i = Player ("Player " ++ show i) [] 100 False Random
    in [newPlayer i | i <- [1..count]]

assignDealer :: Int -> State GameState GameState
assignDealer index = do
    gs <- get
    let players = map (\(i, p) -> p {dealer = (i == index)}) (zip [1..] (activePlayers gs))
    let roundPlayers = map (\p -> (p, 0)) (filter (not.dealer) players)
    let updatedRound = (currentRound gs) { previousPlayerBets = roundPlayers }
    put gs {dealerIndex = index, activePlayers = players, currentRound = updatedRound}
    get

dealHoleCards :: State GameState GameState
dealHoleCards = do
    gs <- get
    -- deal two cards to each player and 5 to the dealer
    let (updatedPlayers, updatedDeck) = dealCardsToPlayers (filter (not.dealer) (activePlayers gs)) (deck gs)
    put gs {activePlayers = updatedPlayers, deck = updatedDeck}
    get
    where
    dealCardsToPlayers :: [Player] -> Deck -> ([Player], Deck)
    dealCardsToPlayers [] deck = ([], deck)
    dealCardsToPlayers (p:ps) (Deck deck) = let
        player = p {hand = hand p ++ (take 2 deck)}
        (players, Deck updatedDeck) = dealCardsToPlayers ps (Deck (drop 2 deck))
        in (player : players, Deck updatedDeck)

dealCommunityCards :: Int -> State GameState GameState
dealCommunityCards n = do
    gs <- get
    let cards (Deck deck) = deck
    let updatedPlayers = map (\p -> if dealer p then p {hand = hand p ++ take n (cards (deck gs))} else p) (activePlayers gs)
    put gs {activePlayers = updatedPlayers, deck = Deck (drop n (cards (deck gs)))}
    get

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

determineWinner :: State GameState [Player]
determineWinner = do
   gs <- get
   let
    filteredPlayers = filter (not.dealer) (activePlayers gs)
    dealerHand = hand (activePlayers gs !! dealerIndex gs)
    playerHands = map (\p -> (p, filter (\h -> length h == 5) (subsequences (hand p ++ dealerHand)))) filteredPlayers
    -- returns the best hand for each player
    bestPlayerHands = map (\(p, hs) -> (p, head (findBestHand hs))) playerHands
    winningHands = findBestHand (map snd bestPlayerHands)
    in
        return (map fst (filter (\(_, h) -> inList h winningHands) bestPlayerHands))
        where
            inList _ [] = False
            inList toFind (x:xs) = if x == toFind then True else inList toFind xs

findBestHand :: [[Card]] -> [[Card]]
findBestHand cs = let
        -- handle different tie scenarios
        resolveHighCardTie = getBestHand highestCard sortedMaxHands
        resolvePairTie = getBestHand highestCard (getBestHand (highestN 2) sortedMaxHands)
        resolveThreeOfAKindTie = getBestHand highestCard (getBestHand (highestN 3) sortedMaxHands)
        resolveFullHouseTie = getBestHand (highestN 2) (getBestHand (highestN 3) sortedMaxHands)
        resolveFourOfAKindTie = getBestHand highestCard (getBestHand (highestN 4) sortedMaxHands)
    in
        if length allMaxHands == 1 then 
            sortedMaxHands
        else case maximumHand of
            HighCard -> resolveHighCardTie
            Pair -> resolvePairTie
            TwoPair -> resolvePairTie
            ThreeOfAKind -> resolveThreeOfAKindTie
            Straight -> resolveHighCardTie -- check the top card, but if it is the same then they are all the same
            Flush -> resolveHighCardTie
            FullHouse -> resolveFullHouseTie
            FourOfAKind -> resolveFourOfAKindTie
            StraightFlush -> resolveHighCardTie
            RoyalFlush -> sortedMaxHands
        where
            maximumHand = evaluateHand (maximumBy (\h1 h2 -> compare (evaluateHand h1) (evaluateHand h2)) cs)
            allMaxHands = filter (\h -> evaluateHand h == maximumHand) cs
            sortedMaxHands = map (\h -> sortBy (\c1 c2 -> compare (value c1) (value c2)) h) allMaxHands
            getBestHand f xs = last (sortBy (\hs1 hs2 -> f (head hs1) (head hs2)) (groupBy (\h1 h2  -> (f h1 h2) == EQ) xs))
            highestN n xs ys = highestCard (getHighestN xs) (getHighestN ys)
                where getHighestN h = sort (map head (filter (\hs -> length hs == n) (groupBy (\h1 h2 -> value h1 == value h2) h)))
            -- orders
            highestCard [] [] = EQ
            highestCard (x:xs) (y:ys)
                | x > y = GT
                | y > x = LT
                | x == y = highestCard xs ys

-- ROUND LOGIC --

-- call can act as check provided no one bets
-- returns a boolean for if they are able to call or not
playerBet :: Player -> Int -> State GameState ()
playerBet plr amount = do
    gs <- get
    let round = currentRound gs
    let updatedPlayer = plr {chips = chips plr - amount}
    let updatedRoundBets = map (\(p, n) -> if p == plr then (updatedPlayer, n+amount) else (p, n)) (previousPlayerBets round)
    let updatedActiveplayers = map (\p -> if p == plr then updatedPlayer else p) (activePlayers gs)
    let updatedRound = round { previousPlayerBets = updatedRoundBets}
    put gs {currentRound = updatedRound, pot = pot gs + amount, activePlayers = updatedActiveplayers}

call :: Player -> State GameState ()
call plr = do
    gs <- get
    let round = currentRound gs
    let amountNeededToBet = highestBet round - snd (head (filter (\(p,n) -> p == plr) (previousPlayerBets round)))
    let newState = execState (playerBet plr amountNeededToBet) gs
    put newState

fold :: Player -> State GameState ()
fold plr = do
    gs <- get
    put gs {activePlayers = (filter (\p -> p /= plr) (activePlayers gs))}

raise :: Int -> Player -> State GameState ()
raise amount plr = do
    gs <- get
    let round = currentRound gs
    let newState = execState (playerBet plr amount) gs
    put newState

checkIfCallValid :: RoundState -> Player -> (Bool, State GameState ())
checkIfCallValid rs plr = let
    amountNeededToBet = highestBet rs - snd (head (filter (\(p, _) -> p == plr) (previousPlayerBets rs)))
    in (chips plr >= amountNeededToBet, call plr)

checkIfFoldValid :: Player -> (Bool, State GameState ())
checkIfFoldValid plr = (True, fold plr)

checkIfRaiseValid :: RoundState -> Player -> (Bool, State GameState ())
checkIfRaiseValid rs plr = (chips plr >= minimumRaise rs, raise determineRandomRaiseAmount plr)
    where
        determineRandomRaiseAmount = 50

chooseActionBasedOnStrategy :: Player -> State GameState GameState
chooseActionBasedOnStrategy plr = do
    gs <- get
    case strategy plr of
        Random -> do
            let newState = testAllActions (shuffledActions (currentRound gs)) gs
            put newState
            get
    where
        actions currentRound = [checkIfCallValid currentRound, checkIfFoldValid, checkIfRaiseValid currentRound]
        shuffledActions currentRound = [action | (action, _) <- sortBy (\(_, n1) (_, n2) -> compare n1 n2) (zip (actions currentRound) randomNumbers)]
            where
            randomNumbers = randoms (mkStdGen 1234) :: [Int]
        -- do not need a base case as fold will always return true
        testAllActions (a:as) gs = let (valid, f) = a plr in
            if valid then execState f gs else testAllActions as gs

iterateEachPlayer :: [Player] -> State GameState GameState
iterateEachPlayer [] = do get
iterateEachPlayer (p:ps) = do
    gs <- get
    let newState = execState (chooseActionBasedOnStrategy p) gs
    put newState
    iterateEachPlayer ps

bettingRound :: State GameState GameState
bettingRound = do
    gs <- get
    let round = currentRound gs
    case roundType round of
        Preflop -> do
            let dealtCardState = execState dealHoleCards gs
            put dealtCardState
            let players = filter (not.dealer) (activePlayers dealtCardState)
            let bettingRoundState = execState (iterateEachPlayer players) dealtCardState
            put bettingRoundState
            get
        Flop -> dealCommunityCards 3
        Turn -> dealCommunityCards 1
        River -> dealCommunityCards 1