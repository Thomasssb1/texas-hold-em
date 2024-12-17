import System.Random
import Control.Monad.State
import Data.List
import Data.Foldable (Foldable(maximum), maximumBy)
import System.Random (StdGen, mkStdGen)
import Prelude
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

data HandRank = HighCard | Pair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush deriving (Show, Eq, Ord)

data PlayerStrategy = Random | Passive | Aggressive | Smart deriving Show

data Player = Player {name :: String, hand :: [Card], chips :: Int, dealer :: Bool, strategy :: PlayerStrategy, randomGen :: StdGen}

instance Eq Player where
    (==) (Player name1 _ _ _ _ _) (Player name2 _ _ _ _ _) = name1 == name2

instance Show Player where
    show (Player name hand chips dealer _ _) = "{name: "++name++", hand: "++showHand hand++", chips: " ++show chips++", dealer: "++show dealer++"}"

showHand :: [Card] -> String
showHand [] = ""
showHand [x] = show x
showHand (x:xs) = show x ++ ", " ++ showHand xs

data RoundType = Preflop | Flop | Turn | River | Showdown deriving (Show, Enum, Eq)

data GameState = GameState {activePlayers :: [Player], deck :: Deck, pot :: Int, currentBets :: [(Player, Int)], dealerIndex :: Int, smallBlindIndex :: Int, bigBlindIndex :: Int, communityCards :: [Card], highestBet :: Int, minimumRaise :: Int, roundType :: RoundType} deriving Show

main :: IO ()
main = do
    initialPlayerSeed <- randomIO
    let initialState = createInitialGameState (createPlayers 6 initialPlayerSeed)

    dealerIndex <- randomRIO (0, 5)
    print dealerIndex
    (result, dealerState) <- runStateT (assignDealer dealerIndex) initialState

    --let dealer = activePlayers dealerState !! dealerIndex
    --(deck, shuffledDeckState) <- runStateT (shuffleDeck dealer) dealerState

    (result, finalState) <- runStateT (gameLoop 0) dealerState

    print (pot finalState)
    print (activePlayers finalState)

createInitialGameState :: [Player] -> GameState
createInitialGameState players = let
        in
        GameState players (Deck []) 0 (map (\p -> (p,0)) players) 0 1 2 [] 0 1 Preflop

updateGameState :: StateT GameState IO ()
updateGameState = do
    gs <- get
    let updatedPlayers = map (\p -> p {hand = []}) (filter (\p -> chips p > 0) (activePlayers gs))
    put gs {activePlayers = updatedPlayers, deck = Deck[], pot = 0, currentBets = (map (\p -> (p,0)) updatedPlayers), dealerIndex = incrementIndex (dealerIndex gs) updatedPlayers, communityCards = [], highestBet = 0, minimumRaise = 1, roundType = Preflop}
    where
        incrementIndex n ps = (n + 1) `mod` (length ps)

-- Shuffles the a deck of cards
-- Takes the dealer and uses their StdGen in order to shuffle the deck
shuffleDeck :: Player -> StateT GameState IO Deck
shuffleDeck dealer = do
    gs <- get
    liftIO $ putStrLn "hi"
    let cmp (_, n1) (_, n2) = compare n1 n2
    let (Deck cards) = Deck [Card suit value | suit <- [Spades, Hearts, Diamonds, Clubs], value <- [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]]
    let updatedDeck = Deck [ card | (card, _) <- sortBy cmp (zip cards (randoms (randomGen dealer) :: [Int]))]
    put gs {deck = updatedDeck}
    return updatedDeck

createPlayers :: Int -> Int -> [Player]
createPlayers count initialSeed = let 
    newPlayer i = Player ("Player " ++ show i) [] 100 False Smart (mkStdGen (initialSeed + i))
    in [newPlayer i | i <- [1..count]]

assignDealer :: Int -> StateT GameState IO GameState
assignDealer index = do
    gs <- get
    let players = map (\(i, p) -> p {dealer = (i == index)}) (zip [0..] (activePlayers gs))
    put gs {dealerIndex = index, activePlayers = players, currentBets = map (\p -> (p, 0)) players}
    get

dealHoleCards :: StateT GameState IO GameState
dealHoleCards = do
    gs <- get
    -- deal two cards to each player
    let (updatedPlayers, updatedDeck) = dealCardsToPlayers (activePlayers gs) (deck gs)
    let updatedRoundPlayers = map (\p -> (p,0)) updatedPlayers
    put gs {activePlayers = updatedPlayers, deck = updatedDeck, currentBets = updatedRoundPlayers}
    get
    where
    dealCardsToPlayers :: [Player] -> Deck -> ([Player], Deck)
    dealCardsToPlayers [] deck = ([], deck)
    dealCardsToPlayers (p:ps) (Deck deck) = let
        player = p {hand = hand p ++ (take 2 deck)}
        (players, Deck updatedDeck) = dealCardsToPlayers ps (Deck (drop 2 deck))
        in (player : players, Deck updatedDeck)

dealCommunityCards :: Int -> StateT GameState IO GameState
dealCommunityCards n = do
    gs <- get
    let cards (Deck deck) = deck
    put gs {communityCards = communityCards gs ++ take n (cards (deck gs)), deck = Deck (drop n (cards (deck gs)))}
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
            in all (\(c1, c2) -> fromEnum (value c1) - fromEnum (value c2) == 1) (zip sortedHand (tail sortedHand))
        getNOfAKind n = length (filter (\ls -> length ls == n) (group (sort hand)))
        nOfAKind n = getNOfAKind n > 0

determineWinner :: StateT GameState IO [Player]
determineWinner = do
   gs <- get
   let
    roundPlayers = map fst (currentBets gs)
    playerHands = map (\p -> (p, filter (\h -> length h == 5) (subsequences (hand p ++ communityCards gs)))) roundPlayers
    -- returns the best hand for each player
    bestPlayerHands = map (\(p, hs) -> (p, head (findBestHand hs))) playerHands
    winningHands = findBestHand (map snd bestPlayerHands)
    in do
        -- lift $ putStrLn (show playerHands)
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
playerBet :: Player -> Int -> StateT GameState IO ()
playerBet plr amount = do
    gs <- get
    let updatedPlayer = plr {chips = chips plr - amount}
    let updatedRoundBets = map (\(p, n) -> if p == plr then (updatedPlayer, n+amount) else (p, n)) (currentBets gs)
    put gs {highestBet = if amount > highestBet gs then amount else highestBet gs, pot = pot gs + amount, currentBets = updatedRoundBets}

call :: Player -> StateT GameState IO ()
call plr = do
    gs <- get
    liftIO $ putStrLn ((show (name plr)) ++ " called")
    let amountNeededToBet = highestBet gs - snd (head (filter (\(p,n) -> p == plr) (currentBets gs)))
    newState <- lift $ execStateT (playerBet plr amountNeededToBet) gs
    put newState

fold :: Player -> StateT GameState IO ()
fold plr = do
    gs <- get
    liftIO $ putStrLn ((show (name plr)) ++ " folded")
    let playerBet = snd (head (filter (\(p, _) -> p == plr) (currentBets gs)))
    let updatedActivePlayers = map (\p -> if p == plr then p {chips = (chips p) - playerBet} else p) (activePlayers gs)
    let updatedPlayerBets = filter (\(p, _) -> p /= plr) (currentBets gs)
    put gs {currentBets = updatedPlayerBets, activePlayers = updatedActivePlayers}

raise :: Int -> Player -> StateT GameState IO ()
raise amount plr = do
    gs <- get
    liftIO $ putStrLn ((show (name plr)) ++ " raised by " ++ (show amount))
    newState <- lift $ execStateT (playerBet plr amount) gs
    put newState {minimumRaise = amount}

checkIfCallValid :: GameState -> Player -> (Bool, StateT GameState IO ())
checkIfCallValid gs plr = let
    amountNeededToBet = highestBet gs - snd (head (filter (\(p, _) -> p == plr) (currentBets gs)))
    in (chips plr >= amountNeededToBet, call plr)

checkIfFoldValid :: Player -> (Bool, StateT GameState IO ())
checkIfFoldValid plr = (True, fold plr)

checkIfRaiseValid :: GameState -> Maybe Int -> Player -> (Bool, StateT GameState IO ())
checkIfRaiseValid gs (Just raiseAmount) plr = (raiseAmount <= chips plr && raiseAmount >= minimumRaise gs, raise raiseAmount plr)
checkIfRaiseValid gs Nothing plr = (chips plr >= minimumRaise gs, raise raiseAmount plr)
    where
        (raiseAmount, newGen) = randomR (minimumRaise gs, chips plr) (randomGen plr)

needToIterateAgain :: GameState -> [Player]
needToIterateAgain gs = map fst (filter (\(p, b) -> b /= highestBet gs) (currentBets gs))

smartPlayerStrategy :: Player -> StateT GameState IO [Player -> (Bool, StateT GameState IO ())]
smartPlayerStrategy plr = do
    gs <- get
    if roundType gs == Preflop then do
        let bestHand = head (findBestHand [hand plr])
        let handRank = evaluateHand bestHand
        -- lift $ putStrLn ("rank: " ++ (show handRank))
        -- higher than 10, e.g. jack, queen, king
        let highPremiumPair = handRank == Pair && fromEnum (value (head (hand plr))) > 7
        -- combined value of more than 14, but a difference less than 5
        let highConnector = (foldr (\c acc -> acc + fromEnum (value c)) 0 bestHand) > 14 && abs (fromEnum (value (head (hand plr))) - fromEnum (value (last (hand plr)))) < 5
        -- suited and consecutive
        let suitedConnector = all (\c -> suit c == suit (head (hand plr))) (hand plr) && abs (fromEnum (value (head (hand plr))) - fromEnum (value (last (hand plr)))) == 1
        -- in the range 7 - 10
        let mediumPairs = handRank == Pair && fromEnum (value (head (hand plr))) > 4
        -- The players chips to raise by (is higher when more confident)
        -- clamp the amount between the minimum raise and the chips of the player
        let betMultiplier = foldr (\(b, x) acc -> if b then x + acc else acc) 0.0 [(highPremiumPair, 0.4 :: Double), (highConnector, 0.25 :: Double), (suitedConnector, 0.1 :: Double), (mediumPairs, 0.05 :: Double)]
        let betAmount = Just (clamp 0 (chips plr) (floor (fromIntegral (chips plr) * betMultiplier)))
        lift $ putStrLn ("bet: " ++ (show betAmount))
        do case () of
            _ | highPremiumPair || highConnector -> return [checkIfRaiseValid gs betAmount, checkIfCallValid gs, checkIfFoldValid]
            _ | suitedConnector || mediumPairs -> return [checkIfCallValid gs, checkIfRaiseValid gs betAmount, checkIfFoldValid]
            _ -> return [checkIfCallValid gs, checkIfFoldValid, checkIfRaiseValid gs betAmount]
    else do
        let bestHand = head (findBestHand (filter (\h -> length h == 5) (subsequences (hand plr ++ communityCards gs))))
        let handRank = evaluateHand bestHand
        -- lift $ putStrLn ("rank: " ++ (show handRank))
        let allPossibleCards = hand plr ++ communityCards gs
        -- within 1 card
        let getLengthOfLargestGroup f cs = length (maximumBy (\ls1 ls2 -> compare (length ls1) (length ls2)) (groupBy f cs))
        let closeToFlush =  getLengthOfLargestGroup (\c1 c2 -> suit c1 == suit c2) allPossibleCards > 3
        let closeToStraight = getLengthOfLargestGroup (\c1 c2 -> abs (fromEnum (value c1) - fromEnum (value c2)) == 1) (sort allPossibleCards) > 3

        let betMultiplier = foldr (\(b, x) acc -> if b then x + acc else acc) 0.0 [(closeToFlush, 0.7 :: Double), (closeToStraight, 0.6 :: Double)]
        let betAmount = Just (clamp 0 (chips plr) (floor (fromIntegral (chips plr) * betMultiplier)))
        lift $ putStrLn ("bet: " ++ (show betAmount))
        if handRank > Pair || closeToFlush || closeToStraight then do
            return [checkIfRaiseValid gs betAmount, checkIfCallValid gs, checkIfFoldValid]
        else do
            return [checkIfCallValid gs, checkIfFoldValid, checkIfRaiseValid gs betAmount]
    where
        clamp lowerX upperX x = max lowerX (min upperX x) :: Int

chooseActionBasedOnStrategy :: Player -> StateT GameState IO GameState
chooseActionBasedOnStrategy plr = do
    gs <- get
    case strategy plr of
        Random -> do
            newState <- testAllActions (shuffledActions gs (1, 1, 1)) gs
            put newState
            get
        Passive -> do
            newState <- testAllActions (shuffledActions gs (1, 1, 0)) gs
            put newState
            get
        Aggressive -> do
            newState <- testAllActions (shuffledActions gs (3, 1, 3)) gs
            put newState
            get
        Smart -> do
            actions <- lift $ evalStateT (smartPlayerStrategy plr) gs
            newState <- testAllActions actions gs
            put newState
            get
    where
        shuffledActions gs weighting = [action | (action, _) <- sortBy (\(_, n1) (_, n2) -> compare n1 n2) (zip (applyWeightingToActions gs weighting [])  (randoms (randomGen plr) :: [Int]))]

        applyWeightingToActions _ (0, 0, 0) as = as
        applyWeightingToActions gs (cw, fw, rw) as
            | cw > 0 = applyWeightingToActions gs (cw - 1, fw, rw) (checkIfCallValid gs : as)
            | fw > 0 = applyWeightingToActions gs (cw, fw - 1, rw) (checkIfFoldValid : as)
            | rw > 0 = applyWeightingToActions gs (cw, fw, rw - 1) (checkIfRaiseValid gs Nothing : as)

        -- do not need a base case as fold will always return true
        testAllActions (a:as) gs = let (valid, f) = a plr in
            if length (currentBets gs) > 1 then
                if valid then do
                    lift $ execStateT f gs
                else testAllActions as gs
            else do return gs

iterateEachPlayer :: [Player] -> StateT GameState IO GameState
iterateEachPlayer [] = do get
iterateEachPlayer (p:ps) = do
    gs <- get
    chooseActionBasedOnStrategy p
    iterateEachPlayer ps

bettingRound :: StateT GameState IO GameState
bettingRound = do
    gs <- get
    liftIO $ putStrLn ("roundtype " ++ show (roundType gs))
    if length (currentBets gs) > 1 then do
        case roundType gs of
            Preflop -> do
                newState <- lift $ execStateT (dealCardsAndPerformBets dealHoleCards) gs
                put newState
                bettingRound
            Flop -> do
                newState <- lift $ execStateT (dealCardsAndPerformBets (dealCommunityCards 3)) gs
                put newState
                bettingRound
            Turn -> do
                newState <- lift $ execStateT (dealCardsAndPerformBets (dealCommunityCards 1)) gs
                put newState
                bettingRound
            River -> do
                newState <- lift $ execStateT (dealCardsAndPerformBets (dealCommunityCards 1)) gs
                put newState
                bettingRound
            Showdown -> do
                winners <- lift $ evalStateT determineWinner gs
                newState <- lift $ execStateT (awardWinners winners) gs
                put newState
                get
        else do
            -- lift $ putStrLn "fix?"
            -- lift $ putStrLn (show (currentBets gs))
            put gs {roundType = Showdown}
            newState <- lift $ execStateT (awardWinners (map fst (currentBets gs))) gs
            put newState
            get
    where
        dealCardsAndPerformBets :: StateT GameState IO GameState -> StateT GameState IO GameState
        dealCardsAndPerformBets dealFunction = do
            gs <- get
            dealtCardState <- lift $ execStateT dealFunction gs
            put dealtCardState
                        
            let roundPlayers = map fst (currentBets dealtCardState)
            -- lift $ putStrLn (show roundPlayers)
            bettingRoundState <- lift $ execStateT (repeatUntilBetsEqual roundPlayers) dealtCardState
            put bettingRoundState

            put bettingRoundState {roundType = succ (roundType bettingRoundState)}
            get
            where
                repeatUntilBetsEqual :: [Player] -> StateT GameState IO ()
                repeatUntilBetsEqual [] = do return ()
                repeatUntilBetsEqual [p] = do return ()
                repeatUntilBetsEqual ps = do
                    gs <- get
                    newState <- lift $ execStateT (iterateEachPlayer ps) gs
                    put newState
                    -- get all of the players whose bets do not match the highest bet
                    repeatUntilBetsEqual (needToIterateAgain newState)

        awardWinners :: [Player] -> StateT GameState IO GameState
        awardWinners winners = do
            gs <- get

            -- lift $ putStrLn ("winners: " ++ (show winners))

            let playersLeft = map fst (currentBets gs)
            let splitPotAmount = (pot gs `div` fromIntegral (length winners))
            -- remove player bets and also add winnings if they won
            let addChipsToWinners = foldr (\p acc -> if checkIfPlayerInPlayerList p playersLeft then if checkIfPlayerInPlayerList p winners then p {chips = chips p + splitPotAmount - highestBet gs} : acc else  p {chips = chips p - highestBet gs} : acc  else p : acc) [] (activePlayers gs)

            -- lift $ putStrLn ("pot: " ++ (show (pot gs)))

            put gs {activePlayers = addChipsToWinners, pot = 0}
            get
            where
                checkIfPlayerInPlayerList _ [] = False
                checkIfPlayerInPlayerList p (w:ws) = if p == w then True else checkIfPlayerInPlayerList p ws

gameLoop :: Int -> StateT GameState IO GameState
gameLoop 100 = do get
gameLoop i = do
    gs <- get
    liftIO $ putStrLn ("round " ++ (show i))
    if length (activePlayers gs) == 1 then do get else do
        let dealer = activePlayers gs !! (dealerIndex gs)
        (deck, shuffledDeckState) <- lift $ runStateT (shuffleDeck dealer) gs
        finalState <- lift $ execStateT bettingRound shuffledDeckState
        newState <- lift $ execStateT updateGameState finalState
        -- lift $ putStrLn (show newState)
        put newState
        gameLoop (i+1)