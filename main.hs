{-|
    Program     : Texas Hold'em
    Description : An implementation of Texas Hold'em No Limits in Haskell.

    This program simulates Texas Hold'em, which involves:
    - Creating the players (each player can have a unique strategy)
    - Dealing hole and community cards
    - Evaluating the players hand and determining the winner from a list of players
    - Allowing for actions to be chosen (call, fold, raise)
    - Keeping track of game state via IO output
    - Allowing the user to play as a human player

    The game follows the standard rules of Texas Hold'em poker:
    - 52 cards in a deck, 4 suits and 13 values
    - Each player can have 2 hole cards and there are 5 community cards
    - The hand can be made up of hole and community cards
    - Actions including call, raise and fold

    In order to run the program, you must set mtl:
    :set -package mtl
    due to using the state monad.

    Run this from the command line via ghci by loading the file and then
    calling the main function.
-}
import System.Random
import Control.Monad.State
import Data.List
import Data.Foldable (Foldable(maximum), maximumBy)
import Data.Char
import Prelude

data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Show, Eq, Ord)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord, Enum)

data Card = Card {suit :: Suit, value :: Value}

-- only the value matters - the suit does not play a part in ranking
instance Ord Card where
    compare (Card _ value1) (Card _ value2) = compare value1 value2

instance Eq Card where
    (==) (Card _ value1) (Card _ value2) = value1 == value2

instance Show Card where
    show (Card suit value) = show value ++ " of " ++ show suit

newtype Deck = Deck [Card] deriving (Show)    

-- all possible hand ranks from a 5 card combination
data HandRank = HighCard | Pair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush deriving (Show, Eq, Ord)

data PlayerStrategy = Random | Passive | Aggressive | Smart | Human deriving Show

data Player = Player {name :: String, hand :: [Card], chips :: Int, dealer :: Bool, strategy :: PlayerStrategy}

-- players are considered equal if their names are equal
instance Eq Player where
    (==) (Player name1 _ _ _ _ ) (Player name2 _ _ _ _ ) = name1 == name2

instance Show Player where
    show (Player name hand chips dealer strategy) = "{name: "++name++", chips: " ++show chips++", dealer: "++show dealer++", strat: "++show strategy++"}"

data RoundType = Preflop | Flop | Turn | River | Showdown deriving (Show, Enum, Eq)

data GameState = GameState {activePlayers :: [Player], deck :: Deck, pot :: Int, currentBets :: [(Player, Int)], dealerIndex :: Int, smallBlindIndex :: Int, bigBlindIndex :: Int, communityCards :: [Card], highestBet :: Int, minimumRaise :: Int, roundType :: RoundType,  randomGen :: StdGen} deriving Show

{-|
    Function    : main
    Description : entry point for the program
-}
main :: IO ()
main = do
    -- create a random number to be used for randomness throughout the program
    initialSeed <- randomIO

    -- create the players
    putStr ("How many players would you like to create? ")
    playerCountInp <- getLine
    putStrLn ("Creating " ++ playerCountInp ++ " players:")
    let plrCount = read playerCountInp :: Int
    players <- createPlayers plrCount

    let initialState = createInitialGameState players initialSeed

    dealerIndex <- randomRIO (0, 5)
    (result, dealerState) <- runStateT (assignDealer dealerIndex) initialState

    (deck, shuffledDeckState) <- runStateT shuffleDeck dealerState

    (result, finalState) <- runStateT (gameLoop 0) dealerState

    print (pot finalState)
    print (activePlayers finalState)

{-|
    Function    : createInitialGameState
    Description : Creates an empty game state with all default values

    Create a default game state.
    Requires the initial players to be passed as an argument.
-}
createInitialGameState :: [Player] -> Int -> GameState
createInitialGameState players initialSeed = let
        in
        GameState players (Deck []) 0 (map (\p -> (p,0)) players) 0 1 2 [] 0 1 Preflop (mkStdGen initialSeed)

{-|
    Function    : updateGameState
    Description : Updates the game state

    Resets all data to the default values apart from the player list which
    is kept but players are removed if they have no chips left.
-}
updateGameState :: StateT GameState IO ()
updateGameState = do
    gs <- get
    let updatedPlayers = map (\p -> p {hand = []}) (filter (\p -> chips p > 0) (activePlayers gs))
    put gs {activePlayers = updatedPlayers, deck = Deck[], pot = 0, currentBets = (map (\p -> (p,0)) updatedPlayers), dealerIndex = incrementIndex (dealerIndex gs) updatedPlayers, communityCards = [], highestBet = 0, minimumRaise = 1, roundType = Preflop}
    where
        incrementIndex n ps = (n + 1) `mod` (length ps)

{-|
    Function    : shuffleDeck
    Description : Creates a new shuffled deck of cards

    Updates the GameState deck to be an updated shuffled deck of cards.
-}
shuffleDeck :: StateT GameState IO Deck
shuffleDeck = do
    gs <- get
    lift $ putStrLn "hi"
    let cmp (_, n1) (_, n2) = compare n1 n2
    let (Deck cards) = Deck [Card suit value | suit <- [Spades, Hearts, Diamonds, Clubs], value <- [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]]
    let (gen, newGen) = split (randomGen gs)
    let updatedDeck = Deck [ card | (card, _) <- sortBy cmp (zip cards (randoms gen :: [Int]))]
    put gs {deck = updatedDeck, randomGen = newGen}
    return updatedDeck    

{-|
    Function    : createPlayers
    Description : Creates a list of players based on user input.

    Creates a list of players where the strategy is determined by user input.
    If the user input is not valid, it will automatically create a random
    strategy player instead.
    Requires the number of players to be created as an argument.
-}
createPlayers :: Int -> IO [Player]
createPlayers count = do
    putStrLn "Player options include: Random, Agressive, Passive, Smart, Human"
    newPlayers 1
    where
        newPlayers :: Int -> IO [Player]
        -- repeat until all players have been created
        newPlayers n = if n - 1 == count then return [] else do
            putStrLn ("What type of player would you like to create? ("++show n++")")
            inpStr <- getLine
            let plr = Player ("Player " ++ show n) [] 100 False (getType inpStr)
            rest <- newPlayers (n + 1)
            return (plr : rest)
            where
                getType :: String -> PlayerStrategy
                getType inp = case map toLower inp of
                    "random" -> Random
                    "aggressive" -> Aggressive
                    "passive" -> Passive
                    "smart" -> Smart
                    "human" -> Human
                    _ -> Random

{-|
    Function    : assignDealer
    Description : Assigns the dealer based on the given index
-}
assignDealer :: Int -> StateT GameState IO GameState
assignDealer index = do
    gs <- get
    let players = map (\(i, p) -> p {dealer = (i == index)}) (zip [0..] (activePlayers gs))
    put gs {dealerIndex = index, activePlayers = players, currentBets = map (\p -> (p, 0)) players}
    get

{-|
    Function    : dealHoleCards
    Description : Deals hole cards to all players

    Deals hole cards to all players stored in activePlayers within GameState as
    well as removing these cards from the deck.
-}
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

{-|
    Function    : dealCommunityCards
    Description : Deals n community cards

    Deals the community cards and removes the dealt cards from the deck.
    Requires the number of community cards as argument.
-}
dealCommunityCards :: Int -> StateT GameState IO GameState
dealCommunityCards n = do
    gs <- get
    let cards (Deck deck) = deck
    put gs {communityCards = communityCards gs ++ take n (cards (deck gs)), deck = Deck (drop n (cards (deck gs)))}
    get

{-|
    Function    : evaluateHand
    Description : Evaluates a given hand, assigning it the appropriate ranking

    Takes a hand and determines what rank the hand currently is.
-}
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

{-|
    Function    : determineWinner
    Description : Determines the winner based on the remaining players inside
    of currentBets

    For each player, a subsequence of all possible combinations of their hand
    and the available community cards are calculated and then passed to the
    findBestHand function to find each persons best hand.
    Once all players best hands are found, they are then compared with each
    other, also using the findBestHand function.
-}
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
        return (map fst (filter (\(_, h) -> inList h winningHands) bestPlayerHands))
        where
            inList _ [] = False
            inList toFind (x:xs) = if x == toFind then True else inList toFind xs

{-|
    Function    : findBestHand
    Description : Finds the best hand given a list of several hands

    Used to find the best hand that the player could possibly have and also used
    to compare the best player hands with eachother to determine a winner.
-}
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
            -- Order the cards
            highestCard [] [] = EQ
            highestCard (x:xs) (y:ys)
                | x > y = GT
                | y > x = LT
                | x == y = highestCard xs ys

{-|
    Function    : playerBet
    Description : Generalised function used when a player's chips have been used

    General function used both by call and fold in order to remove the chips
    from the player and add them to the pot. Also updates the highestBet value
    if the bet exceeds it.
-}
playerBet :: Player -> Int -> StateT GameState IO ()
playerBet plr amount = do
    gs <- get
    let updatedPlayer = plr {chips = chips plr - amount}
    let updatedRoundBets = map (\(p, n) -> if p == plr then (updatedPlayer, n+amount) else (p, n)) (currentBets gs)
    put gs {highestBet = if amount > highestBet gs then amount else highestBet gs, pot = pot gs + amount, currentBets = updatedRoundBets}

{-|
    Function    : call
    Description : Calls the bet amount
-}
call :: Player -> StateT GameState IO ()
call plr = do
    gs <- get
    lift $ putStrLn ((show (name plr)) ++ " called")
    -- calculate the amount that the player needs to bet to match the highestBet
    let amountNeededToBet = highestBet gs - snd (head (filter (\(p,n) -> p == plr) (currentBets gs)))
    newState <- lift $ execStateT (playerBet plr amountNeededToBet) gs
    put newState

{-|
    Function    : fold
    Description : Folds out of the round
-}
fold :: Player -> StateT GameState IO ()
fold plr = do
    gs <- get
    lift $ putStrLn ((show (name plr)) ++ " folded")
    -- remove any data related to that player for the round
    let playerBet = snd (head (filter (\(p, _) -> p == plr) (currentBets gs)))
    -- update their chips in the activePlayers list of players (not round based)
    let updatedActivePlayers = map (\p -> if p == plr then p {chips = (chips p) - playerBet} else p) (activePlayers gs)
    let updatedPlayerBets = filter (\(p, _) -> p /= plr) (currentBets gs)
    put gs {currentBets = updatedPlayerBets, activePlayers = updatedActivePlayers}

{-|
    Function    : raise
    Description : Raises by an amount given
-}
raise :: Int -> Player -> StateT GameState IO ()
raise amount plr = do
    gs <- get
    lift $ putStrLn ((show (name plr)) ++ " raised by " ++ (show amount))
    newState <- lift $ execStateT (playerBet plr amount) gs
    put newState {minimumRaise = amount}

{-|
    Function    : checkIfCallValid
    Description : Returns whether or not it is valid to call

    Checks if the player has enough chips in order to call.
-}
checkIfCallValid :: GameState -> Player -> (Bool, StateT GameState IO ())
checkIfCallValid gs plr = let
    amountNeededToBet = highestBet gs - snd (head (filter (\(p, _) -> p == plr) (currentBets gs)))
    in (chips plr >= amountNeededToBet, call plr)

{-|
    Function    : checkIfFoldValid
    Description : Returns whether it is valid to fold

    Always is valid to fold, and so is used as a base case when recursing the
    actions.
-}
checkIfFoldValid :: Player -> (Bool, StateT GameState IO ())
checkIfFoldValid plr = (True, fold plr)

{-|
    Function    : checkIfRaiseValid
    Description : Returns whether it is valid to raise

    If the player strategy is random, Nothing is passed and so a random amount
    is generated to be used for the raise. Otherwise the amount must be given.
    Validation is to ensure that the player has enough chips to match the
    minimumRaise.
-}
checkIfRaiseValid :: GameState -> Maybe Int -> Player -> (Bool, StateT GameState IO ())
checkIfRaiseValid gs (Just raiseAmount) plr = (raiseAmount <= chips plr && raiseAmount >= minimumRaise gs, raise raiseAmount plr)
checkIfRaiseValid gs Nothing plr = (chips plr >= minimumRaise gs, raise raiseAmount plr)
    where
        (raiseAmount, newGen) = randomR (minimumRaise gs, chips plr) (randomGen gs)

{-|
    Function    : needToIterateAgain
    Description : Returns a list of players whose bets do not match the highest
-}
needToIterateAgain :: GameState -> [Player]
needToIterateAgain gs = map fst (filter (\(p, b) -> b /= highestBet gs) (currentBets gs))

{-|
    Function    : smartPlayerStrategy
    Description : Returns a list action that the smart player has decided to do

    Different actions are chosen based on confidence of the player - as well as
    the bet amount.
-}
smartPlayerStrategy :: Player -> StateT GameState IO [Player -> (Bool, StateT GameState IO ())]
smartPlayerStrategy plr = do
    gs <- get
    if roundType gs == Preflop then do
        let bestHand = head (findBestHand [hand plr])
        let handRank = evaluateHand bestHand
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
        do case () of
            _ | highPremiumPair || highConnector -> return [checkIfRaiseValid gs betAmount, checkIfCallValid gs, checkIfFoldValid]
            _ | suitedConnector || mediumPairs -> return [checkIfCallValid gs, checkIfRaiseValid gs betAmount, checkIfFoldValid]
            _ -> return [checkIfCallValid gs, checkIfFoldValid, checkIfRaiseValid gs betAmount]
    else do
        let bestHand = head (findBestHand (filter (\h -> length h == 5) (subsequences (hand plr ++ communityCards gs))))
        let handRank = evaluateHand bestHand
        let allPossibleCards = hand plr ++ communityCards gs
        -- within 1 card
        let getLengthOfLargestGroup f cs = length (maximumBy (\ls1 ls2 -> compare (length ls1) (length ls2)) (groupBy f cs))
        let closeToFlush =  getLengthOfLargestGroup (\c1 c2 -> suit c1 == suit c2) allPossibleCards > 3
        let closeToStraight = getLengthOfLargestGroup (\c1 c2 -> abs (fromEnum (value c1) - fromEnum (value c2)) == 1) (sort allPossibleCards) > 3

        let betMultiplier = foldr (\(b, x) acc -> if b then x + acc else acc) 0.0 [(closeToFlush, 0.7 :: Double), (closeToStraight, 0.6 :: Double)]
        let betAmount = Just (clamp 0 (chips plr) (floor (fromIntegral (chips plr) * betMultiplier)))
        if handRank > Pair || closeToFlush || closeToStraight then do
            return [checkIfRaiseValid gs betAmount, checkIfCallValid gs, checkIfFoldValid]
        else do
            return [checkIfCallValid gs, checkIfFoldValid, checkIfRaiseValid gs betAmount]
    where
        clamp lowerX upperX x = max lowerX (min upperX x) :: Int

{-|
    Function    : humanPlayerStrategy
    Description : Takes input from a human player to determine an action

    Directly checks whether or not the action is valid before continuing,
    forcing the user to enter a valid action.
-}
humanPlayerStrategy :: Player -> StateT GameState IO [StateT GameState IO ()]
humanPlayerStrategy plr = do
    lift $ putStrLn (show (name plr)++"'s turn: ")
    gs <- get
    action <- checkIfValid
    if fst action then
        return [snd action]
    else do
        lift $ putStrLn ("Invalid action, try another one.")
        humanPlayerStrategy plr
    where
        -- returns the validity of the action as well as the function
        checkIfValid :: StateT GameState IO (Bool, StateT GameState IO ())
        checkIfValid = do
            gs <- get
            inpStr <- lift $ getLine
            case map toLower inpStr of
                "call" -> return (checkIfCallValid gs plr)
                "raise" -> do
                    lift $ putStrLn ("Player has "++(show(chips plr))++" and the minimum raise is "++(show(minimumRaise gs))++". Choose an amount to raise to: ")
                    inpStr <- lift $ getLine
                    let amount = (read inpStr :: Int)
                    return (checkIfRaiseValid gs (Just amount) plr)
                "fold" -> return (checkIfFoldValid plr)
                _ -> do
                    lift $ putStrLn ("Unknown action - only call, raise or fold are accepted commands.")
                    checkIfValid

{-|
    Function    : chooseActionBasedOnStrategy
    Description : Chooses an action based on the players' strategy

    This function also executes the first found valid action returned from the
    respective strategy functions.
-}
chooseActionBasedOnStrategy :: Player -> StateT GameState IO GameState
chooseActionBasedOnStrategy plr = do
    gs <- get
    let roundPlayers = map fst (currentBets gs)
    case strategy plr of
        Random -> do
            (shuffledActions, shuffledState) <- lift $ runStateT (shuffleActions (1,1,1)) gs
            newState <- testAllActions shuffledActions shuffledState
            put newState
            get
        Passive -> do
            (shuffledActions, shuffledState) <- lift $ runStateT (shuffleActions (1,1,0)) gs
            newState <- testAllActions shuffledActions shuffledState
            put newState
            get
        Aggressive -> do
            (shuffledActions, shuffledState) <- lift $ runStateT (shuffleActions (3,1,3)) gs
            newState <- testAllActions shuffledActions shuffledState
            put newState
            get
        Smart -> do
            actions <- lift $ evalStateT (smartPlayerStrategy plr) gs
            newState <- testAllActions actions gs
            put newState
            get
        Human -> do
            action <- lift $ evalStateT (humanPlayerStrategy plr) gs
            newState <- lift $ execStateT (head action) gs
            put newState
            get
    where
        shuffleActions weighting = do
            gs <- get
            -- split one generator into two so that we can update the generator
            let (gen, newGen) = split (randomGen gs)
            put gs {randomGen = newGen}
            return [action | (action, _) <- sortBy (\(_, n1) (_, n2) -> compare n1 n2) (zip (applyWeightingToActions gs weighting [])  (randoms gen :: [Int]))]

        -- create a list of repeated actions in order to weight them
        applyWeightingToActions _ (0, 0, 0) as = as
        applyWeightingToActions gs (cw, fw, rw) as
            | cw > 0 = applyWeightingToActions gs (cw - 1, fw, rw) (checkIfCallValid gs : as)
            | fw > 0 = applyWeightingToActions gs (cw, fw - 1, rw) (checkIfFoldValid : as)
            | rw > 0 = applyWeightingToActions gs (cw, fw, rw - 1) (checkIfRaiseValid gs Nothing : as)

        -- recursively checks if the action is valid
        -- note: there is no explicit base case as fold is always valid, 
        -- so is the base case
        testAllActions (a:as) gs = let (valid, f) = a plr in
            if length (currentBets gs) > 1 then
                if valid then do
                    lift $ execStateT f gs
                else testAllActions as gs
            else do return gs

{-|
    Function    : iterateEachPlayer
    Description : Iterates through a list of players to determine their action
    to take for that round
-}
iterateEachPlayer :: [Player] -> StateT GameState IO GameState
iterateEachPlayer [] = do get
iterateEachPlayer (p:ps) = do
    gs <- get
    let roundPlayers = map fst (currentBets gs)
    if length (currentBets gs) == 1 then do
        get
    else do
        newState <- lift $ execStateT (chooseActionBasedOnStrategy p) gs
        put newState
        iterateEachPlayer ps

{-|
    Function    : repeatUntilBetsEqual
    Description : Repeats until all bets are equal - forcing them to take an
    action if not equal.
-}
repeatUntilBetsEqual :: [Player] -> StateT GameState IO ()
repeatUntilBetsEqual [] = do return ()
repeatUntilBetsEqual [p] = do return ()
repeatUntilBetsEqual ps = do
    gs <- get
    let roundPlayers = map fst (currentBets gs)
    lift $ putStrLn (show roundPlayers)
    newState <- lift $ execStateT (iterateEachPlayer ps) gs
    put newState
    -- get all of the players whose bets do not match the highest bet
    repeatUntilBetsEqual (needToIterateAgain newState)

{-|
    Function    : awardWinners
    Description : Awards the winners with the appropriate split pot amount

    Also removes the bets that those players made at the same time.
-}
awardWinners :: [Player] -> StateT GameState IO GameState
awardWinners winners = do
    gs <- get
    let playersLeft = map fst (currentBets gs)
    let splitPotAmount = (pot gs `div` fromIntegral (length winners))
    
    -- remove player bets and also add winnings if they won
    let addChipsToWinners = foldr (\p acc -> if checkIfPlayerInPlayerList p playersLeft then if checkIfPlayerInPlayerList p winners then p {chips = chips p + splitPotAmount - highestBet gs} : acc else  p {chips = chips p - highestBet gs} : acc  else p : acc) [] (activePlayers gs)

    -- update the state
    put gs {activePlayers = addChipsToWinners, pot = 0}
    get
    where
        checkIfPlayerInPlayerList _ [] = False
        checkIfPlayerInPlayerList p (w:ws) = if p == w then True else checkIfPlayerInPlayerList p ws

{-|
    Function    : bettingRound
    Description : Handles a betting round (preflop, flop, turn, river)

    Handles each sub-round appropriately and also has an extra round type of
    Showdown for if there is more than 1 player after the end of the River.
-}
bettingRound :: StateT GameState IO GameState
bettingRound = do
    gs <- get
    lift $ putStrLn ("roundtype " ++ show (roundType gs))
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
            -- if there is only one player left then they win by default
            put gs {roundType = Showdown}
            newState <- lift $ execStateT (awardWinners (map fst (currentBets gs))) gs
            put newState
            get
    where
        -- Combines two functions and handles the updated states
        dealCardsAndPerformBets :: StateT GameState IO GameState -> StateT GameState IO GameState
        dealCardsAndPerformBets dealFunction = do
            gs <- get
            dealtCardState <- lift $ execStateT dealFunction gs
            put dealtCardState
                        
            let roundPlayers = map fst (currentBets dealtCardState)
            bettingRoundState <- lift $ execStateT (repeatUntilBetsEqual roundPlayers) dealtCardState
            put bettingRoundState

            put bettingRoundState {roundType = succ (roundType bettingRoundState)}
            get

{-|
    Function    : gameLoop
    Description : Handles the game running state and repeating several rounds

    Will repeat until either 100 rounds have passed or there is only one player
    left with all the chips.
-}
gameLoop :: Int -> StateT GameState IO GameState
gameLoop 100 = do get
gameLoop i = do
    gs <- get
    lift $ putStrLn ("round " ++ (show i))
    if length (activePlayers gs) == 1 then do get else do
        (deck, shuffledDeckState) <- lift $ runStateT shuffleDeck gs
        finalState <- lift $ execStateT bettingRound shuffledDeckState
        newState <- lift $ execStateT updateGameState finalState
        put newState
        gameLoop (i+1)