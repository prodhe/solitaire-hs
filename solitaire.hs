{-
    The Solitaire Cipher

    written by Prodhe
-}


-- | Import
import Data.Char (toUpper, isAlpha)
import System.Environment


-- | Data and types 
data Suit = Clubs | Diamonds | Hearts | Spades | Joker deriving (Show, Eq)
data Card = Card Suit Int deriving (Show, Eq)

type Deck = [Card]


-- | Default unscrambled KEY deck
unkeyedDeck :: Deck
unkeyedDeck = [ Card a b | a <- [Clubs,Diamonds,Hearts,Spades,Joker], b <- [1..13],
                         if a == Joker && b > 2
                           then False
                           else True ]


-- |
-- | MAIN
-- |

-- | Get cipher, key and print the processed result
main :: IO ()
main = do
    [action, msgFile, keyFile] <- getArgs
    message <- getMessage msgFile
    key <- getKey keyFile
    case action of
        "encrypt" -> putStrLn $ encrypt message key
        "decrypt" -> putStrLn $ decrypt message key
        _         -> putStrLn "Unrecognized action."


-- |
-- | HANDLE INPUT
-- |

-- | Get the string MESSAGE (CIPHER/PLAIN)
getMessage :: String -> IO String
getMessage fileName = do
    fileCont <- readFile fileName 
    return $ convertString fileCont

-- | Get a string and turn into a valid Deck (our initial KEY)
-- Must be in the format [C|D|H|S|J][0-9]{2}
getKey :: String -> IO Deck
getKey fileName = do
    fileCont <- readFile fileName
    return $ strToDeck $ lines fileCont


-- |
-- | ENCRYPT AND DECRYPT
-- |

-- | Decrypt the CIPHER string using KEY deck
decrypt :: String -> Deck -> String
decrypt cipher key = groupLetters $ map convertNumberToChar (zipWith sub keystreamToNumbers cipherToNumbers)
                       where
                         cleanCipher        = convertString cipher
                         keystream          = generateKeyStream (length cleanCipher) key
                         cipherToNumbers    = map convertCharToNumber cleanCipher
                         keystreamToNumbers = map convertCharToNumber keystream
                         sub k c            = if c <= k
                                                then c + 26 - k
                                                else c - k

-- | Encrypt the PLAIN string using KEY deck
encrypt :: String -> Deck -> String
encrypt plain key = groupLetters $ map convertNumberToChar (zipWith add keystreamToNumbers plainToNumbers) 
                      where
                        cleanPlain = convertString $ groupLetters $ convertString plain
                        keystream  = generateKeyStream (length cleanPlain) key
                        plainToNumbers = map convertCharToNumber cleanPlain
                        keystreamToNumbers = map convertCharToNumber keystream
                        add k c            = if c + k > 26
                                               then c + k - 26
                                               else c + k


-- |
-- | ALGORITHM
-- |

-- | Generate a stream of letters based on KEY deck
-- In case of a Joker, disregard it and run another loop
generateKeyStream :: Int -> Deck -> String
generateKeyStream 0 _ = ""
generateKeyStream n d = if letter == ' '
                          then rest'
                          else letter : rest
                            where
                              letter = findLetter $ shuffleDeck d
                              rest   = generateKeyStream (n-1) $ shuffleDeck d
                              rest'  = generateKeyStream (n) $ shuffleDeck d

-- | Get output letter from a shuffled deck
findLetter :: Deck -> Char
findLetter d = cardToChar $ head $ drop (cardToValue $ head d) d

-- | Shuffle the deck for each new letter
shuffleDeck :: Deck -> Deck
shuffleDeck de = countCut $ tripleCut $ moveCard (Card Joker 2) 2 $ moveCard (Card Joker 1) 1 de
                  where
                    countCut          d = (init $ drop (bottomCardValue d) d) ++ topCardsTaken d ++ ((last d) : [])
                    topCardsTaken     d = take (bottomCardValue d) d
                    bottomCardValue   d = cardToValue $ last d
                    tripleCut         d = postLastJoker d ++ middleCards d ++ preFirstJoker d
                    preFirstJoker     d = takeWhile notJoker d
                    middleCards       d = reverse $ dropWhile notJoker $ reverse $ dropWhile notJoker d
                    postLastJoker     d = reverse $ takeWhile notJoker $ reverse d
                    notJoker (Card s _) = (/=) s Joker

-- | Move one specified card n steps forward and cycles to the beginning if at the end
moveCard :: Card -> Int -> Deck -> Deck
moveCard _ 0 d  = d
moveCard c n d  = if isLastCard
                    then moveCard c (n-1) $ (head d) : c : (drop 1 $ init d)
                    else moveCard c (n-1) $ before ++ ((head after) : c : []) ++ (tail after)
                    where
                      isLastCard = null $ drop 1 $ dropWhile (/=c) d
                      before     = takeWhile (/= c) d
                      after      = drop 1 $ dropWhile (/= c) d


-- |
-- | CONVERSIONS
-- |

-- | Turn three characters into a valid card and vice versa
stringToCard :: String -> Card
stringToCard (c:v) = case c of
                      'C' -> Card Clubs validValue
                      'D' -> Card Diamonds validValue
                      'H' -> Card Hearts validValue
                      'S' -> Card Spades validValue
                      'J' -> Card Joker validValue
                      _   -> undefined
                      where
                          validValue = if value > 0 && value < 14
                                       then value
                                       else 0
                                       where
                                          value = read v :: Int

cardToString :: Card -> String
cardToString (Card s v) = case s of
                           Clubs    -> "C" ++ show v
                           Diamonds -> "D" ++ show v
                           Hearts   -> "H" ++ show v
                           Spades   -> "S" ++ show v
                           Joker    -> "J" ++ show v

-- | Get the card value
cardToValue :: Card -> Int
cardToValue (Card s v) = case (s) of
                            Clubs    -> v
                            Diamonds -> v + 13
                            Hearts   -> v + 26
                            Spades   -> v + 39
                            Joker    -> 53

-- | Get the letter represented by the card
cardToChar :: Card -> Char
cardToChar (Card Joker _) = ' '
cardToChar c@(Card s _) = ['A'..'Z'] !! ((cardToValue c) - offset)
                               where offset = if s==Hearts || s==Spades then 27 else 1

-- | Convert string to no whitespace and all UPPER
convertString :: String -> String
convertString []     = []
convertString (x:xs) = if x' `elem` validChars
                         then x' : convertString xs
                         else convertString xs
                       where
                         x'         = toUpper x
                         validChars = ['A'..'Z']

-- | Put letters in groups of five
groupLetters :: String -> String
groupLetters []  = []
groupLetters str = padding ++ groupLetters (snd split)
                     where
                       split       = splitAt 5 str
                       padding     = if group < 5 && group > 0
                                       then (fst split) ++ (take (5 - group) $ cycle ['X'])
                                       else fst split ++ " "
                       group       = length $ fst split 

-- | Numbers from letters and vice versa
convertCharToNumber :: Char -> Int
convertCharToNumber c = snd $ head pair
                          where
                            pair  = filter (\x -> fst x == c) pairs
                            pairs = zip ['A'..'Z'] [1..26]

convertNumberToChar :: Int -> Char
convertNumberToChar n = snd $ head pair
                          where
                            pair  = filter (\x -> fst x == n) pairs
                            pairs = zip [1..26] ['A'..'Z']


-- | String to Deck and vice versa
strToDeck :: [String] -> Deck
strToDeck [] = []
strToDeck (line:rest)  = stringToCard (line) : strToDeck (rest)

deckToString :: Deck -> [String]
deckToString [] = []
deckToString (card:rest) = cardToString (card) : deckToString (rest)
