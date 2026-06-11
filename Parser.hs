module Parser where

import Data.Char (toLower, isSpace)
import Data.List (isPrefixOf)

-- ---------------------------------------------------------------------------
-- Command ADT
-- ---------------------------------------------------------------------------

data Command
  = Look
  | Go      ExitDir
  | Take    String    -- item name (may be multi-word)
  | Drop    String
  | Use     String (Maybe String)   -- item, optional target ("use key on door")
  | Examine String
  | Inventory
  | Quit
  deriving (Show, Eq)

type ExitDir = String

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

-- | Parse a raw input string into a Command, or return an error message.
parseCommand :: String -> Either String Command
parseCommand raw =
  case words (map toLower (trim raw)) of
    []                         -> Left "Please enter a command."
    ("look"   : _)             -> Right Look
    ("l"      : _)             -> Right Look
    ("inventory" : _)          -> Right Inventory
    ("i"      : _)             -> Right Inventory
    ("quit"   : _)             -> Right Quit
    ("q"      : _)             -> Right Quit
    ("go"     : dir : _)       -> Right (Go (normaliseDir dir))
    ("go"     : [])            -> Left "Go where? (e.g. 'go north')"
    -- bare directions
    (dir : _) | isDirection dir -> Right (Go (normaliseDir dir))
    ("take"   : rest)          -> parseSingleItem "take" Take rest
    ("get"    : rest)          -> parseSingleItem "take" Take rest
    ("drop"   : rest)          -> parseSingleItem "drop" Drop rest
    ("examine": rest)          -> parseSingleItem "examine" Examine rest
    ("x"      : rest)          -> parseSingleItem "examine" Examine rest
    ("use"    : rest)          -> parseUse rest
    (w        : _)             -> Left ("Unknown command: '" ++ w ++ "'. Try 'look', 'go <dir>', 'take <item>', etc.")

-- Strip articles ("the", "a", "an") from the front of a word list
stripArticles :: [String] -> [String]
stripArticles ("the" : ws) = ws
stripArticles ("a"   : ws) = ws
stripArticles ("an"  : ws) = ws
stripArticles ws           = ws

parseSingleItem :: String -> (String -> Command) -> [String] -> Either String Command
parseSingleItem verb ctor ws =
  case stripArticles ws of
    [] -> Left ("What do you want to " ++ verb ++ "?")
    ws' -> Right (ctor (unwords ws'))

-- "use key on door"  or just "use key"
parseUse :: [String] -> Either String Command
parseUse ws =
  case break (== "on") (stripArticles ws) of
    ([], _)       -> Left "Use what?"
    (item, [])    -> Right (Use (unwords item) Nothing)
    (item, _:tgt) -> Right (Use (unwords item) (Just (unwords (stripArticles tgt))))

-- ---------------------------------------------------------------------------
-- Direction normalisation
-- ---------------------------------------------------------------------------

normaliseDir :: String -> ExitDir
normaliseDir "n"         = "north"
normaliseDir "s"         = "south"
normaliseDir "e"         = "east"
normaliseDir "w"         = "west"
normaliseDir "u"         = "up"
normaliseDir "d"         = "down"
normaliseDir "ne"        = "northeast"
normaliseDir "nw"        = "northwest"
normaliseDir "se"        = "southeast"
normaliseDir "sw"        = "southwest"
normaliseDir d           = d

isDirection :: String -> Bool
isDirection d = d `elem`
  [ "north","south","east","west","up","down"
  , "northeast","northwest","southeast","southwest"
  , "n","s","e","w","u","d","ne","nw","se","sw" ]

-- ---------------------------------------------------------------------------
-- Utility
-- ---------------------------------------------------------------------------

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
