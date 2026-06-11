module Loader where

import Types
import qualified Data.Map.Strict as Map
import Data.List  (nub)
import Data.Char  (isSpace)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Load a world from a file path.
loadWorldFromFile :: FilePath -> IO (Either GameError World)
loadWorldFromFile path = do
  contents <- readFile path
  return (parseWorld contents)

-- | Parse a world from a string (useful for testing).
parseWorld :: String -> Either GameError World
parseWorld contents = do
  (rooms, items) <- parseBlocks (parseBlocks' contents)
  validateWorld (World rooms items)

-- ---------------------------------------------------------------------------
-- File format
--
-- A world file is a sequence of blocks separated by blank lines.
-- Each block starts with a keyword on its own line:
--
--   room <id>
--     name "<display name>"
--     description "<text>"
--     exit <direction> <roomId>
--     exit <direction> <roomId> locked <keyItemId>
--     item <itemId>
--
--   item <id>
--     name "<display name>"
--     description "<text>"
--     location <roomId>          -- optional starting room
--
-- ---------------------------------------------------------------------------

data Block
  = RoomBlock  { bId :: String, bLines :: [(String, [String])] }
  | ItemBlock  { bId :: String, bLines :: [(String, [String])] }
  deriving (Show)

-- Split file into indented blocks
parseBlocks' :: String -> [[String]]
parseBlocks' = groupByBlank . filter (not . isComment) . lines
  where
    isComment l = case dropWhile isSpace l of
                    ('#':_) -> True
                    _       -> False

groupByBlank :: [String] -> [[String]]
groupByBlank [] = []
groupByBlank ls =
  let (block, rest) = break isBlankLine ls
      nonEmpty      = filter (not . isBlankLine) block
  in if null nonEmpty
       then groupByBlank (dropWhile isBlankLine rest)
       else nonEmpty : groupByBlank (dropWhile isBlankLine rest)

isBlankLine :: String -> Bool
isBlankLine = null . words

-- Parse each block into rooms/items
parseBlocks :: [[String]] -> Either GameError (Map.Map RoomId Room, Map.Map ItemId Item)
parseBlocks blocks = do
  parsed <- mapM parseBlock blocks
  let rooms = [ r | Right r <- map toRoom parsed ]
      items = [ i | Right i <- map toItem parsed ]
  return ( Map.fromList [(roomId r, r) | r <- rooms]
         , Map.fromList [(itemId i, i) | i <- items] )
  where
    toRoom (RoomBlock bid bls) = Right (buildRoom bid bls)
    toRoom _                    = Left  ()
    toItem (ItemBlock bid bls)  = Right (buildItem bid bls)
    toItem _                    = Left  ()

parseBlock :: [String] -> Either GameError Block
parseBlock [] = Left (WorldError "Empty block")
parseBlock (header:rest) =
  case words header of
    ["room", bid] -> Right (RoomBlock bid (parseProps rest))
    ["item", bid] -> Right (ItemBlock bid (parseProps rest))
    _             -> Left  (WorldError $ "Unknown block header: " ++ header)

parseProps :: [String] -> [(String, [String])]
parseProps = map (\l -> let (k:vs) = words l in (k, vs))

-- ---------------------------------------------------------------------------
-- Build typed values from parsed properties
-- ---------------------------------------------------------------------------

buildRoom :: RoomId -> [(String, [String])] -> Room
buildRoom rid props = Room
  { roomId          = rid
  , roomName        = lookupQuoted "name" props rid
  , roomDescription = lookupQuoted "description" props "(no description)"
  , roomExits       = Map.fromList (buildExits props)
  , roomItems       = [v | ("item", [v]) <- props]
  }

buildExits :: [(String, [String])] -> [(ExitDir, Exit)]
buildExits props =
  [ (dir, Exit target lock)
  | ("exit", dir:target:rest) <- props
  , let lock = case rest of
                 ("locked":keyId:_) -> Just keyId
                 _                  -> Nothing
  ]

buildItem :: ItemId -> [(String, [String])] -> Item
buildItem iid props = Item
  { itemId          = iid
  , itemName        = lookupQuoted "name" props iid
  , itemDescription = lookupQuoted "description" props "(no description)"
  }

lookupQuoted :: String -> [(String, [String])] -> String -> String
lookupQuoted key props def =
  case lookup key props of
    Nothing -> def
    Just vs -> unquote (unwords vs)

unquote :: String -> String
unquote s
  | length s >= 2 && head s == '"' && last s == '"' = init (tail s)
  | otherwise = s

-- ---------------------------------------------------------------------------
-- Validation
-- ---------------------------------------------------------------------------

validateWorld :: World -> Either GameError World
validateWorld world = do
  checkNoDuplicateRooms world
  checkExitTargetsExist world
  checkItemsExist       world
  Right world

checkNoDuplicateRooms :: World -> Either GameError ()
checkNoDuplicateRooms world =
  let rids = Map.keys (worldRooms world)
  in if length rids == length (nub rids)
       then Right ()
       else Left (WorldError "Duplicate room IDs detected")

checkExitTargetsExist :: World -> Either GameError ()
checkExitTargetsExist world =
  let rids   = Map.keys (worldRooms world)
      targets = [ exitTarget e
                | room <- Map.elems (worldRooms world)
                , e    <- Map.elems (roomExits room) ]
      bad = filter (`notElem` rids) targets
  in case bad of
       [] -> Right ()
       (t:_) -> Left (WorldError $ "Exit references unknown room: " ++ t)

checkItemsExist :: World -> Either GameError ()
checkItemsExist world =
  let iids    = Map.keys (worldItems world)
      roomRefs = [ iid
                 | room <- Map.elems (worldRooms world)
                 , iid  <- roomItems room ]
      bad = filter (`notElem` iids) roomRefs
  in case bad of
       [] -> Right ()
       (i:_) -> Left (WorldError $ "Room references unknown item: " ++ i)