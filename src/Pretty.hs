module Pretty where

import Types
import qualified Data.Map.Strict as Map

-- ---------------------------------------------------------------------------
-- Room description
-- ---------------------------------------------------------------------------

-- | Render a full room description for the player.
describeRoom :: Room -> World -> Player -> String
describeRoom room world player =
  unlines $ filter (not . null)
    [ "=== " ++ roomName room ++ " ==="
    , roomDescription room
    , describeExits room
    , describeItems room world
    ]

describeExits :: Room -> String
describeExits room =
  let exits = Map.toList (roomExits room)
  in if null exits
       then "There are no obvious exits."
       else "Exits: " ++ commaList (map formatExit exits)
  where
    formatExit (dir, exit) =
      dir ++ case exitLocked exit of
               Nothing  -> ""
               Just _   -> " (locked)"

describeItems :: Room -> World -> String
describeItems room world =
  let iids = roomItems room
  in if null iids
       then ""
       else "You can see: " ++ commaList (map (resolveName world) iids) ++ "."
  where
    resolveName w iid =
      maybe iid itemName (Map.lookup iid (worldItems w))

-- ---------------------------------------------------------------------------
-- Error messages
-- ---------------------------------------------------------------------------

-- | Turn a GameError into a human-readable string.
renderError :: GameError -> String
renderError (UnknownCommand s)  = "I don't understand '" ++ s ++ "'."
renderError (UnknownRoom rid)   = "There is no room called '" ++ rid ++ "'. (World error)"
renderError (UnknownItem iid)   = "You don't see any '" ++ iid ++ "' here."
renderError (NoExit dir)        = "You can't go " ++ dir ++ " from here."
renderError (ExitLocked dir k)  = "The way " ++ dir ++ " is locked. You need the " ++ k ++ "."
renderError (ItemNotHere iid)   = "There is no '" ++ iid ++ "' here to take."
renderError (ItemNotCarried iid)= "You aren't carrying '" ++ iid ++ "'."
renderError (WorldError msg)    = "World error: " ++ msg

-- ---------------------------------------------------------------------------
-- Pretty-print world back to file format (stretch goal skeleton)
-- ---------------------------------------------------------------------------

-- | Serialise a World back to the text format understood by the Loader.
--   Useful for testing round-trip properties.
prettyWorld :: World -> String
prettyWorld world =
  unlines $ concatMap prettyRoom (Map.elems (worldRooms world))
         ++ [""]
         ++ concatMap prettyItem (Map.elems (worldItems world))

prettyRoom :: Room -> [String]
prettyRoom room =
  [ "room " ++ roomId room
  , "  name \"" ++ roomName room ++ "\""
  , "  description \"" ++ roomDescription room ++ "\""
  ]
  ++ map prettyExit (Map.toList (roomExits room))
  ++ map (\i -> "  item " ++ i) (roomItems room)
  ++ [""]

prettyExit :: (ExitDir, Exit) -> String
prettyExit (dir, exit) =
  "  exit " ++ dir ++ " " ++ exitTarget exit
  ++ maybe "" (" locked " ++) (exitLocked exit)

prettyItem :: Item -> [String]
prettyItem item =
  [ "item " ++ itemId item
  , "  name \"" ++ itemName item ++ "\""
  , "  description \"" ++ itemDescription item ++ "\""
  , ""
  ]

-- ---------------------------------------------------------------------------
-- Utility
-- ---------------------------------------------------------------------------

commaList :: [String] -> String
commaList []     = ""
commaList [x]    = x
commaList [x, y] = x ++ " and " ++ y
commaList (x:xs) = x ++ ", " ++ commaList xs