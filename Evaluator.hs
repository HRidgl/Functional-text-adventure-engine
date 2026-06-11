module Evaluator where

import Data.Char     (toLower)
import Types
import World
import Parser        (Command(..))
import Pretty        (describeRoom)
import qualified Data.Map.Strict as Map

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Evaluate a parsed command against the current game state.
--   Returns either a GameError or a (newState, message) pair.
evaluate :: Command -> GameState -> Result
evaluate Look         gs = evalLook gs
evaluate (Go dir)     gs = evalGo dir gs
evaluate (Take name)  gs = evalTake name gs
evaluate (Drop name)  gs = evalDrop name gs
evaluate (Use n tgt)  gs = evalUse n tgt gs
evaluate (Examine n)  gs = evalExamine n gs
evaluate Inventory    gs = evalInventory gs
evaluate Quit         gs = Right (gs, "Goodbye!")   -- REPL checks for Quit separately

-- ---------------------------------------------------------------------------
-- Individual evaluators
-- ---------------------------------------------------------------------------

evalLook :: GameState -> Result
evalLook gs = do
  room <- currentRoom gs
  let msg = describeRoom room (gsWorld gs) (gsPlayer gs)
  Right (gs, msg)

evalGo :: ExitDir -> GameState -> Result
evalGo dir gs = do
  room <- currentRoom gs
  exit <- maybe (Left (NoExit dir)) Right (Map.lookup dir (roomExits room))
  -- Check lock
  case exitLocked exit of
    Just keyId ->
      if playerHasItem keyId (gsPlayer gs)
        then do
          -- Use the key: unlock exit and move
          let world'  = unlockExit (roomId room) dir (gsWorld gs)
              gs'     = movePlayer (exitTarget exit) gs { gsWorld = world' }
          destRoom <- currentRoom gs'
          let msg = "You use the " ++ keyId ++ " to unlock the door.\n\n"
                 ++ describeRoom destRoom (gsWorld gs') (gsPlayer gs')
          Right (gs', msg)
        else Left (ExitLocked dir keyId)
    Nothing -> do
      let gs' = movePlayer (exitTarget exit) gs
      destRoom <- currentRoom gs'
      Right (gs', describeRoom destRoom (gsWorld gs) (gsPlayer gs'))

evalTake :: String -> GameState -> Result
evalTake name gs = do
  room <- currentRoom gs
  iid  <- resolveItemByName name (roomItems room) (gsWorld gs)
            `orError` ItemNotHere name
  let gs' = pickUpItem iid gs
  item <- lookupItem iid (gsWorld gs)
  Right (gs', "Taken: " ++ itemName item ++ ".")

evalDrop :: String -> GameState -> Result
evalDrop name gs = do
  iid  <- resolveItemByName name (playerInventory (gsPlayer gs)) (gsWorld gs)
            `orError` ItemNotCarried name
  let gs' = dropItem iid gs
  item <- lookupItem iid (gsWorld gs)
  Right (gs', "Dropped: " ++ itemName item ++ ".")

evalExamine :: String -> GameState -> Result
evalExamine name gs = do
  room <- currentRoom gs
  let candidates = roomItems room ++ playerInventory (gsPlayer gs)
  iid  <- resolveItemByName name candidates (gsWorld gs)
            `orError` UnknownItem name
  item <- lookupItem iid (gsWorld gs)
  Right (gs, itemDescription item)

evalUse :: String -> Maybe String -> GameState -> Result
evalUse name _target gs = do
  -- Basic use: if the player carries the item, acknowledge it.
  -- Extend this with richer effect logic as a stretch goal.
  iid <- resolveItemByName name (playerInventory (gsPlayer gs)) (gsWorld gs)
           `orError` ItemNotCarried name
  item <- lookupItem iid (gsWorld gs)
  -- TODO: look up item effects table and apply them
  Right (gs, "You fiddle with the " ++ itemName item ++ ", but nothing obvious happens.")

evalInventory :: GameState -> Result
evalInventory gs =
  let inv = playerInventory (gsPlayer gs)
  in if null inv
       then Right (gs, "You are not carrying anything.")
       else do
         names <- mapM (\iid -> itemName <$> lookupItem iid (gsWorld gs)) inv
         Right (gs, "You are carrying:\n" ++ unlines (map ("  - " ++) names))

-- ---------------------------------------------------------------------------
-- Name resolution helpers
-- ---------------------------------------------------------------------------

-- | Given a display name and a list of candidate item IDs, find the first
--   whose name matches (case-insensitive, prefix allowed).
resolveItemByName :: String -> [ItemId] -> World -> Maybe ItemId
resolveItemByName name candidates world =
  let needle = map Data.Char.toLower name
      matches iid = case Map.lookup iid (worldItems world) of
        Nothing   -> False
        Just item -> needle `isPrefixOf` map Data.Char.toLower (itemName item)
                  || needle == map Data.Char.toLower iid
  in case filter matches candidates of
       (iid:_) -> Just iid
       []      -> Nothing
  where
    isPrefixOf xs ys = take (length xs) ys == xs

orError :: Maybe a -> GameError -> Either GameError a
orError Nothing  e = Left e
orError (Just x) _ = Right x