module World where

import Types
import qualified Data.Map.Strict as Map

-- ---------------------------------------------------------------------------
-- Lookups (safe wrappers around Map.lookup)
-- ---------------------------------------------------------------------------

lookupRoom :: RoomId -> World -> Either GameError Room
lookupRoom rid w =
  maybe (Left $ UnknownRoom rid) Right (Map.lookup rid (worldRooms w))

lookupItem :: ItemId -> World -> Either GameError Item
lookupItem iid w =
  maybe (Left $ UnknownItem iid) Right (Map.lookup iid (worldItems w))

currentRoom :: GameState -> Either GameError Room
currentRoom gs = lookupRoom (playerLocation (gsPlayer gs)) (gsWorld gs)

-- ---------------------------------------------------------------------------
-- Predicates
-- ---------------------------------------------------------------------------

playerHasItem :: ItemId -> Player -> Bool
playerHasItem iid p = iid `elem` playerInventory p

roomHasItem :: ItemId -> Room -> Bool
roomHasItem iid r = iid `elem` roomItems r

-- ---------------------------------------------------------------------------
-- Pure state updates
-- ---------------------------------------------------------------------------

-- Move player to a new room (does not validate)
movePlayer :: RoomId -> GameState -> GameState
movePlayer rid gs = gs { gsPlayer = (gsPlayer gs) { playerLocation = rid } }

-- Add item to player inventory
pickUpItem :: ItemId -> GameState -> GameState
pickUpItem iid gs =
  let player' = (gsPlayer gs) { playerInventory = iid : playerInventory (gsPlayer gs) }
      -- Remove item from its room
      world'  = removeItemFromRoom iid (playerLocation (gsPlayer gs)) (gsWorld gs)
  in gs { gsPlayer = player', gsWorld = world' }

-- Remove item from player inventory, place in current room
dropItem :: ItemId -> GameState -> GameState
dropItem iid gs =
  let player' = (gsPlayer gs) { playerInventory = filter (/= iid) (playerInventory (gsPlayer gs)) }
      world'  = addItemToRoom iid (playerLocation (gsPlayer gs)) (gsWorld gs)
  in gs { gsPlayer = player', gsWorld = world' }

-- Unlock an exit in a room (remove lock)
unlockExit :: RoomId -> ExitDir -> World -> World
unlockExit rid dir w =
  case Map.lookup rid (worldRooms w) of
    Nothing   -> w
    Just room ->
      let exits' = Map.adjust (\e -> e { exitLocked = Nothing }) dir (roomExits room)
          room'  = room { roomExits = exits' }
      in w { worldRooms = Map.insert rid room' (worldRooms w) }

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

removeItemFromRoom :: ItemId -> RoomId -> World -> World
removeItemFromRoom iid rid w =
  adjustRoom rid (\r -> r { roomItems = filter (/= iid) (roomItems r) }) w

addItemToRoom :: ItemId -> RoomId -> World -> World
addItemToRoom iid rid w =
  adjustRoom rid (\r -> r { roomItems = iid : roomItems r }) w

adjustRoom :: RoomId -> (Room -> Room) -> World -> World
adjustRoom rid f w =
  w { worldRooms = Map.adjust f rid (worldRooms w) }