module Types where

import qualified Data.Map.Strict as Map

-- ---------------------------------------------------------------------------
-- Identifiers
-- ---------------------------------------------------------------------------

type RoomId  = String
type ItemId  = String
type ExitDir = String   -- "north", "south", "east", "west", "up", "down", …

-- ---------------------------------------------------------------------------
-- World components
-- ---------------------------------------------------------------------------

data Exit = Exit
  { exitTarget  :: RoomId
  , exitLocked  :: Maybe ItemId   -- Nothing = open, Just k = needs key k
  } deriving (Show, Eq)

data Item = Item
  { itemId          :: ItemId
  , itemName        :: String     -- display name
  , itemDescription :: String
  } deriving (Show, Eq)

data Room = Room
  { roomId          :: RoomId
  , roomName        :: String
  , roomDescription :: String
  , roomExits       :: Map.Map ExitDir Exit
  , roomItems       :: [ItemId]   -- items currently in this room
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Top-level world and player
-- ---------------------------------------------------------------------------

data World = World
  { worldRooms    :: Map.Map RoomId Room
  , worldItems    :: Map.Map ItemId Item   -- master item registry
  } deriving (Show, Eq)

data Player = Player
  { playerLocation  :: RoomId
  , playerInventory :: [ItemId]
  , playerScore     :: Int
  } deriving (Show, Eq)

-- The complete game state passed around the evaluator
data GameState = GameState
  { gsWorld  :: World
  , gsPlayer :: Player
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

data GameError
  = UnknownCommand  String
  | UnknownRoom     RoomId
  | UnknownItem     ItemId
  | NoExit          ExitDir
  | ExitLocked      ExitDir ItemId   -- direction, required key
  | ItemNotHere     ItemId           -- item not in room
  | ItemNotCarried  ItemId           -- item not in inventory
  | WorldError      String           -- load-time validation failure
  deriving (Show, Eq)

-- A result is either an error or a new state with a message for the player
type Result = Either GameError (GameState, String)