module Main where

  import System.IO      (hSetBuffering, stdout, BufferMode(..))
  import System.Exit    (exitSuccess)
  import System.Environment (getArgs)

  import Types
  import World
  import Parser
  import Evaluator
  import Loader
  import Pretty

  -- ---------------------------------------------------------------------------
  -- Entry point
  -- ---------------------------------------------------------------------------

  main :: IO ()
  main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    worldPath <- case args of
      (p:_) -> return p
      []    -> do
        putStrLn "Usage: adventure <world-file>"
        putStrLn "Using default world: worlds/demo.world"
        return "worlds/demo.world"

    result <- loadWorldFromFile worldPath
    case result of
      Left err    -> putStrLn ("Failed to load world: " ++ renderError err)
      Right world -> startGame world

  -- ---------------------------------------------------------------------------
  -- Game initialisation
  -- ---------------------------------------------------------------------------

  startGame :: World -> IO ()
  startGame world =
    case Map.keys (worldRooms world) of
      []      -> putStrLn "Error: world has no rooms."
      (rid:_) -> do
        let player = Player
              { playerLocation  = rid
              , playerInventory = []
              , playerScore     = 0
              }
            gs = GameState world player
        putStrLn banner
        -- Show the starting room
        case currentRoom gs of
          Left  err  -> putStrLn (renderError err)
          Right room -> putStrLn (describeRoom room world player)
        repl gs
    where
      -- pick the room marked as "start" if present, else first room
      Map = import qualified Data.Map.Strict as Map

  -- We need the Map import; restructure:
  import qualified Data.Map.Strict as Map

  startGame' :: World -> IO ()
  startGame' world =
    let rid = case Map.lookup "start" (worldRooms world) of
                Just _  -> "start"
                Nothing -> head (Map.keys (worldRooms world))
        player = Player rid [] 0
        gs     = GameState world player
    in do
      putStrLn banner
      case currentRoom gs of
        Left  err  -> putStrLn (renderError err)
        Right room -> putStrLn (describeRoom room world player)
      repl gs

  -- ---------------------------------------------------------------------------
  -- REPL
  -- ---------------------------------------------------------------------------

  repl :: GameState -> IO ()
  repl gs = do
    putStr "> "
    line <- getLine
    case parseCommand line of
      Left  msg -> do putStrLn msg;  repl gs
      Right Quit -> putStrLn "Goodbye!" >> exitSuccess
      Right cmd  ->
        case evaluate cmd gs of
          Left  err        -> do putStrLn (renderError err); repl gs
          Right (gs', msg) -> do putStrLn msg;               repl gs'

  -- ---------------------------------------------------------------------------
  -- Banner
  -- ---------------------------------------------------------------------------

  banner :: String
  banner = unlines
    [ "╔══════════════════════════════════════╗"
    , "║   F U N C T I O N A L   A D V E N T U R E  ║"
    , "╚══════════════════════════════════════╝"
    , "Type 'look' to examine your surroundings."
    , "Type 'help' or '?' for a list of commands."
    , "Type 'quit' to exit."
    , ""
    ]
