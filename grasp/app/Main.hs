{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use Down" #-}

import Control.Monad (forM, replicateM, when)
import Data.List (maximumBy, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.Random (randomRIO)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Player = Player
  { playerId :: Int,
    pos :: String,
    name :: String,
    club :: String,
    points :: Int,
    price :: Double
  }
  deriving (Eq, Show)

type Team = [Player]

parsePlayer :: String -> Maybe Player
parsePlayer line = case splitOn "," line of
  [idMaybe, playerPos, playerName, playerTeam, pointsMaybe, costMaybe] -> do
    pid <- readMaybe idMaybe
    playerPoints <- readMaybe pointsMaybe
    playerCost <- readMaybe costMaybe
    return (Player pid playerPos playerName playerTeam playerPoints playerCost)
  _ -> Nothing

loadPlayers :: FilePath -> IO [Player]
loadPlayers path = do
  content <- readFile path
  let players = mapMaybe parsePlayer $ lines content
  return players

posOrder :: Player -> Int
posOrder p = case pos p of
  "GK" -> 0
  "DEF" -> 1
  "MID" -> 2
  "FW" -> 3
  _ -> 4

countBy :: (Eq b) => (a -> b) -> [a] -> b -> Int
countBy fn xs value = length $ filter (\x -> fn x == value) xs

totalCost :: Team -> Double
totalCost team = sum $ map price team

totalPoints :: Team -> Int
totalPoints team = sum $ map points team

-- is the players position maxed out
breaksPositions :: Team -> Player -> Bool
breaksPositions t player =
  case pos player of
    "FW" -> countBy pos t "FW" == 3
    "MID" -> countBy pos t "MID" == 5
    "DEF" -> countBy pos t "DEF" == 5
    "GK" -> countBy pos t "GK" == 2
    _ -> True

validFirstTeam :: Team -> Bool
validFirstTeam t =
  countBy pos t "GK" == 1
    && countBy pos t "DEF" >= 3
    && countBy pos t "FW" >= 1

-- can we add player p to team team
isFeasibleAdd :: Team -> Player -> Bool
isFeasibleAdd team p =
  countBy club team (club p) < 3
    && totalCost (p : team) <= 100
    && not (breaksPositions team p)

scoreCandidate :: Player -> Double
scoreCandidate p = fromIntegral (points p) / price p

randomChoice :: [a] -> IO a
randomChoice xs = do
  idx <- randomRIO (0, length xs - 1)
  return $ xs !! idx

-- calculate score for each feasible player as points / price
-- sort players by descending score.
-- take the top alpha% of this sorted list as the RC
-- randomly select one player from the RCL to add to the team
takeRCL :: Double -> [(Player, Double)] -> [Player]
takeRCL alpha scored =
  let n = max 1 (floor $ alpha * fromIntegral (length scored))
   in map fst $ take n scored

buildSquad :: Team -> [Player] -> Double -> IO Team
buildSquad team remaining alpha
  | length team == 15 = return team
  | otherwise = do
      let feasible = filter (isFeasibleAdd team) remaining
      when (null feasible) $ print feasible
      if null feasible
        then return team
        else do
          let scored = [(p, scoreCandidate p) | p <- feasible]
              sorted = sortBy (flip $ comparing snd) scored
              rcl = takeRCL alpha sorted
          chosen <- randomChoice rcl
          buildSquad (chosen : team) (filter (/= chosen) remaining) alpha

selectFirstTeam :: [Player] -> Team
selectFirstTeam squad =
  let sorted = sortBy (flip $ comparing points) squad
      gks = filter (\p -> pos p == "GK") sorted
      defs = filter (\p -> pos p == "DEF") sorted
      fws = filter (\p -> pos p == "FW") sorted
      -- mids = filter (\p -> pos p == "MID") sorted

      firstGK = take 1 gks
      firstDEF = take 3 defs
      firstFW = take 1 fws

      remainingSlots = 11 - length (firstGK ++ firstDEF ++ firstFW)
      remainingPlayers = filter (`notElem` (firstGK ++ firstDEF ++ firstFW)) sorted
      remainingSelection = take remainingSlots remainingPlayers
   in firstGK ++ firstDEF ++ firstFW ++ remainingSelection

pickFirstTeam :: Team -> (Team, Team)
pickFirstTeam squad =
  let firstTeam = selectFirstTeam squad
      subs = filter (`notElem` firstTeam) squad
   in (firstTeam, subs)

-- smaller alpha -> more greedy
construct :: [Player] -> Double -> IO (Team, Team)
construct playerList alpha = do
  squad <- buildSquad [] playerList alpha
  let (firstTeam, subs) = pickFirstTeam squad
      sortedFirstTeam = sortBy (comparing posOrder) firstTeam
      sortedSubs = sortBy (comparing posOrder) subs
  return (sortedFirstTeam, sortedSubs)

replace :: (Eq a) => [a] -> a -> a -> [a]
replace lst old new = map (\x -> if x == old then new else x) lst

-- neighbourhood: swap first team player with sub player
getNeighboursBench :: (Team, Team) -> [(Team, Team)]
getNeighboursBench (firstTeam, subs) =
  [ (newFirstTeam, newSubs)
    | f <- firstTeam,
      s <- subs,
      let newFirstTeam = replace firstTeam f s
          newSubs = replace subs s f,
      validFirstTeam newFirstTeam
  ]

isFeasibleSwap :: Team -> Team -> Bool
isFeasibleSwap newFirstTeam newSubs =
  length newFirstTeam == 11
    && length newSubs == 4
    && countBy pos newFirstTeam "GK" == 1
    && countBy pos newFirstTeam "DEF" >= 3
    && countBy pos newFirstTeam "FW" >= 1
    && countBy pos totalSquad "GK" <= 2
    && countBy pos totalSquad "DEF" <= 5
    && countBy pos totalSquad "MID" <= 5
    && countBy pos totalSquad "FW" <= 3
    && totalCost (newFirstTeam ++ newSubs) <= 100
    && all (\c -> countBy club totalSquad c <= 3) allClubs
  where
    totalSquad = newFirstTeam ++ newSubs
    allClubs = map club totalSquad

-- neighbourhood: swap first team player with player from all players
getNeighboursAll :: [Player] -> (Team, Team) -> [(Team, Team)]
getNeighboursAll playerList (firstTeam, subs) =
  [ (replace firstTeam f p, replace subs p s)
    | f <- firstTeam,
      s <- subs,
      p <- playerList,
      p `notElem` firstTeam,
      let newFirstTeam = replace firstTeam f p
          newSubs = replace subs p s,
      isFeasibleSwap newFirstTeam newSubs
  ]

localSearch :: [Player] -> (Team, Team) -> (Team, Team)
localSearch playerList (firstTeam, subs) =
  -- let neighbours = getNeighboursBench (firstTeam, subs)
  let neighbours = getNeighboursAll playerList (firstTeam, subs)
      better = filter (\(ft, _) -> totalPoints ft > totalPoints firstTeam) neighbours
   in case better of
        [] -> (firstTeam, subs)
        ((ft', sb') : _) -> localSearch playerList (ft', sb')

prettyPrintTeam :: (Team, Team) -> String
prettyPrintTeam (firstTeam, subs) =
  "Total Cost: "
    ++ printf "%.1f" totalCostAll
    ++ "\nTotal Points: "
    ++ show totalPointsAll
    ++ "\n\nStarters:\n"
    ++ header
    ++ "\n"
    ++ ppTeam firstTeam
    ++ "\nSubs:\n"
    ++ header
    ++ "\n"
    ++ ppTeam subs
  where
    totalCostAll = totalCost firstTeam + totalCost subs
    totalPointsAll = totalPoints firstTeam

    header =
      printf
        "%-5s %-4s %-15s %-18s %6s %7s"
        "id"
        "pos"
        "name"
        "club"
        "points"
        "price"

    ppTeam :: Team -> String
    ppTeam t =
      unlines $
        map
          ( \p ->
              printf
                "%-5d %-4s %-15s %-18s %6d %7.1f"
                (playerId p)
                (pos p)
                (name p)
                (club p)
                (points p)
                (price p)
          )
          t

grasp :: [Player] -> IO [(Double, (Team, Team))]
grasp players = do
  let alphas = [0.1, 0.3, 0.5, 0.7, 0.9]
      iterations = 10

  forM alphas $ \alpha -> do
    solutions <- replicateM iterations $ do
      initial <- construct players alpha
      let improved = localSearch players initial
      return improved

    let bestForAlpha = maximumBy (comparing (totalPoints . fst)) solutions
    return (alpha, bestForAlpha)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [csvPath] -> do
      players <- loadPlayers csvPath

      greedy <- construct players 0
      putStrLn $ prettyPrintTeam greedy

      alphaSolutions <- grasp players

      -- putStrLn "\nBest Solutions per Alpha:"
      -- forM_ alphaSolutions $ \(alpha, (ft, subs)) -> do
      --   putStrLn $ "\nAlpha: " ++ show alpha
      --   putStrLn $ prettyPrintTeam (ft, subs)

      let (bestAlpha, bestTeam) = maximumBy (comparing (totalPoints . fst . snd)) alphaSolutions
      putStrLn "\nOverall Best Solution Found:"
      putStrLn $ "Alpha used: " ++ show bestAlpha
      putStrLn $ prettyPrintTeam bestTeam
    _ -> putStrLn "usage: grasp <path/to/file.csv>"
