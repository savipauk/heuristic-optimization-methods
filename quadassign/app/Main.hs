{-# OPTIONS_GHC -Wno-unused-top-binds #-}
import Data.Function (on)
import Data.List (delete, minimumBy, sortOn)
import Data.Map (Map, empty)
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.Random (randomRIO)

data Instance = Instance
  { size :: Int,
    distances :: [[Int]],
    flows :: [[Int]]
  }
  deriving (Show)

type Facility = Int

type Location = Int

type TemporaryAssignedMap = [(Facility, Location)]

-- Swap facilities at location i and j
type TabuMove = (Location, Location)

type TabuList = Map TabuMove Int

type Solution = [Int]

-- Solution representation
-- A permutation vector [Int] of size n, where vector[i] = j means that at
-- facility j is located at location i.

-- Greedy heuristic
-- Step 1. Anchor location 1 to facility 1
-- Step 2. For location i, pick facility j that minimizes the incremental
--         objective function value with respect to previously picked facility
--         locations (from 0 to i-1).
-- Step 3. ???
-- Step 4. Profit

-- Tabu Search
-- Neighborhood is a pair swap (swap locations of two facilities)
-- The neighborhood size is (n * (n - 1) / 2)
-- Attributive memory, tabu list contains a swap (facility1, facility2)
-- Initial tabu tenure is n - 1
-- Max iterations

-- Objective function
-- cost = sum (flow(i, j) * distance(pi(i), pi(j))
-- O(n^2)
cost :: Instance -> Solution -> Int
cost qap sol =
  sum
    [ flows qap !! fi !! fj * distances qap !! li !! lj
      | li <- [0 .. n - 1],
        let fi = sol !! li,
        lj <- [0 .. n - 1],
        let fj = sol !! lj
    ]
  where
    n = size qap

-- Incremental objective function when assigning facility f to location l
-- cost = sum (flow(f, f-i) * distance(pi(l), pi(l-i) +
--             flow(f-i, f) * distance(pi(l-i), pi(l))
greedyDeltaCost :: Instance -> TemporaryAssignedMap -> Facility -> Location -> Int
greedyDeltaCost qap assigned f l =
  sum
    [ flows qap !! f !! fPrev * distances qap !! l !! lPrev
        + flows qap !! fPrev !! f * distances qap !! lPrev !! l
      | (fPrev, lPrev) <- assigned
    ]

-- delta cost = new move cost - incumbent cost
-- since cost is O(n^2), this is O(n^2)
swapDeltaCost :: Instance -> Int -> Solution -> TabuMove -> Int
swapDeltaCost qap incumbentCost sol move =
  cost qap (applyMove sol move) - incumbentCost

-- delta cost = sum of interactions of every facility at k, with facilities at i and j
-- this is O(n) and performs better
swapDeltaCost' :: Instance -> Solution -> TabuMove -> Int
swapDeltaCost' qap sol (i, j) =
  let fi = sol !! i
      fj = sol !! j
      n = size qap
   in sum
        [ (flows qap !! fi !! fk - flows qap !! fj !! fk)
            * (distances qap !! j !! k - distances qap !! i !! k)
            + (flows qap !! fk !! fi - flows qap !! fk !! fj)
              * (distances qap !! k !! j - distances qap !! k !! i)
          | k <- [0 .. n - 1],
            k /= i,
            k /= j,
            let fk = sol !! k
        ]

-- Greedy Step 1.
greedy :: Instance -> Solution
greedy qap =
  build qap [(0, 0)] remainingFacilities 1
  where
    n = size qap
    remainingFacilities = [1 .. n - 1]

-- Greedy Step 2.
build :: Instance -> TemporaryAssignedMap -> [Facility] -> Location -> Solution
build _ assigned [] _ = assignmentToSolution assigned
build qap assigned fs loc =
  let bestFacility =
        minimumBy
          ( \thisFacility otherFacility ->
              compare
                (greedyDeltaCost qap assigned thisFacility loc)
                (greedyDeltaCost qap assigned otherFacility loc)
          )
          fs

      newAssigned = (bestFacility, loc) : assigned
      newRemaining = delete bestFacility fs
   in build qap newAssigned newRemaining (loc + 1)

-- Sort by Location and then pick Facility
assignmentToSolution :: TemporaryAssignedMap -> Solution
assignmentToSolution assigned = map fst $ sortOn snd assigned

-- Tabu Search
-- Neighborhood is a pair swap (swap locations of two facilities)
-- The neighborhood size is (n * (n - 1) / 2)
-- Attributive memory, tabu list contains a swap (facility1, facility2)
-- Initial tabu tenure is n - 1
-- Allow aspiration criteria if they improve the incumbent solution

tabuIterations :: Int
tabuIterations = 1000

saIterations :: Int
saIterations = 10000000

neighborhood :: Instance -> Int -> Solution -> [(TabuMove, Int)]
neighborhood qap incumbentCost sol =
  [ ((i, j), swapDeltaCost qap incumbentCost sol (i, j))
    | i <- [0 .. n - 2],
      j <- [i + 1 .. n - 1]
  ]
  where
    n = size qap

neighborhood' :: Instance -> Solution -> [(TabuMove, Int)]
neighborhood' qap sol =
  [ ((i, j), swapDeltaCost' qap sol (i, j))
    | i <- [0 .. n - 2],
      j <- [i + 1 .. n - 1]
  ]
  where
    n = size qap

applyMove :: Solution -> TabuMove -> Solution
applyMove sol (i, j) =
  let fi = sol !! i
      fj = sol !! j
   in replace j fi (replace i fj sol)
  where
    replace :: Location -> Facility -> Solution -> Solution
    replace jloc fi solution = take jloc solution ++ [fi] ++ drop (jloc + 1) solution

isTabu :: Int -> TabuList -> TabuMove -> Bool
isTabu iter tabu move =
  case Map.lookup move tabu of
    Just expiry -> iter < expiry
    Nothing -> False

allowedMove :: Int -> Int -> Int -> TabuList -> TabuMove -> Bool
allowedMove iter newCost incumbentCost tabu move =
  not (isTabu iter tabu move) || newCost < incumbentCost

bestMove :: Instance -> Solution -> Int -> Int -> TabuList -> Int -> (TabuMove, Int)
bestMove qap sol currentCost iter tabu incumbentCost =
  minimumBy
    (compare `on` snd)
    [ (move, delta)
      | (move, delta) <- neighborhood' qap sol,
        let newCost = currentCost + delta,
        allowedMove iter newCost incumbentCost tabu move
    ]

updateTabu :: Int -> Int -> TabuList -> TabuMove -> TabuList
updateTabu iter tenure tabu move =
  Map.insert move (iter + tenure) tabu

tabuSearch' :: Instance -> Int -> TabuList -> Solution -> Int -> Solution -> Int -> Solution
tabuSearch' qap tenure = go 0
  where
    go :: Int -> TabuList -> Solution -> Int -> Solution -> Int -> Solution
    go iter tabu current currentCost incumbent incumbentCost
      | iter >= tabuIterations = incumbent
      | otherwise =
          let (move, delta) = bestMove qap current currentCost iter tabu incumbentCost
              nextSol = applyMove current move
              nextCost = currentCost + delta
              (newIncumbent, newIncumbentCost)
                | nextCost < incumbentCost = (nextSol, nextCost)
                | otherwise = (incumbent, incumbentCost)

              newTabu = updateTabu iter tenure tabu move
           in go (iter + 1) newTabu nextSol nextCost newIncumbent newIncumbentCost

tabuSearchIterations ::
  Instance ->
  Int ->
  TabuList ->
  Solution ->
  Int ->
  Solution ->
  Int ->
  (Solution, [(Int, Int)])
tabuSearchIterations qap tenure = go 0 []
  where
    go ::
      Int ->
      [(Int, Int)] ->
      TabuList ->
      Solution ->
      Int ->
      Solution ->
      Int ->
      (Solution, [(Int, Int)])

    go iter trace tabu current currentCost incumbent incumbentCost
      | iter >= tabuIterations =
          (incumbent, reverse trace)
      | otherwise =
          let (move, delta) =
                bestMove qap current currentCost iter tabu incumbentCost

              nextSol = applyMove current move
              nextCost = currentCost + delta

              (newIncumbent, newIncumbentCost)
                | nextCost < incumbentCost = (nextSol, nextCost)
                | otherwise = (incumbent, incumbentCost)

              newTabu = updateTabu iter tenure tabu move
              newTrace = (iter, newIncumbentCost) : trace
           in go
                (iter + 1)
                newTrace
                newTabu
                nextSol
                nextCost
                newIncumbent
                newIncumbentCost

tabuSearch :: Instance -> Int -> Solution -> Solution
tabuSearch qap tenure initial =
  tabuSearch' qap tenure Data.Map.empty initial (cost qap initial) initial (cost qap initial)

printSolutionLine :: Solution -> String
printSolutionLine sol =
  unwords (map show sol)

formatResult :: Instance -> Solution -> Int -> String
formatResult qap sol obj =
  let n = size qap
      header = show n ++ " " ++ show obj
      body = printSolutionLine (map (+ 1) sol)
   in header ++ "\n" ++ body

tabuTenure :: Instance -> Int
tabuTenure qap = (size qap * 13) `div` 7

tenureRange :: Instance -> [Int]
tenureRange qap =
  let n = size qap
   in [n `div` 2 .. 2 * n]

runExperiment :: Instance -> Solution -> Int -> (Int, Int, Solution)
runExperiment qap initial tenure =
  let sol = tabuSearch qap tenure initial
      obj = cost qap sol
   in (tenure, obj, sol)

runAllExperiments :: Instance -> Solution -> [(Int, Int, Solution)]
runAllExperiments qap initial =
  map (runExperiment qap initial) (tenureRange qap)

formatExperiment :: (Int, Int, Solution) -> String
formatExperiment (tenure, obj, _) =
  show tenure ++ " " ++ show obj

writeExperimentResults :: FilePath -> [(Int, Int, Solution)] -> IO ()
writeExperimentResults path results =
  writeFile path $
    unlines (map formatExperiment results)

bestResult :: [(Int, Int, Solution)] -> (Int, Int, Solution)
bestResult =
  minimumBy (\(_, c1, _) (_, c2, _) -> compare c1 c2)

randomMove :: Int -> IO TabuMove
randomMove n = do
  i <- randomRIO (0, n - 2)
  j <- randomRIO (i + 1, n - 1)
  pure (i, j)

acceptMove :: Double -> Int -> IO Bool
acceptMove temp delta
  | delta <= 0 = pure True
  | otherwise = do
      r <- randomRIO (0.0, 1.0)
      pure (r < exp (-(fromIntegral delta / temp)))

cooling :: Double -> Double
cooling t = 0.995 * t

simulatedAnnealing ::
  Instance ->
  Solution ->
  Double ->
  (Double -> Double) ->
  Int ->
  IO Solution
simulatedAnnealing qap initial temp0 cool maxIter =
  go 0 temp0 initial initCost initial initCost
  where
    n = size qap
    initCost = cost qap initial

    go :: Int -> Double -> Solution -> Int -> Solution -> Int -> IO Solution
    go iter temp current currentCost best bestCost
      | iter >= maxIter = pure best
      | temp <= 1e-6 = pure best
      | otherwise = do
          move <- randomMove n
          let delta = swapDeltaCost' qap current move
              newCost = currentCost + delta

          accept <- acceptMove temp delta

          let (nextSol, nextCost) =
                if accept
                  then (applyMove current move, newCost)
                  else (current, currentCost)

              (best', bestCost') =
                if nextCost < bestCost
                  then (nextSol, nextCost)
                  else (best, bestCost)

          go
            (iter + 1)
            (cool temp)
            nextSol
            nextCost
            best'
            bestCost'

simulatedAnnealingLog ::
  Instance ->
  Solution ->
  Double ->
  (Double -> Double) ->
  Int ->
  Int ->
  IO (Solution, [(Int, Int)])
simulatedAnnealingLog qap initial temp0 cool initCost maxIter =
  go 0 temp0 initial initCost initial initCost []
  where
    go ::
      Int ->
      Double ->
      Solution ->
      Int ->
      Solution ->
      Int ->
      [(Int, Int)] ->
      IO (Solution, [(Int, Int)])
    go iter temp current currentCost incumbent incumbentCost log'
      | iter >= maxIter =
          pure (incumbent, reverse log')
      | temp <= 1e-6 =
          pure (incumbent, reverse log')
      | otherwise = do
          move <- randomMove (size qap)

          let delta = swapDeltaCost' qap current move
              newCost = currentCost + delta

          accept <- acceptMove temp delta

          let (nextSol, nextCost) =
                if accept
                  then (applyMove current move, newCost)
                  else (current, currentCost)

              (newIncumbent, newIncumbentCost) =
                if nextCost < incumbentCost
                  then (nextSol, nextCost)
                  else (incumbent, incumbentCost)

          go
            (iter + 1)
            (cool temp)
            nextSol
            nextCost
            newIncumbent
            newIncumbentCost
            ((iter, newIncumbentCost) : log')

saInitialTemp :: Instance -> Double
saInitialTemp qap = 10000 * fromIntegral (size qap)

saTemperatures :: Instance -> [Double]
saTemperatures qap = map (\f -> f * fromIntegral (size qap)) [1000, 2500, 5000, 7500, 10000, 25000, 50000, 75000, 100000]

runSAOnce :: Instance -> Solution -> Double -> IO Int
runSAOnce qap initial temp = do
  finalSol <- simulatedAnnealing qap initial temp cooling (cost qap initial)
  pure (cost qap finalSol)

runSAExperiments :: Instance -> Solution -> IO [(Double, Int)]
runSAExperiments qap initial = do
  mapM
    ( \temp -> do
        obj <- runSAOnce qap initial temp
        pure (temp, obj)
    )
    (saTemperatures qap)

writeSATemps :: FilePath -> [(Double, Int)] -> IO ()
writeSATemps path results =
  writeFile path $
    unlines
      [ show (round temp :: Int) ++ " " ++ show obj
        | (temp, obj) <- results
      ]

runSATemperatureStudy :: Instance -> Solution -> IO ()
runSATemperatureStudy qap initial = do
  results <- runSAExperiments qap initial
  writeSATemps "sa_temps3.txt" results

writeSAIters :: FilePath -> [(Int, Int)] -> IO ()
writeSAIters path log' =
  writeFile path $
    unlines
      [ show iter ++ " " ++ show cost'
        | (iter, cost') <- log'
      ]

runSAIterationStudy :: Instance -> Solution -> Double -> IO ()
runSAIterationStudy qap initial temp = do
  let initCost = cost qap initial
  (_, log') <- simulatedAnnealingLog qap initial temp cooling initCost saIterations
  writeSAIters "sa_iters.txt" log'


main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      qap <- loadData path
      -- let initial = greedy qap
      --     results = runAllExperiments qap initial
      --     (bestTenure, bestCost, bestSol) = bestResult results
      --
      -- writeExperimentResults "tabu_results.txt" results
      --
      -- putStrLn "Best tabu tenure:"
      -- putStrLn $
      --   "tenure = "
      --     ++ show bestTenure
      --     ++ ", cost = "
      --     ++ show bestCost
      --
      -- putStrLn "\nBest solution:"
      -- putStrLn (formatResult qap bestSol bestCost)
      -- let sols = tabuSearchIterations qap (tabuTenure qap) Data.Map.empty initial (cost qap initial) initial (cost qap initial)
      -- let (_, trace) =
      --       tabuSearchIterations
      --         qap
      --         (tabuTenure qap)
      --         Data.Map.empty
      --         initial
      --         (cost qap initial)
      --         initial
      --         (cost qap initial)
      --
      -- writeFile
      --   "tabu_iterations.txt"
      --   (unlines [show i ++ " " ++ show c | (i, c) <- trace])
      -- print sols

      let initial = greedy qap
      let greedyCost = cost qap initial
      let tabuSol = tabuSearch qap (tabuTenure qap) initial
      let tabuCost = cost qap tabuSol
      saSol <- simulatedAnnealing qap initial (saInitialTemp qap) cooling saIterations
      let saCost = cost qap saSol
      -- runSATemperatureStudy qap initial
      -- runSAIterationStudy qap initial (saInitialTemp qap)

      print initial
      print greedyCost
      print tabuSol
      print tabuCost
      print saSol
      print saCost
      putStrLn ""
      putStrLn "tabu"
      putStrLn (formatResult qap tabuSol tabuCost)
      putStrLn ""
      putStrLn "SA"
      putStrLn (formatResult qap saSol saCost)
    _ -> putStrLn "usage: quadassign <path/to/file.dat>"

-- Takes n and a flat list [Int].
-- Splits the flat list into a matrix [[Int]] where the size of the inner
-- vectors is equal to n.
flatToMatrix :: Int -> [Int] -> [[Int]]
flatToMatrix _ [] = []
flatToMatrix n xs = take n xs : flatToMatrix n (drop n xs)

-- Parses the flattened [Int] instance file
-- First n is the distance
-- Then n*n is the distance matrix
-- Then n*n is the flow matrix
parseProblem :: [Int] -> Instance
parseProblem (n : rest) =
  let (flatDistance, restAfterFlatDistance) = splitAt (n * n) rest
      (flatFlow, _) = splitAt (n * n) restAfterFlatDistance
      distanceMat = flatToMatrix n flatDistance
      flowMat = flatToMatrix n flatFlow
   in Instance
        { size = n,
          distances = distanceMat,
          flows = flowMat
        }
parseProblem [] = error "Empty input file"

-- Flatten the instance file into [Int] and parse as such
loadData :: FilePath -> IO Instance
loadData path = do
  content <- readFile path
  let nums = map read (words content) :: [Int]
  pure (parseProblem nums)
