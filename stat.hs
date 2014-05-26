import System.IO
import System.Cmd
import Text.Regex.Posix
import System.Random
import Control.Exception
import Data.List

file_res :: String -> IO String
file_res path = do
  input <- readFile path
  return $  (input =~ "([^T]*)" :: String)


test :: IO()
test = do
  wi <- randomRIO (1::Int, 100)
  hi <- randomRIO (1::Int, 100)
  seed <- randomRIO (1::Int, 100000)
  e <- system $ "mpirun -np 4 ./msp-par.exe " ++ show wi ++ " " ++ show hi ++ " " 
    ++ show seed ++ " 2> _par_out.err"
  ee <- evaluate e 
  e2 <- system $ "./msp-seq-naive.exe " ++ show wi ++ " " ++ show hi ++ " " 
    ++ show seed ++ " 2> _seq_out.err"
  ee2 <- evaluate e2
  mr <- file_res "_par_out.err"
  nr <- file_res "_seq_out.err"
  if mr /= nr then do
    putStrLn "Test failed!"
    putStrLn "My output:"
    putStrLn mr
    putStrLn "Naive output"
    putStrLn nr
  else
    return ()

substr :: Int -> Int -> [a] -> [a]
substr first last ls = take (last - first) $ drop first ls

seq_time::Double
seq_time = 2786.86341

pprint :: String -> IO()
pprint str = appendFile "out.csv" str

stat :: Maybe Double -> Int -> Int -> IO Double
stat sp pr mach = do
  str <- readFile $ "show/" ++ show (pr * mach) ++ "_" ++ show mach ++ ".show"
  print $ elemIndices '\n' str
  print str
  let inds = elemIndices '\n' str in
    let vals = map (read::String -> Double) [substr 0 (inds!!0 - 1) str, substr (inds!!0 + 1) (inds!!1 - 1) str, substr (inds!!1 + 1) (inds!!2 - 1) str] in do
      putStrLn $ ("max " ++ (show  $ maximum vals))
      putStrLn $ ("min " ++ (show  $ minimum vals))
      let avg =  ((foldr (+) 0  vals) / 3.0) in do
        putStrLn $ ("avg " ++ show avg)
        speedup <- case sp of
          Just v -> putStrLn ("speedup " ++ (show $ v / avg)) >> return (v/avg)
          Nothing -> putStrLn "speedup 1.0" >> return (1.0::Double)
        putStrLn ("efficiency " ++ show (speedup / fromIntegral (pr * mach)))
        let seq_speedup = seq_time /avg in do
          putStrLn ("seq speedup " ++ show seq_speedup)
          putStrLn ("seq efficiency " ++ show (seq_speedup / fromIntegral (pr * mach)))
          pprint (show (pr * mach) ++ ", " ++ (show mach) ++ ", ")
          pprint ( (show $ maximum vals) ++ ", " ++ (show $ minimum vals) ++ ", " ++ (show avg))
          pprint ( ", " ++ (show speedup) ++ ", " ++ (show $ speedup / fromIntegral (pr * mach)))
          pprint ( ", " ++ (show seq_speedup) ++ ", ")
          pprint ((show $ seq_speedup / fromIntegral (pr * mach)))
          pprint "\n"
        return avg



main = do
  avg <- stat Nothing 1 1 
  mapM_ (\x -> stat (Just avg) (fst x) (snd x)) [(x, y) | x <- [1,2,4], y <-[1,2,4,8,16]]
  return ()

