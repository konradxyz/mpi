import System.IO
import System.Cmd
import Text.Regex.Posix
import System.Random
import Control.Exception


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


main = do
  sequence_ $ replicate 100 test
  putStrLn "done"



