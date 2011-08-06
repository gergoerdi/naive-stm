module Test where

import STM
import Control.Concurrent
import Control.Parallel
import Control.Monad (forM)
import Control.Concurrent.MVar

type Account = TVar Amount
type Amount = Integer

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

withdraw :: Account -> Amount -> STM ()
withdraw from amount = do
  balance <- readTVar from
  fib 26 `pseq` return ()
  writeTVar from (balance - amount)

main = do
  account <- newTVarIO 120
  rs <- forM [100, 50, 20] $ \amount -> do
    ready <- newEmptyMVar
    forkIO $ do
      atomically $ withdraw account amount
      putMVar ready ()
    return ready
  mapM_ takeMVar rs

  balance <- readTVarIO account
  print balance
