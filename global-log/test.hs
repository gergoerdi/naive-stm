module Main(main) where

import STM
import Control.Concurrent
import Control.Parallel
import Control.Monad (forM, when)
import Control.Concurrent.MVar

type Account = TVar Amount
type Amount = Integer

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

busy :: STM ()
busy = fib 34 `pseq` return ()         
{-# INLINE busy #-}
        
withdraw :: Account -> Amount -> STM ()
withdraw from amount = do
  balance <- readTVar from
  when (balance < amount) retry
  busy
  writeTVar from (balance - amount)

deposit :: Account -> Amount -> STM ()
deposit to amount = do
  balance <- readTVar to
  busy
  busy
  busy
  writeTVar to (balance + amount)
            
main = do
  account <- newTVarIO 120
  rs <- forM [100, 50, 20] $ \amount -> do
    ready <- newEmptyMVar
    forkIO $ do
      atomically $ (withdraw account amount `orElse` return ())
      putMVar ready ()
    return ready
           
  r <- newEmptyMVar
  forkIO $ do
    atomically $ deposit account 100
    putMVar r ()               
  
  mapM_ takeMVar (r:rs)

  balance <- readTVarIO account
  print balance
