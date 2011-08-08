{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module STM ( STM, atomically
           , TVar, newTVar, readTVar, writeTVar
           , retry, orElse
           , newTVarIO, readTVarIO
           ) where

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Maybe
import Control.Concurrent.QSem
import Control.Concurrent.MVar

import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

newtype STM α = STM { runSTM :: StateT Log (MaybeT IO) α }
              deriving (Monad, MonadPlus)

newtype ID = ID Int deriving (Eq, Ord, Enum)
data TVar α = TVar ID (IORef (IORef α))

data Val = forall α. Val (TVar α) (IORef α)
data Log = Log { readLog, writeLog :: Map ID Val }

idCounter :: MVar ID
idCounter = unsafePerformIO $ newMVar (ID 0)

globalLock :: QSem
globalLock = unsafePerformIO $ newQSem 1

lock :: IO ()
lock = waitQSem globalLock

unlock :: IO ()
unlock = signalQSem globalLock

newTVar :: α -> STM (TVar α)
newTVar x = STM $ liftIO $ do
  id <- takeMVar idCounter
  putMVar idCounter $ succ id

  ref <- newIORef x
  refref <- newIORef ref

  return $ TVar id refref

newTVarIO :: α -> IO (TVar α)
newTVarIO x = do
  mx <- runMaybeT $ evalStateT (runSTM $ newTVar x) (error "newTVar tried accessing the STM state")
  case mx of
    Just x -> return x
    Nothing -> error "Internal error: newTVar called retry??"

readTVar :: TVar α -> STM α
readTVar tvar@(TVar id refref) = STM $ do
  w <- gets $ Map.lookup id . writeLog
  case w of
    Just (Val _ refWritten) -> liftIO $ readIORef $ unsafeCoerce refWritten
    Nothing -> do
      r <- gets $ Map.lookup id . readLog
      case r of
        Just (Val _ refStored) -> liftIO $ readIORef $ unsafeCoerce refStored
        Nothing -> do
          ref <- liftIO $ readIORef refref
          modify $ store ref
          liftIO $ readIORef ref
  where
    store ref log = log{ readLog = Map.insert id (Val tvar ref) $ readLog log }

readTVarIO :: TVar α -> IO α
readTVarIO tvar = do
  ref <- readRef tvar
  readIORef ref

writeTVar :: TVar α -> α -> STM ()
writeTVar tvar@(TVar id _) x = STM $ do
  ref <- liftIO $ newIORef x
  modify $ write ref
  where
    write ref log = log{ writeLog = Map.insert id (Val tvar ref) $ writeLog log }

atomically :: STM α -> IO α
atomically transaction = do
  mres <- runMaybeT $ runStateT (runSTM transaction) (Log Map.empty Map.empty)
  case mres of
    Nothing -> atomically transaction
    Just (x, log) -> do
      lock
      valid <- tryCommit log
      unlock
      if valid then return x else atomically transaction

retry :: STM α
retry = mzero

orElse :: STM α -> STM α -> STM α          
orElse = mplus
          
tryCommit :: Log -> IO Bool
tryCommit log = do
  valid <- isValid log
  when valid $ commit log
  return valid

readRef :: TVar α -> IO (IORef α)
readRef (TVar _ refref) = readIORef refref

writeRef :: TVar α -> IORef α -> IO ()
writeRef (TVar _ refref) ref = writeIORef refref ref

isValid :: Log -> IO Bool
isValid log = do
  vs <- mapM checkVar $ Map.elems $ readLog log
  return $ and vs
  where
    checkVar :: Val -> IO Bool
    checkVar (Val tvar refStored) = do
      ref <- readRef tvar
      return $ ref == refStored

commit :: Log -> IO ()
commit log = do
  forM_ (Map.elems $ writeLog log) $ \(Val tvar refWritten) -> do
    writeRef tvar refWritten
