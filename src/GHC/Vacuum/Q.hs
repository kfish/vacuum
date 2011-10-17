{-# LANGUAGE BangPatterns, PostfixOperators #-}

module GHC.Vacuum.Q (
   Ref,ref,(!),(.=),(!=)
  ,Q,isEmptyQ,newQ,putQ,takeQ,tryTakeQ
  ,drainQ,getQContents,takeWhileQ
) where

import Data.IORef
import Control.Monad
import Control.Concurrent
import Control.Applicative
import System.IO.Unsafe(unsafeInterleaveIO)

------------------------------------------------

newtype Ref a = Ref
  {unRef :: IORef a}

ref :: a -> IO (Ref a)
ref a = Ref <$> newIORef a

(!) :: Ref a -> IO a
(!) (Ref r) = readIORef r

(.=) :: Ref a -> a -> IO ()
Ref r .= x = writeIORef r x

(!=) :: Ref a -> (a -> (a, b)) -> IO b
Ref r != f = atomicModifyIORef r f

------------------------------------------------

data Q a = Q (MVar (Tail a))
             (MVar (Tail a))

newtype Tail a = Tail (Ref (Maybe (a, Tail a)))

emptyTail :: IO (Tail a)
emptyTail = Tail <$> ref Nothing

isEmptyTail :: Tail a -> IO Bool
isEmptyTail (Tail r) = maybe True (const False) <$> (r!)

isEmptyQ :: Q a -> IO Bool
isEmptyQ (Q rd _) = isEmptyMVar rd

newQ :: IO (Q a)
newQ = do
  hole  <- emptyTail 
  readVar  <- newEmptyMVar
  writeVar <- newMVar hole
  return (Q readVar writeVar)

putQ :: Q a -> a -> IO ()
putQ (Q rd wr) val = do
  Tail old <- takeMVar wr
  new <- emptyTail
  old .= Just (val, new)
  first <- isEmptyMVar rd
  when first (putMVar rd (Tail old))
  putMVar wr new

takeQ :: Q a -> IO a
takeQ q@(Q rd _) = do
  Tail end <- takeMVar rd
  m <- (end!)
  case m of
    Nothing -> takeQ q
    Just (a, new) -> do last <- isEmptyTail new
                        when (not last) (putMVar rd new)
                        return a

tryTakeQ :: Q a -> IO (Maybe a)
tryTakeQ  q@(Q rd _) = do
  o <- tryTakeMVar rd
  case o of
    Nothing -> return Nothing
    Just (Tail end) -> do
      m <- (end!)
      case m of
        Nothing -> error "impossible!"
        Just (a, new) -> do last <- isEmptyTail new
                            when (not last) (putMVar rd new)
                            return (Just a)

drainQ :: Q a -> IO [a]
drainQ q = do
  a <- tryTakeQ q
  case a of
    Nothing -> return []
    Just a -> do as <- unsafeInterleaveIO (drainQ q)
                 return (a:as)

getQContents :: Q a -> IO [a]
getQContents q = do
  a <- takeQ q
  as <- unsafeInterleaveIO (getQContents q)
  return (a:as)

takeWhileQ :: (a -> Bool) -> Q a -> IO [a]
takeWhileQ p q = do
  a <- takeQ q
  case p a of
    False -> return []
    True -> do
      as <- unsafeInterleaveIO (takeWhileQ p q)
      return (a:as)

------------------------------------------------
