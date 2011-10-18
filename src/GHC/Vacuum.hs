{-# LANGUAGE BangPatterns, UnboxedTuples, MagicHash #-}

{- |
>
> ghci> toAdjList $ vacuum (fix (0:))
> [(0,[1,0]),(1,[])]
>
> ghci> ppHs $ vacuum (fix (0:))
> fromList
>   [(0,
>     HNode{nodePtrs = [1, 0], nodeLits = [40425920],
>           nodeInfo =
>             ConInfo{itabPkg = "ghc-prim", itabMod = "GHC.Types", itabCon = ":",
>                     itabPtrs = 2, itabLits = 0, itabType = CONSTR_2_0, itabSrtLen = 1,
>                     itabCode =
>                       [72, 131, 195, 2, 255, 101, 0, 144, 224, 30, 0, 0, 0, 0, 0, 0]}}),
>    (1,
>     HNode{nodePtrs = [], nodeLits = [0, 40425920],
>           nodeInfo =
>             ConInfo{itabPkg = "integer", itabMod = "GHC.Integer.Internals",
>                     itabCon = "S#", itabPtrs = 0, itabLits = 1, itabType = CONSTR_0_1,
>                     itabSrtLen = 0,
>                     itabCode =
>                       [72, 255, 195, 255, 101, 0, 102, 144, 152, 0, 0, 0, 0, 0, 0, 0]}})]
>
> ghci> ppDot . nameGraph $ vacuum (fix (0:))
> digraph g {
>   graph [rankdir=LR, splines=true];
>   node [label="\N", shape=none, fontcolor=blue, fontname=courier];
>   edge [color=black, style=dotted, fontname=courier, arrowname=onormal];
>       ":|0" -> {"S#|1";":|0"}
>       "S#|1" -> {}
> }
>
> ghci> let a = [0..]
> ghci> toAdjList $ vacuumLazy a
> [(0,[])]
> ghci> take 2 a
> [0,1]
> ghci> toAdjList $ vacuumLazy a
> [(0,[1,2]),(1,[]),(2,[3,4]),(3,[]),(4,[])]
> ghci> take 3 a
> [0,1,2]
> ghci> toAdjList $ vacuumLazy a
> [(0,[1,2]),(1,[]),(2,[3,4]),(3,[]),(4,[5,6]),(5,[]),(6,[])]
>
-}

module GHC.Vacuum (
   HNodeId
  ,HNode(..)
  ,emptyHNode
  ,summary
  ,vacuum,vacuumTo,vacuumLazy,vacuumStream,vacuumDebug
  ,dump,dumpTo,dumpLazy
  ,toAdjList,toAdjPair
  ,nameGraph
  ,ShowHNode(..)
  ,showHNodes
  ,graphToDot
  ,Draw(..),G(..)
  ,draw,printDraw,split
  ,Closure(..)
  ,InfoTab(..)
  ,getClosure
  ,closureType
  ,getInfoTab
  ,getInfoPtr
  ,peekInfoTab
  ,nodePkg,nodeMod
  ,nodeName,itabName
  ,HValue
  --,module GHC.Vacuum.Q
) where

import GHC.Vacuum.Q
import GHC.Vacuum.Util
import GHC.Vacuum.Types
import GHC.Vacuum.Pretty
import GHC.Vacuum.Pretty.Dot
import GHC.Vacuum.ClosureType
import GHC.Vacuum.Internal as GHC

import Data.List
import Data.Char
import Data.Word
import Data.Bits
import Data.Map(Map)
import Data.IntMap(IntMap)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Monoid(Monoid(..))
import System.IO.Unsafe
import Control.Monad
import Control.Applicative
import Control.Exception
import Prelude hiding(catch)
import Control.Concurrent

import Foreign hiding (unsafePerformIO)
import GHC.Arr(Array(..))
import GHC.Exts

import System.Mem.StableName

-----------------------------------------------------------------------------

-- | Vacuums the entire reachable heap subgraph rooted at the @a@.
vacuum :: a -> IntMap HNode
vacuum a = unsafePerformIO (dump a)

-- | Returns nodes as it encounters them.
vacuumStream :: a -> [(HNodeId, HNode)]
vacuumStream a = unsafePerformIO (dumpStream a)

vacuumDebug :: a -> IntMap [(StableName HValue, HNodeId)]
vacuumDebug a = unsafePerformIO (dumpDebug a)

-- | Stop after a given depth.
vacuumTo :: Int -> a -> IntMap HNode
vacuumTo n a = unsafePerformIO (dumpTo n a)

-- | Doesn't force anything.
vacuumLazy :: a -> IntMap HNode
vacuumLazy a = unsafePerformIO (dumpLazy a)

dump :: a -> IO (IntMap HNode)
dump a = execH (dumpH a)

dumpStream :: a -> IO [(HNodeId, HNode)]
dumpStream a = streamH (flip dumpStreamH a)

dumpDebug :: a -> IO (IntMap [(StableName HValue, HNodeId)])
dumpDebug a = debugH (dumpH a)

dumpTo :: Int -> a -> IO (IntMap HNode)
dumpTo n a = execH (dumpToH n a)

dumpLazy :: a -> IO (IntMap HNode)
dumpLazy a = execH (dumpLazyH a)

-----------------------------------------------------------------------------

getInfoPtr :: a -> Ptr StgInfoTable
getInfoPtr a = let b = a `seq` Box a
                in b `seq` case unpackClosure# a of
                            (# iptr,_,_ #)
                              | ghciTablesNextToCode -> Ptr iptr
                              | otherwise -> Ptr iptr `plusPtr`
                                              negate wORD_SIZE

-- | Turn @undefined@ into the the exception value it throws.
defined :: HValue -> IO HValue
defined a = grab (return $! a) (return . unsafeCoerce#)

grab :: IO a -> (SomeException -> IO a) -> IO a
grab = catch

-- | This is in part borrowed from @RtClosureInspect.getClosureData@.
getClosure :: a -> IO Closure
getClosure a = grab (getClosure_ a) getClosure

getClosure_ :: a -> IO Closure
getClosure_ a =
  case unpackClosure# a of
      (# iptr
        ,ptrs
        ,nptrs #) -> do
          let iptr' | ghciTablesNextToCode = Ptr iptr
                    | otherwise = Ptr iptr `plusPtr` negate wORD_SIZE
          itab <- peekInfoTab iptr'
          let elems = fromIntegral (itabPtrs itab)
              ptrs0 = dumpArray# ptrs 0 elems
              lits = [W# (indexWordArray# nptrs i)
                        | I# i <- [0.. fromIntegral (itabLits itab-1)] ]
          case itab of
              -- follow indirections, because mkStableName follows
              -- indirections as well.
              OtherInfo { itabType = tipe }
                  | tipe == IND || tipe == IND_OLDGEN || tipe == IND_PERM ||
                    tipe == IND_OLDGEN_PERM || tipe == IND_STATIC ->
                  case ptrs0 of
                      (dest : _) -> getClosure_ dest
              _ -> return (Closure ptrs0 lits itab)

-- using indexArray# makes sure that the HValue is looked up without
-- evaluating the value itself. This is not possible with Data.Array.!
dumpArray# :: Array# HValue -> Int -> Int -> [HValue]
dumpArray# arr# i@(I# i#) l
    | i >= l = []
    | otherwise = case indexArray# arr# i# of
                      (# h #) -> h : dumpArray# arr# (i+1) l

closureType :: a -> IO ClosureType
closureType a = itabType <$> getInfoTab a

getInfoTab :: a -> IO InfoTab
getInfoTab a =
  case unpackClosure# a of
    (# iptr
      ,_
      ,_ #) -> do
        let iptr' | ghciTablesNextToCode = Ptr iptr
                  | otherwise = Ptr iptr `plusPtr` negate wORD_SIZE
        peekInfoTab iptr'

peekInfoTab :: Ptr StgInfoTable -> IO InfoTab
peekInfoTab p = do
  stg <- peek p
  let ct = (toEnum . fromIntegral . GHC.tipe) stg
  case ct of
    _ | hasName stg -> do (a,b,c) <- dataConInfoPtrToNames (castPtr p)
                          return $ ConInfo
                            {itabPkg    = a
                            ,itabMod    = b
                            ,itabCon    = c
                            ,itabPtrs   = (fromIntegral . GHC.ptrs) stg
                            ,itabLits   = (fromIntegral . GHC.nptrs) stg
                            ,itabType   = ct
                            ,itabSrtLen = fromIntegral (GHC.srtlen stg)
                            ,itabCode   = fmap fromIntegral (GHC.code stg)}
    _ -> return $ OtherInfo
          {itabPtrs   = (fromIntegral . GHC.ptrs) stg
          ,itabLits   = (fromIntegral . GHC.nptrs) stg
          ,itabType   = ct
          ,itabSrtLen = fromIntegral (GHC.srtlen stg)
          ,itabCode   = fmap fromIntegral (GHC.code stg)}


-- Check whether this closure is a datacon and sanity check
-- to make sure we didn't read garbage from memory into this
-- StgInfoTable (because if we did, we'll probably segfault
--  during dataConInfoPtrToNames).
hasName :: StgInfoTable -> Bool
hasName stg = let ct = (toEnum . fromIntegral . GHC.tipe) stg :: ClosureType
                  lits = (fromIntegral . GHC.nptrs) stg       :: Int
                  ptrs = (fromIntegral . GHC.ptrs) stg :: Int
              in  isCon ct
                && lits < 1024  -- It seems the ptrs info the ItblEnv
                && ptrs < 1024  -- gotten from ByteCodeItbls are borked
                                -- in some way, *OR* (and more likely)
                                -- there's some caveat i'm not aware of.

------------------------------------------------

type H a = S Env a

execH :: H a -> IO (IntMap HNode)
execH m = snd `fmap` runH m

runH :: H a -> IO (a, IntMap HNode)
runH m = do
  (a, s) <- runS m emptyEnv
  return (a, graph s)

runH_ :: H a -> IO ()
runH_ m = do
  _ <- runS m emptyEnv
  return ()

debugH :: H a -> IO (IntMap [(StableName HValue,HNodeId)])
debugH m = (seen . snd) <$> runS m emptyEnv

streamH :: (Q (Maybe a) -> H b) -> IO [a]
streamH m = do
  q <- newQ
  tid <- forkIO (runH_ (m q) `finally` putQ q Nothing)
  fmap fromJust <$> takeWhileQ isJust q

fromJust :: Maybe a -> a
fromJust (Just a) = a

isJust :: Maybe a -> Bool
isJust (Just{}) = True
isJust  _       = False

------------------------------------------------

-- | Walk the reachable heap (sub)graph rooted at @a@,
-- and collect it as a graph of @HNode@s in @H@'s state.
vacuumH :: (HValue -> H [HNodeId]) -> a -> H ()
vacuumH scan a = go =<< rootH a
  where go :: HValue -> H ()
        go a = do
          ids <- scan a
          case ids of
            [] -> return ()
            _  -> mapM_ go =<< mapM getHVal ids

dumpH :: a -> H ()
dumpH = vacuumH nodeH

dumpLazyH :: a -> H ()
dumpLazyH = vacuumH nodeLazyH

dumpStreamH :: Q (Maybe (HNodeId,HNode)) -> a -> H ()
dumpStreamH q = vacuumH (nodeStreamH q)

-- argh, this one doesn't
-- quite fit into the pattern
dumpToH :: Int -> a -> H ()
dumpToH n _ | n < 1 = return ()
dumpToH n a = go (n-1) =<< rootH a
  where go :: Int -> HValue -> H ()
        go 0 _ = return ()
        go n a = do
          ids <- nodeH a
          case ids of
            [] -> return ()
            _  -> mapM_ (go (n-1)) =<< mapM getHVal ids

-- | Turn the root into an @HValue@ to start off.
rootH :: a -> H HValue
rootH a = return (unsafeCoerce# a)

-- | Add this @HValue@ to the graph, then
--  add it's successor's not already seen, and
--  return the @HNodeId@'s of these newly-seen nodes
--  (which we've added to the graph in @H@'s state).
scanNodeH :: (HValue -> H (HNodeId,Closure,[HValue]))
          -> (HValue -> H (HNodeId, Bool))
          -> (HNodeId -> HNode -> H ())
          ->  HValue  -> H [HNodeId]
scanNodeH getNode getId withNode a = do
  (i,clos,ptrs) <- getNode a
  xs <- mapM getId ptrs
  let news = (fmap fst . fst . partition snd) xs
      n    = HNode (fmap fst xs)
                    (closLits clos)
                    (closITab clos)
  withNode i n
  return news

nodeH :: HValue -> H [HNodeId]
nodeH = scanNodeH getNodeH' getId' insertG

nodeLazyH :: HValue -> H [HNodeId]
nodeLazyH = scanNodeH getNodeH getId insertG

nodeStreamH :: Q (Maybe (HNodeId, HNode)) -> HValue -> H [HNodeId]
nodeStreamH q = scanNodeH getNodeH' getId'
            (\i n -> io (putQ q (Just (i,n))))

getNodeH :: HValue -> H (HNodeId, Closure, [HValue])
getNodeH a = do
  clos <- io (getClosure a)
  (i, _) <- getId a
  let itab = closITab clos
      ptrs = closPtrs clos
  case itabType itab of
    t   -- IMPORTANT: Following any of the pointer(s)
        -- inside a @THUNK@ results in the chop (aka segfault).
      | isThunk t -> return (i,clos,[])
      | otherwise -> return (i,clos,ptrs)

getNodeH' :: HValue -> H (HNodeId, Closure, [HValue])
getNodeH' a = do
  clos <- io (getClosure a)
  let itab = closITab clos
      ptrs = closPtrs clos
  case itabType itab of
    t | isThunk t -> getNodeH' =<< io (defined a)
      | otherwise -> do
          (i, _) <- getId a
          return (i,clos,ptrs)

------------------------------------------------

getHVal :: HNodeId -> H HValue
getHVal i = do
    -- the following pattern match is important: it evaluates the lookup
    -- without evaluating x itself
    Box x <- (IM.! i) `fmap` gets hvals
    return x

insertG :: HNodeId -> HNode -> H ()
insertG i n = do
  g <- gets graph
  modify (\e->e{graph = IM.insert i n g})

newId :: H HNodeId
newId = do
  n <- gets uniq
  modify (\e->e{uniq=n+1})
  return n

getId :: HValue -> H (HNodeId, Bool)
getId hval = do
  sn <- io (makeStableName hval)
  let h = hashStableName sn
  s <- gets seen
  case lookup sn =<< IM.lookup h s of
    Just i -> return (i, False)
    Nothing -> do
      i <- newId
      vs <- gets hvals
      modify (\e->e{seen= IM.insertWith (++) h [(sn,i)] s
                   ,hvals= IM.insert i (Box hval) vs})
      return (i, True)

getId' :: HValue -> H (HNodeId, Bool)
getId' hval = do
  clos <- io (getClosure hval)
  let itab = closITab clos
  case itabType itab of
    t | isThunk t -> getId' =<< io (defined hval)
      | otherwise -> getId hval

------------------------------------------------

{-

rts/StgMiscClosures.cmm

/* ----------------------------------------------------------------------------
  Arrays

  These come in two basic flavours: arrays of data (StgArrWords) and arrays of
  pointers (StgArrPtrs).  They all have a similar layout:

  ___________________________
  | Info | No. of | data....
  |  Ptr | Words  |
  ---------------------------

  These are *unpointed* objects: i.e. they cannot be entered.

  ------------------------------------------------------------------------- */

INFO_TABLE(stg_ARR_WORDS, 0, 0, ARR_WORDS, "ARR_WORDS", "ARR_WORDS")
{ foreign "C" barf("ARR_WORDS object entered!") never returns; }

INFO_TABLE(stg_MUT_ARR_PTRS_CLEAN, 0, 0, MUT_ARR_PTRS_CLEAN, "MUT_ARR_PTRS_CLEAN", "MUT_ARR_PTRS_CLEAN")
{ foreign "C" barf("MUT_ARR_PTRS_CLEAN object entered!") never returns; }

INFO_TABLE(stg_MUT_ARR_PTRS_DIRTY, 0, 0, MUT_ARR_PTRS_DIRTY, "MUT_ARR_PTRS_DIRTY", "MUT_ARR_PTRS_DIRTY")
{ foreign "C" barf("MUT_ARR_PTRS_DIRTY object entered!") never returns; }

INFO_TABLE(stg_MUT_ARR_PTRS_FROZEN, 0, 0, MUT_ARR_PTRS_FROZEN, "MUT_ARR_PTRS_FROZEN", "MUT_ARR_PTRS_FROZEN")
{ foreign "C" barf("MUT_ARR_PTRS_FROZEN object entered!") never returns; }

INFO_TABLE(stg_MUT_ARR_PTRS_FROZEN0, 0, 0, MUT_ARR_PTRS_FROZEN0, "MUT_ARR_PTRS_FROZEN0", "MUT_ARR_PTRS_FROZEN0")
{ foreign "C" barf("MUT_ARR_PTRS_FROZEN0 object entered!") never returns; }
-}

{-
unpackClosurezh_fast
{
/* args: R1 = closure to analyze */
// TODO: Consider the absence of ptrs or nonptrs as a special case ?

    W_ info, ptrs, nptrs, p, ptrs_arr, nptrs_arr;
    info  = %GET_STD_INFO(UNTAG(R1));

    // Some closures have non-standard layout, so we omit those here.
    W_ type;
    type = TO_W_(%INFO_TYPE(info));
    switch [0 .. N_CLOSURE_TYPES] type {
    case THUNK_SELECTOR : {
        ptrs = 1;
        nptrs = 0;
        goto out;
    }
    case THUNK, THUNK_1_0, THUNK_0_1, THUNK_2_0, THUNK_1_1,
         THUNK_0_2, THUNK_STATIC, AP, PAP, AP_STACK, BCO : {
        ptrs = 0;
        nptrs = 0;
        goto out;
    }
    default: {
        ptrs  = TO_W_(%INFO_PTRS(info));
        nptrs = TO_W_(%INFO_NPTRS(info));
        goto out;
    }}
out:

    W_ ptrs_arr_sz, nptrs_arr_sz;
    nptrs_arr_sz = SIZEOF_StgArrWords   + WDS(nptrs);
    ptrs_arr_sz  = SIZEOF_StgMutArrPtrs + WDS(ptrs);

    ALLOC_PRIM (ptrs_arr_sz + nptrs_arr_sz, R1_PTR, unpackClosurezh_fast);

    W_ clos;
    clos = UNTAG(R1);

    ptrs_arr  = Hp - nptrs_arr_sz - ptrs_arr_sz + WDS(1);
    nptrs_arr = Hp - nptrs_arr_sz + WDS(1);

    SET_HDR(ptrs_arr, stg_MUT_ARR_PTRS_FROZEN_info, W_[CCCS]);
    StgMutArrPtrs_ptrs(ptrs_arr) = ptrs;
    p = 0;
for:
    if(p < ptrs) {
       W_[ptrs_arr + SIZEOF_StgMutArrPtrs + WDS(p)] = StgClosure_payload(clos,p);
   p = p + 1;
   goto for;
    }

    SET_HDR(nptrs_arr, stg_ARR_WORDS_info, W_[CCCS]);
    StgArrWords_words(nptrs_arr) = nptrs;
    p = 0;
for2:
    if(p < nptrs) {
       W_[BYTE_ARR_CTS(nptrs_arr) + WDS(p)] = StgClosure_payload(clos, p+ptrs);
       p = p + 1;
   goto for2;
    }
    RET_NPP(info, ptrs_arr, nptrs_arr);
}

-}
