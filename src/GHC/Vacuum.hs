{-# LANGUAGE BangPatterns #-}

{- |
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
> graph [rankdir=LR, splines=true];
> node [label="\N", shape=none, fontcolor=blue, fontname=courier];
> edge [color=black, style=dotted, fontname=courier, arrowname=onormal];
>
>     ":|0" -> {"S#|1",":|0"}
>     "S#|1" -> {}
> }
-}

module GHC.Vacuum (
   HNodeId
  ,HNode(..)
  ,emptyHNode
  ,vacuum,vacuumTo,vacuumLazy
  ,dump,dumpTo,dumpLazy
  ,toAdjList
  ,nameGraph
  ,ShowHNode(..)
  ,showHNodes
  ,ppHs,ppDot
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
) where

import Prelude hiding(catch)
import GHC.Vacuum.Dot as Dot
import GHC.Vacuum.ClosureType
import GHC.Vacuum.Internal as GHC
import Data.Char
import Data.Word
import Data.List
import Data.Map(Map)
import Data.IntMap(IntMap)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Monoid(Monoid(..))
import Data.Array.IArray
import System.IO.Unsafe
import Control.Monad
import Data.Bits
import Text.PrettyPrint(Doc,text)
import Language.Haskell.Meta.Utils(pretty)
import Control.Applicative
import Control.Exception

import Foreign
import GHC.Arr(Array(..))
import GHC.Exts

-----------------------------------------------------------------------------

-- | Suck up @a@.
vacuum :: a -> IntMap HNode
vacuum a = unsafePerformIO (dump a)

-- | Stop after a given depth.
vacuumTo :: Int -> a -> IntMap HNode
vacuumTo n a = unsafePerformIO (dumpTo n a)

-- | Doesn't really work like you'd want it to.
-- Working on this, but there's a slight chance that getting
-- it to work as one would expect isn't possible given the
-- ever-so small hook that GHC gives us (@unpackClosure#@).
-- (Just so that the possibility of impossibility is stated).
vacuumLazy :: a -> IntMap HNode
vacuumLazy a = unsafePerformIO (dumpLazy a)

dump :: a -> IO (IntMap HNode)
dump a = execH (dumpH a)

dumpTo :: Int -> a -> IO (IntMap HNode)
dumpTo n a = execH (dumpToH n a)

dumpLazy :: a -> IO (IntMap HNode)
dumpLazy a = execH (dumpLazyH a)

-----------------------------------------------------------------------------

toAdjList :: IntMap HNode -> [(Int, [Int])]
toAdjList = fmap (mapsnd nodePtrs) . IM.toList

nameGraph :: IntMap HNode -> [(String, [String])]
nameGraph m = let g = toAdjList m
                  pp i = maybe "..."
                          (\n -> nodeName n ++ "|" ++ show i)
                          (IM.lookup i m)
              in fmap (\(x,xs) -> (pp x, fmap pp xs)) g

data ShowHNode = ShowHNode
  {showHNode   :: Int -> HNode -> String
  ,externHNode :: Int -> String}

showHNodes :: ShowHNode -> IntMap HNode -> [(String, [String])]
showHNodes (ShowHNode showN externN) m
  = let g = toAdjList m
        pp i = maybe (externN i) (showN i) (IM.lookup i m)
    in fmap (\(x,xs) -> (pp x, fmap pp xs)) g

-----------------------------------------------------------------------------

ppHs :: (Show a) => a -> Doc
ppHs = text . pretty

ppDot :: [(String, [String])] -> Doc
ppDot = Dot.graphToDot id

-----------------------------------------------------------------------------

type HNodeId = Int

data HNode = HNode
  {nodePtrs  :: [HNodeId]
  ,nodeLits  :: [Word]
  ,nodeInfo  :: InfoTab}
  deriving(Eq,Ord,Read,Show)

data InfoTab
  = ConInfo   {itabPkg    :: String
              ,itabMod    :: String
              ,itabCon    :: String
              ,itabPtrs   ::  Word
              ,itabLits   ::  Word
              ,itabType   ::  ClosureType
              ,itabSrtLen ::  Word
              ,itabCode   :: [Word]}
  | OtherInfo {itabPtrs   ::  Word
              ,itabLits   ::  Word
              ,itabType   ::  ClosureType
              ,itabSrtLen ::  Word
              ,itabCode   :: [Word]}
  deriving(Eq,Ord,Read,Show)

data Closure = Closure
  {closPtrs :: [HValue]
  ,closLits :: [Word]
  ,closITab :: InfoTab}
  deriving(Show)

-- So we can derive Show for Closure
instance Show HValue where show _ = "(HValue)"

------------------------------------------------

-- | To assist in \"rendering\"
--  the graph to some source.
data Draw e v m a = Draw
  {mkV   :: Int -> a -> m v
  ,mkE   :: v -> v -> m e
  ,succs :: a -> [Int]}

newtype G e v = G {unG :: IntMap (v, IntMap e)}
  deriving(Eq,Ord,Read,Show)

draw :: (Monad m) => Draw e v m a -> IntMap a -> m (G e v)
draw (Draw mkV mkE succs) g = do
  vs <- IM.fromList `liftM` forM (IM.toList g)
          (\(i,a) -> do v <- mkV i a
                        return (i,(v,succs a)))
  (G . IM.fromList) `liftM` forM (IM.toList vs)
    (\(i,(v,ps)) -> do let us = fmap (vs IM.!) ps
                       es <- IM.fromList `liftM` forM ps
                               (\p -> do e <- mkE v (fst (vs IM.! p))
                                         return (p,e))
                       return (i,(v,es)))

-- | An example @Draw@
printDraw :: Draw (Int,Int) Int IO HNode
printDraw = Draw
  {mkV   = \i _ -> print i >> return i
  ,mkE   = \u v -> print (u,v) >> return (u,v)
  ,succs = nodePtrs}

-- | Build a map to @(preds,succs)@
split :: (a -> [Int]) -> IntMap a -> IntMap ([Int],[Int])
split f = flip IM.foldWithKey mempty (\i a m ->
            let ps = f a
            in foldl' (\m p -> IM.insertWith mappend p ([i],[]) m)
                      (IM.insertWith mappend i ([],ps) m)
                      ps)

------------------------------------------------

emptyHNode :: ClosureType -> HNode
emptyHNode ct = HNode
  {nodePtrs   = []
  ,nodeLits   = []
  ,nodeInfo   = if isCon ct
                  then ConInfo [] [] [] 0 0 ct 0 []
                  else OtherInfo 0 0 ct 0 []}

nodePkg   :: HNode -> String
nodeMod   :: HNode -> String
nodeName  :: HNode -> String
nodePkg   = fst3 . itabName . nodeInfo
nodeMod   = snd3 . itabName . nodeInfo
nodeName  = trd3 . itabName . nodeInfo

fst3 (x,_,_) = x
snd3 (_,x,_) = x
trd3 (_,_,x) = x

itabName :: InfoTab -> (String, String, String)
itabName i@(ConInfo{}) = (itabPkg i, itabMod i, itabCon i)
itabName  _            = ([], [], [])

------------------------------------------------

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
                        -- the info pointer we get back from unpackClosure#
                        -- is to the beginning of the standard info table,
                        -- but the Storable instance for info tables takes
                        -- into account the extra entry pointer when
                        -- !ghciTablesNextToCode, so we must adjust here.
          itab <- peekInfoTab iptr'
          let elems = fromIntegral (itabPtrs itab)
              ptrs0 = if elems < 1
                        then []
                        else dumpArray (Array 0 (elems - 1) elems ptrs)
              lits = [W# (indexWordArray# nptrs i)
                        | I# i <- [0.. fromIntegral (itabLits itab-1)] ]
          -- ptrs <- mapM defined ptrs0
          return (Closure ptrs0 lits itab)

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
                      -- the info pointer we get back from unpackClosure#
                      -- is to the beginning of the standard info table,
                      -- but the Storable instance for info tables takes
                      -- into account the extra entry pointer when
                      -- !ghciTablesNextToCode, so we must adjust here.
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

data Env = Env
  {uniq  :: HNodeId
  ,seen  :: [(HValue, HNodeId)]
  ,hvals :: IntMap HValue
  ,graph :: IntMap HNode}

emptyEnv :: Env
emptyEnv = Env
  {uniq = 0
  ,seen = []
  ,hvals = mempty
  ,graph = mempty}

------------------------------------------------

-- | Walk the reachable heap (sub)graph rooted at @a@,
-- and collect it as a graph of @HNode@s in @H@'s state.
dumpH :: a -> H ()
dumpH a = go =<< rootH a
  where go :: HValue -> H ()
        go a = do
          ids <- nodeH a
          case ids of
            [] -> return ()
            _  -> mapM_ go =<< mapM getHVal ids

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

dumpLazyH :: a -> H ()
dumpLazyH !a = go =<< rootH a
  where go :: HValue -> H ()
        go a = do
          ids <- nodeLazyH a
          case ids of
            [] -> return ()
            _  -> mapM_ go =<< mapM getHVal ids

-- | Needed since i don't know of a way
-- to go @a -> HValue@ directly (unsafeCoercing
-- directly doesn't work (i tried)).
data Box a = Box a

-- | Turn the root into an @HValue@ to start off.
rootH :: a -> H HValue
rootH a = do
  let b = Box a
  c <- io (getClosure $! b)
  case closPtrs c of
    [hval] -> io (defined hval)
    _ -> error "zomg"

-- | Add this @HValue@ to the graph, then
--  add it's successor's not already seen, and
--  return the @HNodeId@'s of these newly-seen nodes
--  (which we've added to the graph in @H@'s state).
--  CURRENTLY GHC COERCES UNPOINTED CLOSURES TO
--  @HVALUE@, which is a bug in the sense that
--  unpointed closures cannot be entered, which HValues
--  can.
nodeH :: HValue -> H [HNodeId]
nodeH a = do
  clos <- io (getClosure $! a)
  (i, _) <- getId a
  let itab = closITab clos
      ptrs = closPtrs clos
  ptrs' <- case itabType itab of
              t | isCon t -> return (avoid (itabCon itab) ptrs)
                | otherwise -> return ptrs
  ptrs'' <- io (mapM defined ptrs')
  xs <- mapM getId ptrs''
  let news = (fmap fst . fst . partition snd) xs
      n    = HNode (fmap fst xs)
                    (closLits clos)
                    (closITab clos)
  insertG i n
  return news

nodeLazyH :: HValue -> H [HNodeId]
nodeLazyH a = do
  clos <- io (getClosure a)
  (i, _) <- getId a
  let itab = closITab clos
      ptrs = closPtrs clos
  ptrs' <- case itabType itab of
              t | isCon t -> return (avoid (itabCon itab) ptrs)
                  -- IMPORTANT: Following either (or both) of
                  -- the pointer inside a @THUNK@ results in a segfault.
                | isThunk t -> return []
                | otherwise -> return ptrs
  xs <- mapM getIdLazy ptrs'
  let news = (fmap fst . fst . partition snd) xs
      n    = HNode (fmap fst xs)
                    (closLits clos)
                    (closITab clos)
  insertG i n
  return news

------------------------------------------------

-- XXXXXX: USE A TRIE FOR THIS INSTEAD

-- XXX: hackish casing on conname until unpackClosure# is fixed.
-- Try to cover a few common cases.
avoid :: String -> [HValue] -> [HValue]
avoid con = maybe id id (IM.lookup (hash con) criminals)

criminals :: IntMap ([HValue] -> [HValue])
criminals = IM.fromList . fmap (mapfst hash) $
  [("J#",               const [])
  ,("MVar",             const [])
  ,("STRef",            const [])
  ,("Array",            take   2)
  ,("MallocPtr",        const [])
  ,("PlainPtr",         const [])
  ,("PS",               drop   1)
  ,("Chunk",            drop   1)
  ,("FileHandle",       take   1)
  ,("DuplexHandle",     take   1)
  --,("",                 id)
  ]

hash :: String -> Int
hash [] = 0
hash  s = go 0 (fmap ord s)
  where go !h [] = h
        go !h (n:ns) =
          let a = (h `shiftL` 4)
              b = a + n
              c = b .&. 0xf0000000
              !d = case c==0 of
                    False -> let !e = c `shiftR` 24
                              in b `xor` e
                    True  -> b
              !e = complement c
              !f = d `xor` e
          in go f ns

{-
unsigned long
elfhash(const char *s)
{
  unsigned long h=0, g;
  while (*s){
    h = (h << 4) + *s++;
    if((g = h & 0xf0000000))
      h ^= g >> 24;
    h &= ~g;
  }
  return h;
}
-}

------------------------------------------------

getHVal :: HNodeId -> H HValue
getHVal i = (IM.! i) `fmap` gets hvals

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
getId hval = hval `seq` do
  s <- gets seen
  case look hval s of
    Just i -> return (i, False)
    Nothing -> do
      i <- newId
      vs <- gets hvals
      modify (\e->e{seen=(hval,i):s
                   ,hvals= IM.insert i hval vs})
      return (i, True)

getIdLazy :: HValue -> H (HNodeId, Bool)
getIdLazy hval = do
  s <- gets seen
  case lookLazy hval s of
    Just i -> return (i, False)
    Nothing -> do
      i <- newId
      vs <- gets hvals
      modify (\e->e{seen=(hval,i):s
                   ,hvals= IM.insert i hval vs})
      return (i, True)

------------------------------------------------

look :: HValue -> [(HValue, a)] -> Maybe a
look _      [] = Nothing
look hval ((x,i):xs)
  | hval .==. x = Just i
  | otherwise   = look hval xs

(.==.) :: HValue -> HValue -> Bool
a .==. b = a `seq` b `seq`
  (0 /= I# (reallyUnsafePtrEquality# a b))

lookLazy :: HValue -> [(HValue, a)] -> Maybe a
lookLazy _      [] = Nothing
lookLazy hval ((x,i):xs)
  | hval =.= x = Just i
  | otherwise   = lookLazy hval xs

(=.=) :: HValue -> HValue -> Bool
a =.= b = (0 /= I# (reallyUnsafePtrEquality# a b))

dumpArray :: Array Int a -> [a]
dumpArray a = let (m,n) = bounds a
              in fmap (a!) [m..n]

mapfst f = \(a,b) -> (f a,b)
mapsnd f = \(a,b) -> (a,f b)
f *** g = \(a, b) -> (f a, g b)

p2i :: Ptr a -> Int
i2p :: Int -> Ptr a
p2i (Ptr a#) = I# (addr2Int# a#)
i2p (I# n#) = Ptr (int2Addr# n#)

------------------------------------------------

{-
newtype S s a = S {unS :: forall o. s -> (s -> a -> IO o) -> IO o}
instance Functor (S s) where
  fmap f (S g) = S (\s k -> g s (\s a -> k s (f a)))
instance Monad (S s) where
  return a = S (\s k -> k s a)
  S g >>= f = S (\s k -> g s (\s a -> unS (f a) s k))
get :: S s s
get = S (\s k -> k s s)
gets :: (s -> a) -> S s a
gets f = S (\s k -> k s (f s))
set :: s -> S s ()
set s = S (\_ k -> k s ())
io :: IO a -> S s a
io m = S (\s k -> k s =<< m)
modify :: (s -> s) -> S s ()
modify f = S (\s k -> k (f s) ())
runS :: S s a -> s -> IO (a, s)
runS (S g) s = g s (\s a -> return (a, s))
-}

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

  These are *unpointed* objects: i.e. they cannot be entered.                           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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


