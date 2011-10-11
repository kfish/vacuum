
{- |
> ghci> ppHs . toAdjList $ vacuum (fix (0:))
> [(0, [1, 0]), (1, [])]
>
> ghci> ppHs $ vacuum (fix (0:))
> fromList
>   [(0,
>     HNode{nodePtrs = [1, 0], nodeLits = [1084647872], nodeTag = 4,
>           nodeIPtr = 1515520,
>           nodeICode =
>             [72, 131, 195, 2, 255, 101, 0, 144, 224, 30, 0, 0, 0, 0, 0, 0],
>           nodeCType = CONSTR_2_0, nodePkg = "ghc-prim",
>           nodeMod = "GHC.Types", nodeName = ":"}),
>    (1,
>     HNode{nodePtrs = [], nodeLits = [0, 1084647872], nodeTag = 3,
>           nodeIPtr = 1871952,
>           nodeICode =
>             [72, 255, 195, 255, 101, 0, 102, 144, 152, 0, 0, 0, 0, 0, 0, 0],
>           nodeCType = CONSTR_0_1, nodePkg = "integer",
>           nodeMod = "GHC.Integer.Internals", nodeName = "S#"})]
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
  ,vacuum,dump
  ,toAdjList
  ,nameGraph
  ,ppHs,ppDot
) where
import GHC.Vacuum.Dot as Dot
import GHC.Vacuum.ClosureType
import GHC.Vacuum.GHC as GHC
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
import Language.Haskell.Meta.Utils(pretty)

-----------------------------------------------------------------------------

vacuum :: a -> IntMap HNode
vacuum a = unsafePerformIO (dump a)

dump :: a -> IO (IntMap HNode)
dump a = execH (dumpH a)

toAdjList :: IntMap HNode -> [(Int, [Int])]
toAdjList = fmap (mapsnd nodePtrs) . IM.toList

nameGraph :: IntMap HNode -> [(String, [String])]
nameGraph m = let g = toAdjList m
                  pp i = nodeName (m IM.! i) ++ "|" ++ show i
              in fmap (\(x,xs) -> (pp x, fmap pp xs)) g

ppHs :: (Show a) => a -> Doc
ppHs = text . pretty

ppDot :: [(String, [String])] -> Doc
ppDot = Dot.graphToDot id

-----------------------------------------------------------------------------

type HNodeId = Int

data HNode = HNode
  {nodePtrs  :: [HNodeId]
  ,nodeLits  :: [Word]
  ,nodeTag   ::  Word
  ,nodeIPtr  ::  Word
  ,nodeICode :: [Word]
  ,nodeCType :: ClosureType
  ,nodePkg   :: String
  ,nodeMod   :: String
  ,nodeName  :: String}
  deriving(Eq,Ord,Read,Show)

emptyHNode :: ClosureType -> HNode
emptyHNode ct = HNode
  {nodePtrs   = []
  ,nodeLits   = []
  ,nodeTag    = 0
  ,nodeIPtr   = 0
  ,nodeICode  = []
  ,nodeCType  = ct
  ,nodePkg    = []
  ,nodeMod    = []
  ,nodeName   = []}

-----------------------------------------------------------------------------

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
        go a = a `seq` do
          ids <- nodeH a
          case ids of
            [] -> return ()
            _  -> mapM_ go =<< mapM getHVal ids

-- | Needed since i don't know of a way
-- to go @a -> HValue@ directly (unsafeCoercing
-- directly doesn't work (i tried)).
data Box a = Box a

-- | Turn the root into an @HValue@ to start off.
rootH :: a -> H HValue
rootH a = let b = a `seq` Box a
          in b `seq` do
            c <- io (getClosureData b)
            case dumpArray (GHC.ptrs c) of
              [hval] -> return hval
              _ -> error "zomg"

-- | Add this @HValue@ to the graph, then
--  add it's successor's not already seen, and
--  return the @HNodeId@'s of these newly-seen nodes
--  (which we've added to the graph in @H@'s state).
--  CURRENTLY CAN'T SEEM TO MANAGE TO NOT ENTER AN
--  ARR_WORDS (e.g. BbyteArray#). THIS IS A PROBLEM
--  FOR LARGE INTEGERS, AMONG OTHER THINGS.
nodeH :: HValue -> H [HNodeId]
nodeH a = a `seq` do
  c <- io (getClosureData a)
  (i, _) <- getId a
  let itab  = infoTable c
      tag   = (fromIntegral . GHC.tipe) itab
      ctype = (toEnum . fromIntegral) tag
  case ctype of
    -- XXX: i think this isn't necessary for BCOs
    BCO -> insertG i (emptyHNode BCO) >> return []
    t | isThunk t -> insertG i (emptyHNode t) >> return []
    _ -> do
    (pkg,mod,name) <- case isFun ctype of
                        False -> io (GHC.dataConInfoPtrToNames (infoPtr c))
                        True -> return ([],[],[])
    let iptr  = (fromIntegral . p2i . infoPtr) c
        ls    = nonPtrs c
    let icode = (fmap fromIntegral . GHC.code) itab
    -- XXX: do something better with the thunks than discarding them
--     xs <- io (filterM (\a -> (not . isBadNews) `fmap` closureType a)
--                       (dumpArray (GHC.ptrs c)))
    ys <- mapM getId (dumpArray (GHC.ptrs c)) -- xs
    let news = (fmap fst . fst . partition snd) ys
        n    = HNode (fmap fst ys) ls tag iptr icode ctype pkg mod name
    insertG i n
    return news

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

------------------------------------------------

look :: HValue -> [(HValue, a)] -> Maybe a
look _      [] = Nothing
look hval ((x,i):xs)
  | hval .==. x = Just i
  | otherwise   = look hval xs

(.==.) :: HValue -> HValue -> Bool
a .==. b = a `seq` b `seq`
  (0 /= I# (reallyUnsafePtrEquality# a b))

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

------------------------------------------------
