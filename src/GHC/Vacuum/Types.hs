{-# LANGUAGE TypeSynonymInstances #-}

module GHC.Vacuum.Types (
   module GHC.Vacuum.Types
) where

import GHC.Vacuum.ClosureType
import GHC.Vacuum.Internal(HValue)

import Data.List
import Data.Word
import Data.IntMap(IntMap)
import Data.Monoid(Monoid(..))
import qualified Data.IntMap as IM
import System.Mem.StableName

------------------------------------------------

type HNodeId = Int

data HNode = HNode
  {nodePtrs  :: [HNodeId]
  ,nodeLits  :: [Word]
  ,nodeInfo  :: InfoTab}
  deriving(Eq,Ord,Read,Show)

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

summary :: HNode -> ([String],[HNodeId],[Word])
summary (HNode ps ls info) = case itabName info of
                              (a,b,c) -> ([a,b,c],ps,ls)

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

-- A box for safe deposit of HValues
data Box a = Box a

------------------------------------------------

data Env = Env
  {uniq  :: HNodeId
    -- the keys are hashes of StableNames
  ,seen  :: IntMap [(StableName HValue,HNodeId)]
  ,hvals :: IntMap (Box HValue)
  ,graph :: IntMap HNode}

emptyEnv :: Env
emptyEnv = Env
  {uniq = 0
  ,seen = mempty
  ,hvals = mempty
  ,graph = mempty}

------------------------------------------------
