
-- | Want this module to be as isolated as possible,
--  due to the extreme volatility of the GHC-API.

module GHC.Vacuum.GHC.Internal (
   GhcApiCfg(..)
  ,defaultGhcApiConfig
  ,withGhcApiCfg
  ,dynFlagsOn,dynFlagsOff
  ,defaultEnv,newEnv,myRunGhc
  ,CabalPkg(..)
  ,CabalPkgId
  ,CabalPkgVersion
  ,CabalModuleId
  ,CabalModule(..)
  ,cabalModulePkgId
  ,cabalModulePkgVersion
  ,cabalModuleModuleId
  ,preludeCM
  ,collectCabalModules
  ,cabalPkgToModules
  ,dataConInfoPtrToNames
) where

import GHC.Paths(libdir)
import GHC.Vacuum.GHC.Imports as Imports

import Data.Char
import Data.Word
import Data.List
import Data.IORef
import Data.Array.IArray
import Control.Monad
import Foreign

import Data.List
import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Monoid(Monoid(..))

------------------------------------------------



------------------------------------------------

data GhcApiCfg = GhcApiCfg
  {ghcApiLibDir :: FilePath
  ,ghcApiImports :: [CabalPkg]
  ,ghcApiDynFlagsOn :: [DynFlag]
  ,ghcApiDynFlagsOff :: [DynFlag]}
  deriving(Eq,Ord,Read,Show)

deriving instance Ord DynFlag
deriving instance Read DynFlag

defaultGhcApiConfig :: GhcApiCfg
defaultGhcApiConfig = GhcApiCfg
  {ghcApiLibDir = GHC.Paths.libdir
  ,ghcApiImports
        -- e.g.
      = CabalPkg "base" [] ["Prelude"]
          : collectCabalModules
              [CabalModule "base" [] "Prelude"
              ,CabalModule "base" [] "Prelude"]
  ,ghcApiDynFlagsOn
      = [Opt_TemplateHaskell
        ,Opt_QuasiQuotes
        ,Opt_ViewPatterns
        ,Opt_RankNTypes
        ,Opt_KindSignatures
        ,Opt_UnicodeSyntax
          -- um, i assume this turn it _off_ (?)
        ,Opt_MonomorphismRestriction
        ,Opt_PatternGuards
        ,Opt_ParallelListComp
        ,Opt_ImplicitParams
        ,Opt_BangPatterns]
  ,ghcApiDynFlagsOff
      = [Opt_PrintBindResult
        ,Opt_PrintBindContents
        ,Opt_PrintEvldWithShow]}

withGhcApiCfg :: GhcApiCfg
              -> (FilePath -> DynFlags -> [Module] -> o)
              -> (DynFlags -> o)
withGhcApiCfg (GhcApiCfg
                    libdir
                    imports
                    ons offs) k dflags = k libdir
                                           ((dynFlagsOn ons
                                              . dynFlagsOff offs) dflags)
                                           (concatMap cabalPkgToModules imports)

dynFlagsOn  :: [DynFlag] -> (DynFlags -> DynFlags)
dynFlagsOn = flip (foldl dopt_set)

dynFlagsOff :: [DynFlag] -> (DynFlags -> DynFlags)
dynFlagsOff = flip (foldl dopt_unset)

------------------------------------------------



------------------------------------------------

defaultEnv :: IO HscEnv
defaultEnv = newEnv defaultGhcApiConfig
                    (Just defaultDynFlags)

newEnv :: GhcApiCfg -> Maybe DynFlags -> IO HscEnv
newEnv cfg dflagsM
  = let
        initEnv :: HscEnv -> [Module] -> IO HscEnv
        initEnv hsc modules = do
          let dflags = hsc_dflags hsc
          (dflags', preload) <- initPackages
                    (dflags{ghcLink=LinkInMemory})
          let hsc' = hsc{hsc_dflags = dflags'}
          myRunGhc hsc' (setContext [] modules)
          return hsc'

        newEnv' :: Maybe FilePath -> DynFlags -> IO HscEnv
        newEnv' mb_top_dir dflags00 = do
          initStaticOpts
          dflags0 <- initDynFlags dflags00
          dflags  <- initSysTools mb_top_dir dflags0
          hsc <- newHscEnv dflags
          return hsc

    in withGhcApiCfg cfg (\libdir dflags modules ->
           do env <- newEnv' (Just libdir) dflags
              env' <- initEnv env modules
              return env')
          (maybe defaultDynFlags id dflagsM)

-- | Escape this hideous Ghc monad :-)
myRunGhc :: HscEnv -> Ghc a -> IO a
myRunGhc hsc_env ghc = do
  wref <- newIORef emptyBag
  ref <- newIORef hsc_env
  unGhc ghc (Session ref wref)

------------------------------------------------



------------------------------------------------

data CabalPkg = CabalPkg
  {cabalPkgPkg     :: CabalPkgId
  ,cabalPkgVersion :: CabalPkgVersion
  ,cabalPkgModules :: [CabalModuleId]}
  deriving(Eq,Ord,Read,Show)

type CabalPkgId      = String
type CabalPkgVersion = [Int]
type CabalModuleId   = String

data CabalModule = CabalModule
                    CabalPkgId
                    CabalPkgVersion
                    CabalModuleId
  deriving(Eq,Ord,Read,Show)

cabalModulePkgId      :: CabalModule -> CabalPkgId
cabalModulePkgVersion :: CabalModule -> CabalPkgVersion
cabalModuleModuleId   :: CabalModule -> CabalModuleId

cabalModulePkgId      (CabalModule x _ _) = x
cabalModulePkgVersion (CabalModule _ x _) = x
cabalModuleModuleId   (CabalModule _ _ x) = x

preludeCM :: CabalModule
preludeCM = CabalModule "base" [] "Prelude"

collectCabalModules :: [CabalModule] -> [CabalPkg]
collectCabalModules
  = let f &&& g = \x -> (f x, g x)
        keyify = cabalModulePkgId
                  &&& cabalModulePkgVersion
        elemify = S.singleton . cabalModuleModuleId
        toPkg ((pid,v),ms) = CabalPkg pid v (S.toList ms)
        collect (<>) f g = M.toList . flip foldl' mempty
                            (\m a -> M.insertWith' (<>) (f a)
                                                        (g a) m)
    in fmap toPkg . collect S.union keyify elemify

cabalPkgToModules :: CabalPkg -> [Module]
cabalPkgToModules (CabalPkg
                    pid
                    ver
                    mods) = fmap (mkModule
                                    (mkPackageId
                                      (PackageIdentifier
                                        (PackageName pid)
                                        (Version ver [])))
                                    . mkModuleName) mods

------------------------------------------------



------------------------------------------------

-- * This section is taken from Linker.lhs

-- %
-- % (c) The University of Glasgow 2005-2006
-- %

-- | Given a data constructor in the heap, find its Name.
--   The info tables for data constructors have a field which records
--   the source name of the constructor as a Ptr Word8 (UTF-8 encoded
--   string). The format is:
--
--    Package:Module.Name
--
--   We use this string to lookup the interpreter's internal representation of the name
--   using the lookupOrig.

dataConInfoPtrToNames :: Ptr () -> IO (String, String, String) -- (Either String Name) -- TcM (Either String Name)
dataConInfoPtrToNames x = do
   readIORef justToInitGhc
   initStaticOpts
   theString <- do -- liftIO $ do
      let ptr = castPtr x :: Ptr StgInfoTable
      conDescAddress <- getConDescAddress ptr
      peekArray0 0 conDescAddress
   let (pkg, mod, occ) = parse theString 
       pkgFS = mkFastStringByteList pkg
       modFS = mkFastStringByteList mod
       occFS = mkFastStringByteList occ
       occName = mkOccNameFS dataName occFS
       modName = mkModule (fsToPackageId pkgFS) (mkModuleNameFS modFS)
   return ((packageIdString . modulePackageId) modName
          ,(moduleNameString . moduleName) modName
          ,occNameString occName)
--    return (showSDoc $ ppr modName O.<> O.dot O.<> ppr occName)
   -- return (Left$ showSDoc$ ppr modName O.<> O.dot O.<> ppr occName)
    -- `recoverM` (Right `fmap` lookupOrig modName occName)


-- | This is needed to make sure that GHC is all initialized with its
--  plethora of well-hidden and ill-documented global vars. I'm not
--  bothering to NOINLINE it because i like to live dangerously.
--  (clearly i'm beligerent at this point).
justToInitGhc :: IORef HscEnv
justToInitGhc = unsafePerformIO (newIORef =<< defaultEnv)


   {- To find the string in the constructor's info table we need to consider
      the layout of info tables relative to the entry code for a closure.

      An info table can be next to the entry code for the closure, or it can
      be separate. The former (faster) is used in registerised versions of ghc,
      and the latter (portable) is for non-registerised versions.

      The diagrams below show where the string is to be found relative to
      the normal info table of the closure.

      1) Code next to table:

         --------------
         |            |   <- pointer to the start of the string
         --------------
         |            |   <- the (start of the) info table structure
         |            |
         |            |
         --------------
         | entry code |
         |    ....    |

         In this case the pointer to the start of the string can be found in
         the memory location _one word before_ the first entry in the normal info
         table.

      2) Code NOT next to table:

                                 --------------
         info table structure -> |     *------------------> --------------
                                 |            |             | entry code |
                                 |            |             |    ....    |
                                 --------------
         ptr to start of str ->  |            |
                                 --------------

         In this case the pointer to the start of the string can be found
         in the memory location: info_table_ptr + info_table_size
   -}

getConDescAddress :: Ptr StgInfoTable -> IO (Ptr Word8)
getConDescAddress ptr
  | ghciTablesNextToCode = do
      offsetToString <- peek (ptr `plusPtr` (negate wORD_SIZE))
      return $ (ptr `plusPtr` stdInfoTableSizeB)
                `plusPtr` (fromIntegral (offsetToString :: StgWord))
  | otherwise = peek . intPtrToPtr
                  . (+ fromIntegral
                        stdInfoTableSizeB)
                    . ptrToIntPtr $ ptr
   -- parsing names is a little bit fiddly because we have a string in the form: 
   -- pkg:A.B.C.foo, and we want to split it into three parts: ("pkg", "A.B.C", "foo").
   -- Thus we split at the leftmost colon and the rightmost occurrence of the dot.
   -- It would be easier if the string was in the form pkg:A.B.C:foo, but alas
   -- this is not the conventional way of writing Haskell names. We stick with
   -- convention, even though it makes the parsing code more troublesome.
   -- Warning: this code assumes that the string is well formed. XXXXXXXXXXXXXXXXXXX
parse :: [Word8] -> ([Word8], [Word8], [Word8])
parse input = if not . all (>0) . fmap length $ [pkg,mod,occ]
                then (error . concat)
                        ["getConDescAddress:parse:"
                        ,"(not . all (>0) . fmap le"
                        ,"ngth $ [pkg,mod,occ]"]
                else (pkg, mod, occ)
--   = ASSERT (all (>0) (map length [pkg, mod, occ])) (pkg, mod, occ)   -- XXXXXXXXXXXXXXXX
  where
        (pkg, rest1) = break (== fromIntegral (ord ':')) input
        (mod, occ)
            = (concat $ intersperse [dot] $ reverse modWords, occWord)
            where
            (modWords, occWord) = if (length rest1 < 1) --  XXXXXXXXx YUKX
                                    then error "getConDescAddress:parse:length rest1 < 1"
                                    else parseModOcc [] (tail rest1)
        -- ASSERT (length rest1 > 0) (parseModOcc [] (tail rest1))
        dot = fromIntegral (ord '.')
        parseModOcc :: [[Word8]] -> [Word8] -> ([[Word8]], [Word8])
        parseModOcc acc str
            = case break (== dot) str of
                (top, []) -> (acc, top)
                (top, _:bot) -> parseModOcc (top : acc) bot

------------------------------------------------
