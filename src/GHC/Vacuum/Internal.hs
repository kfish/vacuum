{-# LANGUAGE CPP #-}

module GHC.Vacuum.Internal (
   HValue
  ,HalfWord
  ,ItblCode
  ,StgInfoTable(..)
  ,ghciTablesNextToCode
  ,dataConInfoPtrToNames
  ,wORD_SIZE
  ,hALF_WORD_SIZE
  ,S(..),get,gets,set,io,modify,runS
) where

import Data.Char
import Data.Word
import Data.List
import Data.IORef
import Data.Array.IArray
import Control.Monad
import Control.Monad.Fix
import Foreign

import Data.List
import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Monoid(Monoid(..))

import GHC.Prim
import GHC.Exts

#include "ghcplatform.h"
#include "ghcautoconf.h"
#define GHCI_TABLES_NEXT_TO_CODE
        -- is there somewhere to get this define?

-----------------------------------------------------------------------------

-- * Fabricate what we need to avoid the ghc pkg dep

type HValue = Any

#if SIZEOF_VOID_P == 8
type HalfWord = Word32
#else
type HalfWord = Word16
#endif

-- | From SMRep
type ByteOff = Int

-- | From SMRep
type WordOff = Int

-- | From SMRep
type StgWord = Word

-- hmmmmmm. Is there any way to tell this?
opt_SccProfilingOn = False

-- ghci> wORD_SIZE
-- 8
-- ghci> sizeOf (undefined :: Word)
-- 8
wORD_SIZE :: Int
wORD_SIZE = sizeOf (undefined :: Word)

hALF_WORD_SIZE :: Int
hALF_WORD_SIZE  = wORD_SIZE `div` 2

-- | This is currently always True since
-- i'm not sure how to get at the CPP define
-- \"GHCI_TABLES_NEXT_TO_CODE\" (or equiv) to tell.
ghciTablesNextToCode :: Bool
#ifdef GHCI_TABLES_NEXT_TO_CODE
ghciTablesNextToCode = True
#else
ghciTablesNextToCode = False
#endif

-----------------------------------------------------------------------------

data StgInfoTable = StgInfoTable {
#ifndef GHCI_TABLES_NEXT_TO_CODE
   entry  :: Ptr (),
#endif
   ptrs   :: HalfWord,
   nptrs  :: HalfWord,
   tipe   :: HalfWord,
   srtlen :: HalfWord
#ifdef GHCI_TABLES_NEXT_TO_CODE
 , code   :: [ItblCode]
#endif
  }

instance Storable StgInfoTable where

   sizeOf itbl
      = sum
        [
#ifndef GHCI_TABLES_NEXT_TO_CODE
         fieldSz entry itbl,
#endif
         fieldSz ptrs itbl,
         fieldSz nptrs itbl,
         fieldSz tipe itbl,
         fieldSz srtlen itbl
#ifdef GHCI_TABLES_NEXT_TO_CODE
        ,fieldSz (head.code) itbl * itblCodeLength
#endif
        ]

   alignment itbl
      = SIZEOF_VOID_P

   poke a0 itbl
      = flip evalS (castPtr a0)
      $ do
#ifndef GHCI_TABLES_NEXT_TO_CODE
           store (entry  itbl)
#endif
           store (ptrs   itbl)
           store (nptrs  itbl)
           store (tipe   itbl)
           store (srtlen itbl)
#ifdef GHCI_TABLES_NEXT_TO_CODE
           sequence_ (map store (code itbl))
#endif

   peek a0
      = flip evalS (castPtr a0)
      $ do
#ifndef GHCI_TABLES_NEXT_TO_CODE
           entry  <- load
#endif
           ptrs   <- load
           nptrs  <- load
           tipe   <- load
           srtlen <- load
#ifdef GHCI_TABLES_NEXT_TO_CODE
           code   <- sequence (replicate itblCodeLength load)
#endif
           return
              StgInfoTable {
#ifndef GHCI_TABLES_NEXT_TO_CODE
                 entry  = entry,
#endif
                 ptrs   = ptrs,
                 nptrs  = nptrs,
                 tipe   = tipe,
                 srtlen = srtlen
#ifdef GHCI_TABLES_NEXT_TO_CODE
                ,code   = code
#endif
              }

fieldSz :: (Storable a, Storable b) => (a -> b) -> a -> Int
fieldSz sel x = sizeOf (sel x)

type PtrIO = S (Ptr Word8)

advance :: Storable a => PtrIO (Ptr a)
advance = S adv where
    adv k addr = case castPtr addr of
                  addrCast -> k addrCast
                                (addr `plusPtr`
                                  sizeOfPointee addrCast)

sizeOfPointee :: (Storable a) => Ptr a -> Int
sizeOfPointee addr = sizeOf (typeHack addr)
  where typeHack = undefined  :: Ptr a -> a

store :: Storable a => a -> PtrIO ()
store x = do addr <- advance
             io (poke addr x)

load :: Storable a => PtrIO a
load = do addr <- advance
          io (peek addr)

newtype S s a = S {unS :: forall o. (a -> s -> IO o) -> s -> IO o}
instance Functor (S s) where
  fmap f (S g) = S (\k -> g (k . f))
instance Monad (S s) where
  return a = S (\k -> k a)
  S g >>= f = S (\k -> g (\a -> unS (f a) k))
instance MonadFix (S s) where
  mfix f = S (\k s ->
    uncurry k =<< mfix (\ ~(a,_) ->
    -- the lazy pattern is ESSENTIAL, otherwise <<loop>>
      unS (f a) (\a s -> return (a,s)) s))
get :: S s s
get = S (\k s -> k s s)
gets :: (s -> a) -> S s a
gets f = S (\k s -> k (f s) s)
set :: s -> S s ()
set s = S (\k _ -> k () s)
io :: IO a -> S s a
io m = S (\k s -> flip k s =<< m)
modify :: (s -> s) -> S s ()
modify f = S (\k -> k () . f)
runS :: S s a -> s -> IO (a, s)
runS (S g) = g (\a -> return . (,) a)
evalS :: S s a -> s -> IO a
evalS (S g) = g (\a _ -> return a)
execS :: S s a -> s -> IO s
execS (S g) = g (\_ -> return)

-----------------------------------------------------------------------------

-- VACUUM: All this just to get itblCodeLength.

-- Make code which causes a jump to the given address.  This is the
-- only arch-dependent bit of the itbl story.  The returned list is
-- itblCodeLength elements (bytes) long.

-- For sparc_TARGET_ARCH, i386_TARGET_ARCH, etc.
-- #include "nativeGen/NCG.h"
-- VACUUM: we get *_TARGET_ARCH from ghcplatform.h instead

itblCodeLength :: Int
itblCodeLength = length (mkJumpToAddr undefined)

mkJumpToAddr :: Ptr () -> [ItblCode]

ptrToInt (Ptr a#) = I# (addr2Int# a#)

#if sparc_TARGET_ARCH
-- After some consideration, we'll try this, where
-- 0x55555555 stands in for the address to jump to.
-- According to ghc/includes/MachRegs.h, %g3 is very
-- likely indeed to be baggable.
--
--   0000 07155555              sethi   %hi(0x55555555), %g3
--   0004 8610E155              or      %g3, %lo(0x55555555), %g3
--   0008 81C0C000              jmp     %g3
--   000c 01000000              nop

type ItblCode = Word32
mkJumpToAddr a
   = let w32 = fromIntegral (ptrToInt a)

         hi22, lo10 :: Word32 -> Word32
         lo10 x = x .&. 0x3FF
         hi22 x = (x `shiftR` 10) .&. 0x3FFFF

     in  [ 0x07000000 .|. (hi22 w32),
           0x8610E000 .|. (lo10 w32),
           0x81C0C000,
           0x01000000 ]

#elif powerpc_TARGET_ARCH
-- We'll use r12, for no particular reason.
-- 0xDEADBEEF stands for the adress:
-- 3D80DEAD lis r12,0xDEAD
-- 618CBEEF ori r12,r12,0xBEEF
-- 7D8903A6 mtctr r12
-- 4E800420 bctr

type ItblCode = Word32
mkJumpToAddr a =
    let w32 = fromIntegral (ptrToInt a)
  hi16 x = (x `shiftR` 16) .&. 0xFFFF
  lo16 x = x .&. 0xFFFF
    in  [
  0x3D800000 .|. hi16 w32,
  0x618C0000 .|. lo16 w32,
  0x7D8903A6, 0x4E800420
  ]

#elif i386_TARGET_ARCH
-- Let the address to jump to be 0xWWXXYYZZ.
-- Generate   movl $0xWWXXYYZZ,%eax  ;  jmp *%eax
-- which is
-- B8 ZZ YY XX WW FF E0

type ItblCode = Word8
mkJumpToAddr a
   = let w32 = fromIntegral (ptrToInt a) :: Word32
         insnBytes :: [Word8]
         insnBytes
            = [0xB8, byte0 w32, byte1 w32, 
                     byte2 w32, byte3 w32, 
               0xFF, 0xE0]
     in
         insnBytes

#elif x86_64_TARGET_ARCH
-- Generates:
--  jmpq *.L1(%rip)
--  .align 8
-- .L1:
--  .quad <addr>
--
-- We need a full 64-bit pointer (we can't assume the info table is
-- allocated in low memory).  Assuming the info pointer is aligned to
-- an 8-byte boundary, the addr will also be aligned.

type ItblCode = Word8
mkJumpToAddr a
   = let w64 = fromIntegral (ptrToInt a) :: Word64
         insnBytes :: [Word8]
         insnBytes
            = [0xff, 0x25, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00,
                byte0 w64, byte1 w64, byte2 w64, byte3 w64,
                byte4 w64, byte5 w64, byte6 w64, byte7 w64]
     in
         insnBytes

#elif alpha_TARGET_ARCH
type ItblCode = Word32
mkJumpToAddr a
    = [ 0xc3800000      -- br   at, .+4
      , 0xa79c000c      -- ldq  at, 12(at)
      , 0x6bfc0000      -- jmp  (at)    # with zero hint -- oh well
      , 0x47ff041f      -- nop
      , fromIntegral (w64 .&. 0x0000FFFF)
      , fromIntegral ((w64 `shiftR` 32) .&. 0x0000FFFF) ]
    where w64 = fromIntegral (ptrToInt a) :: Word64

#else
type ItblCode = Word32
mkJumpToAddr a
    = undefined
#endif

byte0, byte1, byte2, byte3, byte4, byte5, byte6, byte7
   :: (Integral w, Bits w) => w -> Word8
byte0 w = fromIntegral w
byte1 w = fromIntegral (w `shiftR` 8)
byte2 w = fromIntegral (w `shiftR` 16)
byte3 w = fromIntegral (w `shiftR` 24)
byte4 w = fromIntegral (w `shiftR` 32)
byte5 w = fromIntegral (w `shiftR` 40)
byte6 w = fromIntegral (w `shiftR` 48)
byte7 w = fromIntegral (w `shiftR` 56)

-----------------------------------------------------------------------------
--
--  Info table offsets
--
-----------------------------------------------------------------------------

stdInfoTableSizeW :: WordOff
-- The size of a standard info table varies with profiling/ticky etc,
-- so we can't get it from Constants
-- It must vary in sync with mkStdInfoTable
stdInfoTableSizeW
  = size_fixed + size_prof
  where
    size_fixed = 2  -- layout, type
    size_prof | opt_SccProfilingOn = 2
              | otherwise    = 0

stdInfoTableSizeB :: ByteOff
stdInfoTableSizeB = stdInfoTableSizeW * wORD_SIZE

stdSrtBitmapOffset :: ByteOff
-- Byte offset of the SRT bitmap half-word which is 
-- in the *higher-addressed* part of the type_lit
stdSrtBitmapOffset = stdInfoTableSizeB - hALF_WORD_SIZE

stdClosureTypeOffset :: ByteOff
-- Byte offset of the closure type half-word 
stdClosureTypeOffset = stdInfoTableSizeB - wORD_SIZE

stdPtrsOffset, stdNonPtrsOffset :: ByteOff
stdPtrsOffset    = stdInfoTableSizeB - 2*wORD_SIZE
stdNonPtrsOffset = stdInfoTableSizeB - 2*wORD_SIZE + hALF_WORD_SIZE

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

b2s :: [Word8] -> String
b2s = fmap (chr . fromIntegral)

dataConInfoPtrToNames :: Ptr () -> IO (String, String, String)
dataConInfoPtrToNames x = do
  let ptr = castPtr x :: Ptr StgInfoTable
  conDescAddress <- getConDescAddress ptr
  theString <- peekArray0 0 conDescAddress
  let (pkg, mod, occ) = parse theString
  return (b2s pkg, b2s mod, b2s occ)

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
