
-- | Want this module to be as isolated as possible,
--  due to the extreme volatility of the GHC-API.

module GHC.Vacuum.GHC.Imports (
   module Constants
  ,module GHC.Ptr
  ,module GHC.Prim
  ,module GHC.Exts
  ,module ByteCodeLink
  ,module BCI
  ,module Rt -- RtClosureInspect
  ,module CgInfoTbls
  ,module SMRep
--   ,module GHC
  ,module HscMain
  ,module HscTypes
  ,module DynFlags
  ,module StaticFlags
  ,module SysTools
  ,module Packages
--   ,module PackageConfig
  ,module Distribution.Package
  ,module Name
  ,module Module
  ,module IfaceEnv
--   ,module TcRnMonad
  ,module Outputable
  ,module FastString
--   ,module Util
  ,module Bag
  ,ghciTablesNextToCode
  ,setContext
) where

------------------------------------------------

import Constants

import GHC.Ptr(Ptr(..))
import GHC.Prim
import GHC.Exts(Int(..))

import ByteCodeLink
import ByteCodeItbls as BCI hiding (ptrs)
-- import RtClosureInspect hiding (ClosureType)
import RtClosureInspect as Rt hiding (tipe,ClosureType(..),isFun)
import CgInfoTbls hiding (infoTable)
import SMRep hiding (ClosureType(..))

import GHC (setContext)
-- import GHC hiding (lookupName,compileExpr,LIE)
import HscMain
import HscTypes
import DynFlags
import StaticFlags
import SysTools (initSysTools)

import Packages
-- import PackageConfig
import Distribution.Package (PackageName(..))

import Name
import Module

import IfaceEnv
-- import TcRnMonad hiding (Env)

import Outputable(ppr,showSDoc)
import qualified Outputable as O

import FastString
  hiding (uniq)
import Util (ghciTablesNextToCode)
import Bag
  (unitBag
  ,listToBag
  ,emptyBag
  ,isEmptyBag)

------------------------------------------------
