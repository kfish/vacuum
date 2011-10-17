module GHC.Vacuum.ClosureType (
   isFun,isThunk,isCon
  ,ClosureType(..)
) where

------------------------------------------------

isFun :: ClosureType -> Bool
isFun FUN = True
isFun FUN_1_0 = True
isFun FUN_0_1 = True
isFun FUN_2_0 = True
isFun FUN_1_1 = True
isFun FUN_0_2 = True
isFun FUN_STATIC = True
isFun _ = False

isThunk :: ClosureType -> Bool
isThunk AP = True
isThunk THUNK = True
isThunk THUNK_1_0 = True
isThunk THUNK_0_1 = True
isThunk THUNK_2_0 = True
isThunk THUNK_1_1 = True
isThunk THUNK_0_2 = True
isThunk THUNK_STATIC = True
isThunk THUNK_SELECTOR = True
isThunk _ = False

isCon :: ClosureType -> Bool
isCon CONSTR = True
isCon CONSTR_1_0 = True
isCon CONSTR_0_1 = True
isCon CONSTR_2_0 = True
isCon CONSTR_1_1 = True
isCon CONSTR_0_2 = True
isCon CONSTR_STATIC = True
isCon CONSTR_NOCAF_STATIC = True
isCon _ = False

------------------------------------------------

data ClosureType
  = INVALID_OBJECT
  | CONSTR
  | CONSTR_1_0
  | CONSTR_0_1
  | CONSTR_2_0
  | CONSTR_1_1
  | CONSTR_0_2
  | CONSTR_STATIC
  | CONSTR_NOCAF_STATIC
  | FUN
  | FUN_1_0
  | FUN_0_1
  | FUN_2_0
  | FUN_1_1
  | FUN_0_2
  | FUN_STATIC
  | THUNK
  | THUNK_1_0
  | THUNK_0_1
  | THUNK_2_0
  | THUNK_1_1
  | THUNK_0_2
  | THUNK_STATIC
  | THUNK_SELECTOR
  | BCO
  | AP
  | PAP
  | AP_STACK
  | IND
  | IND_OLDGEN
  | IND_PERM
  | IND_OLDGEN_PERM
  | IND_STATIC
  | RET_BCO
  | RET_SMALL
  | RET_BIG
  | RET_DYN
  | RET_FUN
  | UPDATE_FRAME
  | CATCH_FRAME
  | STOP_FRAME
  | CAF_BLACKHOLE
  | BLACKHOLE
  | SE_BLACKHOLE
  | SE_CAF_BLACKHOLE
  | MVAR_CLEAN
  | MVAR_DIRTY
  | ARR_WORDS
  | MUT_ARR_PTRS_CLEAN
  | MUT_ARR_PTRS_DIRTY
  | MUT_ARR_PTRS_FROZEN0
  | MUT_ARR_PTRS_FROZEN
  | MUT_VAR_CLEAN
  | MUT_VAR_DIRTY
  | WEAK
  | STABLE_NAME
  | TSO
  | BLOCKED_FETCH
  | FETCH_ME
  | FETCH_ME_BQ
  | RBH
  | REMOTE_REF
  | TVAR_WATCH_QUEUE
  | INVARIANT_CHECK_QUEUE
  | ATOMIC_INVARIANT
  | TVAR
  | TREC_CHUNK
  | TREC_HEADER
  | ATOMICALLY_FRAME
  | CATCH_RETRY_FRAME
  | CATCH_STM_FRAME
  | WHITEHOLE
  deriving(Eq,Ord,Read,Show)

instance Enum ClosureType where
  fromEnum INVALID_OBJECT          = 0
  fromEnum CONSTR                  = 1
  fromEnum CONSTR_1_0              = 2
  fromEnum CONSTR_0_1              = 3
  fromEnum CONSTR_2_0              = 4
  fromEnum CONSTR_1_1              = 5
  fromEnum CONSTR_0_2              = 6
  fromEnum CONSTR_STATIC           = 7
  fromEnum CONSTR_NOCAF_STATIC     = 8
  fromEnum FUN                     = 9
  fromEnum FUN_1_0                 = 10
  fromEnum FUN_0_1                 = 11
  fromEnum FUN_2_0                 = 12
  fromEnum FUN_1_1                 = 13
  fromEnum FUN_0_2                 = 14
  fromEnum FUN_STATIC              = 15
  fromEnum THUNK                   = 16
  fromEnum THUNK_1_0               = 17
  fromEnum THUNK_0_1               = 18
  fromEnum THUNK_2_0               = 19
  fromEnum THUNK_1_1               = 20
  fromEnum THUNK_0_2               = 21
  fromEnum THUNK_STATIC            = 22
  fromEnum THUNK_SELECTOR          = 23
  fromEnum BCO                     = 24
  fromEnum AP                      = 25
  fromEnum PAP                     = 26
  fromEnum AP_STACK                = 27
  fromEnum IND                     = 28
  fromEnum IND_OLDGEN              = 29
  fromEnum IND_PERM                = 30
  fromEnum IND_OLDGEN_PERM         = 31
  fromEnum IND_STATIC              = 32
  fromEnum RET_BCO                 = 33
  fromEnum RET_SMALL               = 34
  fromEnum RET_BIG                 = 35
  fromEnum RET_DYN                 = 36
  fromEnum RET_FUN                 = 37
  fromEnum UPDATE_FRAME            = 38
  fromEnum CATCH_FRAME             = 39
  fromEnum STOP_FRAME              = 40
  fromEnum CAF_BLACKHOLE           = 41
  fromEnum BLACKHOLE               = 42
  fromEnum SE_BLACKHOLE            = 43
  fromEnum SE_CAF_BLACKHOLE        = 44
  fromEnum MVAR_CLEAN              = 45
  fromEnum MVAR_DIRTY              = 46
  fromEnum ARR_WORDS               = 47
  fromEnum MUT_ARR_PTRS_CLEAN      = 48
  fromEnum MUT_ARR_PTRS_DIRTY      = 49
  fromEnum MUT_ARR_PTRS_FROZEN0    = 50
  fromEnum MUT_ARR_PTRS_FROZEN     = 51
  fromEnum MUT_VAR_CLEAN           = 52
  fromEnum MUT_VAR_DIRTY           = 53
  fromEnum WEAK                    = 54
  fromEnum STABLE_NAME             = 55
  fromEnum TSO                     = 56
  fromEnum BLOCKED_FETCH           = 57
  fromEnum FETCH_ME                = 58
  fromEnum FETCH_ME_BQ             = 59
  fromEnum RBH                     = 60
  fromEnum REMOTE_REF              = 62
  fromEnum TVAR_WATCH_QUEUE        = 63
  fromEnum INVARIANT_CHECK_QUEUE   = 64
  fromEnum ATOMIC_INVARIANT        = 65
  fromEnum TVAR                    = 66
  fromEnum TREC_CHUNK              = 67
  fromEnum TREC_HEADER             = 68
  fromEnum ATOMICALLY_FRAME        = 69
  fromEnum CATCH_RETRY_FRAME       = 70
  fromEnum CATCH_STM_FRAME         = 71
  fromEnum WHITEHOLE               = 72
  toEnum 0 = INVALID_OBJECT
  toEnum 1 = CONSTR
  toEnum 2 = CONSTR_1_0
  toEnum 3 = CONSTR_0_1
  toEnum 4 = CONSTR_2_0
  toEnum 5 = CONSTR_1_1
  toEnum 6 = CONSTR_0_2
  toEnum 7 = CONSTR_STATIC
  toEnum 8 = CONSTR_NOCAF_STATIC
  toEnum 9 = FUN
  toEnum 10 = FUN_1_0
  toEnum 11 = FUN_0_1
  toEnum 12 = FUN_2_0
  toEnum 13 = FUN_1_1
  toEnum 14 = FUN_0_2
  toEnum 15 = FUN_STATIC
  toEnum 16 = THUNK
  toEnum 17 = THUNK_1_0
  toEnum 18 = THUNK_0_1
  toEnum 19 = THUNK_2_0
  toEnum 20 = THUNK_1_1
  toEnum 21 = THUNK_0_2
  toEnum 22 = THUNK_STATIC
  toEnum 23 = THUNK_SELECTOR
  toEnum 24 = BCO
  toEnum 25 = AP
  toEnum 26 = PAP
  toEnum 27 = AP_STACK
  toEnum 28 = IND
  toEnum 29 = IND_OLDGEN
  toEnum 30 = IND_PERM
  toEnum 31 = IND_OLDGEN_PERM
  toEnum 32 = IND_STATIC
  toEnum 33 = RET_BCO
  toEnum 34 = RET_SMALL
  toEnum 35 = RET_BIG
  toEnum 36 = RET_DYN
  toEnum 37 = RET_FUN
  toEnum 38 = UPDATE_FRAME
  toEnum 39 = CATCH_FRAME
  toEnum 40 = STOP_FRAME
  toEnum 41 = CAF_BLACKHOLE
  toEnum 42 = BLACKHOLE
  toEnum 43 = SE_BLACKHOLE
  toEnum 44 = SE_CAF_BLACKHOLE
  toEnum 45 = MVAR_CLEAN
  toEnum 46 = MVAR_DIRTY
  toEnum 47 = ARR_WORDS
  toEnum 48 = MUT_ARR_PTRS_CLEAN
  toEnum 49 = MUT_ARR_PTRS_DIRTY
  toEnum 50 = MUT_ARR_PTRS_FROZEN0
  toEnum 51 = MUT_ARR_PTRS_FROZEN
  toEnum 52 = MUT_VAR_CLEAN
  toEnum 53 = MUT_VAR_DIRTY
  toEnum 54 = WEAK
  toEnum 55 = STABLE_NAME
  toEnum 56 = TSO
  toEnum 57 = BLOCKED_FETCH
  toEnum 58 = FETCH_ME
  toEnum 59 = FETCH_ME_BQ
  toEnum 60 = RBH
  toEnum 62 = REMOTE_REF
  toEnum 63 = TVAR_WATCH_QUEUE
  toEnum 64 = INVARIANT_CHECK_QUEUE
  toEnum 65 = ATOMIC_INVARIANT
  toEnum 66 = TVAR
  toEnum 67 = TREC_CHUNK
  toEnum 68 = TREC_HEADER
  toEnum 69 = ATOMICALLY_FRAME
  toEnum 70 = CATCH_RETRY_FRAME
  toEnum 71 = CATCH_STM_FRAME
  toEnum 72 = WHITEHOLE
  toEnum n  = error ("toEnum: ClosureType: invalid ClosureType: " ++ show n)

