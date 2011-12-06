module GHC.Vacuum.ClosureType.V702 ( ClosureType(..) ) where


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
  | IND_PERM
  | IND_STATIC
  | RET_BCO
  | RET_SMALL
  | RET_BIG
  | RET_DYN
  | RET_FUN
  | UPDATE_FRAME
  | CATCH_FRAME
  | UNDERFLOW_FRAME
  | STOP_FRAME
  | BLOCKING_QUEUE
  | BLACKHOLE
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
  | PRIM
  | MUT_PRIM
  | TSO
  | STACK
  | TREC_CHUNK
  | ATOMICALLY_FRAME
  | CATCH_RETRY_FRAME
  | CATCH_STM_FRAME
  | WHITEHOLE
  | N_CLOSURE_TYPES
  deriving (Eq, Ord, Show, Read)

instance Enum ClosureType where
  fromEnum INVALID_OBJECT	= 0
  fromEnum CONSTR	= 1
  fromEnum CONSTR_1_0	= 2
  fromEnum CONSTR_0_1	= 3
  fromEnum CONSTR_2_0	= 4
  fromEnum CONSTR_1_1	= 5
  fromEnum CONSTR_0_2	= 6
  fromEnum CONSTR_STATIC	= 7
  fromEnum CONSTR_NOCAF_STATIC	= 8
  fromEnum FUN	= 9
  fromEnum FUN_1_0	= 10
  fromEnum FUN_0_1	= 11
  fromEnum FUN_2_0	= 12
  fromEnum FUN_1_1	= 13
  fromEnum FUN_0_2	= 14
  fromEnum FUN_STATIC	= 15
  fromEnum THUNK	= 16
  fromEnum THUNK_1_0	= 17
  fromEnum THUNK_0_1	= 18
  fromEnum THUNK_2_0	= 19
  fromEnum THUNK_1_1	= 20
  fromEnum THUNK_0_2	= 21
  fromEnum THUNK_STATIC	= 22
  fromEnum THUNK_SELECTOR	= 23
  fromEnum BCO	= 24
  fromEnum AP	= 25
  fromEnum PAP	= 26
  fromEnum AP_STACK	= 27
  fromEnum IND	= 28
  fromEnum IND_PERM	= 29
  fromEnum IND_STATIC	= 30
  fromEnum RET_BCO	= 31
  fromEnum RET_SMALL	= 32
  fromEnum RET_BIG	= 33
  fromEnum RET_DYN	= 34
  fromEnum RET_FUN	= 35
  fromEnum UPDATE_FRAME	= 36
  fromEnum CATCH_FRAME	= 37
  fromEnum UNDERFLOW_FRAME	= 38
  fromEnum STOP_FRAME	= 39
  fromEnum BLOCKING_QUEUE	= 40
  fromEnum BLACKHOLE	= 41
  fromEnum MVAR_CLEAN	= 42
  fromEnum MVAR_DIRTY	= 43
  fromEnum ARR_WORDS	= 44
  fromEnum MUT_ARR_PTRS_CLEAN	= 45
  fromEnum MUT_ARR_PTRS_DIRTY	= 46
  fromEnum MUT_ARR_PTRS_FROZEN0	= 47
  fromEnum MUT_ARR_PTRS_FROZEN	= 48
  fromEnum MUT_VAR_CLEAN	= 49
  fromEnum MUT_VAR_DIRTY	= 50
  fromEnum WEAK	= 51
  fromEnum PRIM	= 52
  fromEnum MUT_PRIM	= 53
  fromEnum TSO	= 54
  fromEnum STACK	= 55
  fromEnum TREC_CHUNK	= 56
  fromEnum ATOMICALLY_FRAME	= 57
  fromEnum CATCH_RETRY_FRAME	= 58
  fromEnum CATCH_STM_FRAME	= 59
  fromEnum WHITEHOLE	= 60
  fromEnum N_CLOSURE_TYPES	= 61

  toEnum 0	= INVALID_OBJECT
  toEnum 1	= CONSTR
  toEnum 2	= CONSTR_1_0
  toEnum 3	= CONSTR_0_1
  toEnum 4	= CONSTR_2_0
  toEnum 5	= CONSTR_1_1
  toEnum 6	= CONSTR_0_2
  toEnum 7	= CONSTR_STATIC
  toEnum 8	= CONSTR_NOCAF_STATIC
  toEnum 9	= FUN
  toEnum 10	= FUN_1_0
  toEnum 11	= FUN_0_1
  toEnum 12	= FUN_2_0
  toEnum 13	= FUN_1_1
  toEnum 14	= FUN_0_2
  toEnum 15	= FUN_STATIC
  toEnum 16	= THUNK
  toEnum 17	= THUNK_1_0
  toEnum 18	= THUNK_0_1
  toEnum 19	= THUNK_2_0
  toEnum 20	= THUNK_1_1
  toEnum 21	= THUNK_0_2
  toEnum 22	= THUNK_STATIC
  toEnum 23	= THUNK_SELECTOR
  toEnum 24	= BCO
  toEnum 25	= AP
  toEnum 26	= PAP
  toEnum 27	= AP_STACK
  toEnum 28	= IND
  toEnum 29	= IND_PERM
  toEnum 30	= IND_STATIC
  toEnum 31	= RET_BCO
  toEnum 32	= RET_SMALL
  toEnum 33	= RET_BIG
  toEnum 34	= RET_DYN
  toEnum 35	= RET_FUN
  toEnum 36	= UPDATE_FRAME
  toEnum 37	= CATCH_FRAME
  toEnum 38	= UNDERFLOW_FRAME
  toEnum 39	= STOP_FRAME
  toEnum 40	= BLOCKING_QUEUE
  toEnum 41	= BLACKHOLE
  toEnum 42	= MVAR_CLEAN
  toEnum 43	= MVAR_DIRTY
  toEnum 44	= ARR_WORDS
  toEnum 45	= MUT_ARR_PTRS_CLEAN
  toEnum 46	= MUT_ARR_PTRS_DIRTY
  toEnum 47	= MUT_ARR_PTRS_FROZEN0
  toEnum 48	= MUT_ARR_PTRS_FROZEN
  toEnum 49	= MUT_VAR_CLEAN
  toEnum 50	= MUT_VAR_DIRTY
  toEnum 51	= WEAK
  toEnum 52	= PRIM
  toEnum 53	= MUT_PRIM
  toEnum 54	= TSO
  toEnum 55	= STACK
  toEnum 56	= TREC_CHUNK
  toEnum 57	= ATOMICALLY_FRAME
  toEnum 58	= CATCH_RETRY_FRAME
  toEnum 59	= CATCH_STM_FRAME
  toEnum 60	= WHITEHOLE
  toEnum 61	= N_CLOSURE_TYPES
  toEnum n	= error ("toEnum: ClosureType: invalid ClosureType: " ++ show n)

