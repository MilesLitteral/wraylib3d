data PlatformConstants = PlatformConstants {
      pc_CONTROL_GROUP_CONST_291 :: Int
    , pc_STD_HDR_SIZE :: Int
    , pc_PROF_HDR_SIZE :: Int
    , pc_BLOCK_SIZE :: Int
    , pc_BLOCKS_PER_MBLOCK :: Int
    , pc_TICKY_BIN_COUNT :: Int
    , pc_OFFSET_StgRegTable_rR1 :: Int
    , pc_OFFSET_StgRegTable_rR2 :: Int
    , pc_OFFSET_StgRegTable_rR3 :: Int
    , pc_OFFSET_StgRegTable_rR4 :: Int
    , pc_OFFSET_StgRegTable_rR5 :: Int
    , pc_OFFSET_StgRegTable_rR6 :: Int
    , pc_OFFSET_StgRegTable_rR7 :: Int
    , pc_OFFSET_StgRegTable_rR8 :: Int
    , pc_OFFSET_StgRegTable_rR9 :: Int
    , pc_OFFSET_StgRegTable_rR10 :: Int
    , pc_OFFSET_StgRegTable_rF1 :: Int
    , pc_OFFSET_StgRegTable_rF2 :: Int
    , pc_OFFSET_StgRegTable_rF3 :: Int
    , pc_OFFSET_StgRegTable_rF4 :: Int
    , pc_OFFSET_StgRegTable_rF5 :: Int
    , pc_OFFSET_StgRegTable_rF6 :: Int
    , pc_OFFSET_StgRegTable_rD1 :: Int
    , pc_OFFSET_StgRegTable_rD2 :: Int
    , pc_OFFSET_StgRegTable_rD3 :: Int
    , pc_OFFSET_StgRegTable_rD4 :: Int
    , pc_OFFSET_StgRegTable_rD5 :: Int
    , pc_OFFSET_StgRegTable_rD6 :: Int
    , pc_OFFSET_StgRegTable_rXMM1 :: Int
    , pc_OFFSET_StgRegTable_rXMM2 :: Int
    , pc_OFFSET_StgRegTable_rXMM3 :: Int
    , pc_OFFSET_StgRegTable_rXMM4 :: Int
    , pc_OFFSET_StgRegTable_rXMM5 :: Int
    , pc_OFFSET_StgRegTable_rXMM6 :: Int
    , pc_OFFSET_StgRegTable_rYMM1 :: Int
    , pc_OFFSET_StgRegTable_rYMM2 :: Int
    , pc_OFFSET_StgRegTable_rYMM3 :: Int
    , pc_OFFSET_StgRegTable_rYMM4 :: Int
    , pc_OFFSET_StgRegTable_rYMM5 :: Int
    , pc_OFFSET_StgRegTable_rYMM6 :: Int
    , pc_OFFSET_StgRegTable_rZMM1 :: Int
    , pc_OFFSET_StgRegTable_rZMM2 :: Int
    , pc_OFFSET_StgRegTable_rZMM3 :: Int
    , pc_OFFSET_StgRegTable_rZMM4 :: Int
    , pc_OFFSET_StgRegTable_rZMM5 :: Int
    , pc_OFFSET_StgRegTable_rZMM6 :: Int
    , pc_OFFSET_StgRegTable_rL1 :: Int
    , pc_OFFSET_StgRegTable_rSp :: Int
    , pc_OFFSET_StgRegTable_rSpLim :: Int
    , pc_OFFSET_StgRegTable_rHp :: Int
    , pc_OFFSET_StgRegTable_rHpLim :: Int
    , pc_OFFSET_StgRegTable_rCCCS :: Int
    , pc_OFFSET_StgRegTable_rCurrentTSO :: Int
    , pc_OFFSET_StgRegTable_rCurrentNursery :: Int
    , pc_OFFSET_StgRegTable_rHpAlloc :: Int
    , pc_OFFSET_stgEagerBlackholeInfo :: Int
    , pc_OFFSET_stgGCEnter1 :: Int
    , pc_OFFSET_stgGCFun :: Int
    , pc_OFFSET_Capability_r :: Int
    , pc_OFFSET_bdescr_start :: Int
    , pc_OFFSET_bdescr_free :: Int
    , pc_OFFSET_bdescr_blocks :: Int
    , pc_OFFSET_bdescr_flags :: Int
    , pc_SIZEOF_CostCentreStack :: Int
    , pc_OFFSET_CostCentreStack_mem_alloc :: Int
    , pc_REP_CostCentreStack_mem_alloc :: Int
    , pc_OFFSET_CostCentreStack_scc_count :: Int
    , pc_REP_CostCentreStack_scc_count :: Int
    , pc_OFFSET_StgHeader_ccs :: Int
    , pc_OFFSET_StgHeader_ldvw :: Int
    , pc_SIZEOF_StgSMPThunkHeader :: Int
    , pc_OFFSET_StgEntCounter_allocs :: Int
    , pc_REP_StgEntCounter_allocs :: Int
    , pc_OFFSET_StgEntCounter_allocd :: Int
    , pc_REP_StgEntCounter_allocd :: Int
    , pc_OFFSET_StgEntCounter_registeredp :: Int
    , pc_OFFSET_StgEntCounter_link :: Int
    , pc_OFFSET_StgEntCounter_entry_count :: Int
    , pc_SIZEOF_StgUpdateFrame_NoHdr :: Int
    , pc_SIZEOF_StgMutArrPtrs_NoHdr :: Int
    , pc_OFFSET_StgMutArrPtrs_ptrs :: Int
    , pc_OFFSET_StgMutArrPtrs_size :: Int
    , pc_SIZEOF_StgSmallMutArrPtrs_NoHdr :: Int
    , pc_OFFSET_StgSmallMutArrPtrs_ptrs :: Int
    , pc_SIZEOF_StgArrBytes_NoHdr :: Int
    , pc_OFFSET_StgArrBytes_bytes :: Int
    , pc_OFFSET_StgTSO_alloc_limit :: Int
    , pc_OFFSET_StgTSO_cccs :: Int
    , pc_OFFSET_StgTSO_stackobj :: Int
    , pc_OFFSET_StgStack_sp :: Int
    , pc_OFFSET_StgStack_stack :: Int
    , pc_OFFSET_StgUpdateFrame_updatee :: Int
    , pc_OFFSET_StgFunInfoExtraFwd_arity :: Int
    , pc_REP_StgFunInfoExtraFwd_arity :: Int
    , pc_SIZEOF_StgFunInfoExtraRev :: Int
    , pc_OFFSET_StgFunInfoExtraRev_arity :: Int
    , pc_REP_StgFunInfoExtraRev_arity :: Int
    , pc_MAX_SPEC_SELECTEE_SIZE :: Int
    , pc_MAX_SPEC_AP_SIZE :: Int
    , pc_MIN_PAYLOAD_SIZE :: Int
    , pc_MIN_INTLIKE :: Int
    , pc_MAX_INTLIKE :: Int
    , pc_MIN_CHARLIKE :: Int
    , pc_MAX_CHARLIKE :: Int
    , pc_MUT_ARR_PTRS_CARD_BITS :: Int
    , pc_MAX_Vanilla_REG :: Int
    , pc_MAX_Float_REG :: Int
    , pc_MAX_Double_REG :: Int
    , pc_MAX_Long_REG :: Int
    , pc_MAX_XMM_REG :: Int
    , pc_MAX_Real_Vanilla_REG :: Int
    , pc_MAX_Real_Float_REG :: Int
    , pc_MAX_Real_Double_REG :: Int
    , pc_MAX_Real_XMM_REG :: Int
    , pc_MAX_Real_Long_REG :: Int
    , pc_RESERVED_C_STACK_BYTES :: Int
    , pc_RESERVED_STACK_WORDS :: Int
    , pc_AP_STACK_SPLIM :: Int
    , pc_WORD_SIZE :: Int
    , pc_DOUBLE_SIZE :: Int
    , pc_CINT_SIZE :: Int
    , pc_CLONG_SIZE :: Int
    , pc_CLONG_LONG_SIZE :: Int
    , pc_BITMAP_BITS_SHIFT :: Int
    , pc_TAG_BITS :: Int
    , pc_WORDS_BIGENDIAN :: Bool
    , pc_DYNAMIC_BY_DEFAULT :: Bool
    , pc_LDV_SHIFT :: Int
    , pc_ILDV_CREATE_MASK :: Integer
    , pc_ILDV_STATE_CREATE :: Integer
    , pc_ILDV_STATE_USE :: Integer
  } deriving Read
