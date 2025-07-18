{-# OPTIONS_GHC -w #-}
module NakeParser (parseProgram) where
import NakeLexer
import NakeAST
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (Program)
	| HappyAbsSyn5 ([TopLevel])
	| HappyAbsSyn6 (TopLevel)
	| HappyAbsSyn7 (NakeEnum)
	| HappyAbsSyn8 ([Ident])
	| HappyAbsSyn9 (Deriving)
	| HappyAbsSyn10 ([Builtins])
	| HappyAbsSyn11 (Instance)
	| HappyAbsSyn12 (Ident)
	| HappyAbsSyn13 ([Assignment])
	| HappyAbsSyn14 (Assignment)
	| HappyAbsSyn15 ([Function])
	| HappyAbsSyn16 (Function)
	| HappyAbsSyn17 ([(Ident, Ident)])
	| HappyAbsSyn18 ([Stmt])
	| HappyAbsSyn19 (Stmt)
	| HappyAbsSyn20 ([MatchArm])
	| HappyAbsSyn21 (MatchArm)
	| HappyAbsSyn22 ((Name, Rule))
	| HappyAbsSyn23 ([(Ident, Expr)])
	| HappyAbsSyn24 ((Ident, Expr))
	| HappyAbsSyn25 (Expr)
	| HappyAbsSyn27 ([Expr])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178 :: () => Prelude.Int -> ({-HappyReduction (Either String) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either String) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either String) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Either String) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73 :: () => ({-HappyReduction (Either String) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either String) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either String) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Either String) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,246) ([0,57344,0,2,0,0,7,16,0,0,0,0,0,0,448,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,4352,0,0,0,0,8,0,0,0,16384,0,0,0,0,0,0,0,1024,0,0,0,128,0,0,0,2048,0,0,0,0,0,0,0,0,66,0,0,0,512,0,0,0,0,0,0,0,8192,0,0,0,0,1,0,0,4,0,0,0,0,64,0,0,0,16384,0,0,0,0,64,0,0,254,0,0,0,0,32,0,0,16256,0,0,0,0,0,16,0,0,0,128,0,0,0,1024,0,0,0,8192,0,0,0,0,1,0,0,0,8,0,0,0,64,0,0,512,0,0,0,8192,0,0,0,0,32,0,0,0,512,0,0,256,0,0,0,0,4096,0,0,0,0,2,0,0,0,32,0,0,0,0,1,0,0,256,0,0,0,0,64,0,0,16384,0,0,0,0,128,0,0,0,0,0,0,0,0,4,0,0,1024,0,0,0,8192,0,0,0,0,0,0,0,4096,190,12,0,32768,1520,96,0,0,12164,768,0,0,31776,6145,0,0,57600,49163,0,0,2048,95,6,0,16384,760,48,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,4096,0,0,0,64,0,0,0,256,0,0,0,4096,0,0,0,0,2088,0,0,0,0,0,0,24576,8,0,0,32768,1520,96,0,0,512,0,0,0,4096,0,0,0,0,1024,0,0,0,8192,0,0,0,0,1,0,0,0,8,0,0,0,64,0,0,0,512,0,0,0,4096,0,0,0,128,0,0,0,128,0,0,0,0,64,0,0,0,0,0,0,1,1,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,64,0,0,0,2048,0,0,0,16384,0,0,0,6082,384,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,16,0,0,0,32,0,0,32768,33,0,0,0,0,16,0,0,0,128,0,0,0,1024,0,0,33792,47,3,0,0,16,0,0,56832,128,0,0,0,1072,0,0,0,0,0,0,0,3072,1,0,0,0,0,0,0,0,512,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,24328,1536,0,0,63552,12290,0,0,49664,32791,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31776,6145,0,0,0,0,0,0,0,8192,0,0,0,32,0,0,0,256,0,0,0,8192,0,0,0,0,0,0,0,33796,47,3,0,0,16384,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,16384,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,2048,0,0,16384,16384,0,0,0,0,16,0,0,8224,380,24,0,0,0,0,0,0,1024,0,0,0,32768,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,57600,49163,0,0,0,32,0,0,0,32768,0,0,0,0,32,0,0,48656,3072,0,0,61568,24581,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseProgram","Program","TopLevelList","TopLevel","EnumDecl","IdentList","DerivingDecl","BuiltinList","InstanceDecl","BuiltinIdent","AssignmentList","Assignment","FunctionList","Function","ParamList","StmtList","Stmt","MatchArmList","MatchArm","RuleDecl","RuleFieldList","RuleField","Expr","FieldName","ExprList","RecordFieldList","RecordField","enum","instance","deriving","fn","self","match","flags","src","cmd","cmds","dependencies","inputs","outputs","file","name","path","fmt","for","glob","withFlake","ident","string","'{'","'}'","'['","']'","'('","')'","'::'","':'","','","';'","'='","'=>'","'.'","'$$'","'$'","%eof"]
        bit_start = st Prelude.* 67
        bit_end = (st Prelude.+ 1) Prelude.* 67
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..66]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (30) = happyShift action_8
action_0 (31) = happyShift action_9
action_0 (32) = happyShift action_10
action_0 (50) = happyShift action_11
action_0 (4) = happyGoto action_12
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (9) = happyGoto action_5
action_0 (11) = happyGoto action_6
action_0 (22) = happyGoto action_7
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (30) = happyShift action_8
action_1 (31) = happyShift action_9
action_1 (32) = happyShift action_10
action_1 (50) = happyShift action_11
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (9) = happyGoto action_5
action_1 (11) = happyGoto action_6
action_1 (22) = happyGoto action_7
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (30) = happyShift action_8
action_3 (31) = happyShift action_9
action_3 (32) = happyShift action_10
action_3 (50) = happyShift action_11
action_3 (5) = happyGoto action_19
action_3 (6) = happyGoto action_3
action_3 (7) = happyGoto action_4
action_3 (9) = happyGoto action_5
action_3 (11) = happyGoto action_6
action_3 (22) = happyGoto action_7
action_3 _ = happyReduce_3

action_4 _ = happyReduce_4

action_5 _ = happyReduce_5

action_6 _ = happyReduce_6

action_7 _ = happyReduce_7

action_8 (50) = happyShift action_18
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (46) = happyShift action_16
action_9 (50) = happyShift action_17
action_9 (12) = happyGoto action_15
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (54) = happyShift action_14
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (62) = happyShift action_13
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (67) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (52) = happyShift action_26
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (46) = happyShift action_25
action_14 (10) = happyGoto action_24
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (47) = happyShift action_23
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_17

action_17 (47) = happyShift action_21
action_17 (52) = happyShift action_22
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (52) = happyShift action_20
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_2

action_20 (50) = happyShift action_44
action_20 (8) = happyGoto action_43
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (50) = happyShift action_42
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (33) = happyShift action_41
action_22 (15) = happyGoto action_39
action_22 (16) = happyGoto action_40
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (50) = happyShift action_38
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (55) = happyShift action_37
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (60) = happyShift action_36
action_25 _ = happyReduce_13

action_26 (36) = happyShift action_29
action_26 (37) = happyShift action_30
action_26 (38) = happyShift action_31
action_26 (39) = happyShift action_32
action_26 (40) = happyShift action_33
action_26 (41) = happyShift action_34
action_26 (42) = happyShift action_35
action_26 (23) = happyGoto action_27
action_26 (24) = happyGoto action_28
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (53) = happyShift action_62
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (36) = happyShift action_29
action_28 (37) = happyShift action_30
action_28 (38) = happyShift action_31
action_28 (39) = happyShift action_32
action_28 (40) = happyShift action_33
action_28 (41) = happyShift action_34
action_28 (42) = happyShift action_35
action_28 (23) = happyGoto action_61
action_28 (24) = happyGoto action_28
action_28 _ = happyReduce_38

action_29 (62) = happyShift action_60
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (62) = happyShift action_59
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (62) = happyShift action_58
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (62) = happyShift action_57
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (62) = happyShift action_56
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (62) = happyShift action_55
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (62) = happyShift action_54
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (46) = happyShift action_25
action_36 (10) = happyGoto action_53
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (47) = happyShift action_52
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (52) = happyShift action_51
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (53) = happyShift action_50
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (33) = happyShift action_41
action_40 (15) = happyGoto action_49
action_40 (16) = happyGoto action_40
action_40 _ = happyReduce_22

action_41 (50) = happyShift action_48
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (52) = happyShift action_47
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (53) = happyShift action_46
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (61) = happyShift action_45
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (50) = happyShift action_44
action_45 (8) = happyGoto action_88
action_45 _ = happyReduce_10

action_46 (61) = happyShift action_87
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (50) = happyShift action_83
action_47 (13) = happyGoto action_86
action_47 (14) = happyGoto action_82
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (56) = happyShift action_85
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_21

action_50 (61) = happyShift action_84
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (50) = happyShift action_83
action_51 (13) = happyGoto action_81
action_51 (14) = happyGoto action_82
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (50) = happyShift action_80
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_12

action_54 (43) = happyShift action_65
action_54 (48) = happyShift action_66
action_54 (49) = happyShift action_67
action_54 (50) = happyShift action_68
action_54 (51) = happyShift action_69
action_54 (52) = happyShift action_70
action_54 (54) = happyShift action_71
action_54 (65) = happyShift action_72
action_54 (66) = happyShift action_73
action_54 (25) = happyGoto action_79
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (43) = happyShift action_65
action_55 (48) = happyShift action_66
action_55 (49) = happyShift action_67
action_55 (50) = happyShift action_68
action_55 (51) = happyShift action_69
action_55 (52) = happyShift action_70
action_55 (54) = happyShift action_71
action_55 (65) = happyShift action_72
action_55 (66) = happyShift action_73
action_55 (25) = happyGoto action_78
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (43) = happyShift action_65
action_56 (48) = happyShift action_66
action_56 (49) = happyShift action_67
action_56 (50) = happyShift action_68
action_56 (51) = happyShift action_69
action_56 (52) = happyShift action_70
action_56 (54) = happyShift action_71
action_56 (65) = happyShift action_72
action_56 (66) = happyShift action_73
action_56 (25) = happyGoto action_77
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (43) = happyShift action_65
action_57 (48) = happyShift action_66
action_57 (49) = happyShift action_67
action_57 (50) = happyShift action_68
action_57 (51) = happyShift action_69
action_57 (52) = happyShift action_70
action_57 (54) = happyShift action_71
action_57 (65) = happyShift action_72
action_57 (66) = happyShift action_73
action_57 (25) = happyGoto action_76
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (43) = happyShift action_65
action_58 (48) = happyShift action_66
action_58 (49) = happyShift action_67
action_58 (50) = happyShift action_68
action_58 (51) = happyShift action_69
action_58 (52) = happyShift action_70
action_58 (54) = happyShift action_71
action_58 (65) = happyShift action_72
action_58 (66) = happyShift action_73
action_58 (25) = happyGoto action_75
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (43) = happyShift action_65
action_59 (48) = happyShift action_66
action_59 (49) = happyShift action_67
action_59 (50) = happyShift action_68
action_59 (51) = happyShift action_69
action_59 (52) = happyShift action_70
action_59 (54) = happyShift action_71
action_59 (65) = happyShift action_72
action_59 (66) = happyShift action_73
action_59 (25) = happyGoto action_74
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (43) = happyShift action_65
action_60 (48) = happyShift action_66
action_60 (49) = happyShift action_67
action_60 (50) = happyShift action_68
action_60 (51) = happyShift action_69
action_60 (52) = happyShift action_70
action_60 (54) = happyShift action_71
action_60 (65) = happyShift action_72
action_60 (66) = happyShift action_73
action_60 (25) = happyGoto action_64
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_37

action_62 (61) = happyShift action_63
action_62 _ = happyFail (happyExpListPerState 62)

action_63 _ = happyReduce_36

action_64 (61) = happyShift action_118
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (52) = happyShift action_117
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (51) = happyShift action_116
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (52) = happyShift action_115
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (56) = happyShift action_112
action_68 (58) = happyShift action_113
action_68 (64) = happyShift action_114
action_68 _ = happyReduce_47

action_69 _ = happyReduce_46

action_70 (44) = happyShift action_109
action_70 (45) = happyShift action_110
action_70 (50) = happyShift action_111
action_70 (28) = happyGoto action_107
action_70 (29) = happyGoto action_108
action_70 _ = happyReduce_70

action_71 (43) = happyShift action_65
action_71 (48) = happyShift action_66
action_71 (49) = happyShift action_67
action_71 (50) = happyShift action_68
action_71 (51) = happyShift action_69
action_71 (52) = happyShift action_70
action_71 (54) = happyShift action_71
action_71 (65) = happyShift action_72
action_71 (66) = happyShift action_73
action_71 (25) = happyGoto action_105
action_71 (27) = happyGoto action_106
action_71 _ = happyReduce_67

action_72 (50) = happyShift action_104
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (50) = happyShift action_103
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (61) = happyShift action_102
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (61) = happyShift action_101
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (61) = happyShift action_100
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (61) = happyShift action_99
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (61) = happyShift action_98
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (61) = happyShift action_97
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (61) = happyShift action_96
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (53) = happyShift action_95
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (50) = happyShift action_83
action_82 (13) = happyGoto action_94
action_82 (14) = happyGoto action_82
action_82 _ = happyReduce_19

action_83 (62) = happyShift action_93
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_16

action_85 (34) = happyShift action_91
action_85 (50) = happyShift action_92
action_85 (17) = happyGoto action_90
action_85 _ = happyReduce_28

action_86 (53) = happyShift action_89
action_86 _ = happyFail (happyExpListPerState 86)

action_87 _ = happyReduce_8

action_88 _ = happyReduce_9

action_89 (61) = happyShift action_143
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (57) = happyShift action_142
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (59) = happyShift action_141
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (59) = happyShift action_140
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (43) = happyShift action_65
action_93 (48) = happyShift action_66
action_93 (49) = happyShift action_67
action_93 (50) = happyShift action_68
action_93 (51) = happyShift action_69
action_93 (52) = happyShift action_70
action_93 (54) = happyShift action_71
action_93 (65) = happyShift action_72
action_93 (66) = happyShift action_73
action_93 (25) = happyGoto action_139
action_93 _ = happyFail (happyExpListPerState 93)

action_94 _ = happyReduce_18

action_95 (61) = happyShift action_138
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_11

action_97 _ = happyReduce_45

action_98 _ = happyReduce_43

action_99 _ = happyReduce_44

action_100 _ = happyReduce_42

action_101 _ = happyReduce_41

action_102 _ = happyReduce_40

action_103 _ = happyReduce_54

action_104 _ = happyReduce_55

action_105 (60) = happyShift action_137
action_105 _ = happyReduce_66

action_106 (55) = happyShift action_136
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (53) = happyShift action_135
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (44) = happyShift action_109
action_108 (45) = happyShift action_110
action_108 (50) = happyShift action_111
action_108 (53) = happyReduce_70
action_108 (28) = happyGoto action_134
action_108 (29) = happyGoto action_108
action_108 _ = happyReduce_70

action_109 (62) = happyShift action_133
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (62) = happyShift action_132
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (62) = happyShift action_131
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (43) = happyShift action_65
action_112 (48) = happyShift action_66
action_112 (49) = happyShift action_67
action_112 (50) = happyShift action_68
action_112 (51) = happyShift action_69
action_112 (52) = happyShift action_70
action_112 (54) = happyShift action_71
action_112 (65) = happyShift action_72
action_112 (66) = happyShift action_73
action_112 (25) = happyGoto action_105
action_112 (27) = happyGoto action_130
action_112 _ = happyReduce_67

action_113 (50) = happyShift action_129
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (36) = happyShift action_122
action_114 (37) = happyShift action_123
action_114 (38) = happyShift action_124
action_114 (39) = happyShift action_125
action_114 (41) = happyShift action_126
action_114 (42) = happyShift action_127
action_114 (50) = happyShift action_128
action_114 (26) = happyGoto action_121
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (44) = happyShift action_109
action_115 (45) = happyShift action_110
action_115 (50) = happyShift action_111
action_115 (28) = happyGoto action_120
action_115 (29) = happyGoto action_108
action_115 _ = happyReduce_70

action_116 _ = happyReduce_48

action_117 (44) = happyShift action_109
action_117 (45) = happyShift action_110
action_117 (50) = happyShift action_111
action_117 (28) = happyGoto action_119
action_117 (29) = happyGoto action_108
action_117 _ = happyReduce_70

action_118 _ = happyReduce_39

action_119 (53) = happyShift action_154
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (53) = happyShift action_153
action_120 _ = happyFail (happyExpListPerState 120)

action_121 _ = happyReduce_58

action_122 _ = happyReduce_61

action_123 _ = happyReduce_62

action_124 _ = happyReduce_63

action_125 _ = happyReduce_64

action_126 _ = happyReduce_60

action_127 _ = happyReduce_59

action_128 _ = happyReduce_57

action_129 _ = happyReduce_56

action_130 (57) = happyShift action_152
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (43) = happyShift action_65
action_131 (48) = happyShift action_66
action_131 (49) = happyShift action_67
action_131 (50) = happyShift action_68
action_131 (51) = happyShift action_69
action_131 (52) = happyShift action_70
action_131 (54) = happyShift action_71
action_131 (65) = happyShift action_72
action_131 (66) = happyShift action_73
action_131 (25) = happyGoto action_151
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (43) = happyShift action_65
action_132 (48) = happyShift action_66
action_132 (49) = happyShift action_67
action_132 (50) = happyShift action_68
action_132 (51) = happyShift action_69
action_132 (52) = happyShift action_70
action_132 (54) = happyShift action_71
action_132 (65) = happyShift action_72
action_132 (66) = happyShift action_73
action_132 (25) = happyGoto action_150
action_132 _ = happyFail (happyExpListPerState 132)

action_133 (43) = happyShift action_65
action_133 (48) = happyShift action_66
action_133 (49) = happyShift action_67
action_133 (50) = happyShift action_68
action_133 (51) = happyShift action_69
action_133 (52) = happyShift action_70
action_133 (54) = happyShift action_71
action_133 (65) = happyShift action_72
action_133 (66) = happyShift action_73
action_133 (25) = happyGoto action_149
action_133 _ = happyFail (happyExpListPerState 133)

action_134 _ = happyReduce_68

action_135 _ = happyReduce_51

action_136 _ = happyReduce_50

action_137 (43) = happyShift action_65
action_137 (48) = happyShift action_66
action_137 (49) = happyShift action_67
action_137 (50) = happyShift action_68
action_137 (51) = happyShift action_69
action_137 (52) = happyShift action_70
action_137 (54) = happyShift action_71
action_137 (65) = happyShift action_72
action_137 (66) = happyShift action_73
action_137 (25) = happyGoto action_105
action_137 (27) = happyGoto action_148
action_137 _ = happyReduce_67

action_138 _ = happyReduce_15

action_139 (61) = happyShift action_147
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (50) = happyShift action_146
action_140 _ = happyFail (happyExpListPerState 140)

action_141 (50) = happyShift action_145
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (52) = happyShift action_144
action_142 _ = happyFail (happyExpListPerState 142)

action_143 _ = happyReduce_14

action_144 (35) = happyShift action_163
action_144 (43) = happyShift action_65
action_144 (48) = happyShift action_66
action_144 (49) = happyShift action_67
action_144 (50) = happyShift action_68
action_144 (51) = happyShift action_69
action_144 (52) = happyShift action_70
action_144 (54) = happyShift action_71
action_144 (65) = happyShift action_72
action_144 (66) = happyShift action_73
action_144 (18) = happyGoto action_160
action_144 (19) = happyGoto action_161
action_144 (25) = happyGoto action_162
action_144 _ = happyFail (happyExpListPerState 144)

action_145 (60) = happyShift action_159
action_145 _ = happyReduce_27

action_146 (60) = happyShift action_158
action_146 _ = happyReduce_25

action_147 _ = happyReduce_20

action_148 _ = happyReduce_65

action_149 (61) = happyShift action_157
action_149 _ = happyFail (happyExpListPerState 149)

action_150 (61) = happyShift action_156
action_150 _ = happyFail (happyExpListPerState 150)

action_151 (61) = happyShift action_155
action_151 _ = happyFail (happyExpListPerState 151)

action_152 _ = happyReduce_49

action_153 _ = happyReduce_53

action_154 _ = happyReduce_52

action_155 _ = happyReduce_71

action_156 _ = happyReduce_73

action_157 _ = happyReduce_72

action_158 (34) = happyShift action_91
action_158 (50) = happyShift action_92
action_158 (17) = happyGoto action_168
action_158 _ = happyReduce_28

action_159 (34) = happyShift action_91
action_159 (50) = happyShift action_92
action_159 (17) = happyGoto action_167
action_159 _ = happyReduce_28

action_160 (53) = happyShift action_166
action_160 _ = happyFail (happyExpListPerState 160)

action_161 (35) = happyShift action_163
action_161 (43) = happyShift action_65
action_161 (48) = happyShift action_66
action_161 (49) = happyShift action_67
action_161 (50) = happyShift action_68
action_161 (51) = happyShift action_69
action_161 (52) = happyShift action_70
action_161 (54) = happyShift action_71
action_161 (65) = happyShift action_72
action_161 (66) = happyShift action_73
action_161 (18) = happyGoto action_165
action_161 (19) = happyGoto action_161
action_161 (25) = happyGoto action_162
action_161 _ = happyReduce_30

action_162 _ = happyReduce_32

action_163 (50) = happyShift action_164
action_163 _ = happyFail (happyExpListPerState 163)

action_164 (52) = happyShift action_170
action_164 _ = happyFail (happyExpListPerState 164)

action_165 _ = happyReduce_29

action_166 (61) = happyShift action_169
action_166 _ = happyFail (happyExpListPerState 166)

action_167 _ = happyReduce_26

action_168 _ = happyReduce_24

action_169 _ = happyReduce_23

action_170 (43) = happyShift action_65
action_170 (48) = happyShift action_66
action_170 (49) = happyShift action_67
action_170 (50) = happyShift action_68
action_170 (51) = happyShift action_69
action_170 (52) = happyShift action_70
action_170 (54) = happyShift action_71
action_170 (65) = happyShift action_72
action_170 (66) = happyShift action_73
action_170 (20) = happyGoto action_171
action_170 (21) = happyGoto action_172
action_170 (25) = happyGoto action_173
action_170 _ = happyFail (happyExpListPerState 170)

action_171 (53) = happyShift action_176
action_171 _ = happyFail (happyExpListPerState 171)

action_172 (60) = happyShift action_175
action_172 _ = happyReduce_34

action_173 (63) = happyShift action_174
action_173 _ = happyFail (happyExpListPerState 173)

action_174 (43) = happyShift action_65
action_174 (48) = happyShift action_66
action_174 (49) = happyShift action_67
action_174 (50) = happyShift action_68
action_174 (51) = happyShift action_69
action_174 (52) = happyShift action_70
action_174 (54) = happyShift action_71
action_174 (65) = happyShift action_72
action_174 (66) = happyShift action_73
action_174 (25) = happyGoto action_178
action_174 _ = happyFail (happyExpListPerState 174)

action_175 (43) = happyShift action_65
action_175 (48) = happyShift action_66
action_175 (49) = happyShift action_67
action_175 (50) = happyShift action_68
action_175 (51) = happyShift action_69
action_175 (52) = happyShift action_70
action_175 (54) = happyShift action_71
action_175 (65) = happyShift action_72
action_175 (66) = happyShift action_73
action_175 (20) = happyGoto action_177
action_175 (21) = happyGoto action_172
action_175 (25) = happyGoto action_173
action_175 _ = happyFail (happyExpListPerState 175)

action_176 _ = happyReduce_31

action_177 _ = happyReduce_33

action_178 _ = happyReduce_35

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Program happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (EnumDecl happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (DerivingDecl happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn6
		 (InstanceDecl happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn6
		 (uncurry RuleDecl happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happyReduce 6 7 happyReduction_8
happyReduction_8 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_Ident _ happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (NakeEnum happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_3)
	_
	(HappyTerminal (T_Ident _ happy_var_1))
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  8 happyReduction_10
happyReduction_10 _
	(HappyTerminal (T_Ident _ happy_var_1))
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 7 9 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyTerminal (T_Ident _ happy_var_6)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Deriving happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_3  10 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_3)
	_
	_
	 =  HappyAbsSyn10
		 (FMT : happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn10
		 ([FMT]
	)

happyReduce_14 = happyReduce 8 11 happyReduction_14
happyReduction_14 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_Ident _ happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_Ident _ happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (InstanceFor happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 8 11 happyReduction_15
happyReduction_15 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_Ident _ happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (InstanceFor happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 6 11 happyReduction_16
happyReduction_16 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_Ident _ happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (InstanceSingle happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_1  12 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn12
		 ("fmt"
	)

happyReduce_18 = happySpecReduce_2  13 happyReduction_18
happyReduction_18 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  13 happyReduction_19
happyReduction_19 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 14 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_Ident _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Assignment happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_2  15 happyReduction_21
happyReduction_21 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 : happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  15 happyReduction_22
happyReduction_22 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happyReduce 9 16 happyReduction_23
happyReduction_23 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_Ident _ happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (Function happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 5 17 happyReduction_24
happyReduction_24 ((HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_Ident _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_Ident _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ((happy_var_1, happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_3  17 happyReduction_25
happyReduction_25 (HappyTerminal (T_Ident _ happy_var_3))
	_
	(HappyTerminal (T_Ident _ happy_var_1))
	 =  HappyAbsSyn17
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 5 17 happyReduction_26
happyReduction_26 ((HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_Ident _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (("self", happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_3  17 happyReduction_27
happyReduction_27 (HappyTerminal (T_Ident _ happy_var_3))
	_
	_
	 =  HappyAbsSyn17
		 ([("self", happy_var_3)]
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_0  17 happyReduction_28
happyReduction_28  =  HappyAbsSyn17
		 ([]
	)

happyReduce_29 = happySpecReduce_2  18 happyReduction_29
happyReduction_29 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 : happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  18 happyReduction_30
happyReduction_30 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happyReduce 5 19 happyReduction_31
happyReduction_31 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_Ident _ happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (MatchStmt happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_1  19 happyReduction_32
happyReduction_32 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn19
		 (ExprStmt happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  20 happyReduction_33
happyReduction_33 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 : happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  20 happyReduction_34
happyReduction_34 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 ([happy_var_1]
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  21 happyReduction_35
happyReduction_35 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn21
		 (MatchArm happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happyReduce 6 22 happyReduction_36
happyReduction_36 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_Ident _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 ((happy_var_1, makeRule happy_var_1 happy_var_4)
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_2  23 happyReduction_37
happyReduction_37 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1 : happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  23 happyReduction_38
happyReduction_38 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 ([happy_var_1]
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happyReduce 4 24 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (("flags", happy_var_3)
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 4 24 happyReduction_40
happyReduction_40 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (("src", happy_var_3)
	) `HappyStk` happyRest

happyReduce_41 = happyReduce 4 24 happyReduction_41
happyReduction_41 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (("cmd", happy_var_3)
	) `HappyStk` happyRest

happyReduce_42 = happyReduce 4 24 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (("cmds", happy_var_3)
	) `HappyStk` happyRest

happyReduce_43 = happyReduce 4 24 happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (("inputs", happy_var_3)
	) `HappyStk` happyRest

happyReduce_44 = happyReduce 4 24 happyReduction_44
happyReduction_44 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (("dependencies", happy_var_3)
	) `HappyStk` happyRest

happyReduce_45 = happyReduce 4 24 happyReduction_45
happyReduction_45 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (("outputs", happy_var_3)
	) `HappyStk` happyRest

happyReduce_46 = happySpecReduce_1  25 happyReduction_46
happyReduction_46 (HappyTerminal (T_String _ happy_var_1))
	 =  HappyAbsSyn25
		 (StringLit happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  25 happyReduction_47
happyReduction_47 (HappyTerminal (T_Ident _ happy_var_1))
	 =  HappyAbsSyn25
		 (IdentExpr happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  25 happyReduction_48
happyReduction_48 (HappyTerminal (T_String _ happy_var_2))
	_
	 =  HappyAbsSyn25
		 (GlobExpr happy_var_2
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happyReduce 4 25 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_Ident _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (FuncCall happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_50 = happySpecReduce_3  25 happyReduction_50
happyReduction_50 _
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (ListExpr happy_var_2
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  25 happyReduction_51
happyReduction_51 _
	(HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (RecordExpr happy_var_2
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happyReduce 4 25 happyReduction_52
happyReduction_52 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (RecordExpr happy_var_3
	) `HappyStk` happyRest

happyReduce_53 = happyReduce 4 25 happyReduction_53
happyReduction_53 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (RecordExpr happy_var_3
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_2  25 happyReduction_54
happyReduction_54 (HappyTerminal (T_Ident _ happy_var_2))
	_
	 =  HappyAbsSyn25
		 (MetaVar happy_var_2
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  25 happyReduction_55
happyReduction_55 (HappyTerminal (T_Ident _ happy_var_2))
	_
	 =  HappyAbsSyn25
		 (MetaFunc happy_var_2 []
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  25 happyReduction_56
happyReduction_56 (HappyTerminal (T_Ident _ happy_var_3))
	_
	(HappyTerminal (T_Ident _ happy_var_1))
	 =  HappyAbsSyn25
		 (QualifiedIdent happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  25 happyReduction_57
happyReduction_57 (HappyTerminal (T_Ident _ happy_var_3))
	_
	(HappyTerminal (T_Ident _ happy_var_1))
	 =  HappyAbsSyn25
		 (FuncCall "field_access" [IdentExpr happy_var_1, IdentExpr happy_var_3]
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  25 happyReduction_58
happyReduction_58 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (T_Ident _ happy_var_1))
	 =  HappyAbsSyn25
		 (FuncCall "field_access" [IdentExpr happy_var_1, IdentExpr happy_var_3]
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  26 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn12
		 ("outputs"
	)

happyReduce_60 = happySpecReduce_1  26 happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn12
		 ("inputs"
	)

happyReduce_61 = happySpecReduce_1  26 happyReduction_61
happyReduction_61 _
	 =  HappyAbsSyn12
		 ("flags"
	)

happyReduce_62 = happySpecReduce_1  26 happyReduction_62
happyReduction_62 _
	 =  HappyAbsSyn12
		 ("src"
	)

happyReduce_63 = happySpecReduce_1  26 happyReduction_63
happyReduction_63 _
	 =  HappyAbsSyn12
		 ("cmd"
	)

happyReduce_64 = happySpecReduce_1  26 happyReduction_64
happyReduction_64 _
	 =  HappyAbsSyn12
		 ("cmds"
	)

happyReduce_65 = happySpecReduce_3  27 happyReduction_65
happyReduction_65 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1 : happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  27 happyReduction_66
happyReduction_66 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn27
		 ([happy_var_1]
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_0  27 happyReduction_67
happyReduction_67  =  HappyAbsSyn27
		 ([]
	)

happyReduce_68 = happySpecReduce_2  28 happyReduction_68
happyReduction_68 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1 : happy_var_2
	)
happyReduction_68 _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  28 happyReduction_69
happyReduction_69 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 ([happy_var_1]
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_0  28 happyReduction_70
happyReduction_70  =  HappyAbsSyn23
		 ([]
	)

happyReduce_71 = happyReduce 4 29 happyReduction_71
happyReduction_71 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T_Ident _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_72 = happyReduce 4 29 happyReduction_72
happyReduction_72 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (("name", happy_var_3)
	) `HappyStk` happyRest

happyReduce_73 = happyReduce 4 29 happyReduction_73
happyReduction_73 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (("path", happy_var_3)
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 67 67 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	T_Enum _ -> cont 30;
	T_Instance _ -> cont 31;
	T_Deriving _ -> cont 32;
	T_Fn _ -> cont 33;
	T_Self _ -> cont 34;
	T_Match _ -> cont 35;
	T_Flags _ -> cont 36;
	T_Src _ -> cont 37;
	T_Cmd _ -> cont 38;
	T_Cmds _ -> cont 39;
	T_Dependencies _ -> cont 40;
	T_Inputs _ -> cont 41;
	T_Outputs _ -> cont 42;
	T_File _ -> cont 43;
	T_Name _ -> cont 44;
	T_Path _ -> cont 45;
	T_Fmt _ -> cont 46;
	T_For _ -> cont 47;
	T_Glob _ -> cont 48;
	T_WithFlake _ -> cont 49;
	T_Ident _ happy_dollar_dollar -> cont 50;
	T_String _ happy_dollar_dollar -> cont 51;
	T_LBrace _ -> cont 52;
	T_RBrace _ -> cont 53;
	T_LBracket _ -> cont 54;
	T_RBracket _ -> cont 55;
	T_LParen _ -> cont 56;
	T_RParen _ -> cont 57;
	T_DoubleColon _ -> cont 58;
	T_Colon _ -> cont 59;
	T_Comma _ -> cont 60;
	T_Semi _ -> cont 61;
	T_Eq _ -> cont 62;
	T_Arrow _ -> cont 63;
	T_Dot _ -> cont 64;
	T_MetaMetaFunc _ -> cont 65;
	T_MetaVar _ -> cont 66;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 67 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either String a -> (a -> Either String b) -> Either String b
happyThen = (>>=)
happyReturn :: () => a -> Either String a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Either String a
happyError' = (\(tokens, _) -> parseError tokens)
parseProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> Either String a
parseError tokens = Left $ "Parse error at: " ++ show (take 5 tokens)

makeRule :: Name -> [(Ident, Expr)] -> Rule
makeRule name fields = Rule
  { ruleName = name
  , ruleFlags = getFlags fields
  , ruleSrc = lookup "src" fields
  , ruleCmd = lookup "cmd" fields
  , ruleCmds = lookup "cmds" fields
  , ruleInputs = lookup "inputs" fields
  , ruleDependencies = getDependencies fields
  , ruleOutputs = getOutputs fields
  }

getFlags :: [(Ident, Expr)] -> [FlagName]
getFlags fields = case lookup "flags" fields of
  Just (ListExpr exprs) -> map exprToIdent exprs
  _ -> []

exprToIdent :: Expr -> Ident
exprToIdent (IdentExpr i) = i
exprToIdent _ = ""

getDependencies :: [(Ident, Expr)] -> Maybe Dependencies
getDependencies fields = case lookup "dependencies" fields of
  Just (RecordExpr rs) -> Just (WithFlake rs)
  _ -> Nothing

getOutputs :: [(Ident, Expr)] -> [Output]
getOutputs fields = case lookup "outputs" fields of
  Just (ListExpr exprs) -> map exprToOutput exprs
  _ -> []

exprToOutput :: Expr -> Output
exprToOutput (RecordExpr fields) = 
  let nameVal = case lookup "name" fields of
        Just (StringLit s) -> s
        _ -> ""
      pathVal = case lookup "path" fields of
        Just (StringLit s) -> s
        _ -> ""
  in OutputFile (File nameVal pathVal)
exprToOutput _ = OutputFile (File "" "")
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
