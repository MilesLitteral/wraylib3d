-- Ideas taken from http://www.andres-loeh.de/LambdaPi/
-- and "Efficient Bracket Abstraction Using Iconic Representations for Combinators" by Antoni Diller
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Control.Arrow
import Data.Maybe
import Data.List
import Data.Char
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
--import System.Console.Readline
import qualified Data.Map as Map
import Text.Show.Pretty (ppShow)
import Debug.Trace
import System.Environment

import Unsafe.Coerce
{- possible improvements
- use Bound
- remove Tag
- review sharing, tests
- VPi, VStar -> VCCon ...
- unify data types
-}
--------------------------------------------------------------------------------

data CTerm_ t
    = Inf (ITerm_ t)
    | Lam (CTerm_ t)
    deriving (Eq, Show)

type CTerm = CTerm_ Var
type ITerm = ITerm_ Var

data Var
    = Bound_ !Int
    | Global_ String
    | Local_ !Int Type
    deriving (Show)

instance Eq Var where
    Bound_ i == Bound_ j = i == j
    Global_ i == Global_ j = i == j
    Local_ i _ == Local_ j _ = i == j

pattern Bound a = Var (Bound_ a)
pattern Local a t = Var (Local_ a t)
pattern Global a = Var (Global_ a)

data ITerm_ t
    = Ann (CTerm_ t) (CTerm_ t)
    | Star
    | ILam Relevance (CTerm_ t) (ITerm_ t)
    | Pi Relevance (CTerm_ t) (CTerm_ t)
    | Var t

    | CCon CConName [CTerm_ t] [CTerm_ t]
    | ITCon TConName [CTerm_ t]
    | IInt !Int

    -- neutral
    | ITerm_ t :$: CTerm_ t

    | ICase TConName [CTerm_ t] (CTerm_ t) [CTerm_ t] [CTerm_ t] (CTerm_ t)
    | Prim PrimName' [CTerm_ t]
    deriving (Eq, Show) -- , Functor)

data Value_ f t
    -- real values
    = VLam_ !(f t)
    | VCCon_ !Int [t]
    | VInt_ !Int

    -- type values
    | VStar_
    | VPi_ Relevance t (f t)
    | VCon_ !TConName [t]

    -- neutral values
    | VNeutral_ (Neutral_ t)
    | VTag_ (Neutral_ t) t      -- todo: eliminate
--    deriving Functor

type Neutral = Neutral_ Value
data Neutral_ t
    = NLocal !Int Type
    | NQuote !Int
    | NApp (Neutral_ t) t

    | NCase t [t] (Neutral_ t)
    | NPrim PrimName' [t]
--    deriving Functor

newtype Value = V {unV :: Value_ ((->) Value) Value}

type Type = Value

data IConName = Nat | Int | Bool' | UName !Int
    deriving (Eq)

data PrimName = Add | Fix | Sub | Mod | Sqrt | IntEq | IntLess
    deriving (Show, Eq, Enum)

data CConName = CConName !Int String TConName Type

data TConName = TConName !IConName String !Int{-num of params-} Type{-type-} [CConName]{-constructors-} Type{-case type-}

data PrimName' = PrimName' !PrimName String ([Value] -> Value) Type

data Relevance = Irr | TRel | Rel
    deriving (Eq, Show)

----------------

type Env = [Value]
type NameEnv v = Map.Map String v

type TEnv = NameEnv ((EnvValue, Value), Type)

type EnvValue2 = (EnvValue, Env -> Value)

--------------------------------------------------------------------------------

pattern VLam f = V (VLam_ f)
pattern VPi r a b = V (VPi_ r a b)
pattern VNeutral a = V (VNeutral_ a)
pattern VStar = V VStar_
pattern VTag a b = V (VTag_ a b)
pattern VCCon a b = V (VCCon_ a b)
pattern VCon a b = V (VCon_ a b)
pattern VInt a = V (VInt_ a)

instance Show Value where
    show = show . quote0 tInt -- (error "show value")
{-
instance Eq ITerm where
    Star == Star = True
    Bound i == Bound j = i == j
    _ == _ = False  -- TODO
-}
instance Show CConName where show (CConName _ s _ _) = s
instance Eq CConName where CConName i _ _ _ == CConName j _ _ _ = i == j

instance Show TConName where show (TConName _ s _ _ _ _) = s
instance Eq TConName where TConName i _ _ _ _ _ == TConName j _ _ _ _ _ = i == j

instance Show PrimName' where show (PrimName' _ s _ _) = s
instance Eq PrimName' where PrimName' i _ _ _ == PrimName' j _ _ _ = i == j

--------------------------------------------------------------------------------

infixr 4 ~>

(~>) :: Value -> Value -> Value
a ~> b = VPi Rel a (\_ -> b)

iPi r a = Inf . Pi r a

tnBool = TConName Bool' "Bool" 0 VStar [cFalse, cTrue] tElimBool
cFalse = CConName 0 "False" tnBool tBool
cTrue = CConName 1 "True" tnBool tBool
tBool = VCon tnBool []
vFalse = VCCon 0 []
vTrue = VCCon 1 []
tElimBool = VPi TRel (VPi Rel tBool $ \_ -> VStar) $ \m -> (m `vappT` vFalse) ~> (m `vappT` vTrue) ~> VPi Rel tBool (\n -> m `vappT` n)

vBool False = vFalse
vBool True = vTrue

tnNat = TConName Nat "Nat" 0 VStar [cZero, cSucc] tElimNat
cZero = CConName 0 "Zero" tnNat tNat
cSucc = CConName 1 "Succ" tnNat $ tNat ~> tNat
tNat = VCon tnNat []
vZero = VCCon 0 []
vSucc x = VCCon 1 [x]
tElimNat = VPi TRel (VPi Rel tNat $ \_ -> VStar) $ \m -> (m `vappT` vZero) ~> VPi Rel tNat (\k -> m `vappT` vSucc k) ~> VPi Rel tNat (\n -> m `vappT` n)

toNat :: Integer -> ITerm
toNat 0 = CCon (CConName 0 "Zero" tnNat tNat) [] []
toNat n = CCon (CConName 1 "Succ" tnNat $ tNat ~> tNat) [] [Inf $ toNat (n - 1)]

tInt = VCon (TConName Int "Int" 0 VStar (error "Intconstr") $ error "tElimInt") []

tenv = Map.fromList [("Int", (({-pure' tInt-} error "tyin", tInt), VStar))]

lams' :: Int -> ([Value] -> Value) -> Value
lams' i f = g i f id where
    g 0 f = \c -> f $ c []
    g i f = \c -> VLam $ \d -> g (i-1) f (c . (d:))

lamsT'' :: Value -> ([Value] -> Value) -> Value
lamsT'' i f = g i f id where
    g (VPi r _ h) f = ifT r (g (h $ vQuote undefined) f) $ \c -> VLam $ \d -> g (h $ vQuote undefined) f (c . (d:))
    g _ f = \c -> f $ c []

downTo n m = map (Inf . Bound) [n+m-1, n+m-2..n]

showPrimName n = "prim" ++ show n
primNames = Map.fromList [(showPrimName n, n) | n <- [Add .. IntLess]]

rels (VPi r v f) = r: rels (f $ vQuote undefined)
rels _ = []

arityq = length . rels

dropPi [] x = x
dropPi (t':ts) (Inf (Pi _ t'' t))
--    | t' /= t'' = error $ "dropPi: " ++ e ++ show (t', t'')
    | otherwise = dropPi ts t

nameOf = \case
    "Int" -> return Int
    "Bool" -> return Bool'
    "Nat" -> return Nat
    _ -> Nothing

addParams r ps t = foldr (iPi r) t ps

chain q ps end = f [] ps where
    f acc [] ty = end (reverse acc) ty
    f acc (x: xs) (VPi _ a b) = f (q a x: acc) xs (b x)

--------------------------------------------------------------------------------

--iSubst :: Int -> ITerm -> ITerm -> ITerm
iSubst f ii (Pi r ty ty')    = Pi r (cSubst' f ii ty) (cSubst' f (ii + 1) ty')
iSubst f ii (Var j)      = f ii j
iSubst f ii (i :$: c)      = iSubst f ii i :$: cSubst' f ii c
iSubst f ii x              = case x of
    Ann a b -> Ann (g a) (g b)
    CCon con a b -> CCon con (g <$> a) (g <$> b)
    ITCon con a -> ITCon con (g <$> a)
    Prim con a -> Prim con (g <$> a)
    ICase con a b c d e -> ICase con (g <$> a) (g b) (g <$> c) (g <$> d) (g e)
    x   -> x
  where
    g = cSubst' f ii

--cSubst' :: Int -> ITerm -> CTerm -> CTerm
cSubst' f ii (Inf i)      =  Inf (iSubst f ii i)
cSubst' f ii (Lam c)      =  Lam (cSubst' f (ii + 1) c)

cSubst tt t = cSubst' (\ii j -> if Bound_ ii == j then t else Var j) tt
renum b = cSubst' $ \ii i -> case i of Bound_ i -> Bound $ if i >= ii then i + b else i; x -> Var x

--------------------------------------------------------------------------------

observe (VTag _ x) = x
observe x = x

vappT :: Value -> Value -> Value
vappT (VLam f) v = f v
vappT (VNeutral n) v = VNeutral (NApp n v)
vappT (VTag n f) v = VTag (NApp n v) $ observe $ vappT f v
vappT x v = error $ "vapp: " ++ show (x, v)

evalCaseT :: Value -> [Value] -> Value -> Value
evalCaseT m es n = case observe n of
    VCCon i args -> foldl vappT (es !! i) $ reverse args
    VNeutral n -> VNeutral $ NCase m es n
    x -> error $ "internal: eval caseT: " ++ show x

--------------------------------------------------------------------------------

vQuote = VNeutral . NQuote

quote0 :: Type -> Value -> CTerm
quote0 = quote 0

quote :: Int -> Type -> Value -> CTerm
quote ii (VPi Irr _ b) t = Lam $ quote (ii + 1) (b $ vQuote ii) $ t
quote ii (VPi _ _ b) (VLam t) = Lam $ quote (ii + 1) (b $ vQuote ii) $ t $ vQuote ii
quote ii ~VStar VStar = Inf Star
quote ii ~VStar (VPi r v f) = Inf $ Pi r (quote ii VStar v) $ quote (ii + 1) VStar $ f $ vQuote ii
quote ii t (VNeutral n) = Inf $ snd $ neutralQuote' ii n
quote ii _ (VCon con@(TConName _ _ _ ty _ _) vs) = ($ ty) $ chain (quote ii) vs $ \vs' -> const $ Inf $ ITCon con vs'
quote ii ~(VCon (TConName _ _ _ _ cs _)  ps) (VCCon i vs) = ($ ty) $ chain (quote ii) (take pnum ps) $ \ps' -> chain (quote ii) (reverse vs) $ \vs' -> const $ Inf $ CCon con ps' vs'
  where
    con@(CConName _ _ (TConName _ _ pnum _ _ _) ty) = cs !! i
quote ii t (VTag _ x) = quote ii t x
quote ii _ (VInt i) = Inf $ IInt i
--quote ii b c = error $ "quote: " ++ show (ii, c)

neutralQuote' :: Int -> Neutral -> (Type, ITerm)
neutralQuote' ii (NQuote k) | k >= 0 = (error "nq", Bound (ii - k - 1))
neutralQuote' ii (NApp n v) = (ty' v, f :$: quote ii ty v)  where (VPi _ ty ty', f) = neutralQuote' ii n
neutralQuote' ii (NPrim con@(PrimName' _ _ _ ty) ps) = ($ ty) $ chain (quote ii) ps $ \ps' rt -> (rt, Prim con ps')
neutralQuote' ii (NLocal i t) = (t, Local i t)
neutralQuote' ii (NCase m ts x) = ($ ty) $ chain (quote ii) (take pnum ps_) $ \ps' -> chain (quote ii) [m] $ \[m'] -> chain (quote ii) ts $ \ts' -> chain (quote ii) (drop pnum ps_) $ \is' rt -> (rt, ICase con ps' m' ts' is' $ Inf x')
  where
    (VCon con@(TConName _ _ pnum _ _ ty) ps_, x') = neutralQuote' ii x

--------------------------------------------------------------------------------

quoteEq = eq 0 where

    eq, eq' :: Int -> Value -> Value -> Bool
    eq ii (VTag x _) (VTag x' _) | eqN ii x x' = True
    eq ii a b = eq' ii (observe a) (observe b)

    eq' ii (VLam t) (VLam t') = eq (ii + 1) (t $ vQuote ii) (t' $ vQuote ii)
    eq' ii (VPi r v f) (VPi r' v' f') = r == r' && eq ii v v' && eq (ii + 1) (f $ vQuote ii) (f' $ vQuote ii)
    eq' ii VStar VStar = True
    eq' ii (VNeutral n) (VNeutral n') = eqN ii n n'
    eq' ii (VCon con vs) (VCon con' vs') = con == con' && eqs ii vs vs'
    eq' ii (VCCon con vs) (VCCon con' vs') = con == con' && eqs ii vs vs'
    eq' ii _ _ = False

    eqN :: Int -> Neutral -> Neutral -> Bool
    eqN ii (NLocal v _) (NLocal v' _) = v == v'
    eqN ii (NQuote k) (NQuote k') = k == k'
    eqN ii (NApp n v) (NApp n' v') =  eqN ii n n' && eq ii v v'
    eqN ii (NCase m ts x) (NCase m' ts' x') = eqN ii x x' && eqs ii (m: ts) (m': ts')
    eqN ii _ _ = False

    eqs ii vs vs' = all (uncurry $ eq ii) (zip vs vs')

--------------------------------------------------------------------------------

idx 0 = \(x:_) -> x
idx 1 = \(_:x:_) -> x
idx 2 = \(_:_:x:_) -> x
idx 3 = \(_:_:_:x:_) -> x
idx 4 = \(_:_:_:_:x:_) -> x
idx 5 = \(_:_:_:_:_:x:_) -> x
idx 6 = \(_:_:_:_:_:_:x:_) -> x
idx 7 = \(_:_:_:_:_:_:_:x:_) -> x
idx 8 = \(_:_:_:_:_:_:_:_:x:_) -> x
idx 9 = \(_:_:_:_:_:_:_:_:_:x:_) -> x
idx n = \(_:_:_:_:_:_:_:_:_:_:xs) -> idx (n-10) xs

adj q rs i = sum $ map (\(r, _) -> q r 0 1) $ take i rs

ifV Rel _ a = a
ifV _ x _ = x

ifT Irr a _ = a
ifT _ _ x = x

appLoc t = asks $ (\ls -> let n = length ls in t $ zipWith (\i (_, t) -> VNeutral $ NLocal i t) [n-1,n-2..0] ls) . fst

type TCM m = ReaderT ([(Relevance, Value)], TEnv) (ExceptT String m)
type AddM m = StateT (TEnv, Int) (ExceptT String m)

convM :: Monad m => TCM m a -> AddM m a
convM m = gets fst >>= \te -> lift $ flip runReaderT ([], te) m

cTEval e t = convM $ ($ []) . snd <$> cType e t
cIEval t = convM $ ((id *** ($ [])) *** id) <$> iType t

cType :: Monad m => CTerm -> Type -> TCM m EnvValue2
cType (Inf e) v = do
    (x, v') <- iType e
    unless (quoteEq v v') (throwError ("type mismatch:\n" ++ "type inferred:  " ++ show v' ++ "\n" ++ "type expected:  " ++ show v ++ "\n" ++ "for expression: " ++ show e))
    return x
cType (Lam e) (observe -> VPi r ty ty') = do
    li <- asks $ VNeutral . flip NLocal ty . length . fst
    (x1, x2) <- local (((r, ty):) *** id) $ cType e $ ty' li
    return (ifV r x1 $ EVLam x1, ifT r x2 $ \d -> VLam (x2 . (: d)))
cType x y = throwError $ "type mismatch2:\n" ++ "term:  " ++ show x ++ "\ntype:  " ++ show y

-- TODO: avoid re-check
absLoc' :: Int -> Type -> Type -> Type -> Type
absLoc' i tt x = {-trace ("absLoc: " ++ show (i,tt,x)) $ -} either error id $ runIdentity $ runExceptT $ flip runReaderT mempty $ do
    (_, x2) <- local (((Rel, tt):) *** id) $ cType (cSubst' (\lev j -> case j of Local_ ii _ | i == ii -> Bound lev; _ -> Var j) 0 $ quote0 VStar x) VStar
    return $ \v -> x2 [v]

iType :: Monad m => ITerm -> TCM m (EnvValue2, Type)
iType (Ann e tyt) = do
    (_, ty) <- cType tyt VStar
    x <- appLoc ty
    v <- cType e x
    return (v, x)
iType Star = return (({-pure' VStar-} error "starr", pure VStar), VStar)
iType (ILam r tyt e) = do
    (_, ty) <- cType tyt VStar
    tt <- appLoc ty
    ((x1, x2), ty') <- local (((r, tt):) *** id) $ iType e
    n <- asks $ length . fst
    return ((ifV r x1 $ EVLam x1, ifT r x2 $ \d -> VLam (x2 . (: d))), VPi r tt $ absLoc' n tt ty')
iType (Pi r tyt tyt') = do
    (ty1, ty2) <- cType tyt VStar
    tt <- appLoc ty2
    (x1, x2) <- local (((Rel, tt):) *** id) $ cType tyt' VStar
    return (({-VPi r <$> ty1 <*> (x1 .) . flip (:)-} error "ittt", \d -> VPi r (ty2 d) (x2 . (: d))), VStar)
iType (Bound i) = asks $ \(ii, _) -> ((EVBound $ adj ifV ii i, idx $ adj ifT ii i), snd $ ii !! i)
iType (Global n) = asks (Map.lookup n . snd) >>= \case
    Just ((v1, v2), ty)   ->  return ((v1, pure v2), ty)
    Nothing        ->  throwError ("unknown identifier: " ++ n)
iType (e1 :$: e2) = do
    ((vi1, vi2), observe -> VPi r ty ty') <- iType e1
    (vb1, vb2) <- cType e2 ty
    tt <- appLoc vb2
    return ((ifV r vi1 $ vi1 .$ vb1, ifT r vi2 $ vappT <$> vi2 <*> vb2), ty' tt)
iType (ITCon con@(TConName _ _ _ ty _ _) ts) =
    (({-(VCon con <$>) . sequenceA . reverse-} error "itt" *** (VCon con . reverse <$>)) *** id) <$> foldM icont' (([], const []), ty) ts
iType (CCon con@(CConName i _ _ ty) ps ts) =
    (({-(VCCon con <$>) . sequenceA . reverse-} error "itt" *** (VCCon i <$>)) *** id) <$> foldM icont' (([], const []), ty) (ps ++ ts)
iType (IInt i) = return ((EVInt i, const $ VInt i), tInt)
iType (Local i t) = return ((undefined, const $ VNeutral $ NLocal i t), t)
iType (ICase con@(TConName _ _ _ _ _ ty) ps m ts ps' n) = error "icase"

icont' :: Monad m => (([EnvValue], Env -> [Value]), Type) -> CTerm -> TCM m (([EnvValue], Env -> [Value]), Type)
icont' ((vi1, vi2), observe -> VPi r ty ty') e2 = do
    (vb1, vb2) <- cType e2 ty
    tt <- appLoc vb2
    return ((ifV r vi1 $ vb1: vi1, ifT r vi2 $ (:) <$> vb2 <*> vi2), ty' tt)

--------------------------------------------------------------------------------

data EnvValue
    = EVLam EnvValue
    | EVApp [Int] EnvValue EnvValue
    | EVPrim PrimName
    | EVCase [Int]
    | EVCon Int Int Int
    | EVInt Int
    | EVBound !Int

--evCon 2 0 0 = EVLam $ EVLam $ EVBound 1
--evCon 2 1 0 = EVLam $ EVLam $ EVBound 0
--evCon 2 1 2 = EVLam $ EVLam $ EVLam $ EVLam $ EVBound 0 .$ EVBound 3 .$ EVBound 2
evCon i j k = EVCon i j k

--evCase is = case length is of 2 -> EVLam $ EVLam $ EVLam $ EVBound 0 .$ EVBound 2 .$ EVBound 1
evCase is = EVCase is

grad x = 1 + maximum ((-1): freeVars x)

freeVars = \case
    EVBound b -> b: []
    EVLam x -> filter (>=0) $ map (+(-1)) $ freeVars x
    EVApp gr _ _ -> gr
    _ -> []

EVLam z .$ b | null c || c == [0] || inlineable b = ssubst_ comp 0 z
  where
    c = count' 0 z
    comp i j = case compare i j of
        EQ -> ssubst_ (\i' j' -> EVBound $ j' + if j' >= i' then i else 0) 0 b
        LT -> EVBound (j-1)
        GT -> EVBound j
    inlineable = \case
        EVApp{} -> False
        x@EVLam{} -> null (tail c) -- && null (freeVars x)
--        EVBound{} -> all (==0) c  || null (tail c)
        _ -> True
a .$ b = EVApp (freeVars a ++ freeVars b) a b

count' = ssubst__ (const []) (map (+1)) (++) $ \i j -> if i == j then [0] else []
ssubst_ = ssubst__ id EVLam (.$)

ssubst__ f l a b i = \case
    z | grad z <= i -> f z
    EVLam v -> l $ ssubst__ f l a b (i + 1) v
    EVApp _ x y -> ssubst__ f l a b i x `a` ssubst__ f l a b i y
    EVBound j -> b i j
    x -> f x

--------------------------------------------------------------------------------

infixl 1 `YY`, `NN`, `NY`, `YN`

pattern SkipNY x = Skip x
pattern SkipY x = x

data GV x where
    GInt :: Int -> GV Int

    GSqrt :: GV (Int -> Int)
    GAdd :: GV (Int -> Int -> Int)
    GSub :: GV (Int -> Int -> Int)
    GMod :: GV (Int -> Int -> Int)
    GEq :: GV (Int -> Int -> a -> a -> a)
    GLess :: GV (Int -> Int -> a -> a -> a)

    GFix  :: GV ((a -> a) -> a)
    GFixS :: GV (a -> a) -> GV a
    GFixD :: GV (d -> a -> a) -> GV (d -> a)

    Con2_0_0 :: GV (a -> b -> a)
    Con2_1_0 :: GV (a -> b -> b)
    Con2_1_2 :: GV (x0 -> x1 -> c0 -> (x0 -> x1 -> e) -> e)

    Case2 :: GV (a -> b -> (a -> b -> c) -> c)

    Skip :: GV a -> GV (b -> a)
    SkipYN :: GV (d -> a) -> GV (d -> b -> a) -- (const .)
    Init :: GV (() -> a) -> GV a

    T2 :: GV ((a0, a1) -> e) -> GV (a0 -> a1 -> e) 

    Id :: GV (a -> a)
    Fst :: GV ((a, b) -> a)
    Snd :: GV ((a, b) -> b)

    YY :: GV (d -> a0 -> a1) -> GV (d -> a0) -> GV (d -> a1)
    YN :: GV (d -> a0 -> a1) -> GV a0 -> GV (d -> a1)
    NY :: GV (a0 -> a1) -> GV (d -> a0) -> GV (d -> a1)
    NN :: GV (a0 -> a1) -> GV a0 -> GV a1

    Del0 :: GV ((e, x) -> e)  -- Fst
    Del1 :: GV (((e, y), x) -> (e, x))
    Del2 :: GV ((((e, z), y), x) -> ((e, y), x))
    Del3 :: GV (((((e, v), z), y), x) -> (((e, z), y), x))

    YYr :: GV (d -> d1) -> GV (d -> d2) -> GV (d1 -> a -> b) -> GV (d2 -> a) -> GV (d -> b)

instance Show (GV x) where
 show x = parens' $ case x of
    GInt i -> show i :[]
    GSqrt -> "Sqrt" :[]
    GAdd -> "Add" :[]
    GMod -> "Mod" :[]
    GSub -> "Sub" :[]
    GEq -> "Eq" :[]
    GLess -> "Less" :[]

    GFix -> "Fix" :[]
    GFixS x -> "FixS" : show x :[]
    GFixD x -> "FixD" : show x :[]

    Skip x -> "Skip" : show x :[]

    T2 x -> "T2" : show x :[]
    SkipYN x -> "SkipYN" : show x :[]
    Init x -> "Init" : show x :[]

    Id -> "Id" :[]
    Fst -> "Fst" :[]
    Snd -> "Snd" :[]

    Con2_0_0 -> "Con2_0_0" :[]
    Con2_1_0 -> "Con2_1_0" :[]
    Con2_1_2 -> "Con2_1_2" :[]

    Case2 -> "Case2" :[]

    YY f x -> "YY" : show f : show x :[]
    YN a b -> "YN" : show a : show b :[]
    NY a b -> "NY" : show a : show b :[]
    NN a b -> "NN" : show a : show b :[]

    Del0 -> "Del0" :[]
    Del1 -> "Del1" :[]
    Del2 -> "Del2" :[]
    Del3 -> "Del3" :[]

    YYr a b c d -> "YYr": show a: show b: show c: show d: []
  where
    parens' [x] = x
    parens' x = "(" ++ unwords x ++ ")"

evv :: GV x -> x
evv = \case
    Del0 -> \(y, x) -> y
    Del1 -> \((e, y), x) -> (e, x)
    Del2 -> \(((e, z), y), x) -> ((e, y), x)
    Del3 -> \((((e, v), z), y), x) -> (((e, z), y), x)
    Del1 `NY` Del0 -> \(((e, z), y), x) -> (e, y)
    Del1 `NY` Del1 -> \(((e, z), y), x) -> (e, x)
    Del2 `NY` Del1 -> \((((e, v), z), y), x) -> ((e, z), x)

    GInt i -> i

    GSqrt -> \x -> round $ (sqrt :: Double -> Double) $ fromIntegral x
    GAdd -> (+)
    GMod -> mod
    GSub -> (-)
    GEq -> \a b -> if (a :: Int) == b then \_ x -> x else \x _ -> x
    GLess -> \a b -> if (a :: Int) < b then \_ x -> x else \x _ -> x

    GFix -> \f -> let x = f x in x
    GFixS (evv1 -> f) -> let x = f x in x
    GFixD (evv2 -> f) -> \d -> let x = f d x in x

    Con2_0_0 -> \c0 c1 -> c0
    Con2_1_0 -> \c0 c1 -> c1
    Con2_1_2 -> \x0 x1 c0 c1 -> c1 x0 x1

    Case2 -> \a b c -> c a b

    Id -> \x -> x
    Fst -> \(x, _) -> x
    Snd -> \(_, x) -> x
    NY (evv1 -> a) Fst -> \(x, _) -> a x

    Skip (Init (T2 (evv -> x))) -> \a0 a1 -> x ((), a1)
    Skip Id -> \_ x -> x
    Skip (evv -> x) -> \_ -> x

    SkipYN Id -> \x _ -> x
    SkipYN (evv -> x) -> \d _ -> x d
    Init (T2 (SkipYN Snd)) -> \x y -> x
    Init (T2 (T2 (T2 (evv -> x)))) -> \a0 a1 a2 -> x ((((), a0), a1), a2)
    Init (T2 (T2 (evv -> x))) -> \a0 a1 -> x (((), a0), a1)
    Init (T2 (evv -> x)) -> \a0 -> x ((), a0)
    Init (evv -> x) -> x ()
    T2 (T2 (evv -> x)) -> \a0 a1 a2 -> x ((a0, a1), a2)
    T2 (evv -> x) -> \a0 a1 -> x (a0, a1)

    (evv3 -> a0) `NN` (evv -> a1) `NN` (evv -> a2) `NN` (evv -> a3) -> a0 a1 a2 a3
    (evv3 -> a0) `NN` (evv -> a1) `NN` (evv -> a2) `NY` (evv -> a3) -> \d -> a0 a1 a2 (a3 d)
    (evv3 -> a0) `NN` (evv -> a1) `NY` (evv -> a2) `YN` (evv -> a3) -> \d -> a0 a1 (a2 d) a3
--    (evv3 -> a0) `NN` (evv -> a1) `NY` (evv -> a2) `YY` (evv -> a3) -> \d -> a0 a1 (a2 d) (a3 d)
    (evv3 -> a0) `NY` (evv -> a1) `YN` (evv -> a2) `YN` (evv -> a3) -> \d -> a0 (a1 d) a2 a3
--    (evv3 -> a0) `NY` (evv -> a1) `YN` (evv -> a2) `YY` (evv -> a3) -> \d -> a0 (a1 d) a2 (a3 d)
--    (evv3 -> a0) `NY` (evv -> a1) `YY` (evv -> a2) `YN` (evv -> a3) -> \d -> a0 (a1 d) (a2 d) a3
--    (evv3 -> a0) `NY` (evv -> a1) `YY` (evv -> a2) `YY` (evv -> a3) -> \d -> a0 (a1 d) (a2 d) (a3 d)
    (evv  -> a0) `YN` (evv -> a1) `YN` (evv -> a2) `YN` (evv -> a3) -> \d -> a0 d a1 a2 a3
{-
    (evv  -> a0) `YN` (evv -> a1) `YN` (evv -> a2) `YY` (evv -> a3) -> \d -> a0 d a1 a2 (a3 d)
    (evv  -> a0) `YN` (evv -> a1) `YY` (evv -> a2) `YN` (evv -> a3) -> \d -> a0 d a1 (a2 d) a3
    (evv  -> a0) `YN` (evv -> a1) `YY` (evv -> a2) `YY` (evv -> a3) -> \d -> a0 d a1 (a2 d) (a3 d)
    (evv  -> a0) `YY` (evv -> a1) `YN` (evv -> a2) `YN` (evv -> a3) -> \d -> a0 d (a1 d) a2 a3
    (evv  -> a0) `YY` (evv -> a1) `YN` (evv -> a2) `YY` (evv -> a3) -> \d -> a0 d (a1 d) a2 (a3 d)
    (evv  -> a0) `YY` (evv -> a1) `YY` (evv -> a2) `YN` (evv -> a3) -> \d -> a0 d (a1 d) (a2 d) a3
    (evv  -> a0) `YY` (evv -> a1) `YY` (evv -> a2) `YY` (evv -> a3) -> \d -> a0 d (a1 d) (a2 d) (a3 d)
-}
    (evv2 -> a0) `NN` (evv -> a1) `NN` (evv -> a2) -> a0 a1 a2
    (evv2 -> a0) `NN` (evv -> a1) `NY` (evv -> a2) -> \d -> a0 a1 (a2 d)
    (evv2 -> a0) `NY` (evv -> a1) `YN` (evv -> a2) -> \d -> a0 (a1 d) a2
--    (evv2 -> a0) `NY` (evv -> a1) `YY` (evv -> a2) -> \d -> a0 (a1 d) (a2 d)
    (evv  -> a0) `YN` (evv -> a1) `YN` (evv -> a2) -> \d -> a0 d a1 a2
{-
    (evv  -> a0) `YN` (evv -> a1) `YY` (evv -> a2) -> \d -> a0 d a1 (a2 d)
    (evv  -> a0) `YY` (evv -> a1) `YN` (evv -> a2) -> \d -> a0 d (a1 d) a2
    (evv  -> a0) `YY` (evv -> a1) `YY` (evv -> a2) -> \d -> a0 d (a1 d) (a2 d)
-}
--    YYr (evv -> f1) (evv -> f2) (evv  -> a0) (evv -> a1) `YN` (evv -> a2) -> \d -> let !d1 = f1 d; !d2 = f2 d in a0 d1 (a1 d2) a2

    NN (evv1 -> a0) (evv -> a1) -> a0 a1
    NY (evv1 -> a0) (evv -> a1) -> \d -> a0 (a1 d)
    YN (evv  -> a0) (evv -> a1) -> \d -> a0 d a1
    YY (evv  -> a0) (evv -> a1) -> \d -> a0 d (a1 d)

    YYr Id (evv -> f2) (evv1 -> f) (evv1 -> x) -> \d -> let !d2 = f2 d in f d (x d2)
    YYr (evv -> f1) Id (evv1 -> f) (evv1 -> x) -> \d -> let !d1 = f1 d in f d1 (x d)
    YYr (evv -> f1) (evv -> f2) (evv1 -> f) (evv1 -> x) -> \d -> let !d1 = f1 d; !d2 = f2 d in f d1 (x d2)

{-# INLINE evv1 #-}
evv1 :: GV (a -> b) -> a -> b
evv1 GSqrt x = round $ (sqrt :: Double -> Double) $ fromIntegral x :: Int
--evv1 GFix x = let y = x y in y
evv1 Fst x = fst x
evv1 Snd x = snd x
{-
evv1 (NY (evv1 -> a) Fst) ~(x, _) = a x
evv1 (NY (evv1 -> a) (evv1 -> b)) x = a (b x)
-}
evv1 x a = evv x a

{-# INLINE evv2 #-}
evv2 :: GV (a -> b -> c) -> a -> b -> c
evv2 GAdd a b = a + b
evv2 GMod a b = mod a b
evv2 GSub a b = a - b
evv2 GEq a b = if (a :: Int) == b then \_ x -> x else \x _ -> x
evv2 GLess a b = if (a :: Int) < b then \_ x -> x else \x _ -> x
evv2 Con2_1_2 x0 x1 = \c0 c1 -> c1 x0 x1
evv2 Case2 a b = \c -> c a b
--evv2 (T2 (evv1 -> x)) a0 a1 = x (a0, a1)
evv2 x a b = evv x a b

{-# INLINE evv3 #-}
evv3 :: GV (a -> b -> c -> d) -> a -> b -> c -> d
evv3 Case2 a b c = c a b
evv3 x a b c = evv x a b c

--------------------------------------------------------------------------------

yy :: GV (e -> a -> b) -> GV (e -> a) -> GV (e -> b)
Skip GFix `yy` Skip a1 = Skip (GFixS a1)
Skip GFix `yy` a1 = GFixD a1
Skip a `yy` Skip b = Skip $ a `NN` b
a `yy` Skip b = YN a b
Skip a `yy` b = NY a b
a `yy` b = YY a b

Id `ny` x = x
x `ny` Id = x
x `ny` y = NY x y

init' (T2 Snd) = Id
init' x = Init x

--------------------------------------------------------------------------------

uGV :: GV a -> GV b
uGV = unsafeCoerce

(Just  x, a) `yyR` (Just  y, b) = YYr (uGV x) (uGV y) a b
(Nothing, a) `yyR` (Just  y, b) = YYr Id (uGV y) a b
(Just  x, a) `yyR` (Nothing, b) = YYr (uGV x) Id a b
(Nothing, a) `yyR` (Nothing, b) = a `yy` b

evva_' ss a = (if null dif || null ss' then Nothing else Just $ xx dif, evva_ ss' a)
  where
    dif = map (fromJust . (`elemIndex` ss)) (ss \\ ss')
    ss' = filter (`elem` freeVars a) ss

    del_i :: Int -> GV ()
    del_i i = case i of
        0 -> uGV Del0
        1 -> uGV Del1
        2 -> uGV Del2
        3 -> uGV Del3

    xx :: [Int] -> GV ()
    xx ii = foldr1 (\x y -> uGV $ uGV x `NY` uGV y) $ map del_i $ reverse $ zipWith (-) ii [0,1..]

getLams (EVLam x)
    | null $ count' 0 x = Just (False, ssubst_ comp 0 x)
    | otherwise = Just (True, x)
  where
    comp i j = case compare i j of
        LT -> EVBound (j-1)
        GT -> EVBound j
getLams x = Nothing

evva_ :: [Int] -> EnvValue -> GV (Env -> Value)
evva_ ss = \case

    EVApp _ a b -> (id *** uGV) (evva_' ss a) `yyR` (id *** id) (evva_' ss b)

    z@(getLams -> Just (i, x))
        | b -> Skip $ case i of
            False -> uGV evva'x
            True  -> uGV $ init' $ T2 $ uGV evva'x
        | otherwise -> case i of
            False -> uGV $ SkipYN $ uGV evva'x
            True  -> uGV $ T2 $ uGV evva'x
      where
        b = null ss
        addSkip x = if b then uGV $ Skip x else x
        evva'x = evva_ ss' x

        n = if i then 1 else 0
        ss' = [0..n-1] ++ map (+n) ss

    EVBound i -> uGV Snd

    EVInt i -> Skip $ uGV $ GInt i

    EVPrim x -> Skip $ case x of
        Fix     -> uGV GFix
        Mod     -> uGV GMod
        Add     -> uGV GAdd
        Sub     -> uGV GSub
        IntEq   -> uGV GEq
        IntLess -> uGV GLess
        Sqrt    -> uGV GSqrt
    EVCase x -> Skip $ case length x of
        2       -> uGV Case2
        _ -> error $ "evpr: " ++ show x
    EVCon n i j -> Skip $ case (n, i, j) of
        (2, 0, 0)  -> uGV Con2_0_0
        (2, 1, 0)  -> uGV Con2_1_0
        (2, 1, 2)  -> uGV Con2_1_2
        _ -> error $ "evpr: " ++ show (i, j)

evval :: ((EnvValue, Value), Type) -> (GV a, Value)
evval ((x, _), ty) = (uGV y, tr ty $ evv y)
  where
    y = init' $ uGV $ evva_ [] x
    tr (VCon (TConName Int _ _ _ _ _) _) x = VInt (unsafeCoerce x :: Int)

    init' :: GV (() -> b) -> GV b
    init' (Skip x) = x

-------------------------------------------------------------------------------- interpreter

addToEnv s x = modify $ Map.insert s x *** id

data Stmt
    = Let String ITerm
    | Data String [CTerm] CTerm [(String, CTerm)]
    | Primitive String CTerm

handleStmt :: MonadFix m => Stmt -> AddM m ()
handleStmt (Let n t) = cIEval t >>= addToEnv n

handleStmt (Primitive s t) = do
    vt <- cTEval t VStar
    let n = primNames Map.! s
        ePT = case n of
            -- TODO: make it more efficient?
            Fix -> \case (VInt t: VLam g: _) -> let r = VTag (NQuote $ negate $ 1 + t {-TODO!-}) $ g r in r; vs -> f vs
            Add -> \case (VInt x: VInt y: _) -> VInt $ x + y; vs -> f vs
            Sub -> \case (VInt x: VInt y: _) -> VInt $ x - y; vs -> f vs
            Mod -> \case (VInt x: VInt y: _) -> VInt $ x `mod` y; vs -> f vs
            Sqrt -> \case (VInt x: _) -> VInt $ round $ sqrt $ fromIntegral x; vs -> f vs
            IntEq -> \case (VInt x: VInt y: _) -> vBool $ x == y; vs -> f vs
            IntLess -> \case (VInt x: VInt y: _) -> vBool $ x < y; vs -> f vs
        f = VNeutral . NPrim (PrimName' n s ePT vt)

    addToEnv s ((EVPrim n, lamsT'' vt ePT), vt)

handleStmt (Data s ps t_ cs) = do
   n <- case nameOf s of
            Just n -> return n
            Nothing -> do
                i <- gets snd
                modify $ id *** (+1)
                return $ UName i

   let ps' = ps --ps' <- mapM (\x -> quote0 <$> cTEval 0 te x VStar) ps       -- not needed
   vty <- cTEval (addParams Rel ps' t_) VStar

   mfix $ \ (~(cons_, caseTy_)) -> do

    let
      pnum = length ps
      cnum = length cs
      inum = arityq vty - pnum

      cn = TConName n s pnum vty cons_ caseTy_

      mkCon i (cs, ct_) = do
          ty <- cTEval (addParams Irr ps' ct_) VStar
          return (CConName i cs cn ty, arityq ty - pnum)

      mkCon' ccn@(CConName i cs _ ty) = (cs, ((evCon cnum i (length $ filter (== Rel) $ rels ty), lamsT'' ty $ VCCon i . reverse), ty))

    addToEnv s (({-pure' $ lams'' (rels vty) $ VCon cn-} error "pvcon", lamsT'' vty $ VCon cn), vty)

    cons <- zipWithM mkCon [0..] cs

    let
      t_' = dropPi ps' $ quote0 VStar vty

      addConstr (ccn@(CConName j cstr _ cty), act) = iPi Rel
            $ mkMotive Rel (renum (1 + j) 0 $ dropPi ps' $ quote0 VStar cty)
            $ \tt -> Inf $ foldl (:$:) (Bound $ j + act) $ mkTT tt ++ [Inf $ CCon ccn (downTo (1 + j + act) pnum) (downTo 0 act)]
        where
          mkTT (Inf (ITCon c xs))
                | c == cn && take pnum xs == downTo (1 + j + act) pnum = drop pnum xs
                | otherwise = error $ "illegal data definition (parameters are not uniform) " ++ show (take pnum xs)
                        -- TODO: err

      mkMotive r (Inf (Pi _ x t)) e = iPi r x $ mkMotive r t e
      mkMotive r t e = e t

    caseTy <- flip cTEval VStar     -- null env possible
            $ addParams Irr ps'
            $ iPi TRel (mkMotive Rel t_' $ \(Inf Star){-TODO: err-} -> iPi Rel (Inf $ ITCon cn $ downTo inum pnum ++ downTo 0 inum) $ Inf Star)
            $ flip (foldr addConstr) cons
            $ mkMotive Irr (renum (1 + cnum) 0 t_')
            $ \(Inf Star) -> iPi Rel (Inf $ ITCon cn $ downTo (1 + cnum + inum) pnum ++ downTo 0 inum)
            $ Inf $ foldl (:$:) (Bound $ cnum + inum + 1) $ downTo 1 inum ++ [Inf $ Bound 0]

    let
      elims = case s of (c:cs) -> toLower c: cs ++ "Case"

      lamCase = evCase $ map snd cons
      lamCaseT = VLam $ \b -> lams' cnum $ VLam . evalCaseT b

      fcons = map fst cons

    addToEnv elims ((lamCase, lamCaseT), caseTy)
    mapM_ (uncurry addToEnv . mkCon') fcons

    return (fcons, caseTy)
   return ()

-------------------------------------------------------------------------------- parser

lang = makeTokenParser (haskellStyle { identStart = letter <|> P.char '_',
                                       reservedNames = ["forall", "let", "data", "primitive", "fix"] })

parseType vs = reserved lang "::" *> parseCTerm 0 vs
typedId vs = (,) <$> identifier lang <*> parseType vs

type Pars = CharParser Int

parseStmt_ :: [String] -> Pars Stmt
parseStmt_ e = do
     do Let <$ reserved lang "let" <*> identifier lang <* reserved lang "=" <*> parseITerm 0 e
 <|> do uncurry Primitive <$ reserved lang "primitive" <*> typedId []
 <|> do
      x <- reserved lang "data" *> identifier lang
      let params vs = option [] $ parens lang (typedId vs) >>= \xt -> (xt:) <$> params (fst xt: vs)
      (nps, ts) <- unzip <$> params []
      let parseCons = option [] $ (:) <$> typedId nps <*> option [] (reserved lang ";" *> parseCons)
      Data x ts <$> parseType nps <* reserved lang "=" <*> parseCons

parseITerm :: Int -> [String] -> Pars ITerm
parseITerm 0 e =
   do reserved lang "forall"
      (fe,(r,t):ts) <- rec (e, []) <|> xt Rel (e, [])
      reserved lang "."
      t' <- parseCTerm 0 fe
      return $ foldl (\p (r, t) -> Pi r t (Inf p)) (Pi r t t') ts
 <|> do try $ parseITerm 1 e >>= \t -> option t $ rest (Inf t)
 <|> do parens lang (parseLam e) >>= rest
 where
    rec b = (parens lang (xt Rel b) <|> braces lang (braces lang (xt Irr b) <|> xt TRel b)) >>= \x -> option x $ rec x
    xt r (e, ts) = ((:e) *** (:ts) . (,) r) <$> typedId e
    rest t = Pi Rel t <$ reserved lang "->" <*> parseCTerm 0 ([]:e)
parseITerm 1 e =
     do try $ parseITerm 2 e >>= \t -> option t $ rest (Inf t)
 <|> do parens lang (parseLam e) >>= rest
 where
    rest t = Ann t <$> parseType e
parseITerm 2 e = foldl (:$:) <$> parseITerm 3 e <*> many (optional (P.char '!') >> parseCTerm 3 e)
parseITerm 3 e =
     do Star <$ reserved lang "*"
 <|> do IInt . fromIntegral <$ P.char '#' <*> natural lang
 <|> do toNat <$> natural lang
 <|> do reserved lang "fix"
        i <- P.getState
        P.setState (i+1)
        return $ Global "primFix" :$: Inf (IInt i)
 <|> parseILam e
 <|> do identifier lang >>= \x -> return $ maybe (Global x) Bound $ findIndex (== x) e
 <|> parens lang (parseITerm 0 e)
  
parseCTerm :: Int -> [String] -> Pars CTerm
parseCTerm 0 e = parseLam e <|> Inf <$> parseITerm 0 e
parseCTerm p e = try (parens lang $ parseLam e) <|> Inf <$> parseITerm p e
  
parseLam :: [String] -> Pars CTerm
parseLam e = do
    xs <- reservedOp lang "\\" *> many1 (identifier lang) <* reservedOp lang "->"
    t <- parseCTerm 0 (reverse xs ++ e)
    return $ iterate Lam t !! length xs

parseILam :: [String] -> Pars ITerm
parseILam e = do
    reservedOp lang "\\"
    (fe, ts) <- rec (e, []) <|> xt Rel (e, [])
    reserved lang "->"
    t' <- parseITerm 0 fe
    return $ foldl (\p (r, t) -> ILam r t p) t' ts
 where
    rec b = (parens lang (xt Rel b) <|> braces lang (braces lang (xt Irr b) <|> xt TRel b)) >>= \x -> option x $ rec x
    xt r (e, ts) = ((:e) *** (:ts) . (,) r) <$> typedId e

--------------------------------------------------------------------------------

primes :: [Int]
primes = 2:3: filter (\n -> and $ map (\p -> n `mod` p /= 0) (takeWhile (\x -> x <= round (sqrt $ fromIntegral n)) primes)) [5,7..]

check f m = do
    x <- readFile f
    case P.runParser (whiteSpace lang >> many (parseStmt_ []) >>= \ x -> eof >> return x) 0 f x of
      Left e -> error $ show e
      Right stmts -> do
        runExceptT $ flip evalStateT (tenv, 0) $ mapM_ handleStmt stmts >> m

main = getArgs >>= \case
  ["fast", n] -> print $ primes !! read n
  [read -> n] -> do
    let f = "primes.lam"
    v <- check f $ do
            handleStmt $ Let "main'" $ Global "main" :$: Inf (IInt n)
            gets $ fmap evval . Map.lookup "main'" . fst
    case v of
      Right (Just (x, y)) -> do
        putStrLn "typechecked and inlined expression:"
        putStrLn $ {- ppShow -} show x
        putStrLn "reduced value:"
        putStrLn $ show y
      e -> error $ show e

