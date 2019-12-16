{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Arr where
import Data.Traversable
import qualified Data.Map.Strict as M
import Control.Arrow
import Data.List (intercalate)

-- | Width of the vector instruction
newtype Width = Width Int

-- | Identifier
type Id = String

-- | Index expression
type Ix = Exp

-- | Length expression
type Length = Exp


-- | Eliminate superfluous skip expressions in the code.
elimSkip :: Code -> Code
elimSkip (Skip :>>: c) = elimSkip c
elimSkip (c :>>: Skip) = elimSkip c
elimSkip (c :>>: c') = elimSkip c :>>: elimSkip c'
elimSkip c = c

data Value =
  IntVal Int
  | FloatVal Float
  | BoolVal Bool
  deriving(Eq, Ord)

instance Show Value where
  show (IntVal i) = show i
  show (FloatVal f) = show f
  show (BoolVal b) = show b


data Exp =
  Var Id
  | MatMul Exp Exp
  | Literal Value
  | Index Id Ix
  | Exp :+: Exp
  | Exp :-: Exp
  | Exp :*: Exp
  | Mod Exp Exp
  | Div Exp Exp
  | Eq  Exp Exp
  | Gt  Exp Exp
  | LEq Exp Exp
  | Min Exp Exp
  | IfThenElse Exp Exp Exp 
  deriving(Eq, Ord)

showSexp :: [String] -> String
showSexp as =  "(" <> (intercalate " " as) <> ")"

instance Show Exp where
  show (Var id) = id
  show (MatMul e e') = showSexp $ ["matmul", show e, show e']
  show (Literal l) = show l
  show (Index id ix) = id <> "[" <> show ix <> "]"
  show (e :+: e') = showSexp $ ["+", show e, show e']
  show (e :*: e') = showSexp $ ["*", show e, show e']
  show (Mod e e') = showSexp $ ["%", show e, show e']
  show (Div e e') = showSexp $ ["/", show e, show e']
  show (Eq e e') = showSexp $ ["==", show e, show e']
  show (LEq e e') = showSexp $ ["<=", show e, show e']
  show (Min e e') = showSexp $ ["min", show e, show e']
  show (IfThenElse i t e) = 
    showSexp $ ["if", show i, show t, show e]

instance Num Exp where
  (+) = (:+:)
  (-) = (:-:)
  (*) = (:*:)
  fromInteger = Literal . IntVal . fromInteger
  abs = error "no abs on Exp"
  signum = error "no signum on Exp"

data Code =
  Skip
  | Code :>>: Code
  | For Id Exp Code
  | Allocate Id Length
  | Write Id Ix Exp
  deriving(Eq, Ord)

instance Show Code where
  show Skip = "skip"
  show (c :>>: c') = show c <> ";\n" <> show c'
  show (For id lim body) =
    showSexp ["for", showSexp [id, "<=", show lim], show body]
  show (Allocate id len) = showSexp $ ["alloc", id, show len]
  show (Write id ix exp) = 
    showSexp $ [id <> "[" <> show ix <> "]", ":=", show exp]


instance Semigroup Code where
  (<>) = (:>>:)

instance Monoid Code where
  mempty = Skip

newtype CM a = CM {runCM :: Integer -> (Integer, Code, a) }

instance Functor CM where
  fmap f cm =
    CM $ \i ->
      let (i', c, a) = runCM cm i
      in (i', c, f a)

instance Applicative CM where
  pure = return

  cma2b <*> cma = do
   a <- cma
   a2b <- cma2b
   return $ a2b a

instance Monad CM where
  return a = CM $ \i -> (i, mempty, a)
  cm >>= f = CM $ \i ->
    let (i', c', a') = runCM cm i
        (i'', c'', a'') = runCM (f a') i'
    in (i'', c' <> c'', a'')

-- | Generate a new ID
newID :: String -> CM Id
newID name = CM $ \i -> (i+1, mempty,  (name <> "-" <> show i))

-- | Append a section of code
appendCode :: Code -> CM ()
appendCode c = CM $ \i -> (i, c, ())

-- | Run a CM to extract out the code. Useful to generate code
-- | and then transplant to another location while ensure we 
-- | do not create overlapping IDs
extractCMCode :: CM () -> CM Code
extractCMCode cm =
  CM $ \i ->
    let (i', c, _) = runCM cm i
    in (i', mempty, c)

-- | Generate code from the CM
genCMCode :: CM () -> Code
genCMCode cm = let (_, c, _) = runCM cm 0 in c 

--- for loop
for_ :: Exp -- ^ Limit of the loop. Variable goes from 0 <= v <= limit
  -> (Exp -> CM ()) -- ^ Function that receives the loop induction variable and generates the loop body
  -> CM ()
for_ lim f = do
  id <- newID "%iv"
  code <- extractCMCode $ f (Var id)
  appendCode $ For id lim code

-- | A chunk of linear memory with an ID and a length attached to it
data CMMem = CMMem Id Length

-- | Generate an index expression into the CMMem
cmIndex :: CMMem -> Ix -> Exp
cmIndex (CMMem name _) ix = Index name ix

-- | Generate a write statement into the CMMem
cmWrite ::CMMem -- ^ Array to be written
  -> Ix -- ^ Index to write to
  -> Exp -- ^ Value to write
  -> CM ()
cmWrite (CMMem name _) ix v =
  appendCode $ Write name ix v

-- | Defunctionalized push array
data PushT where
  Generate :: Length -> (Ix -> Exp) -> PushT
  Use :: CMMem -> PushT
  Map :: (Exp -> Exp) -> PushT -> PushT
  Append :: Length -> PushT -> PushT -> PushT

-- | Compute the length of a PushT
pushTLength :: PushT -> Length
pushTLength (Generate l _ ) = l
pushTLength (Use (CMMem _ l)) = l
pushTLength (Map _ p) = pushTLength p
pushTLength (Append l p1 p2) = pushTLength p1 + pushTLength p2

-- | code to index into the PushT
index :: PushT -> Ix -> Exp
index (Generate n ixf) ix = ixf ix
index (Use (CMMem id _)) ix = Index id ix
index (Map f p) ix = f (index p ix)
index (Append l p p') ix =
  IfThenElse
    (Gt ix l)
    (index p' (ix - l))
    (index p ix)

-- | Generate code from a pushT given an index and an expression for
-- | the value at that index
apply :: PushT ->  (Ix -> Exp -> CM ()) -> CM ()
apply (Generate l ix2v) k = 
  for_ l (\ix -> k ix (ix2v ix))
apply (Use cmem@(CMMem _ n)) k = for_ n $ \ix -> k ix (cmIndex cmem ix)
apply (Map f p) k = apply p (\i a -> k i (f a))
apply (Append l p1 p2) k =
   apply p1 k >>
   apply p2 (\i a -> k (l + i) a)

-- | Code generate the allocation of an array and return a handle
-- | to the alocated array
allocate :: Length -> CM (CMMem)
allocate l  = do
  id <- newID "#arr"
  appendCode $ Allocate id l
  return (CMMem id l)


-- | Materialize an array, and return a handle to the materialized array
toVector :: PushT -> CM (CMMem)
toVector p = do
  -- | How do I get the length of the array I need to materialize?
  writeloc <- allocate (pushTLength p)
  apply p $ \ix val -> (cmWrite writeloc ix val)
  return $ writeloc

-- | Materialize an array and ignore the handle
toVector_ :: PushT -> CM ()
toVector_ p = toVector p >> pure ()

pushTZipWith :: (Exp -> Exp -> Exp) -> PushT -> PushT -> PushT
pushTZipWith f a1 a2 =
  Generate
    (min (pushTLength a1) (pushTLength a2))
    (\i -> f (index a1 i) (index a2 i))


saxpy :: Exp -- ^ a
  -> PushT -- ^ x
  -> PushT -- ^ b
  -> PushT
saxpy a x b = pushTZipWith (\x b -> a * x + b) x b

main :: IO ()
main = do
  let vec1 = CMMem "src1" 10
  let vec2 = CMMem "src2" 10
  let code = elimSkip $ genCMCode $ toVector_ $ saxpy 10 (Use vec1) (Generate 100 (\ix -> Div ix 2))
  print code
  
