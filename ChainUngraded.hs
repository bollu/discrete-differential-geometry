module ChainUngraded where
import Control.Monad
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Monoid(Sum, Sum(..))

type FreeAbTerm a = (a, Int)

data FreeAb a = FreeAb [FreeAbTerm a] deriving(Eq, Ord)
instance Show a => Show (FreeAb a) where
  show (FreeAb ais) = intercalate " + " [ show i <> show a | (a, i) <- ais]

instance Semigroup (FreeAb a) where
    (FreeAb as) <> (FreeAb bs) = FreeAb $ as <> bs

instance Monoid (FreeAb a) where
    mempty = FreeAb []

scaleTerm :: Int -> FreeAbTerm a -> FreeAbTerm a
scaleTerm i (a, j) = (a, i*j)

simplifyFreeAb :: Ord a => FreeAb a -> FreeAb a
simplifyFreeAb (FreeAb ais) = 
  FreeAb $ M.toList $ M.filter (/= 0) $ M.fromListWith (+) ais


class Monoid m => ZModule m where
  -- | action of Z 
  actZ :: Int -> m -> m
  (*<>) :: Int -> m -> m
  (*<>) = actZ


instance Num a => ZModule (Sum a) where
  actZ i (Sum r) = Sum $ fromIntegral i * r

instance ZModule (FreeAb a) where
  actZ i (FreeAb ais) = FreeAb $ map (scaleTerm i) ais
   

-- | This is a continuation: (a -> r) -> r
-- TODO: consider doing something with the continuation
evalFreeAb :: ZModule r => FreeAb a -> (a -> r) -> r
evalFreeAb (FreeAb ais) f = mconcat [i *<> f a | (a, i) <- ais]

-- This is pretty unsatisfactory, IMO

-- | We cannot ensure that all the chain layers are of the same
-- grade. So I can create something of the sort:
-- Boundary ([Vertex 'b', Boundary [(2, CRet 'a')] ])
-- which is incorrect!
data Chain a = Vertex a | Boundary { boundary :: FreeAb (Chain a) } deriving(Eq, Ord)


scaleChain :: Int -> Chain a -> Chain a
scaleChain i (Boundary f) = Boundary (i *<> f)
scaleChain i _ = error "canot scale 0D chain"

instance (Ord a, Show a) => Show (Chain a) where
  show (Vertex a) = show a
  show (Boundary ais) = case simplifyFreeAb ais of
                        ais -> "<" <> show ais <> ">"



{-
(<>-) :: Chain a -> Chain a -> Chain a
(<>-) c c' = c <> inv c'

(*<>) :: Int -> Chain a -> Chain a
i *<> c = scaleChain i c
-}


-- | We need the inner data to be a chain for this to work
chainCollapseLayer :: Ord a => Chain a -> FreeAb (Chain a)
chainCollapseLayer (Vertex a) = error "cannot take boundary of 0D chain"
chainCollapseLayer (Boundary ais) = simplifyFreeAb $ evalFreeAb ais (boundary)

a, b, c, ab, bc, ca, abc, abbc :: Chain Char
a = Vertex 'a'
b = Vertex 'b'
c = Vertex 'c'
ab = Boundary $ FreeAb $ [(b, 1), (a, -1)]
bc = Boundary $ FreeAb $ [(c, 1), (b, -1)]
ca = Boundary $ FreeAb $ [(a, 1), (c, -1)]
abc = Boundary $ FreeAb $ [(ab, 1), (bc, 1), (ca, 1)]
abbc = Boundary $ FreeAb $ [(ab, 1), (bc, 1)]


-- | differential form
data Form a = Function { runFunction :: (a -> Double) } | Der (Form a)


-- | integral omega df = integral d(omega) f
integrateForm :: Ord a => Show a => Form a -> Chain a -> Sum Double
integrateForm (Function f) (Vertex a) = Sum $ f a
integrateForm (Der f) (Boundary b) = evalFreeAb b (integrateForm f)
integrateForm f b = error $ "cannot integrate form on boundary: |" <> show b <> "|"

formabc :: Form  Char
formabc = Function $ \a -> 
  case a of
    'a' -> 1
    'b' -> 2
    'c' -> 3

main :: IO ()
main = do
  putStrLn "vvv[ChainUngraded]vvv"
  putStrLn $ "abc:"  <> show abc
  putStrLn $ "D^2 abc:"  <> show (chainCollapseLayer abc)
  putStrLn $ "abbc:"  <> show abbc
  putStrLn $ "D^2 abbc:"  <> show (chainCollapseLayer abbc)
  putStrLn $ "ab:"  <> show ab

  putStrLn $ "integrating dformabc on ab" <> show (integrateForm (Der formabc) ab)
  putStrLn $ "integrating ddformabc on abc" <> show (integrateForm (Der (Der formabc)) abc)
  -- print (chainCollapseLayer ab)
  putStrLn "^^^[ChainUngraded]^^^"


-- So far, we have homology and cohomology. Next, we need 
