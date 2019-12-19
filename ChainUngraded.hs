module ChainUngraded where
import Control.Monad
import Data.List (intercalate)
import qualified Data.Map as M

-- This is pretty unsatisfactory, IMO

-- | We cannot ensure that all the chain layers are of the same
-- grade. So I can create something of the sort:
-- CLayer ([CRet 'b', CLayer [(2, CRet 'a')] ])
-- which is incorrect!
data Chain a = CRet a | CLayer [(Chain a, Int)] deriving(Eq, Ord)


instance (Ord a, Show a) => Show (Chain a) where
  show (CRet a) = show a
  show layer = case simplifyChain layer of
                (CLayer ais) -> "<" <> intercalate " + " [ show i <> show a | (a, i) <- ais] <> ">"

scaleChain :: Int -> Chain a -> Chain a
scaleChain _ (CRet a) = error "cannot scale 0D chain"
scaleChain j (CLayer ais) = CLayer [(a, i*j) | (a, i) <- ais]

simplifyChain :: Ord a => Chain a -> Chain a
simplifyChain (CLayer ais) = 
  CLayer $ M.toList $ M.filter (/= 0) $ M.fromListWith (+) ais
simplifyChain a = a

instance Semigroup (Chain a) where
    CLayer as <> CLayer bs = CLayer $ as <> bs
    _ <> _ = error $ "cannot concat chains of unequal grade"

-- | We can only create a unit for a grade 0 object
instance Monoid (Chain a) where
   mempty = CLayer []

class Monoid a => AbelianGroup a where
    inv :: a -> a

instance AbelianGroup (Chain a) where
    inv (CLayer ais) = CLayer [(a, -i) | (a, i) <- ais]
    inv _ = error "cannot invert a degree 0 object"

instance Monad Chain where
  return = CRet
  -- Chain a -> (a -> Chain b) -> Chain b
  (CRet a) >>= f = f a
  (CLayer ais) >>= f =
    CLayer [(a >>= f, i) | (a, i) <- ais]

instance Applicative Chain where
  pure = return
  (<*>) = ap

instance Functor Chain where
    fmap f ca = ca >>= (return . f)

-- | We need the inner data to be a chain for this to work
chainBoundary :: Ord a => Chain a -> Chain a
chainBoundary (CRet a) = error "cannot take boundary of 0D chain"
chainBoundary (CLayer ais) =  
    mconcat [(scaleChain i a) | (a, i) <- ais]
a, b, c, ab, bc, ca, abc :: Chain Char
a = CRet 'a'
b = CRet 'b'
c = CRet 'c'
ab = CLayer [(b, 1), (a, -1)]
bc = CLayer [(c, 1), (b, -1)]
ca = CLayer [(a, 1), (c, -1)]
abc = CLayer [(ab, 1), (bc, 1), (ca, 1)]

main :: IO ()
main = do
  putStrLn "abc:"  
  print abc
  putStrLn "D abc:"  
  print (chainBoundary abc)
  -- putStrLn "D ab:"  
  -- print (chainBoundary ab)

