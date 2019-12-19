module ChainUngraded where
import Control.Monad
import Data.List (intercalate)
import qualified Data.Map as M


-- This is pretty unsatisfactory, IMO

-- | We cannot ensure that all the chain layers are of the same
-- grade. So I can create something of the sort:
-- Boundary ([Vertex 'b', Boundary [(2, CRet 'a')] ])
-- which is incorrect!
data Chain a = Vertex a | Boundary [(Chain a, Int)] deriving(Eq, Ord)



instance (Ord a, Show a) => Show (Chain a) where
  show (Vertex a) = show a
  show layer = case simplifyChain layer of
                (Boundary ais) -> "<" <> intercalate " + " [ show i <> show a | (a, i) <- ais] <> ">"

scaleChain :: Int -> Chain a -> Chain a
scaleChain _ (Vertex a) = error "cannot scale 0D chain"
scaleChain j (Boundary ais) = Boundary [(a, i*j) | (a, i) <- ais]

simplifyChain :: Ord a => Chain a -> Chain a
simplifyChain (Boundary ais) = 
  Boundary $ M.toList $ M.filter (/= 0) $ M.fromListWith (+) ais
simplifyChain a = a

instance Semigroup (Chain a) where
    Boundary as <> Boundary bs = Boundary $ as <> bs
    _ <> _ = error $ "cannot concat chains of unequal grade"

-- | We can only create a unit for a grade 0 object
instance Monoid (Chain a) where
   mempty = Boundary []

class Monoid a => AbelianGroup a where
    inv :: a -> a

instance AbelianGroup (Chain a) where
    inv (Boundary ais) = Boundary [(a, -i) | (a, i) <- ais]
    inv (Vertex v) = Boundary[(Vertex v, -1)]

(<>-) :: Chain a -> Chain a -> Chain a
(<>-) c c' = c <> inv c'

(*<>) :: Int -> Chain a -> Chain a
i *<> c = scaleChain i c


instance Monad Chain where
  return = Vertex
  -- Chain a -> (a -> Chain b) -> Chain b
  (Vertex a) >>= f = f a
  (Boundary ais) >>= f =
    Boundary [(a >>= f, i) | (a, i) <- ais]

instance Applicative Chain where
  pure = return
  (<*>) = ap

instance Functor Chain where
    fmap f ca = ca >>= (return . f)

-- | We need the inner data to be a chain for this to work
chainCollapseLayer :: Ord a => Chain a -> Chain a
chainCollapseLayer (Vertex a) = error "cannot take boundary of 0D chain"
chainCollapseLayer (Boundary ais) =  
    mconcat [(scaleChain i a) | (a, i) <- ais]
a, b, c, ab, bc, ca, abc, abbc :: Chain Char
a = Vertex 'a'
b = Vertex 'b'
c = Vertex 'c'
ab = Boundary [(b, 1), (a, -1)]
bc = Boundary [(c, 1), (b, -1)]
ca = Boundary [(a, 1), (c, -1)]
abc = Boundary [(ab, 1), (bc, 1), (ca, 1)]
abbc = Boundary [(ab, 1), (bc, 1)]

main :: IO ()
main = do
  putStrLn "vvv[ChainUngraded]vvv"
  putStrLn $ "abc:"  <> show abc
  putStrLn $ "D^2 abc:"  <> show (chainCollapseLayer abc)
  putStrLn $ "abbc:"  <> show abbc
  putStrLn $ "D^2 abbc:"  <> show (chainCollapseLayer abbc)
  putStrLn $ "ab:"  <> show ab
  -- print (chainCollapseLayer ab)
  putStrLn "^^^[ChainUngraded]^^^"
