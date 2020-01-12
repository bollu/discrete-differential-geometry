-- | Intrinsic representation of meshes.
module Intrinsic where
import qualified Data.Map as M

type F = Float
type Angle = Float

data V2 = V2 { v2x :: F, v2y :: F }
data V3 = V3 { v3x :: F, v3y :: F, v3z :: F }

data Bary = Bary { bx :: F, by :: F, bz :: F }

data Tri v = Tri { ta:: v, tb :: v, tc :: v }

-- | Edge, has two endpoints and a length
data Edge v = Edge { ea :: v, eb :: v, el :: F }
data Halfedge v = Halfedge { hfwd :: Edge v, hbwd :: Edge v }

class Intrinsic i v where
  -- | nmber of edges in the mesh that contain vertex v
  degree :: i v -> Int
  -- | create a new vertex in the mesh
  newvertex :: i v -> (v, i v)
  -- | erase triangles
  eraseTris :: i v -> i v
  -- | add triangles
  insertTris :: i v -> i v

-- | get the angle for the 3 edge lengths
angle :: F -> F -> F -> Angle
angle = undefined

-- | given the edge lengths and the angle in the middle, return the final angle
edgelen :: F -> Angle -> F -> F
edgelen = undefined

-- | return the smallest angle between the two angles
anglebetween :: Angle -> Angle -> Angle
anglebetween = undefined

-- | get angle between two vectors
argument :: V2 -> V2 -> Angle
argument = undefined


tracevector :: Bary -- ^ Starting point
  -> V2 -- ^ direction to walk
  -> (Tri, Bary)
tracevector = undefined

updateSignpost :: Intrinsic i v => i v -> (v, v, v) -> i v
