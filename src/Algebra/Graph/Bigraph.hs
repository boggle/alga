module Algebra.Graph.Bigraph (
  Bigraph,

  Choose, choose
) where

import Prelude ()
import Prelude.Compat

import qualified Data.Either         as E
import qualified Algebra.Graph       as G
import qualified Algebra.Graph.Class as C

class Choose a where
  -- Run one of the two functions depending on a
  choose :: (a -> b) -> (a -> b) -> a -> b

instance Choose (Either a b) where
  choose g h e = (if E.isLeft e then g else h) e

data Bigraph g a = Bigraph {
  -- the actual bipartite graph
  graph :: g,
  -- vertices of the LHS partition
  left :: g,
  -- vertices of the RHS partition
  right :: g,
  -- difference in number of vertices between left and right
  balance :: Int
}

instance (C.Graph g, C.Vertex g ~ a, C.ToGraph g, C.ToVertex g ~ a, Choose a) => C.Graph (Bigraph g a) where
  -- Bigraph uses the same vertices as the underlying graph
  type Vertex (Bigraph g a) = a

  -- empty graph, empty partitions
  empty = Bigraph { graph = C.empty, left = C.empty, right = C.empty, balance = 0 }

  -- vertex requires putting the vertex into the correct partition
  vertex = choose
      (\x ->
         let single = C.vertex x
         in Bigraph { graph = single, left = single, right = C.empty, balance = 1 })
      (\y ->
         let single = C.vertex y
         in Bigraph { graph = single, left = C.empty, right = single, balance = -1 })

  -- overlay of two bipartite graphs is again a bipartite graph
  overlay l r =
    Bigraph
    { graph = graph l `C.overlay` graph r
    , left = left l `C.overlay` left r
    , right = right l `C.overlay` right r
    , balance = balance l + balance r
    }

  -- connect is overlay plus all extra edges between the two partitions
  connect l r = base { graph = graph base `C.overlay` edges }
    where
      base = l `C.overlay` r
      edges = (if balance base <= 0 then connectLeft else connectRight) (left l) (right r)
      connectLeft l r = G.foldg C.empty (\v -> C.vertex v `C.connect` r) C.overlay C.connect (C.toGraph l)
      connectRight l r = G.foldg C.empty (\v -> l `C.connect` C.vertex v) C.overlay C.connect (C.toGraph r)

instance (C.Graph (Bigraph g a)) => C.Bipartite (Bigraph g a)

instance (C.ToGraph g, C.ToVertex g ~ a) => C.ToGraph (Bigraph g a) where
  type ToVertex (Bigraph g a) = a
  toGraph big = C.toGraph (graph big)
