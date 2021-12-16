module AoC.Lib.Graph where

import AoC.Prelude
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as MinPQueue
import Data.Set qualified as Set

dijkstra ::
  (Ord node) =>
  (node -> [(node, Int)]) ->
  node ->
  (node -> Bool) ->
  [(node, Int)]
dijkstra f = let noHeur (n, c) = (n, c, 0) in astar (fmap noHeur . f)

astar ::
  forall node.
  (Ord node) =>
  (node -> [(node, Int, Int)]) ->
  node ->
  (node -> Bool) ->
  [(node, Int)]
astar nexts start isDest = go Set.empty (MinPQueue.singleton 0 start)
  where
    go :: Set node -> MinPQueue Int node -> [(node, Int)]
    go seen unseen = case MinPQueue.minViewWithKey unseen of
      Nothing -> []
      Just ((cost, node), unseen')
        | isDest node -> [(node, cost)]
        | Set.member node seen -> go seen unseen'
        | otherwise ->
          let f q (node', cost', heur) = MinPQueue.insert (cost + cost' + heur) node' q
           in (node, cost) : go (Set.insert node seen) (foldl' f unseen' (nexts node))
