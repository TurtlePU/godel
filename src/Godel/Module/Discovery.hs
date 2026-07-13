{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Godel.Module.Discovery where

import Data.Tree (Tree)
import Data.Either (partitionEithers)
import Data.Set (Set, (\\))
import Control.Monad.Trans.Writer.CPS (runWriterT, tell)
import Control.Monad.Trans.Class (lift)
import Data.Set qualified as Set
import Data.Maybe (fromMaybe)
import qualified Data.Tree as Tree

data Discover m p a b = MkDiscover
  { dirFiles :: p -> m [b]
  , asModName :: b -> Maybe a
  , declaredNames :: p -> m (Maybe (Set a))
  , appendModName :: p -> a -> p
  }

data DiscoveryWarning a b p
  = NotAModule p b
  | UndeclaredModule p a
  | MissingModule p a
  deriving (Eq, Ord, Functor)

discover ::
    (Monad m, Ord p, Ord a, Ord b) =>
    Discover m p a b -> (p, a) -> m (Tree a, Set (DiscoveryWarning a b p))
discover d = fmap (Set.mapMonotonic (fmap fst) <$>) . runWriterT . impl snd d
    { dirFiles = lift . dirFiles d . fst
    , declaredNames = lift . declaredNames d . fst
    , appendModName = \(p, _) new -> (appendModName d p new, new)
    }
 where
  impl lastModName MkDiscover {..} = go
   where
    go root = do
      df <- dirFiles root
      let (Set.fromList -> notMods, Set.fromList -> present) = partitionEithers
            [ maybe (Left file) Right (asModName file) | file <- df ]
      tell $ Set.mapMonotonic (NotAModule root) notMods
      declared <- fromMaybe present <$> declaredNames root
      tell $ Set.mapMonotonic (UndeclaredModule root) (present \\ declared)
      tell $ Set.mapMonotonic (MissingModule root) (declared \\ present)
      Tree.Node (lastModName root) <$> traverse (go . appendModName root)
        (Set.toList $ Set.intersection declared present)
