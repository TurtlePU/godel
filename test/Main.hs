{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Test.Tasty (defaultMain)
import Test.Tasty.Falsify (testProperty)
import Godel.Module.Discovery (Discover (..))
import Control.Monad (guard)
import Data.Functor (($>))
import Data.Maybe (isJust)
import Control.Monad.Trans.Reader (ReaderT, ask)

newtype TestDir = MkTestDir [(Maybe TestDir, Maybe [Int])]

testDiscover :: Discover (ReaderT TestDir Maybe) [Int] Int (Bool, Int)
testDiscover = MkDiscover
    { dirFiles = foldl'
        (\df ix -> do
            MkTestDir cs <- ask
            guard (0 <= ix && ix < length cs)
            _
        )
        do
            MkTestDir cs <- ask
            pure [ (isJust cs' || isJust ms, i) | ((cs', ms), i) <- zip cs [0..] ]
    , asModName = uncurry $ ($>) . guard
    , declaredNames = foldl' _ _
    , appendModName = flip (:)
    }

main :: IO ()
main = defaultMain $ testProperty "domain discovery test" do
    _
