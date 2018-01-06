{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds   #-}


module Tests.Generic where

import Test.Hspec
import Control.Applicative (liftA2)

compareIoActions :: (HasCallStack, Show a, Eq a) => IO a -> IO a -> Expectation
compareIoActions action1 action2 = liftA2 (==) action1 action2 `shouldReturn` True
