
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.IO.Class

import Data.IORef (IORef, newIORef, readIORef)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Control.Monad.NestedResource

import Test.Control.Monad.NestedResource

main :: IO ()
main =
  hspec $ do
    testStack
    testNestedResource
    quickCheckNestedResource


testStack :: Spec
testStack =
  describe "IORef Stack for testing." $
    it "Push and pop works." $ do
      ref <- newIORef []
      stackPush ref 5 `shouldReturn` 1
      stackPush ref 6 `shouldReturn` 2
      stackPush ref 7 `shouldReturn` 3
      readIORef ref `shouldReturn` [ 7, 6, 5 ]
      stackPop ref `shouldReturn` (Just 7, 2)
      stackPush ref 8 `shouldReturn` 3
      stackPop ref `shouldReturn` (Just 8, 2)
      stackPop ref `shouldReturn` (Just 6, 1)
      stackPop ref `shouldReturn` (Just 5, 0)
      stackPop ref `shouldReturn` (Nothing, 0)
      stackPop ref `shouldReturn` (Nothing, 0)


testNestedResource :: Spec
testNestedResource =
  describe "Test base behaviour:" $ do
    it "Works with empty action." $ do
      runNestedResource (pure True) `shouldReturn` True

    it "Can handle a single resource." $ do
      ref <- newIORef []
      runNestedResource $ do
        len <- nestIO (testAcquire ref) (testRelease ref)
        nestedLift $ len `shouldBe` 1
        nestedLift $ readIORef ref `shouldReturn` [0]
      readIORef ref `shouldReturn` []

    it "Can handle two resources." $ do
      ref <- newIORef []
      runNestedResource $ do
        len1 <- nestIO (testAcquire ref) (testRelease ref)
        nestedLift $ len1 `shouldBe` 1
        nestedLift $ readIORef ref `shouldReturn` [0]
        len2 <- nestIO (testAcquire ref) (testRelease ref)
        nestedLift $ len2 `shouldBe` 2
        nestedLift $ readIORef ref `shouldReturn` [1, 0]
      readIORef ref `shouldReturn` []

    it "MonadPlus mplus is sane." $ do
      ref <- newIORef []
      let computation =
            mkInnerComputation ref $
              fail "Opps" `mplus` pure True
      computation `shouldReturn` True
      readIORef ref `shouldReturn` []

    it "Runs all testRelease actions after an ioError in testAcquire." $ do
      let err = userError "userError"
      ref <- newIORef []
      let computation x =
            runNestedResource $ do
              nestIO_ (testAcquire ref) (testRelease ref)
              nestIO_ (testAcquire ref) (testRelease ref)
              nestIO_ (ioError err >> pure x) (testRelease ref)
              nestIO_ (testAcquire ref) (testRelease ref)
      computation 42 `shouldThrow` (== err)
      readIORef ref `shouldReturn` []

    it "Runs all testRelease actions after an ioError in testRelease." $ do
      let err = userError "userError"
      ref <- newIORef []
      runNestedResource $ do
        nestIO_ (testAcquire ref) (testRelease ref)
        nestIO_ (testAcquire ref) (testRelease ref)
        nestIO_ (testAcquire ref) (\x -> testRelease ref x >> ioError err)
        nestIO_ (testAcquire ref) (testRelease ref)
      readIORef ref `shouldReturn` []

    it "Handles exceptions inside nestedLift." $ do
      let err = userError "userError"
      ref <- newIORef []
      let computation =
            mkInnerComputation ref $ ioError err
      computation `shouldThrow` (== err)
      readIORef ref `shouldReturn` []

    it "Handles error call." $ do
      ref <- newIORef []
      runNestedResource $ do
        nestIO_ (testAcquire ref) (testRelease ref)
        nestIO_ (testAcquire ref) (testRelease ref)
        nestIO_ (testAcquire ref) (testRelease ref)
        nestIO_ (testAcquire ref) (\x -> testRelease ref x >> error "Catch me!")
        nestIO_ (testAcquire ref) (testRelease ref)
      readIORef ref `shouldReturn` []

    it "Handles exceptions inside liftIO." $ do
      let err = TestException
      ref <- newIORef []
      let computation =
            mkInnerComputation ref $ throwIO err
      computation `shouldThrow` (== err)
      readIORef ref `shouldReturn` []

    it "Handles Monad fail." $ do
      ref <- newIORef []
      let computation =
            mkInnerComputation ref $ fail "Test failure"
      computation `shouldThrow` anyIOException
      readIORef ref `shouldReturn` []

    it "Handles MonadPlus mzero." $ do
      ref <- newIORef []
      let computation = mkInnerComputation ref mzero
      computation `shouldThrow` anyIOException
      readIORef ref `shouldReturn` []

    it "Handles partial functions." $ do
      ref <- newIORef []
      let computation =
            mkInnerComputation ref $ do
              xs <- liftIO $ readIORef ref
              pure $ xs !! 10000
      void computation
      readIORef ref `shouldReturn` []


quickCheckNestedResource :: Spec
quickCheckNestedResource =
  describe "QuickCheck:" $
    prop "No resources are leaked." $ \ comp -> do
      ref <- newIORef []
      evalNestedComputation ref comp
      readIORef ref `shouldReturn` []

--------------------------------------------------------------------------------

mkInnerComputation :: IORef [Int] -> IO a -> IO a
mkInnerComputation ref action =
  runNestedResource $ do
    nestIO_ (testAcquire ref) (testRelease ref)
    nestIO_ (testAcquire ref) (testRelease ref)
    nestIO_ (testAcquire ref) (testRelease ref)
    nestedLift action
