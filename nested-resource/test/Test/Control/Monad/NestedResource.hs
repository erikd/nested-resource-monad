{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Control.Monad.NestedResource
  ( NestedComputation
  , TestException (..)
  , evalNestedComputation

  , stackPop
  , stackPush
  , testAcquire
  , testRelease
  ) where


import           Control.Exception (Exception, SomeException, handle, throw, throwIO)
import           Control.Monad (unless, mzero)
import           Control.Monad.NestedResource

import           Data.IORef (IORef, atomicModifyIORef', readIORef)
import           Data.Typeable (Typeable)

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Gen (Gen)
import qualified Test.QuickCheck.Gen as QC


data NestedComputation
  = NestedComputation [Computation] Single
  deriving (Eq, Show)

data Computation
  = CompPair !Acquire !Release
  | CompSingle !Single
  deriving (Eq, Show)

data Acquire
  = Acquire
  | AcquireError !CompError
  deriving (Eq, Show)

data Release
  = Release
  | ReleaseError !CompError
  deriving (Eq, Show)

data Single
  = Single
  | SingleError !CompError
  deriving (Eq, Show)

data CompError
  = CeError
  | CeFail
  | CeThrow
  | CeThrowIO
  | CeMZero
  | CePartial
  deriving (Bounded, Enum, Eq, Show)

instance Arbitrary NestedComputation where
  arbitrary = genNestedComputation
  shrink = shrinkNestedComputation


genNestedComputation :: Gen NestedComputation
genNestedComputation = do
  len <- QC.choose (1, 8)
  NestedComputation
        <$> QC.vectorOf len genComputataion
        <*> genSingle

  where
    genComputataion = QC.oneof
        [ CompPair <$> genAcquire <*> genRelease
        , CompSingle <$> genSingle
        ]
    genAcquire = QC.frequency
        [ (10, pure Acquire)
        , ( 1, AcquireError <$> genCompError)
        ]

    genRelease = QC.frequency
        [ (10, pure Release)
        , ( 1, ReleaseError <$> genCompError)
        ]

    genSingle = QC.frequency
        [ (10, pure Single)
        , ( 1, SingleError <$> genCompError)
        ]

    genCompError = QC.elements [ minBound .. maxBound ]


shrinkNestedComputation :: NestedComputation -> [NestedComputation]
shrinkNestedComputation (NestedComputation xs inner)
  | len < 2 = []
  | otherwise =
      [ NestedComputation lxs inner, NestedComputation rxs inner]
  where
    len = length xs
    (lxs, rxs) = splitAt (len `div` 2) xs

-- Evaluate a NestedComputation on the given IORef.
-- Silently catch and handle all exceptions. We're not interested in the
-- exceptions, we are insterested in the IORef after the evaluation finishes.
evalNestedComputation :: IORef [Int] -> NestedComputation -> IO ()
evalNestedComputation ref (NestedComputation xs inner) =
  handle handler . runNestedResource $ do
    mapM_ evalComp xs
    nestedLift $ evalSingle inner
  where
    evalComp (CompPair acq rel) = evalPair ref acq rel
    evalComp (CompSingle s) = nestedLift $ evalSingle s

    handler :: SomeException -> IO ()
    handler = const $ pure ()

evalSingle :: Single -> IO ()
evalSingle Single = pure ()
evalSingle (SingleError err) = evalError err

evalPair :: IORef [Int] -> Acquire -> Release -> NestedResource ()
evalPair ref acq rel =
  nestIO_ (evalAcquire ref acq) (evalRelease rel ref)

evalAcquire :: IORef [Int] -> Acquire -> IO ResKey
evalAcquire ref Acquire = testAcquire ref
evalAcquire _ (AcquireError err) = evalError err

evalRelease :: Release -> IORef [Int] -> ResKey -> IO ()
evalRelease Release ref key = testRelease ref key
evalRelease (ReleaseError err) ref key = testRelease ref key >> evalError err

evalError :: CompError -> IO a
evalError err =
  case err of
    CeError -> error "error"
    CeFail -> fail "fail"
    CeThrow -> throw TestException
    CeThrowIO -> throwIO TestException
    CeMZero -> mzero
    CePartial -> pure $ head []



data TestException
  = TestException
  deriving (Eq, Show, Typeable)

instance Exception TestException


newtype ResKey = ResKey Int deriving (Num, Eq, Show)

testAcquire :: IORef [Int] -> IO ResKey
testAcquire ref =
  fmap ResKey $ stackPush ref =<< (length <$> readIORef ref)

testRelease :: IORef [Int] -> ResKey -> IO ()
testRelease ref expected = do
    val <- stackPop ref
    case val of
        (Just x, _len) -> unless (ResKey x == expected) $
                            error $ "testRelease : expected " ++ show expected ++ " got " ++ show x ++ "."
        _ -> error "testRelease : unexpected."

stackPush :: IORef [Int] -> Int -> IO Int
stackPush ior i = atomicModifyIORef' ior ( \ lst -> (i : lst, 1 + length lst))

stackPop :: IORef [Int] -> IO (Maybe Int, Int)
stackPop ior =
    atomicModifyIORef' ior $ \lst ->
            case lst of
                [] -> ([], (Nothing, 0))
                (x:xs) -> (xs, (Just x, length xs))
