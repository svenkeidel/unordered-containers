{-# LANGUAGE CPP, DeriveAnyClass, DeriveGeneric, GADTs, PackageImports, RecordWildCards, RankNTypes, ConstraintKinds #-}

module Main where

import Control.DeepSeq
import Gauge (bench, bgroup, defaultMain, env, nf, whnf, Benchmark)
import Data.Bits (Bits,(.&.))
import Data.Functor.Identity
import Data.Hashable (Hashable, hash)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified "hashmap" Data.HashMap as IHM
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Prelude hiding (lookup,map)

import qualified Util.ByteString as UBS
import qualified Util.Int as UI
import qualified Util.String as US

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData BS.ByteString
#endif

main :: IO ()
main = do
    defaultMain
        [
          -- * Comparison to other data structures
          -- ** Map
          benchMap $ \ ~(Env{..}) ->
          [ bench "lookup" $ whnf (lookupM keys) map
          , bench "lookup-miss" $ whnf (lookupM keysMissing) map
          , bench "insert" $ whnf (insertM elems) M.empty
          , bench "insert-dup" $ whnf (insertM elemsDuplicates) map
          , bench "delete" $ whnf (deleteM keys) map
          , bench "delete-miss" $ whnf (deleteM keysMissing) map
          , bench "size" $ whnf M.size map
          , bench "fromList" $ whnf M.fromList elems
          , bench "isSubmapOf" $ whnf (M.isSubmapOf mapSubset) map
          ]

          -- ** Map from the hashmap package
        , benchHashMapPackage $ \ ~(Env{..}) ->
          [ bench "lookup" $ whnf (lookupIHM keys) map
          , bench "lookup-miss" $ whnf (lookupIHM keysMissing) map
          , bench "insert" $ whnf (insertIHM elems) IHM.empty
          , bench "insert-dup" $ whnf (insertIHM elemsDuplicates) map
          , bench "delete" $ whnf (deleteIHM keys) map
          , bench "delete-miss" $ whnf (deleteIHM keysMissing) map
          , bench "size" $ whnf IHM.size map
          , bench "fromList" $ whnf IHM.fromList elems
          , bench "isSubmapOf" $ whnf (IHM.isSubmapOf mapSubset) map
          -- , bench "hash" $ whnf hash map
          ]

          -- ** IntMap
        , benchIntMap $ \ ~(Env{..}) ->
          [ bench "lookup" $ whnf (lookupIM keys) (getIntMap map)
          , bench "lookup-miss" $ whnf (lookupIM keysMissing) (getIntMap map)
          , bench "insert" $ whnf (insertIM elems) IM.empty
          , bench "insert-dup" $ whnf (insertIM elemsDuplicates) (getIntMap map)
          , bench "delete" $ whnf (deleteIM keys) (getIntMap map)
          , bench "delete-miss" $ whnf (deleteIM keysMissing) (getIntMap map)
          , bench "size" $ whnf IM.size (getIntMap map)
          , bench "fromList" $ whnf IM.fromList elems
          , bench "isSubmapOf" $ whnf (IM.isSubmapOf (getIntMap mapSubset)) (getIntMap map)
          ]

        , benchHashMap $ \ ~(Env{..}) ->
          [ -- * Basic interface
            bench "lookup" $ whnf (lookup keys) map
          , bench "lookup-miss" $ whnf (lookup keysMissing) map
          , bench "insert" $ whnf (insert elems) HM.empty
          , bench "insert-dup" $ whnf (insert elemsDuplicates) map
          , bench "delete" $ whnf (delete keys) map
          , bench "delete-miss" $ whnf (delete keysMissing) map
          , bench "alterInsert" $ whnf (alterInsert elems) HM.empty
          , bench "alterFInsert" $ whnf (alterFInsert elems) HM.empty
          , bench "alterInsert-dup" $ whnf (alterInsert elemsDuplicates) map
          , bench "alterFInsert-dup" $ whnf (alterFInsert elems) map

          --   -- Combine
          -- , bench "union" $ whnf (HM.union map) map2

            -- Transformations
          , bench "map" $ whnf (HM.map (\ v -> v + 1)) map

          --   -- * Difference and intersection
          -- , bench "difference" $ whnf (HM.difference map) map2
          -- , bench "intersection" $ whnf (HM.intersection map) map2

            -- Folds
          , bench "foldl'" $ whnf (HM.foldl' (+) 0) map
          , bench "foldr" $ nf (HM.foldr (:) []) map

            -- Filter
          -- , bench "filter" $ whnf (HM.filter (\ v -> v .&. 1 == 0)) map
          -- , bench "filterWithKey" $ whnf (HM.filterWithKey (\ k _ -> k .&. 1 == 0)) map

            -- Size
          , bench "size" $ whnf HM.size map

            -- fromList
          , bgroup "fromList"
            [ bench "long" $ whnf HM.fromList elems
            , bench "short" $ whnf HM.fromList elemsDuplicates
            ]
            -- fromListWith
          , bgroup "fromListWith"
            [ bench "long" $ whnf (HM.fromListWith (+)) elems
            , bench "short" $ whnf (HM.fromListWith (+)) elemsDuplicates
            ]
          ]
        ]

data B where
    B :: NFData a => a -> B

instance NFData B where
    rnf (B b) = rnf b

-- setupEnv :: IO Env
-- setupEnv = do
--     let n = 2^(12 :: Int)

--         elems   = zip keys [1..n]
--         keys    = US.rnd 8 n
--         elemsBS = zip keysBS [1..n]
--         keysBS  = UBS.rnd 8 n
--         elemsI  = zip keysI [1..n]
--         keysI   = UI.rnd (n+n) n
--         elemsI2 = zip [n `div` 2..n + (n `div` 2)] [1..n]  -- for union

--         keys'    = US.rnd' 8 n
--         keysBS'  = UBS.rnd' 8 n
--         keysI'   = UI.rnd' (n+n) n

--         keysDup    = US.rnd 2 n
--         keysDupBS  = UBS.rnd 2 n
--         keysDupI   = UI.rnd (n`div`4) n
--         elemsDup   = zip keysDup [1..n]
--         elemsDupBS = zip keysDupBS [1..n]
--         elemsDupI  = zip keysDupI [1..n]

--         m           = M.fromList elems
--         mSubset     = M.fromList (takeSubset n elems)
--         mbs         = M.fromList elemsBS
--         mbsSubset   = M.fromList (takeSubset n elemsBS)

--         hm          = HM.fromList elems
--         hmSubset    = HM.fromList (takeSubset n elems)
--         hmbs        = HM.fromList elemsBS
--         hmbsSubset  = HM.fromList (takeSubset n elemsBS)
--         hmi         = HM.fromList elemsI
--         hmiSubset   = HM.fromList (takeSubset n elemsI)
--         hmi2        = HM.fromList elemsI2

--         im          = IM.fromList elemsI
--         imSubset    = IM.fromList (takeSubset n elemsI)

--         ihm         = IHM.fromList elems
--         ihmSubset   = IHM.fromList (takeSubset n elems)
--         ihmbs       = IHM.fromList elemsBS
--         ihmbsSubset = IHM.fromList (takeSubset n elemsBS)
--     return Env{..}
--   where
--     takeSubset n elements =
--       -- use 50% of the elements for a subset check.
--       let subsetSize = round (fromIntegral n * 0.5 :: Double) :: Int
--       in take subsetSize elements

-- It is important to inline the following functions to allow GHC to specialize
-- the benchmarked functions to specific types.

type IsKey k = (Ord k, Hashable k, Eq k)

benchMap :: (forall k. (IsKey k) => Env M.Map k Int -> [Benchmark])
         -> Benchmark
benchMap f = bgroup "Map" $ benchSuite M.fromList f
{-# INLINE benchMap #-}

benchHashMapPackage :: (forall k. (IsKey k) => Env IHM.Map k Int -> [Benchmark])
                    -> Benchmark
benchHashMapPackage f = bgroup "hashmap/Map" $ benchSuite IHM.fromList f
{-# INLINE benchHashMapPackage #-}

benchIntMap :: (Env IntMap Int Int -> [Benchmark]) -> Benchmark
benchIntMap f = env (setupInt (IntMap . IM.fromList)) $ \e -> bgroup "IntMap" $ f e
{-# INLINE benchIntMap #-}

benchHashMap :: (forall k. (IsKey k) => Env HM.HashMap k Int -> [Benchmark])
             -> Benchmark
benchHashMap f = bgroup "HashMap" $ benchSuite HM.fromList f
{-# INLINE benchHashMap #-}

benchSuite :: (NFData (m String Int), NFData (m ByteString Int), NFData (m Int Int))
           => (forall k. (IsKey k) => [(k,Int)] -> m k Int)
           -> (forall k. (IsKey k) => Env m k Int -> [Benchmark])
           -> [Benchmark]
benchSuite fromList f =
  [ env (setupString fromList)     $ \e -> bgroup "String" $ f e
  , env (setupByteString fromList) $ \e -> bgroup "ByteString" $ f e
  , env (setupInt fromList)        $ \e -> bgroup "Int" $ f e
  ]
{-# INLINE benchSuite #-}

data Env m k v = Env
  { keys            :: ![k]
  , elems           :: ![(k,v)]
  , elemsDuplicates :: ![(k,v)]
  , keysMissing     :: ![k]
  , map             :: !(m k v)
  , mapSubset       :: !(m k v)
  } deriving (Generic,NFData)

setupString :: ([(String,Int)] -> m String Int) -> IO (Env m String Int)
setupString fromList = do
  let keys            = US.rnd 8 n
      elems           = zip keys [1..n]
      keysDuplicates  = US.rnd 2 n
      elemsDuplicates = zip keysDuplicates [1..n]
      keysMissing     = US.rnd' 8 n
      map             = fromList elems
      mapSubset       = fromList (takeSubset elems)
  return $ Env{..}

setupByteString :: ([(ByteString,Int)] -> m ByteString Int) -> IO (Env m ByteString Int)
setupByteString fromList = do
  let keys            = UBS.rnd 8 n
      elems           = zip keys [1..n]
      keysDuplicates  = UBS.rnd 2 n
      elemsDuplicates = zip keysDuplicates [1..n]
      keysMissing     = UBS.rnd' 8 n
      map             = fromList elems
      mapSubset       = fromList (takeSubset elems)
  return $ Env{..}

setupInt :: ([(Int,Int)] -> m Int Int) -> IO (Env m Int Int)
setupInt fromList = do
  let keys            = UI.rnd (n+n) n
      elems           = zip keys [1..n]
      keysDuplicates  = UI.rnd (n`div`4) n
      elemsDuplicates = zip keysDuplicates [1..n]
      keysMissing     = UI.rnd' (n+n) n
      map             = fromList elems
      mapSubset       = fromList (takeSubset elems)
  return $ Env{..}

takeSubset :: [(k,v)] -> [(k,v)]
takeSubset elements =
  -- use 50% of the elements for a subset check.
  let subsetSize = round (fromIntegral n * 0.5 :: Double) :: Int
  in take subsetSize elements

n = 2^(12::Int)

------------------------------------------------------------------------
-- * HashMap

lookup :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int -> Int
lookup xs m = foldl' (\z k -> fromMaybe z (HM.lookup k m)) 0 xs
{-# SPECIALIZE lookup :: [Int] -> HM.HashMap Int Int -> Int #-}
{-# SPECIALIZE lookup :: [String] -> HM.HashMap String Int -> Int #-}
{-# SPECIALIZE lookup :: [BS.ByteString] -> HM.HashMap BS.ByteString Int
                      -> Int #-}

insert :: (Eq k, Hashable k) => [(k, Int)] -> HM.HashMap k Int
       -> HM.HashMap k Int
insert xs m0 = foldl' (\m (k, v) -> HM.insert k v m) m0 xs
{-# SPECIALIZE insert :: [(Int, Int)] -> HM.HashMap Int Int
                      -> HM.HashMap Int Int #-}
{-# SPECIALIZE insert :: [(String, Int)] -> HM.HashMap String Int
                      -> HM.HashMap String Int #-}
{-# SPECIALIZE insert :: [(BS.ByteString, Int)] -> HM.HashMap BS.ByteString Int
                      -> HM.HashMap BS.ByteString Int #-}

delete :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int -> HM.HashMap k Int
delete xs m0 = foldl' (\m k -> HM.delete k m) m0 xs
{-# SPECIALIZE delete :: [Int] -> HM.HashMap Int Int -> HM.HashMap Int Int #-}
{-# SPECIALIZE delete :: [String] -> HM.HashMap String Int
                      -> HM.HashMap String Int #-}
{-# SPECIALIZE delete :: [BS.ByteString] -> HM.HashMap BS.ByteString Int
                      -> HM.HashMap BS.ByteString Int #-}

alterInsert :: (Eq k, Hashable k) => [(k, Int)] -> HM.HashMap k Int
             -> HM.HashMap k Int
alterInsert xs m0 =
  foldl' (\m (k, v) -> HM.alter (const . Just $ v) k m) m0 xs
{-# SPECIALIZE alterInsert :: [(Int, Int)] -> HM.HashMap Int Int
                           -> HM.HashMap Int Int #-}
{-# SPECIALIZE alterInsert :: [(String, Int)] -> HM.HashMap String Int
                           -> HM.HashMap String Int #-}
{-# SPECIALIZE alterInsert :: [(BS.ByteString, Int)] -> HM.HashMap BS.ByteString Int
                           -> HM.HashMap BS.ByteString Int #-}

alterDelete :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int
             -> HM.HashMap k Int
alterDelete xs m0 =
  foldl' (\m k -> HM.alter (const Nothing) k m) m0 xs
{-# SPECIALIZE alterDelete :: [Int] -> HM.HashMap Int Int
                           -> HM.HashMap Int Int #-}
{-# SPECIALIZE alterDelete :: [String] -> HM.HashMap String Int
                           -> HM.HashMap String Int #-}
{-# SPECIALIZE alterDelete :: [BS.ByteString] -> HM.HashMap BS.ByteString Int
                           -> HM.HashMap BS.ByteString Int #-}

alterFInsert :: (Eq k, Hashable k) => [(k, Int)] -> HM.HashMap k Int
             -> HM.HashMap k Int
alterFInsert xs m0 =
  foldl' (\m (k, v) -> runIdentity $ HM.alterF (const . Identity . Just $ v) k m) m0 xs
{-# SPECIALIZE alterFInsert :: [(Int, Int)] -> HM.HashMap Int Int
                            -> HM.HashMap Int Int #-}
{-# SPECIALIZE alterFInsert :: [(String, Int)] -> HM.HashMap String Int
                            -> HM.HashMap String Int #-}
{-# SPECIALIZE alterFInsert :: [(BS.ByteString, Int)] -> HM.HashMap BS.ByteString Int
                            -> HM.HashMap BS.ByteString Int #-}

alterFDelete :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int
             -> HM.HashMap k Int
alterFDelete xs m0 =
  foldl' (\m k -> runIdentity $ HM.alterF (const . Identity $ Nothing) k m) m0 xs
{-# SPECIALIZE alterFDelete :: [Int] -> HM.HashMap Int Int
                            -> HM.HashMap Int Int #-}
{-# SPECIALIZE alterFDelete :: [String] -> HM.HashMap String Int
                            -> HM.HashMap String Int #-}
{-# SPECIALIZE alterFDelete :: [BS.ByteString] -> HM.HashMap BS.ByteString Int
                            -> HM.HashMap BS.ByteString Int #-}

isSubmapOfNaive :: (Eq k, Hashable k) => HM.HashMap k Int -> HM.HashMap k Int -> Bool
isSubmapOfNaive m1 m2 = and [ Just v1 == HM.lookup k1 m2 | (k1,v1) <- HM.toList m1 ]
{-# SPECIALIZE isSubmapOfNaive :: HM.HashMap Int Int -> HM.HashMap Int Int -> Bool #-}
{-# SPECIALIZE isSubmapOfNaive :: HM.HashMap String Int -> HM.HashMap String Int -> Bool #-}
{-# SPECIALIZE isSubmapOfNaive :: HM.HashMap BS.ByteString Int -> HM.HashMap BS.ByteString Int -> Bool #-}

------------------------------------------------------------------------
-- * Map

lookupM :: Ord k => [k] -> M.Map k Int -> Int
lookupM xs m = foldl' (\z k -> fromMaybe z (M.lookup k m)) 0 xs
{-# SPECIALIZE lookupM :: [String] -> M.Map String Int -> Int #-}
{-# SPECIALIZE lookupM :: [BS.ByteString] -> M.Map BS.ByteString Int -> Int #-}

insertM :: Ord k => [(k, Int)] -> M.Map k Int -> M.Map k Int
insertM xs m0 = foldl' (\m (k, v) -> M.insert k v m) m0 xs
{-# SPECIALIZE insertM :: [(String, Int)] -> M.Map String Int
                       -> M.Map String Int #-}
{-# SPECIALIZE insertM :: [(BS.ByteString, Int)] -> M.Map BS.ByteString Int
                       -> M.Map BS.ByteString Int #-}

deleteM :: Ord k => [k] -> M.Map k Int -> M.Map k Int
deleteM xs m0 = foldl' (\m k -> M.delete k m) m0 xs
{-# SPECIALIZE deleteM :: [String] -> M.Map String Int -> M.Map String Int #-}
{-# SPECIALIZE deleteM :: [BS.ByteString] -> M.Map BS.ByteString Int
                       -> M.Map BS.ByteString Int #-}

------------------------------------------------------------------------
-- * Map from the hashmap package

lookupIHM :: (Eq k, Hashable k, Ord k) => [k] -> IHM.Map k Int -> Int
lookupIHM xs m = foldl' (\z k -> fromMaybe z (IHM.lookup k m)) 0 xs
{-# SPECIALIZE lookupIHM :: [String] -> IHM.Map String Int -> Int #-}
{-# SPECIALIZE lookupIHM :: [BS.ByteString] -> IHM.Map BS.ByteString Int
                         -> Int #-}

insertIHM :: (Eq k, Hashable k, Ord k) => [(k, Int)] -> IHM.Map k Int
          -> IHM.Map k Int
insertIHM xs m0 = foldl' (\m (k, v) -> IHM.insert k v m) m0 xs
{-# SPECIALIZE insertIHM :: [(String, Int)] -> IHM.Map String Int
                         -> IHM.Map String Int #-}
{-# SPECIALIZE insertIHM :: [(BS.ByteString, Int)] -> IHM.Map BS.ByteString Int
                         -> IHM.Map BS.ByteString Int #-}

deleteIHM :: (Eq k, Hashable k, Ord k) => [k] -> IHM.Map k Int -> IHM.Map k Int
deleteIHM xs m0 = foldl' (\m k -> IHM.delete k m) m0 xs
{-# SPECIALIZE deleteIHM :: [String] -> IHM.Map String Int
                         -> IHM.Map String Int #-}
{-# SPECIALIZE deleteIHM :: [BS.ByteString] -> IHM.Map BS.ByteString Int
                         -> IHM.Map BS.ByteString Int #-}

------------------------------------------------------------------------
-- * IntMap

-- Newtype wrapper that fits IntMaps into the key/value scheme of the other maps.
newtype IntMap k v = IntMap { getIntMap :: IM.IntMap v }
  deriving (Generic,NFData)

lookupIM :: [Int] -> IM.IntMap Int -> Int
lookupIM xs m = foldl' (\z k -> fromMaybe z (IM.lookup k m)) 0 xs

insertIM :: [(Int, Int)] -> IM.IntMap Int -> IM.IntMap Int
insertIM xs m0 = foldl' (\m (k, v) -> IM.insert k v m) m0 xs

deleteIM :: [Int] -> IM.IntMap Int -> IM.IntMap Int
deleteIM xs m0 = foldl' (\m k -> IM.delete k m) m0 xs
