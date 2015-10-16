module ExponentialBuckets
  ( ExponentialBuckets
  , Bucket
  , empty
  , insert
  , remove
  , member
  , get
  , fromList
  , fromElementList
  , toList
  , toBucketList
  , map
  , indexSplit
  , indexJoin
  , pruneBuckets
  ) where

import Bitwise exposing (shiftLeft)
import IntDict exposing (IntDict)
import List
import Maybe

-- bucket ix:    0 | 1   | 2       | 3                    | 4
-- bucket size:  1 | 2   | 4       | 8                    | 16
-- global ix:    0 | 1 2 | 3 4 5 6 | 7 8 9 10 11 12 13 14 | 15 16 …
-- elem ix:      0 | 0 1 | 0 1 2 3 | 0 1 2 3  4  5  6  7  | 0  1  …

type alias BucketIndex  = Int
type alias ElementIndex = Int
type alias GlobalIndex  = Int

-- Bucket n will contain 2^n elements.
-- IntDict ~ Dict BucketIndex
type alias ExponentialBuckets a = IntDict (Bucket a)

-- IntDict ~ Dict ElementIndex
type alias Bucket a = IntDict a

empty : ExponentialBuckets a
empty = IntDict.empty

insert : GlobalIndex -> a -> ExponentialBuckets a -> ExponentialBuckets a
insert ix elem =
  let (bucketIx, elemIx) = indexSplit ix

  in  IntDict.update bucketIx
        (Just << IntDict.insert elemIx elem << Maybe.withDefault IntDict.empty)

remove : GlobalIndex -> ExponentialBuckets a -> ExponentialBuckets a
remove ix =
  let (bucketIx, elemIx) = indexSplit ix
  in  IntDict.update bucketIx (Maybe.map (IntDict.remove elemIx))

member : GlobalIndex -> ExponentialBuckets a -> Bool
member ix buckets =
  case get ix buckets of
    Nothing -> False
    Just _  -> True

get : GlobalIndex -> ExponentialBuckets a -> Maybe a
get ix buckets =
  let (bucketIx, elemIx) = indexSplit ix
  in  IntDict.get bucketIx buckets `Maybe.andThen` IntDict.get elemIx

fromList : List (GlobalIndex, a) -> ExponentialBuckets a
fromList = List.foldr (uncurry insert) empty

fromElementList : List a -> ExponentialBuckets a
fromElementList = fromList << List.indexedMap (,)

toList : ExponentialBuckets a -> List (GlobalIndex, a)
toList = List.concat << toBucketList

toBucketList : ExponentialBuckets a -> List (List (GlobalIndex, a))
toBucketList = List.indexedMap bucketToList << toListOfBuckets

toListOfBuckets : ExponentialBuckets a -> List (Bucket a)
toListOfBuckets buckets =
  case IntDict.findMax buckets of
    Just (topBucketIx, _) ->
      List.map (\bucketIx -> IntDict.get bucketIx buckets
                          |> Maybe.withDefault IntDict.empty)
               [0 .. topBucketIx]

    Nothing -> []

fromListOfBuckets : List (Bucket a) -> ExponentialBuckets a
fromListOfBuckets = List.indexedMap (\i bucket ->
                      if IntDict.isEmpty bucket
                        then IntDict.empty
                        else IntDict.singleton i bucket)
                 >> List.foldr IntDict.union IntDict.empty

bucketToList : BucketIndex -> Bucket a -> List (GlobalIndex, a)
bucketToList bucketIx =
  let bottom = bucketMinimum bucketIx
  in  IntDict.foldr (\elemIx elem xs -> (bottom + elemIx, elem) :: xs) []

map : (BucketIndex -> ElementIndex -> a -> b)
   -> ExponentialBuckets a -> ExponentialBuckets b
map f = IntDict.map (\bucketIx -> IntDict.map (f bucketIx))

indexSplit : GlobalIndex -> (BucketIndex, ElementIndex)
indexSplit ix =
  let bucketIx = bucketFor ix
      elemIx   = ix - bucketMinimum bucketIx
  in  (bucketIx, elemIx)

indexJoin : BucketIndex -> ElementIndex -> GlobalIndex
indexJoin bucketIx elemIx = bucketMinimum bucketIx + elemIx

bucketFor : GlobalIndex -> BucketIndex
bucketFor ix =
  let go bucketIx =
        if ix < bucketMinimum (bucketIx + 1)
          then bucketIx
          else go (bucketIx + 1)
  in  go 0

-- The first element of a bucket.
bucketMinimum : BucketIndex -> GlobalIndex
bucketMinimum bucketIx = (1 `shiftLeft` bucketIx) - 1

bucketSize : BucketIndex -> ElementIndex
bucketSize bucketIx = 1 `shiftLeft` bucketIx

-- Keep the object with the highest index in each bucket. Also keep the object
-- closest to the previous bucket if the previous bucket was empty.
pruneBuckets : ExponentialBuckets a -> ExponentialBuckets a
pruneBuckets = fromListOfBuckets << pruneListOfBuckets << toListOfBuckets

pruneListOfBuckets : List (Bucket a) -> List (Bucket a)
pruneListOfBuckets list =
  let go bucket prevBucket =
        let max = IntDict.findMax bucket
            min = if IntDict.isEmpty prevBucket
                    then IntDict.findMin bucket
                    else Nothing
        in  maybeToIntDict min `IntDict.union` maybeToIntDict max

  in  List.map2 go list (IntDict.empty :: list)

maybeToIntDict : Maybe (Int, a) -> IntDict a
maybeToIntDict = Maybe.map (uncurry IntDict.singleton)
              >> Maybe.withDefault IntDict.empty
