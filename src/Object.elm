module Object
  ( Object (..)
  , ID
  , initial
  , add
  , removeLink
  , doesLinkTo
  , toList
  ) where

import List

import ExponentialBuckets as EB

type Object = Object { id        : ID
                     , parent    : Maybe Object
                     , ancestors : EB.ExponentialBuckets ID
                     }

type alias ID = Int

initial : ID -> Object
initial id =
  Object { id = id
         , parent = Nothing
         , ancestors = EB.empty
         }

add : ID -> Object -> Object
add id (Object parent) =
  Object { id = id
         , parent = Just (Object parent)
         , ancestors = EB.toList parent.ancestors
                    |> List.map (\(offset, obj) -> (offset + 1, obj))
                    |> (::) (0, parent.id)
                    |> EB.fromList
                    |> EB.pruneBuckets
         }

removeLink : Int -> Object -> Object
removeLink offset (Object object) =
  if offset > 0  -- Do not remove the parent.
    then Object { object | ancestors <- EB.remove offset object.ancestors }
    else Object object

doesLinkTo : Int -> Object -> Bool
doesLinkTo offset (Object object) = EB.member offset object.ancestors

toList : Object -> List Object
toList (Object object) =
  Object object ::
    case object.parent of
      Just parent -> toList parent
      Nothing     -> []
