module Object
  ( Object (..)
  , ID

  , id
  , parent
  , ancestors

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


id : Object -> ID
id (Object o) = o.id

parent : Object -> Maybe Object
parent (Object o) = o.parent

ancestors : Object -> EB.ExponentialBuckets ID
ancestors (Object o) = o.ancestors


initial : ID -> Object
initial id_ =
  Object { id = id_
         , parent = Nothing
         , ancestors = EB.empty
         }

add : ID -> Object -> Object
add id_ parent =
  Object { id = id_
         , parent = Just parent
         , ancestors = EB.toList (ancestors parent)
                    |> List.map (\(offset, obj) -> (offset + 1, obj))
                    |> (::) (0, id parent)
                    |> EB.fromList
                    |> EB.pruneBuckets
         }

removeLink : Int -> Object -> Object
removeLink offset object =
  case object of
    Object o ->
      if offset > 0  -- Do not remove the parent.
        then Object { o | ancestors <- EB.remove offset o.ancestors }
        else object

doesLinkTo : Int -> Object -> Bool
doesLinkTo offset object = EB.member offset (ancestors object)

toList : Object -> List Object
toList object =
  object ::
    case parent object of
      Just parent_ -> toList parent_
      Nothing      -> []
