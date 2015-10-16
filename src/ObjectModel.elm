module ObjectModel
  ( Model
  , Action
  , init
  , update
  , view
  ) where

import Char
import Html exposing (..)
import Html.Attributes exposing (class, classList, id, key)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy2)
import IntDict exposing (IntDict)
import Json.Decode as Json
import List
import String

import ExponentialBuckets as EB
import Object exposing (ID, Object (..))

type alias Model = { nextId   : ID
                   , head     : Object
                   , removals : RemovalDict
                   }

type alias IntSet = IntDict ()
-- IntDict ~ Dict ID
type alias RemovalDict = IntDict IntSet

type Action = AddObject
            | ToggleRemoveLink ID Int


init : Model
init =
  { nextId = 1
  , head = Object.initial 0
  , removals = IntDict.empty
  }


update : Action -> Model -> Model
update action model =
  case action of
    AddObject ->
      { model
      | nextId <- model.nextId + 1
      , head <- Object.add model.nextId model.head
      }

    ToggleRemoveLink oid offset ->
      applyRemovals oid (toggleRemoval offset) model

toggleRemoval : Int -> IntSet -> IntSet
toggleRemoval offset set =
  if IntDict.member offset set
    then IntDict.remove offset set
    else IntDict.insert offset () set
      
applyRemovals : ID -> (IntSet -> IntSet) -> Model -> Model
applyRemovals oid f model =
  let removals =
        IntDict.update oid (Just << f << Maybe.withDefault IntDict.empty)
                       model.removals
      head = objectApplyRemovals oid removals model.head
  in  { model | head <- head, removals <- removals }

objectApplyRemovals : ID -> RemovalDict -> Object -> Object
objectApplyRemovals oid removals (Object object) =
  case object.parent of
    Just originalParent ->
      let parent =
            if object.id == oid  -- No further parents are modified.
              then originalParent
              else objectApplyRemovals oid removals originalParent

      in  IntDict.foldl (\offset () -> Object.removeLink offset)
            (Object.add object.id parent)
            (objectRemovalSet (Object object) removals)

    Nothing -> Object object  -- No parents to remove.

objectRemovalSet : Object -> RemovalDict -> IntSet
objectRemovalSet (Object object) removals =
  IntDict.get object.id removals
    |> Maybe.withDefault IntDict.empty


view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ button [ onClick address AddObject ] [ text "Add object" ]
    , lazy2 (viewFull address) model.removals model.head
    ]

viewFull : Signal.Address Action -> RemovalDict -> Object -> Html
viewFull address removals object =
  Object.toList object
    |> List.map (\object ->
         let removalSet = objectRemovalSet object removals
         in  lazy2 (viewObject address) removalSet object)
    |> div [ class "object-chain-container" ]

type alias ViewedObject =
  { id      : ID
  , linked  : Bool
  , removed : Bool
  }

viewObject : Signal.Address Action -> IntSet -> Object -> Html
viewObject address removalSet (Object object) =
  let ancestors =
        Maybe.map Object.toList object.parent
          |> Maybe.withDefault []
          |> List.indexedMap (\offset (Object ancestor) ->
               { id      = ancestor.id
               , linked  = Object.doesLinkTo offset (Object object)
               , removed = IntDict.member offset removalSet
               })
          |> EB.fromElementList
          |> EB.toBucketList

  in  div [ class "object-chain" ]
        [ ul [ key (toString object.id) ]
            <| viewObjectHead address (Object object)
                 :: List.map (viewObjectBucket address object.id) ancestors
        ]

viewObjectHead : Signal.Address Action -> Object -> Html
viewObjectHead address (Object object) =
  li [ class "object-head" ] [ text (idToName object.id) ]

viewObjectBucket : Signal.Address Action -> ID -> List (Int, ViewedObject)
                -> Html
viewObjectBucket address headId vobjects =
  li [ class "object-bucket" ]
    [ ul [] <| List.map (viewObjectAncestor address headId) vobjects
    ]

viewObjectAncestor : Signal.Address Action -> ID -> (Int, ViewedObject) -> Html
viewObjectAncestor address headId (offset, vobject) =
  let removable = offset > 0

      classes = classList [ ("object-ancestor",  True)
                          , ("object-linked",    vobject.linked)
                          , ("object-removed",   vobject.removed)
                          , ("object-removable", removable)
                          ]

      content html =
        if removable
          then [ button [ onClick address (ToggleRemoveLink headId offset) ]
                      html
               ]
          else html

  in  li [ classes ] <| content [ text (idToName vobject.id) ]


idToName : ID -> String
idToName n =
  let a = Char.toCode 'A'
      z = Char.toCode 'Z'
  in  String.fromChar (Char.fromCode (a + n % (1 + z - a)))
