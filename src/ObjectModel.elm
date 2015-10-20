module ObjectModel
  ( Model
  , Action
  , init
  , update
  , view
  ) where

import Char
import Html as H exposing (Html)
import Html.Attributes as H exposing (key)
import Html.Events as H
import Html.Lazy as H
import IntDict exposing (IntDict)
import Json.Decode as Json
import List
import String
import Svg
import Svg as S exposing (Svg)
import Svg.Attributes
import Svg.Attributes as S
import Svg.Events as S
import Svg.Lazy as S

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
objectApplyRemovals oid removals object =
  case Object.parent object of
    Just originalParent ->
      let parent =
            if Object.id object == oid  -- No further parents are modified.
              then originalParent
              else objectApplyRemovals oid removals originalParent

      in  IntDict.foldl (\offset () -> Object.removeLink offset)
            (Object.add (Object.id object) parent)
            (objectRemovalSet object removals)

    Nothing -> object  -- No parents to remove.

objectRemovalSet : Object -> RemovalDict -> IntSet
objectRemovalSet object removals =
  IntDict.get (Object.id object) removals
    |> Maybe.withDefault IntDict.empty


view : Signal.Address Action -> Model -> Html
view address model =
  H.div []
    [ H.div [] [ H.button [ H.onClick address AddObject ] [ H.text "Add object" ] ]
    , H.div [] [ H.lazy2 (viewFull address) model.removals model.head ]
    ]

box : Float
box = 16

arrowMargin : Float
arrowMargin = 10

margin : Float
margin = 12

corner : Float
corner = 4

boxCoords : Int -> Int -> (Float, Float, Float, Float)
boxCoords row column =
  let top    = (margin + arrowMargin) * toFloat (row + 1) + box * toFloat row
      left   = margin * toFloat (column + 1) + box * toFloat column
      bottom = top  + box
      right  = left + box
  in  (top, left, bottom, right)

viewFull : Signal.Address Action -> RemovalDict -> Object -> Html
viewFull address removals object =
  let objectList = Object.toList object
      dimension = margin * toFloat (List.length objectList + 1)
                + (box + arrowMargin) * toFloat (List.length objectList)
      viewBox = "-0.5 -0.5 " ++ toString dimension ++ " " ++ toString dimension
      pixels = toString dimension ++ "px"

  in  S.svg [ S.version "1.1"
            , S.width pixels, S.height pixels, S.viewBox viewBox
            ]
        <| List.concat
             [ [ defs ]
             , List.indexedMap (\i -> S.lazy (viewColumnBar i)) objectList
             , List.indexedMap (\i object ->
                 let removalSet = objectRemovalSet object removals
                 in  S.lazy2 (viewObject address i) removalSet object)
                 objectList
             ]

defs : Svg
defs =
  S.defs []
         [ S.marker [ S.id "marker-arrow", S.orient "auto"
                    , S.markerWidth "6", S.markerHeight "6"
                    , S.refX "5", S.refY "3"
                    ]
                    [ Svg.path [ S.d "M0,0 L6,3 L0,6 z"
                               , S.stroke "none", S.fill "#000"
                               ] []
                    ]
         ]

viewColumnBar : Int -> Object -> Svg
viewColumnBar i _ =
  let (top, left, _, _)     = boxCoords 0 i
      (_, _, bottom, right) = boxCoords i i
      x = left
      y = top           - corner
      w = right  - left
      h = bottom - top  + corner*2
  in  S.rect
        [ H.key <| "column-bar-" ++ toString i
        , S.fill "rgba(0,0,0,0.05)"
        , S.x <| toString x, S.y <| toString y
        , S.width <| toString w, S.height <| toString h
        , S.rx <| toString corner, S.ry <| toString corner
        ] []

type alias ViewedObject =
  { id      : ID
  , linked  : Bool
  , removed : Bool
  }

viewObject : Signal.Address Action -> Int -> IntSet -> Object -> Svg
viewObject address i removalSet object =
  let ancestors =
        Maybe.map Object.toList (Object.parent object)
          |> Maybe.withDefault []
          |> List.indexedMap (\offset ancestor ->
               { id      = Object.id ancestor
               , linked  = Object.doesLinkTo offset object
               , removed = IntDict.member offset removalSet
               })
          |> EB.fromElementList
          |> EB.toBucketList

  in  S.g [ H.key <| "object-" ++ toString i ]
        <| viewObjectHead i object
             :: List.indexedMap (viewObjectBucket address i (Object.id object))
                                ancestors

viewObjectHead : Int -> Object -> Svg
viewObjectHead i object =
  let (top, left, bottom, right) = boxCoords i i
      x = left          - corner
      y = top
      w = right  - left + corner*2
      h = bottom - top
  in  S.g [ H.key <| "object-head-" ++ toString i ]
        [ S.rect
            [ S.fill "#d00", S.stroke "#000", S.strokeWidth "1"
            , S.x <| toString x, S.y <| toString y
            , S.width <| toString w, S.height <| toString h
            , S.rx <| toString corner, S.ry <| toString corner
            ] []
        , viewObjectName i i (Object.id object)
            [ S.fill "#fff", S.fontWeight "bold" ]
        ]

viewObjectBucket : Signal.Address Action -> Int -> ID
                -> Int -> List (Int, ViewedObject) -> Svg
viewObjectBucket address i headId bucketIndex vobjects =
  let bucketFirst = EB.indexJoin bucketIndex 0
      bucketLast  = bucketFirst + List.length vobjects - 1

      (top, left, _, _)     = boxCoords i (i + 1 + bucketFirst)
      (_, _, bottom, right) = boxCoords i (i + 1 + bucketLast)
      x = left          - corner
      y = top
      w = right  - left + corner*2
      h = bottom - top

      color =
        if bucketIndex % 2 == 0
          then "rgba(0,255,0,0.2)"
          else "rgba(0,0,255,0.2)"

  in  S.g [ H.key <| "object-bucket" ++ toString bucketIndex ]
        <| S.rect [ H.key "object-bucket-background"
                  , S.fill color
                  , S.x <| toString x, S.y <| toString y
                  , S.width <| toString w, S.height <| toString h
                  , S.rx <| toString corner, S.ry <| toString corner
                  ] []
             :: List.map (viewObjectAncestor address i headId) vobjects

viewObjectAncestor : Signal.Address Action -> Int -> ID -> (Int, ViewedObject)
                  -> Html
viewObjectAncestor address i headId (offset, vobject) =
  let (top, left, bottom, right) = boxCoords i (i + 1 + offset)
      x = left
      y = top
      w = right  - left
      h = bottom - top

      maybeName =
        if vobject.linked
          then [ viewObjectName i (i + 1 + offset) vobject.id
                   [ H.key "object-name"
                   , S.fill "rgba(0,0,0,0.4)", S.fontWeight "bold"
                   ]
               , viewLink i (i + 1 + offset) [ H.key "object-link-arrow" ]
               ]
          else []

      maybeCross =
        if vobject.removed
          then [ Svg.path
                   [ H.key "object-remove-cross"
                   , S.d <| "M"  ++ toString left  ++ "," ++ toString top
                         ++ " L" ++ toString right ++ "," ++ toString bottom
                         ++ " M" ++ toString right ++ "," ++ toString top
                         ++ " L" ++ toString left  ++ "," ++ toString bottom
                   , S.stroke "#d00", S.strokeWidth "5", S.fill "none"
                   ] []
               ]
          else []

      removable = offset > 0
      removeOnClick = S.onClick <| Signal.message address (ToggleRemoveLink headId offset)
      maybeLink =
        if removable
          then [ S.a [ H.key "object-remove-button"
                     , removeOnClick
                     , Svg.Attributes.style "cursor: pointer"
                     ]
                     [ S.rect [ S.fill "rgba(0,0,0,0)"
                              , S.x <| toString x, S.y <| toString y
                              , S.width <| toString w, S.height <| toString h
                              ] []
                     ]
               ]
          else []

  in  S.g [ H.key <| "object-ancestor-" ++ toString i ++ "-" ++ toString vobject.id ]
        <| maybeName ++ maybeCross ++ maybeLink

viewObjectName : Int -> Int -> ID -> List S.Attribute -> Svg
viewObjectName row column id attrs =
  let (top, left, bottom, right) = boxCoords row column
      attrs' = [ S.fontFamily "sans-serif", S.fontSize <| toString (box * 0.8)
               , S.dominantBaseline "middle", S.textAnchor "middle"
               , S.x <| toString <| (left + right)  / 2
               , S.y <| toString <| (top  + bottom) / 2
               ]
  in  S.text' (attrs ++ attrs') [ S.text <| idToName id ]

viewLink : Int -> Int -> List S.Attribute -> Svg
viewLink row column attrs =
  let (top, leftFrom, _, rightFrom) = boxCoords row row
      (_,   leftTo,   _, rightTo)   = boxCoords row column

      x0 = (leftFrom + rightFrom) / 2
      x1 = (leftTo   + rightTo)   / 2
      y0 = top
      y1 = top - arrowMargin

      attrs' = [ S.markerEnd "url(#marker-arrow)"
               , S.d <| "M"  ++ toString x0 ++ "," ++ toString y0
                     ++ " L" ++ toString x0 ++ "," ++ toString y1
                     ++ " L" ++ toString x1 ++ "," ++ toString y1
                     ++ " L" ++ toString x1 ++ "," ++ toString y0
               , S.stroke "#000", S.strokeWidth "1", S.fill "none"
               ]
  in  Svg.path (attrs ++ attrs') []


idToName : ID -> String
idToName n =
  let a = Char.toCode 'A'
      z = Char.toCode 'Z'
  in  String.fromChar (Char.fromCode (a + n % (1 + z - a)))
