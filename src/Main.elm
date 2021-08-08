module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser
import Random
import Random.Array
import Array exposing (Array)

main = Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

type alias Tile = Maybe Int
type alias Grid = Array (Array Tile)

type alias Model = Grid

type Msg = SetGrid Grid
         | Push Int Int

correctGrid : Grid
correctGrid =
    Array.fromList
        [ Array.fromList [Just  1, Just  2, Just  3, Just  4]
        , Array.fromList [Just  5, Just  6, Just  7, Just  8]
        , Array.fromList [Just  9, Just 10, Just 11, Just 12]
        , Array.fromList [Just 13, Just 14, Just 15, Nothing]
        ]

flatten : Array (Array a) -> Array a
flatten arr =
  Array.foldr Array.append (Array.fromList []) arr

reshape : Array Tile -> Grid
reshape arr =
    case Array.toList arr of
        [a, b, c, d] -> Array.fromList [ Array.fromList [a, b, c, d] ]
        (a::b::c::d::rest) -> Array.append (Array.fromList [ Array.fromList [a,b,c,d] ]) (reshape (Array.fromList rest))
        _ -> Array.empty


init : () -> (Model, Cmd Msg)
init _ =
    ( Array.empty
    , Random.generate (\ arr -> SetGrid (reshape arr)) (Random.Array.shuffle (flatten correctGrid))
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetGrid grid -> (grid, Cmd.none)
        Push i j ->
            let
                idx = coordsToIndex (i, j)
            in
                case Array.get idx model of
                    Nothing -> (model, Cmd.none)
                    Just _ -> (push model (i, j), Cmd.none)

push : Grid -> (Int, Int) -> Model
push grid (i, j) =
    let
        arr        = flatten grid
        up_idx     = coordsToIndex (i - 1, j)
        right_idx  = coordsToIndex (i, j + 1)
        bottom_idx = coordsToIndex (i - 1, j)
        left_idx   = coordsToIndex (i, j - 1)
    in
        if Array.get up_idx arr          == Nothing then
            grid
        else if Array.get right_idx arr  == Nothing then
            grid
        else if Array.get bottom_idx arr == Nothing then
            grid
        else if Array.get left_idx arr   == Nothing then
            grid
        else
            grid

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

view : Model -> Browser.Document Msg
view model =
    {
        title = "tiny15",
        body =
            [ node "link" [rel "stylesheet", href "style.css"] []
            , div [ class "col" ]
                [ h1 [] [ text "tiny15" ]
                , h2 []
                    [ span [] [ text "Built with " ]
                    , a [ href "https://elm-lang.org/" ] [ text "Elm" ]
                    , span [] [ text "." ]
                    ]
                , div [ class "content" ] (List.indexedMap viewTile (Array.toList (flatten model)))
                ]
            ]
    }

indexToCoords : Int -> (Int, Int)
indexToCoords n = (n // 4, modBy 4 n)

coordsToIndex : (Int, Int) -> Int
coordsToIndex (i, j) = i * 4 + j

viewTile : Int -> Tile -> Html Msg
viewTile idx tile =
    case tile of
        Just n  ->
            let
                (i, j) = indexToCoords idx
            in
                button [ onClick (Push i j)] [ text (String.fromInt n) ]
        Nothing -> button [] []
