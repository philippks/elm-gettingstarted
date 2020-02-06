-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html
--


module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Circle2d
import Geometry.Svg
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD exposing (Decoder, field, string)
import KMeans
import Length
import Pixels
import Point2d
import Svg
import Svg.Attributes as Attributes



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { queries : String
    , queryResults : List QueryResult
    , buildings : List Building
    }


type alias QueryResult =
    { displayName : String
    , coordinates : Coordinates
    , osmClass : String
    }


type alias Building =
    { displayName : String
    , coordinates : Coordinates
    , cluster : Int
    }


type alias Coordinates =
    { latitude : Float, longitude : Float }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { queries = ""
      , queryResults = []
      , buildings = []
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


fetchQueries : List String -> Cmd Msg
fetchQueries queries =
    queries
        |> List.map fetchQuery
        |> Cmd.batch


fetchQuery : String -> Cmd Msg
fetchQuery query =
    Http.get
        { url = "https://nominatim.openstreetmap.org/search?format=json&q=" ++ query
        , expect = Http.expectJson GotQueryResult queryResultDecoder
        }


queryResultDecoder : Decoder QueryResult
queryResultDecoder =
    let
        displayName =
            field "0" (field "display_name" string)

        latitude =
            field "0" (field "lat" stringFloat)

        longitude =
            field "0" (field "lon" stringFloat)

        osmClass =
            field "0" (field "class" string)

        queryResult displayN lat long osmClazz =
            { displayName = displayN, coordinates = Coordinates lat long, osmClass = osmClazz }
    in
    JD.map4 queryResult displayName latitude longitude osmClass


stringFloat : Decoder Float
stringFloat =
    JD.string
        |> JD.andThen
            (\floatStr ->
                case String.toFloat floatStr of
                    Just f ->
                        JD.succeed f

                    Nothing ->
                        JD.fail "Failed"
            )



-- UPDATE


type Msg
    = QueriesChange String
    | FetchQueries
    | ClusterBuildings
    | GotQueryResult (Result Http.Error QueryResult)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QueriesChange newQueries ->
            ( { model | queries = newQueries }, Cmd.none )

        FetchQueries ->
            let
                splittedQueries =
                    String.split "\n" model.queries
            in
            ( { model | queryResults = [] }, fetchQueries splittedQueries )

        GotQueryResult (Ok newQueryResult) ->
            ( { model | queryResults = newQueryResult :: model.queryResults }, Cmd.none )

        GotQueryResult (Err e) ->
            let
                _ =
                    Debug.log "Error" e
            in
            ( model, Cmd.none )

        ClusterBuildings ->
            ( model, Cmd.none )


queryResultToPoint : QueryResult -> Point2d.Point2d Pixels.Pixels a
queryResultToPoint queryResult =
    Point2d.pixels
        queryResult.coordinates.latitude
        queryResult.coordinates.longitude


scale : Point2d.Point2d unit number -> Point2d.Point2d unit number
scale =
    Point2d.scaleAbout Point2d.origin 20


colors : List String
colors =
    [ "red", "green", "purple" ]



-- VIEW


viewPoints : List (List QueryResult) -> List (Svg.Svg msg)
viewPoints clusters =
    let
        viewPoint opacity color queryResult =
            Geometry.Svg.circle2d
                [ Attributes.opacity (String.fromFloat opacity)
                , Attributes.fill color
                ]
                (Circle2d.withRadius (Pixels.pixels 4) (scale (queryResultToPoint queryResult)))
    in
    List.map2 (\color coordinates -> List.map (viewPoint 0.8 color) coordinates) colors clusters
        |> List.concat


view : Model -> Html Msg
view model =
    div [ style "width" "100%" ]
        [ textarea [ style "width" "100%", placeholder "Addresses", value model.queries, onInput QueriesChange ] []
        , button [ onClick FetchQueries ] [ text "Fetch Coordinates" ]
        , div [ style "width" "100%" ]
            [ p [] [ text "Address:", text model.queries ]
            , p [] [ text "Buildings: ", text (Debug.toString (List.filter (\building -> building.osmClass == "building") model.queryResults)) ]
            , p [] [ text "Others:", text (Debug.toString (List.filter (\building -> building.osmClass /= "building") model.queryResults)) ]
            ]
        , div []
            [ Svg.svg
                [ Attributes.width "2000"
                , Attributes.height "2000"
                ]
              <|
                let
                    clustered =
                        KMeans.clusterExactlyBy (\building -> [ building.coordinates.longitude, building.coordinates.latitude ]) 3 model.queryResults
                in
                viewPoints
                    clustered.clusters
            ]
        ]
