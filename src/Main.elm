-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html
--


module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD exposing (Decoder, field, string)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { addresses : String
    , buildings : List Building
    }


type alias Building =
    { address : String
    , coordinates : Coordinates
    }


type Coordinates
    = ValidCoordinates Float Float
    | InvalidCoordinates String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { addresses = ""
      , buildings = []
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


fetchCoordinatesForAddresses : List String -> Cmd Msg
fetchCoordinatesForAddresses addresses =
    addresses
        |> List.map fetchCoordinatesForAddress
        |> Cmd.batch


fetchCoordinatesForAddress : String -> Cmd Msg
fetchCoordinatesForAddress address =
    Http.get
        { url = "https://nominatim.openstreetmap.org/search?format=json&q=" ++ address
        , expect = Http.expectJson GotCoordinates coordinatesDecoder
        }


coordinatesDecoder : Decoder Building
coordinatesDecoder =
    field "0" (field "class" string)
        |> JD.andThen
            (\osmClass ->
                case osmClass of
                    "building" ->
                        buildingCoordinatesDecoder

                    _ ->
                        invalidCoordinatesDecoder
            )


buildingCoordinatesDecoder : Decoder Building
buildingCoordinatesDecoder =
    let
        address =
            field "0" (field "display_name" string)

        latitude =
            field "0" (field "lat" stringFloat)

        longitude =
            field "0" (field "lon" stringFloat)

        building addr lat long =
            { address = addr, coordinates = ValidCoordinates lat long }
    in
    JD.map3 building address latitude longitude


invalidCoordinatesDecoder : Decoder Building
invalidCoordinatesDecoder =
    let
        address =
            field "0" (field "display_name" string)

        osmClass =
            field "0" (field "class" string)

        building addr class =
            { address = addr, coordinates = InvalidCoordinates class }
    in
    JD.map2 building address osmClass


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
    = AddressesChange String
    | FetchCoordinates
    | GotCoordinates (Result Http.Error Building)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddressesChange newAddresses ->
            ( { model | addresses = newAddresses }, Cmd.none )

        FetchCoordinates ->
            let
                splittedAddresses =
                    String.split "\n" model.addresses
            in
            ( { model | buildings = [] }, fetchCoordinatesForAddresses splittedAddresses )

        GotCoordinates (Ok newBuilding) ->
            ( { model | buildings = newBuilding :: model.buildings }, Cmd.none )

        GotCoordinates (Err e) ->
            let
                _ =
                    Debug.log "Error" e
            in
            ( model, Cmd.none )


hasValidCoordinates : Building -> Bool
hasValidCoordinates building =
    case building.coordinates of
        ValidCoordinates _ _ ->
            True

        InvalidCoordinates _ ->
            False



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ textarea [ placeholder "Addresses", value model.addresses, onInput AddressesChange ] []
        , button [ onClick FetchCoordinates ] [ text "Fetch Coordinates" ]
        , div []
            [ p [] [ text "Address:", text model.addresses ]
            , p [] [ text "Buildings:", text (Debug.toString (List.filter hasValidCoordinates model.buildings)) ]
            , p [] [ text "Invalid Buildings:", text (Debug.toString (List.filter (\building -> not (hasValidCoordinates building)) model.buildings)) ]
            ]
        ]
