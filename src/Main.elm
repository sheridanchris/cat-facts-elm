module Main exposing (..)

import Browser exposing (..)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (Decoder, at, field, list, map, string)


type alias CatFact =
    { fact : String
    }


type alias Model =
    { facts : Maybe (List String)
    , numberOfFacts : Int
    , error : Maybe Http.Error
    }


type Msg
    = RequestCatFact
    | GotCatFact (Result Http.Error (List CatFact))
    | InputChanged String


init : () -> ( Model, Cmd Msg )
init () =
    ( { facts = Nothing, numberOfFacts = 1, error = Nothing }, Cmd.none )


catFactDecoder : Decoder (List CatFact)
catFactDecoder =
    -- let data = field "data" list
    at [ "data" ] (list (map CatFact (field "fact" string)))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestCatFact ->
            ( model, Http.get { url = "https://catfact.ninja/facts?limit=" ++ String.fromInt model.numberOfFacts, expect = Http.expectJson GotCatFact catFactDecoder } )

        GotCatFact result ->
            case result of
                Ok catFacts ->
                    ( { model | facts = Just (catFacts |> List.map (\data -> data.fact)) }, Cmd.none )

                Err err ->
                    ( { model | facts = Nothing, error = Just err }, Cmd.none )

        InputChanged input ->
            ( { model | numberOfFacts = Maybe.withDefault 0 (String.toInt input) }, Cmd.none )


viewCatFact : String -> Html Msg
viewCatFact fact =
    p [] [ text fact ]


errorToBody : Http.Error -> String
errorToBody error =
    case error of
        BadUrl badUrl ->
            "Bad url: " ++ badUrl

        Timeout ->
            "Timeout"

        NetworkError ->
            "Network Error"

        BadStatus badStatus ->
            "Bad status: " ++ String.fromInt badStatus

        BadBody badBody ->
            "Bad body: " ++ badBody


view : Model -> Html Msg
view model =
    div []
        ((model.facts |> Maybe.withDefault [ "Nothing to see here" ] |> List.map viewCatFact)
            ++ [ p [] [ text ("Error: " ++ (model.error |> Maybe.map errorToBody |> Maybe.withDefault "No Error")) ]
               , input [ placeholder "# of cat facts", type_ "number", onInput InputChanged, value (String.fromInt model.numberOfFacts) ] []
               , button [ onClick RequestCatFact ] [ text "Get a random cat fact" ]
               ]
        )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
