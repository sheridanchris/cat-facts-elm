module Main exposing (..)

import Browser exposing (..)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (Decoder, at, field, list, map, string)


type alias CatFact =
    { fact : String }


type alias Model =
    { facts : Maybe (List String)
    , numberOfFacts : Int
    , error : Maybe Http.Error
    }


type Msg
    = RequestCatFacts
    | GotCatFact (Result Http.Error (List CatFact))
    | InputChanged String


init : () -> ( Model, Cmd Msg )
init () =
    ( { facts = Nothing, numberOfFacts = 1, error = Nothing }, Cmd.none )


catFactDecoder : Decoder (List CatFact)
catFactDecoder =
    at [ "data" ] (list (map CatFact (field "fact" string)))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestCatFacts ->
            ( model, Http.get { url = "https://catfact.ninja/facts?limit=" ++ String.fromInt model.numberOfFacts, expect = Http.expectJson GotCatFact catFactDecoder } )

        GotCatFact result ->
            case result of
                Ok catFacts ->
                    ( { model | facts = catFacts |> List.map .fact |> Just }, Cmd.none )

                Err err ->
                    ( { model | facts = Nothing, error = Just err }, Cmd.none )

        InputChanged input ->
            ( { model | numberOfFacts = String.toInt input |> Maybe.withDefault 0 }, Cmd.none )


viewCatFact : String -> Html Msg
viewCatFact fact =
    p [] [ text fact ]


errorToString : Http.Error -> String
errorToString error =
    let
        errorDescription =
            case error of
                BadUrl _ ->
                    "Bad url"

                Timeout ->
                    "Timeout"

                NetworkError ->
                    "Network Error"

                BadStatus _ ->
                    "Bad Status"

                BadBody _ ->
                    "Bad Body"
    in
    "An error has occured: " ++ errorDescription


view : Model -> Html Msg
view model =
    let
        facts =
            model.facts |> Maybe.withDefault [ "Nothing to see here" ] |> List.map viewCatFact

        error =
            model.error |> Maybe.map errorToString |> Maybe.withDefault "No errors"
    in
    div []
        (facts
            ++ [ p [] [ text error ]
               , input [ placeholder "# of cat facts", type_ "number", onInput InputChanged, value (String.fromInt model.numberOfFacts) ] []
               , button [ onClick RequestCatFacts ] [ text "Get random cat facts" ]
               ]
        )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
