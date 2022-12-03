module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame, onKeyDown)
import Char exposing (fromCode, toCode, toUpper)
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Round as R
import String exposing (fromChar, fromFloat, fromInt)
import Task
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( Ready, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Ready
    | InCountDown Int
    | Playing Time.Posix Time.Posix (List Char)
    | Finish Time.Posix Time.Posix


alphabet : List Char
alphabet =
    List.range (toCode 'A') (toCode 'Z') |> List.map fromCode



-- UPDATE


type Msg
    = StartCountDown -- スタートのカウントダウン開始
    | CountDown Time.Posix -- カウントダウン
    | StartTimer Time.Posix
    | Tick Time.Posix
    | InputChar Char
    | IgnoreChar
    | Retry


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartCountDown ->
            ( InCountDown 5, Cmd.none )

        CountDown _ ->
            case model of
                InCountDown 1 ->
                    ( InCountDown 0, Task.perform StartTimer Time.now )

                InCountDown n ->
                    ( InCountDown (n - 1), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartTimer nowTime ->
            case model of
                InCountDown 0 ->
                    ( Playing nowTime nowTime alphabet, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Tick nowTime ->
            case model of
                Playing start _ rest ->
                    ( Playing start nowTime rest, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        InputChar char ->
            case model of
                Playing start now (c :: []) ->
                    if char == c then
                        ( Finish start now, Cmd.none )

                    else
                        ( model, Cmd.none )

                Playing start now (c :: cs) ->
                    if char == c then
                        ( Playing start now cs, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Retry ->
            ( Ready, Cmd.none )

        IgnoreChar ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ countDown model
        , tick model
        , onKeyDown keyDecoder
        ]


countDown : Model -> Sub Msg
countDown model =
    case model of
        InCountDown _ ->
            Time.every 1000 CountDown

        _ ->
            Sub.none


tick : Model -> Sub Msg
tick model =
    case model of
        Playing _ _ _ ->
            onAnimationFrame Tick

        _ ->
            Sub.none


keyDecoder : JD.Decoder Msg
keyDecoder =
    JD.map toKey (JD.field "key" JD.string)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            InputChar (toUpper char)

        _ ->
            IgnoreChar



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Ready ->
            viewRedy

        InCountDown n ->
            viewInCoutDown n

        Playing start now rest ->
            viewPlaying start now rest

        Finish start now ->
            viewFinish start now


viewRedy : Html Msg
viewRedy =
    div
        [ style "width" "500px", style "margin" "auto", style "text-align" "center" ]
        [ viewHeader
        , viewChar "⌨"
        , div [ style "font-size" "5rem" ] [ text "0.000s" ]
        , viewButton StartCountDown "Start"
        ]


viewInCoutDown : Int -> Html Msg
viewInCoutDown count =
    div
        [ style "width" "500px", style "margin" "auto", style "text-align" "center" ]
        [ viewHeader
        , viewChar (fromInt count)
        , div [ style "font-size" "5rem" ] [ text "0.000s" ]
        , viewButton Retry "Retry"
        ]


viewPlaying : Time.Posix -> Time.Posix -> List Char -> Html Msg
viewPlaying start now cs =
    case cs of
        [] ->
            viewFinish start now

        c :: _ ->
            div
                [ style "width" "500px", style "margin" "auto", style "text-align" "center" ]
                [ viewHeader
                , viewChar (fromChar c)
                , viewTimer start now
                , viewButton Retry "Retry"
                ]


viewFinish : Time.Posix -> Time.Posix -> Html Msg
viewFinish start now =
    div
        [ style "width" "500px", style "margin" "auto", style "text-align" "center" ]
        [ viewHeader
        , viewChar "🎉"
        , viewTimer start now
        , viewButton Retry "Retry"
        ]


viewHeader : Html Msg
viewHeader =
    h1 [] [ text "Elm typing A to Z" ]


viewChar : String -> Html Msg
viewChar s =
    div [ style "font-size" "10rem" ] [ text s ]


viewTimer : Time.Posix -> Time.Posix -> Html Msg
viewTimer start now =
    div [ style "font-size" "5rem" ]
        [ toFloat (Time.posixToMillis now - Time.posixToMillis start) / 1000.0 |> R.round 3 |> (\s -> s ++ "s" |> text) ]


viewButton : Msg -> String -> Html Msg
viewButton msg s =
    div [] [ button [ onClick msg ] [ text s ] ]
