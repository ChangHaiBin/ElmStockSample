module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


-- MODEL


type alias Model =
    { amount : Int
    , balance : Float
    , convertedPrice : Result String Float
    , convertedQuantity : Result String Int
    , action : String
    }


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            { amount = 135
            , balance = 100000.0
            , convertedPrice = Err "No Input"
            , convertedQuantity = Err "No Input"
            , action = ""
            }
    in
    ( initialModel, Cmd.none )



-- MESSAGES


type Msg
    = BuyStock
    | SellStock
    | SetPrice String
    | SetQuantity String


buyButtonStyle =
    style
        [ ( "background-color", "#4CAF50" )
        , ( "border", "none" )
        , ( "color", "white" )
        , ( "border-radius", "4px" )
        , ( "padding", "10px 20px" )
        , ( "text-align", "center" )
        , ( "text-decoration", "none" )
        , ( "font-size", "20px" )
        ]


sellButtonStyle =
    style
        [ ( "background-color", "#f44336" )
        , ( "border", "none" )
        , ( "color", "white" )
        , ( "border-radius", "4px" )
        , ( "padding", "10px 20px" )
        , ( "text-align", "center" )
        , ( "text-decoration", "none" )
        , ( "font-size", "20px" )
        ]


backgroundStyle =
    style
        [ ( "backgroundColor", "AliceBlue" )
        , ( "text-align", "center" )
        , ( "padding", "10px 0px" )
        , ( "width", "70%" )
        , ( "margin-left", "15%" )
        , ( "margin-right", "15%" )
        ]


tableStyle =
    style
        [ ( "width", "70%" )
        , ( "margin-left", "15%" )
        , ( "margin-right", "15%" )
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ backgroundStyle ]
        [ div []
            [ button [ onClick BuyStock, buyButtonStyle ] [ text "Buy Stock" ]
            , button [ onClick SellStock, sellButtonStyle ] [ text "Sell Stock" ]
            ]
        , div [ tableStyle ]
            [ table []
                [ thead []
                    [ th [] [ text "Field" ]
                    , th [] [ text "User Input" ]
                    , th [] [ text "Validity" ]
                    ]
                , tr []
                    [ td [] [ text "Price" ]
                    , td []
                        [ input [ type_ "text", placeholder "Price", onInput SetPrice ] [] ]
                    , td []
                        [ model.convertedPrice
                            |> (\temp ->
                                    case temp of
                                        Err errorMessage ->
                                            errorMessage

                                        Ok value ->
                                            "OK"
                               )
                            |> text
                        ]
                    ]
                , tr []
                    [ td [] [ text "Quantity" ]
                    , td []
                        [ input [ type_ "text", placeholder "Quantity", onInput SetQuantity ] [] ]
                    , td []
                        [ model.convertedQuantity
                            |> (\temp ->
                                    case temp of
                                        Err errorMessage ->
                                            errorMessage

                                        Ok value ->
                                            "OK"
                               )
                            |> text
                        ]
                    ]
                ]
            ]
        , div []
            [{--
              text "Please Input Price: "
            , input [ type_ "text", placeholder "Price", onInput SetPrice ] []

            --}
            ]
        , div []
            [{--
              text "Please Input Quantity (no decimals): "
            , input [ type_ "text", placeholder "Quantity", onInput SetQuantity ] []

            --}
            ]
        , div [ tableStyle ]
            [ table []
                [ tr []
                    [ td [] [ text "Remaining Stock" ]
                    , td [] [ text (toString model.amount) ]
                    ]
                , tr []
                    [ td [] [ text "Remaining Balance" ]
                    , td [] [ text ("$" ++ toString model.balance) ]
                    ]
                , tr []
                    [ td [] [ text "Action Taken" ]
                    , td [] [ text (toString model.action) ]
                    ]
                ]
            ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                BuyStock ->
                    let
                        ( action, amount, balance ) =
                            model.convertedPrice
                                |> Result.andThen
                                    (\price ->
                                        model.convertedQuantity
                                            |> Result.map
                                                (\quantity ->
                                                    ( "Bought " ++ toString quantity ++ " Shares"
                                                    , model.amount + quantity
                                                    , model.balance - toFloat quantity * price
                                                    )
                                                )
                                    )
                                |> Result.withDefault ( "Error. No action.", model.amount, model.balance )
                    in
                    { model
                        | amount = amount
                        , balance = balance
                        , action = action
                    }

                SellStock ->
                    let
                        ( action, amount, balance ) =
                            model.convertedPrice
                                |> Result.andThen
                                    (\price ->
                                        model.convertedQuantity
                                            |> Result.map
                                                (\quantity ->
                                                    ( "Sold " ++ toString quantity ++ " Shares"
                                                    , model.amount - quantity
                                                    , model.balance + toFloat quantity * price
                                                    )
                                                )
                                    )
                                |> Result.withDefault ( "Error. No action.", model.amount, model.balance )
                    in
                    { model
                        | amount = amount
                        , balance = balance
                        , action = action
                    }

                SetPrice priceString ->
                    { model
                        | convertedPrice =
                            (case priceString of
                                "" ->
                                    Err "No Input"

                                _ ->
                                    priceString
                                        |> String.toFloat
                                        |> Result.mapError (\_ -> "Invalid Input")
                            )
                                |> Result.andThen
                                    (\x ->
                                        if x <= 0.0 then
                                            Err "Price must be positive."
                                        else
                                            Ok x
                                    )
                    }

                SetQuantity quantityString ->
                    { model
                        | convertedQuantity =
                            (case quantityString of
                                "" ->
                                    Err "No Input"

                                _ ->
                                    quantityString
                                        |> String.toInt
                                        |> Result.mapError (\_ -> "Invalid Input")
                            )
                                |> Result.andThen
                                    (\x ->
                                        if x <= 0 then
                                            Err "Quantity must be positive."
                                        else
                                            Ok x
                                    )
                    }
    in
    ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
