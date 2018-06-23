module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random


-- MODEL


type Answered
    = Unanswered
    | Right
    | Wrong


type Page
    = Index
    | Question ( Int, Answered )


type alias MortgageStep =
    { year : Int
    , principalRemaining : Float
    , interest : Float
    , payment : Float
    }


type alias Model =
    { mortgageAmount : Result String Float
    , interestRate : Result String Float
    , numYear : Result String Int
    , schedule : List MortgageStep
    , page : Page
    }


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            { mortgageAmount = Err "No Input"
            , interestRate = Err "No Input"
            , numYear = Err "No Input"
            , schedule = []
            , page = Question ( 1, Unanswered )
            }
    in
    ( initialModel, Cmd.none )



-- MESSAGES


type Msg
    = Calculate
    | CheckMortgageInput String
    | CheckInterestRate String
    | CheckNumYear String


calculateStyle =
    style
        [ ( "background-color", "grey" )
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


getContent page =
    case page of
        Index ->
            div [] [ "ASDaF" |> text ]

        Question ( i, answered ) ->
            let
                aaaaa =
                    1.0
            in
            div []
                [ "Answer: " |> text
                , input [ type_ "text", onInput CheckMortgageInput ] []
                ]


view : Model -> Html Msg
view model =
    div [ backgroundStyle ]
        [ button [ onClick Calculate, calculateStyle ] [ text "Calculate" ]
        , div [ tableStyle ]
            [ table []
                [ thead []
                    [ th [] [ text "Field" ]
                    , th [] [ text "User Input" ]
                    , th [] [ text "Validity" ]
                    ]
                , tr []
                    [ td [] [ text "Mortgage Amount" ]
                    , td []
                        [ input [ type_ "text", placeholder "e.g. $1000000", onInput CheckMortgageInput ] [] ]
                    , td []
                        [ model.mortgageAmount
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
                    [ td [] [ text "Interest Rate" ]
                    , td []
                        [ input [ type_ "text", placeholder "e.g. 0.0 < x < 1.0", onInput CheckInterestRate ] [] ]
                    , td []
                        [ model.interestRate
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
                    [ td [] [ text "Num of Year" ]
                    , td []
                        [ input [ type_ "text", placeholder "e.g. 0 < n < 30", onInput CheckNumYear ] [] ]
                    , td []
                        [ model.numYear
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
            [ getContent model.page
            ]
        , div [] [ "ASDF" |> text ]
        ]


(||>) : ( a, b ) -> (a -> b -> c) -> c
(||>) ( a, b ) f =
    f a b


(|||>) : ( a, b, c ) -> (a -> b -> c -> d) -> d
(|||>) ( a, b, c ) f =
    f a b c


generateMortgageStep :
    Result x Float
    -> Result x Float
    -> Result x Int
    -> Result x (List MortgageStep)
generateMortgageStep a b c =
    ( a, b, c )
        |||>
            Result.map3
                (\mortgage interestRate numYear ->
                    let
                        v =
                            1.0 / (1.0 + interestRate)

                        payment =
                            mortgage * interestRate / (1.0 - (v ^ (numYear |> toFloat)))

                        firstMortgageStep : MortgageStep
                        firstMortgageStep =
                            { year = 0
                            , principalRemaining = mortgage
                            , interest = mortgage * interestRate
                            , payment = payment
                            }
                    in
                    List.range 1 numYear
                        |> List.scanl
                            (\_ acc ->
                                let
                                    principalRemaining =
                                        acc.principalRemaining + acc.interest - acc.payment
                                in
                                { acc
                                    | year = acc.year + 1
                                    , principalRemaining = principalRemaining
                                    , interest = principalRemaining * interestRate
                                }
                            )
                            firstMortgageStep
                )



{--
getMortgageStep mortgageResult interestRateResult numYearResult =
    ( mortgageResult, interestRateResult, numYearResult )
        |||>
            Result.map3
                (\mortgage interestRate numYear ->
                    List.range 1 numYear
                        |> List.scanl
                            (\x acc ->
                                let
                                    principalRemaining =
                                        acc.principalRemaining + acc.interest - acc.payment
                                in
                                { acc
                                    | year = acc.year + 1
                                    , principalRemaining = principalRemaining
                                    , interest = principalRemaining * interestRate
                                    , payment = acc.payment
                                }
                            )
                            (MortgageStep { year = 0, principalRemaining = mortgageResult, interest = mortgageResult * interestRate, payment = 10000.0 })
                )
--}
-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Calculate ->
                    { model
                        | schedule =
                            generateMortgageStep model.mortgageAmount model.interestRate model.numYear
                                |> Result.withDefault model.schedule
                    }

                CheckNumYear yearInput ->
                    { model
                        | numYear =
                            (case yearInput of
                                "" ->
                                    Err "No Input"

                                _ ->
                                    yearInput
                                        |> String.toInt
                                        |> Result.mapError (\_ -> "Invalid Input")
                            )
                                |> Result.andThen
                                    (\x ->
                                        if x <= 0 then
                                            Err "Too low"
                                        else if x > 30 then
                                            Err "Too high"
                                        else
                                            Ok x
                                    )
                    }

                CheckInterestRate interestInput ->
                    { model
                        | interestRate =
                            (case interestInput of
                                "" ->
                                    Err "No Input"

                                _ ->
                                    interestInput
                                        |> String.toFloat
                                        |> Result.mapError (\_ -> "Invalid Input")
                            )
                                |> Result.andThen
                                    (\x ->
                                        if x <= 0.0 then
                                            Err "Too low"
                                        else if x >= 1.0 then
                                            Err "Too high"
                                        else
                                            Ok x
                                    )
                    }

                CheckMortgageInput mortgageInput ->
                    { model
                        | mortgageAmount =
                            (case mortgageInput of
                                "" ->
                                    Err "No Input"

                                _ ->
                                    mortgageInput
                                        |> String.toFloat
                                        |> Result.mapError (\_ -> "Invalid Input")
                            )
                                |> Result.andThen
                                    (\x ->
                                        if x <= 0.0 then
                                            Err "Mortgage amount must be positive."
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
