module InHouse exposing (..)

import Browser
import Dict exposing (Dict)
import Http
import InHouse.Colors as Colors
import InHouse.Model exposing (Model(..), Msg(..))
import InHouse.Types as Types
import InHouse.Views as Views



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = Views.view
        }



-- MODEL


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getDashboardData )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load ->
            ( Loading, getDashboardData )

        GotDashboard result ->
            case result of
                Ok data ->
                    ( Success data, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getDashboardData : Cmd Msg
getDashboardData =
    Http.get
        { url = "http://www.inhouse.gg/active_tournament"
        , expect = Http.expectJson GotDashboard Types.dashboardDecoder
        }
