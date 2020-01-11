module InHouse.Model exposing (..)

import Http
import InHouse.Types as Types


type Model
    = Failure
    | Loading
    | Success Types.Dashboard


type Msg
    = Load
    | GotDashboard (Result Http.Error Types.Dashboard)
