module InHouse.Types exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List
import Ordering exposing (Ordering)


type alias Summoner =
    { remoteID : String
    , name : String
    , level : Int
    , accountID : String
    }


summonerDecoder : Decoder Summoner
summonerDecoder =
    Decode.succeed Summoner
        |> required "remote_id" Decode.string
        |> required "name" Decode.string
        |> required "level" Decode.int
        |> required "accountID" Decode.string


type alias Match =
    { win : String
    , champion : String
    , queue : Int
    , spell1 : String
    , spell2 : String
    }


matchDecoder : Decoder Match
matchDecoder =
    Decode.succeed Match
        |> required "win" Decode.string
        |> required "champion" Decode.string
        |> required "queue" Decode.int
        |> required "spell1" Decode.string
        |> required "spell2" Decode.string


type alias MatchList =
    List Match


matchListDecoder : Decoder MatchList
matchListDecoder =
    Decode.list matchDecoder



-- PLAYER


type alias Player =
    { summoner : Summoner
    , rank : Rank
    , matchList : MatchList
    }


playerDecoder : Decoder Player
playerDecoder =
    Decode.succeed Player
        |> required "summoner" summonerDecoder
        |> required "rank" rankDecoder
        |> required "match_list" matchListDecoder



-- RANKS ORDERING


type alias Rank =
    { tier : Tier
    , division : Division
    , leaguePoints : Int
    , winRate : Int
    , wins : Int
    , losses : Int
    }


rankDecoder =
    Decode.succeed Rank
        |> required "tier" tierDecoder
        |> required "division" divisionDecoder
        |> required "leaguePoints" Decode.int
        |> required "winRate" Decode.int
        |> required "wins" Decode.int
        |> required "losses" Decode.int


rankOrdering : Ordering Rank
rankOrdering =
    let
        tierTieBreaker =
            Ordering.byFieldWith divisionOrdering .division
                |> Ordering.breakTiesWith divisionTieBreaker

        divisionTieBreaker =
            Ordering.reverse (Ordering.byField .leaguePoints)
                |> Ordering.breakTiesWith lpTieBreaker

        lpTieBreaker =
            Ordering.reverse (Ordering.byField .winRate)
    in
    Ordering.byFieldWith tierOrdering .tier
        |> Ordering.breakTiesWith tierTieBreaker


type Tier
    = Diamond
    | Platinum
    | Gold
    | Silver
    | Bronze
    | Iron
    | NoTier


tierDecoder : Decoder Tier
tierDecoder =
    Decode.string
        |> Decode.andThen
            (\tier ->
                case tier of
                    "DIAMOND" ->
                        Decode.succeed Diamond

                    "PLATINUM" ->
                        Decode.succeed Platinum

                    "GOLD" ->
                        Decode.succeed Gold

                    "SILVER" ->
                        Decode.succeed Silver

                    "BRONZE" ->
                        Decode.succeed Bronze

                    "IRON" ->
                        Decode.succeed Iron

                    _ ->
                        Decode.succeed NoTier
            )


tierOrdering : Ordering Tier
tierOrdering =
    Ordering.explicit [ Diamond, Platinum, Gold, Silver, Bronze, Iron, NoTier ]


tierToString : Tier -> String
tierToString tier =
    case tier of
        Diamond ->
            "Diamond"

        Platinum ->
            "Platinum"

        Gold ->
            "Gold"

        Silver ->
            "Silver"

        Bronze ->
            "Bronze"

        Iron ->
            "Iron"

        NoTier ->
            "Unranked"


type Division
    = One
    | Two
    | Three
    | Four
    | Five
    | NoDivision


divisionToString : Division -> String
divisionToString division =
    case division of
        One ->
            "1"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        NoDivision ->
            ""


divisionDecoder : Decoder Division
divisionDecoder =
    Decode.string
        |> Decode.andThen
            (\division ->
                case division of
                    "I" ->
                        Decode.succeed One

                    "II" ->
                        Decode.succeed Two

                    "III" ->
                        Decode.succeed Three

                    "IV" ->
                        Decode.succeed Four

                    "V" ->
                        Decode.succeed Five

                    _ ->
                        Decode.succeed NoDivision
            )


divisionOrdering : Ordering Division
divisionOrdering =
    Ordering.explicit [ One, Two, Three, Four, Five, NoDivision ]


playerRankOrdering : Ordering Player
playerRankOrdering =
    Ordering.byFieldWith rankOrdering .rank


sortPlayersByRank : List Player -> List Player
sortPlayersByRank =
    List.sortWith playerRankOrdering


type alias Dashboard =
    { players : List Player
    }


dashboardDecoder : Decoder Dashboard
dashboardDecoder =
    Decode.succeed Dashboard
        |> required "players" (Decode.list playerDecoder)
