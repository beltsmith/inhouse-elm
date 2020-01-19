module InHouse.Views exposing (..)

import Css
import Html exposing (Html, div, h2, text)
import Html.Attributes as A
import InHouse.Colors as Colors
import InHouse.Model exposing (Model(..), Msg(..))
import InHouse.Types as Types
import List
import List.Extra exposing (greedyGroupsOf)
import Regex


cdnUrl =
    "https://ddragon.leagueoflegends.com/cdn/9.24.2/img/"


opggUrl =
    "https://opgg-static.akamaized.net/images/medals/"


fontTag : Html Msg
fontTag =
    Html.node "link"
        [ A.href "https://fonts.googleapis.com/css?family=Roboto"
        , A.rel "stylesheet"
        ]
        []


styleTag : Html Msg
styleTag =
    Html.node "link"
        [ A.href "/root.css"
        , A.rel "stylesheet"
        ]
        []


inHouseHeader : Html Msg
inHouseHeader =
    h2
        [ A.style "textAlign" "center"
        ]
        [ text "InHouse Leaderboard (NKU)"
        ]


imageForChampion : String -> String
imageForChampion rawChampion =
    let
        apostropheSanitizer =
            Maybe.withDefault Regex.never (Regex.fromString "'[A-Za-z]")

        champion =
            rawChampion
                |> Regex.replace apostropheSanitizer
                    (\s -> String.toLower (String.right 1 s.match))
                |> String.replace " " ""
                |> String.replace "." ""
    in
    cdnUrl ++ "champion/" ++ champion ++ ".png"


circleImg : String -> Int -> Html Msg
circleImg img size =
    let
        height =
            String.fromInt size ++ "px"
    in
    div
        [ A.style "height" height
        ]
        [ Html.img [ A.src img, A.width size, A.height size, A.style "borderRadius" "50%" ] [] ]


squareImg : String -> Int -> Html Msg
squareImg img size =
    let
        height =
            String.fromInt size ++ "px"
    in
    div
        [ A.style "height" height
        ]
        [ Html.img [ A.src img, A.width size, A.height size ] [] ]


championImg : String -> Html Msg
championImg champion =
    circleImg (imageForChampion champion) 70


viewResult : Types.Match -> Html Msg
viewResult match =
    div
        [ A.style "display" "flex"
        , A.style "justifyContent" "center"
        , A.style "flexGrow" "1"
        , A.style "alignItems" "center"
        , A.style "fontWeight" "bold"
        ]
        [ text match.win ]


rankName : Types.Rank -> String
rankName rank =
    case rank.tier of
        Types.NoTier ->
            "default"

        _ ->
            let
                tier =
                    String.toLower (Types.tierToString rank.tier)

                division =
                    Types.divisionToString rank.division
            in
            tier ++ "_" ++ division


imageForRank : Types.Rank -> String
imageForRank rank =
    opggUrl ++ rankName rank ++ ".png"


rankImg : Types.Rank -> Html Msg
rankImg rank =
    div
        [ A.style "display" "flex"
        , A.style "justifyContent" "center"
        , A.style "flexGrow" "1"
        , A.style "alignItems" "center"
        , A.style "fontWeight" "bold"
        ]
        [ squareImg (imageForRank rank) 150 ]


imageForSummoner : String -> String
imageForSummoner summonerSpell =
    cdnUrl ++ "spell/" ++ summonerSpell ++ ".png"


summonerImg : String -> Html Msg
summonerImg summonerSpell =
    squareImg (imageForSummoner summonerSpell) 35


summonerImgs : Types.Match -> Html Msg
summonerImgs match =
    inlineFlex
        [ summonerImg match.spell1
        , summonerImg match.spell2
        ]


viewMatch : Types.Match -> Html Msg
viewMatch match =
    let
        resultColor =
            case match.win of
                "Win" ->
                    Colors.blue

                "Loss" ->
                    Colors.red

                _ ->
                    ""
    in
    div
        [ A.class "match"
        , A.style "margin" "5px 5px"
        , A.style "background" resultColor
        , A.style "color" Colors.background
        , A.style "display" "flex"
        , A.style "flexDirection" "column"
        , A.style "justifyContent" "center"
        ]
        [ championImg match.champion
        , summonerImgs match

        --, viewResult match
        ]


matchesPerRow =
    10


rowsToShow =
    2


viewMatchList : Types.MatchList -> Html Msg
viewMatchList matchList =
    div
        [ A.class "match-list"
        , A.style "display" "inline-flex"
        ]
        (List.map viewMatch matchList)


tierClass : String -> String
tierClass tier =
    "tier-" ++ String.toLower tier


viewRankStanding : Types.Rank -> Html Msg
viewRankStanding rank =
    div
        [ A.class "standing"
        , A.style "paddingTop" "5px"
        , A.style "display" "inline-flex"
        , A.style "justifyContent" "center"
        , A.style "marginTop" "auto"
        ]
        [ viewWins rank
        , viewLosses rank
        ]


viewMatches : Types.MatchList -> Html Msg
viewMatches matchList =
    let
        matchesToShow =
            List.take (rowsToShow * matchesPerRow) matchList
    in
    div
        [ A.class "match-lists"
        , A.style "display" "flex"
        , A.style "flexDirection" "column"
        , A.style "padding" "5px"
        ]
    <|
        List.map viewMatchList (greedyGroupsOf matchesPerRow matchesToShow)


viewWins : Types.Rank -> Html Msg
viewWins rank =
    inlineFlex
        [ div
            [ A.style "color" "#1f8ecd"
            , A.style "paddingTop" "5px"
            ]
            [ text (String.fromInt rank.wins) ]
        , div
            [ A.style "color" Colors.blue
            , A.style "paddingRight" "5px"
            , A.style "paddingTop" "5px"
            ]
            [ text "W " ]
        ]


viewLosses : Types.Rank -> Html Msg
viewLosses rank =
    inlineFlex
        [ div
            [ A.style "color" "#ee5a52"
            , A.style "paddingTop" "5px"
            ]
            [ text (String.fromInt rank.losses) ]
        , div
            [ A.style "color" Colors.red
            , A.style "paddingTop" "5px"
            ]
            [ text "L " ]
        ]


viewWinRate : Types.Rank -> Html Msg
viewWinRate rank =
    div
        [ A.style "display" "inline-flex"
        , A.style "paddingTop" "3px"
        , A.style "justifyContent" "center"
        , A.style "marginBottom" "auto"
        ]
        [ div
            []
            [ text ("(" ++ String.fromInt rank.winRate ++ "%)") ]
        ]


inlineFlex =
    div [ A.style "display" "inline-flex" ]


viewLeague : Types.Rank -> Html Msg
viewLeague rank =
    div
        [ A.style "display" "inline-flex"
        , A.style "padding" "11px"
        , A.style "justifyContent" "center"
        , A.style "marginBottom" "auto"
        ]
        [ viewTier rank.tier
        , viewDivision rank.division
        , viewLeaguePoints rank.leaguePoints
        ]


viewRank : Types.Rank -> Html Msg
viewRank rank =
    div
        [ A.style "display" "flex"
        , A.style "flexDirection" "column"
        , A.style "borderRight" "1px solid"
        , A.style "width" "175px"
        ]
        [ viewRankStanding rank
        , viewWinRate rank
        , rankImg rank
        , viewLeague rank
        ]


viewTier : Types.Tier -> Html Msg
viewTier tier =
    let
        tierString =
            Types.tierToString tier
    in
    div
        [ A.class (tierClass tierString)
        , A.style "paddingRight" "5px"
        ]
        [ text tierString
        ]


divisionClass : String -> String
divisionClass division =
    "division-" ++ String.toLower division


viewDivision : Types.Division -> Html Msg
viewDivision division =
    let
        divisionString =
            Types.divisionToString division
    in
    div
        [ A.class (divisionClass divisionString)
        , A.style "paddingRight" "5px"
        ]
        [ text divisionString
        ]


viewLeaguePoints : Int -> Html Msg
viewLeaguePoints leaguePoints =
    div []
        [ text ("(" ++ String.fromInt leaguePoints ++ " LP)") ]


viewSummonerName : Types.Summoner -> Html Msg
viewSummonerName summoner =
    div
        [ A.class "summoner"

        --, A.style "padding" "5px"
        ]
        [ text summoner.name ]


viewSummoner : Types.Player -> Html Msg
viewSummoner player =
    div
        [ A.style "width" "160px"
        , A.style "borderRight" "1px solid"
        ]
        [ viewRank player.rank
        ]


viewPosition : String -> Html Msg
viewPosition position =
    div []
        [ text position
        ]


viewPlayerName : Types.Player -> Html Msg
viewPlayerName player =
    div [ A.style "paddingLeft" "10px" ]
        [ text player.summoner.name
        ]


viewPlayerHeader : Int -> Types.Player -> Html Msg
viewPlayerHeader positionInt player =
    let
        position =
            String.fromInt (positionInt + 1) ++ ")"
    in
    div
        [ A.class "playerHeader"
        , A.style "padding" "5px"
        , A.style "fontWeight" "bold"
        , A.style "fontSize" "24px"
        , A.style "display" "flex"
        , A.style "flexDirection" "row"
        , A.style "borderBottom" "1px solid"
        , A.style "backgroundColor" Colors.lightGrey
        ]
        [ viewPosition position
        , viewPlayerName player
        ]


viewPlayer : Int -> Types.Player -> Html Msg
viewPlayer position player =
    div
        [ A.class "player"
        , A.style "display" "flex"
        , A.style "flexDirection" "column"
        , A.style "border" "1px solid"
        , A.style "marginBottom" "5px"
        ]
        [ div []
            [ viewPlayerHeader position player
            ]
        , div
            [ A.style "display" "flex"
            , A.style "flexDirection" "row"
            ]
            [ viewRank player.rank
            , viewMatches player.matchList
            ]
        ]


viewDashboard : Types.Dashboard -> Html Msg
viewDashboard dashboard =
    let
        players =
            Types.sortPlayersByRank dashboard.players
    in
    div
        [ A.class "dashboard"
        ]
        (List.indexedMap viewPlayer players)


view : Model -> Html Msg
view model =
    div
        [ A.style "color" Colors.text
        , A.style "padding" "5px"
        , A.style "fontFamily" "Roboto"
        , A.style "padding" "5px"
        , A.style "display" "flex"
        , A.style "flexDirection" "column"
        , A.style "margin" "0 auto"
        , A.style "width" "min-content"
        ]
        [ fontTag
        , styleTag
        , inHouseHeader
        , viewModel model
        ]


viewModel : Model -> Html Msg
viewModel model =
    case model of
        Failure ->
            div []
                [ text "Bad Data"
                ]

        Loading ->
            text "Loading..."

        Success dashboard ->
            viewDashboard dashboard
