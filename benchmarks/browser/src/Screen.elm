port module Screen exposing (main)

import Browser
import Html            exposing
  (Html, div, text, table, tr, th, td, a, button)
import Html.Attributes exposing (class, id, style, href, target, disabled, colspan)
import Html.Events     exposing (onClick)

-- Incoming ports
port addEntry : ((Name, Link, StateString) -> msg) -> Sub msg
port updateState : ((EntryId, StateString) -> msg) -> Sub msg
port updateTime : ((EntryId, Int, Int) -> msg) -> Sub msg
port updatePct : (Int -> msg) -> Sub msg
port runDone : (() -> msg) -> Sub msg
-- Outgoing ports
port run : () -> Cmd msg

type alias Name = String
type alias Link = String
type alias StateString = String
type alias EntryId = Int
type State = Loading | Ready | Error

type alias Model =
  { entries       : List BenchmarkEntry
  , runPercentage : Maybe Int -- Nothing = not running
  }

type alias BenchmarkEntry =
  { name    : Name
  , link    : Link
  , state   : State
  , time    : Maybe (Int, Int) -- Average time with its 95% confidence deviation
  }

type Msg
  = MsgAddEntry String String State
  | MsgUpdateState EntryId State
  | MsgUpdateTime EntryId Int Int
  | MsgUpdatePct Int
  | MsgRunDone
  | MsgRun

main : Program () Model Msg
main =
  Browser.element
    { init = \() -> ( init, Cmd.none )
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
init : Model
init =
  { entries   = []
  , runPercentage = Nothing
  }

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ addEntry <| \(name, link, state) -> MsgAddEntry name link (parseState state)
    , updateState <| \(id, state) -> MsgUpdateState id (parseState state)
    , updateTime <| \(id, avgTime, timeDev) -> MsgUpdateTime id avgTime timeDev
    , updatePct MsgUpdatePct
    , runDone <| \() -> MsgRunDone
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  case msg of
    MsgAddEntry name link state ->
      (
        { m
          | entries =
              m.entries ++
              [ { name  = name
                , link  = link
                , state = state
                , time  = Nothing
                }
              ]
        }
      , Cmd.none
      )
    MsgUpdateState id state ->
      ( { m | entries = mapAt id (\e -> { e | state = state }) m.entries }
      , Cmd.none
      )
    MsgUpdateTime id avgTime timeDev ->
      ( { m | entries = mapAt id (\e -> { e | time = Just (avgTime, timeDev) }) m.entries }
      , Cmd.none
      )
    MsgUpdatePct pct ->
      ( { m | runPercentage = Just pct }
      , Cmd.none
      )
    MsgRunDone ->
      ( { m | runPercentage = Nothing }
      , Cmd.none
      )
    MsgRun ->
      (
        { m
          | entries = List.map (\e -> { e | time = Nothing }) m.entries
          , runPercentage = Just 0
        }
      , run ()
      )

view : Model -> Html Msg
view m =
  div []
    [ table []
        ( tr []
            [ th [] []
            , th [ style "width" "150pt" ] [text "Name"]
            , th [ style "width" "100pt", colspan 3 ] [text "Time"]
            ]
          :: List.map viewEntry m.entries )
    , button [ disabled <| not <| canRun m, onClick MsgRun ] [ text <| runButtonText m.runPercentage ]
    ]

viewEntry : BenchmarkEntry -> Html Msg
viewEntry e =
  let
    link =
      if e.link /= "#" then
        a [ href e.link, target "_blank" ] [ text e.name ]
      else
        text e.name
  in
  tr []
    ( [ td [] [ viewState e.state ]
      , td [ ] [ link ]
      ] ++
      case e.time of
        Nothing -> [ td [ class "numeric", class "mean" ] [], td [ class "numeric" ] [], td [ class "numeric", class "deviation" ] [ text "?" ] ]
        Just (time, deviation) ->
          let
              timeInMicros = time // 1000000
              devInNanos  = deviation // 1000
          in
          [ td [ class "numeric", class "mean" ] [ text (String.fromInt timeInMicros ++ "μs") ]
          , td [ class "numeric" ] [ text "±" ]
          , td [ class "numeric", class "deviation" ] [ text (String.fromInt devInNanos ++ "ns") ]
          ]
    )

viewState : State -> Html Msg
viewState s =
  case s of
    Loading -> text "L"
    Ready   -> text "R"
    Error   -> text "E"

runButtonText : Maybe Int -> String
runButtonText mPct =
  case mPct of
    Nothing  -> "Run"
    Just pct -> "Running - " ++ String.fromInt pct ++ "%"

canRun : Model -> Bool
canRun m = m.runPercentage == Nothing && List.all (\e -> e.state /= Loading) m.entries

mapAt : Int -> ( a -> a ) -> List a -> List a
mapAt n f xs =
  case xs of
    [] -> []
    (y :: ys) ->
      case n of
        0 -> f y :: ys
        _ -> y :: mapAt (n-1) f ys

-- ## HELPERS ##
parseState : StateString -> State
parseState state =
  case state of
    "loading" -> Loading
    "ready"   -> Ready
    _         -> Error
