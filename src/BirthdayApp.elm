module BirthdayApp exposing (..)

import Browser
import Html exposing (Html, div, p, input, text, label, button)
import Html.Attributes as HA
import Html.Events as HE
import List.Extra exposing (cartesianProduct)
import Set
import Svg
import Svg.Attributes as SA
import Svg.Events as SE
import Task
import Time exposing (Posix, Zone, Month(..))
import Tuple exposing (pair)

ff = String.fromFloat
tf = toFloat
fi = String.fromInt
ti = String.toInt

set_at i v l = (List.take i l)++[v]++(List.drop (i+1) l)

main = Browser.document
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Date = (Int, Int, Int)

type alias MagicSquare = List (List Int)

type GridHighlight
    = NoHighlight
    | Row Int
    | Column Int
    | DownDiagonal
    | UpDiagonal
    | DownBrokenDiagonal
    | UpBrokenDiagonal
    | TopLeftQuadrant
    | TopRightQuadrant
    | BottomLeftQuadrant
    | BottomRightQuadrant
    | Finished

type Msg
    = SetDate String
    | SubmitDate
    | GoToShowMagicSquareScreen
    | SetNow Zone Posix
    | NextHighlight
    | MouseOverCell Int Int

type Screen 
    = EnterBirthdayScreen String
    | ShowMagicNumberScreen Date MagicSquare
    | ShowMagicSquareScreen Date MagicSquare GridHighlight

type alias Model =
    { screen : Screen
    , now : Maybe Posix
    }

nocmd model = (model, Cmd.none)

init_date = (14,1,1986)

init_model = 
    --{ screen = ShowMagicSquareScreen (1,1,1) [[1,1,19,0],[20,-1,-2,0],[-2,17,13,3],[2,4,-3,18]] NoHighlight
    { screen = EnterBirthdayScreen ""
    , now = Nothing
    }

init : () -> (Model, Cmd Msg)
init _ = (init_model, Task.perform identity (Task.map2 SetNow Time.here Time.now))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case model.screen of
    EnterBirthdayScreen date_str ->
        case msg of
            SetDate new_date_str -> { model | screen = EnterBirthdayScreen new_date_str } |> nocmd
            SubmitDate -> case validate_date date_str of
                Just d -> 
                    let
                        square = make_square d |> find_square_fiddle fiddle_tricks
                    in
                        { model | screen = ShowMagicNumberScreen d square } |> nocmd

                Nothing -> nocmd model

            SetNow z t -> { model | now = Just t } |> nocmd

            _ -> nocmd model

    ShowMagicNumberScreen date square -> case msg of
        GoToShowMagicSquareScreen -> { model | screen = ShowMagicSquareScreen date square NoHighlight } |> nocmd

        _ -> nocmd model

    ShowMagicSquareScreen ((d,m,year) as date) square highlighting -> case msg of
        NextHighlight -> 
            if highlighting == Finished then
                { model | screen = EnterBirthdayScreen (format_date (d,m,year)) } |> nocmd
            else
                { model | screen = ShowMagicSquareScreen date square (next_highlight highlighting) } |> nocmd

        MouseOverCell x y -> 
            let
                nhighlighting = case highlighting of
                    Row _ -> Row y
                    Column _ -> Column x
                    NoHighlight -> NoHighlight
                    DownDiagonal -> if x+y==3 then UpDiagonal else DownDiagonal
                    UpDiagonal -> if x==y then DownDiagonal else UpDiagonal
                    DownBrokenDiagonal -> if (modBy 4 (y+2))+x == 3 then UpBrokenDiagonal else DownBrokenDiagonal
                    UpBrokenDiagonal -> if (modBy 4 (x+2)) == y then DownBrokenDiagonal else UpBrokenDiagonal
                    Finished -> Finished
                    _ -> 
                        if y<2 then
                            if x<2 then TopLeftQuadrant else TopRightQuadrant
                        else
                            if x<2 then BottomLeftQuadrant else BottomRightQuadrant
            in
                { model | screen = ShowMagicSquareScreen date square nhighlighting } |> nocmd

        _ -> nocmd model

next_highlight highlighting =
    case highlighting of
        NoHighlight -> Row 0
        Row _ -> Column 0
        Column _ -> DownDiagonal
        DownDiagonal -> DownBrokenDiagonal
        UpDiagonal -> DownBrokenDiagonal
        DownBrokenDiagonal -> TopLeftQuadrant
        UpBrokenDiagonal -> TopLeftQuadrant
        TopLeftQuadrant -> Finished
        TopRightQuadrant -> Finished
        BottomLeftQuadrant -> Finished
        BottomRightQuadrant -> Finished
        Finished -> NoHighlight
    {-
    let
        order = 
               (List.map Row (List.range 0 3))
            ++ (List.map Column (List.range 0 3))
            ++ [ DownDiagonal
                , UpDiagonal
                , DownBrokenDiagonal
                , UpBrokenDiagonal
                , TopLeftQuadrant
                , TopRightQuadrant
                , BottomLeftQuadrant
                , BottomRightQuadrant
               ]
        mi = Debug.log "mi" <| List.Extra.elemIndex highlighting order
    in
          Maybe.andThen (\i -> List.Extra.getAt (i+1) order) (List.Extra.elemIndex highlighting order)
       |> Maybe.withDefault (Row 0)
    -}

format_date (d,m,y) = (fi y)++"-"++(fi m)++"-"++(fi d)

month_number m = case m of
    Jan -> 1
    Feb -> 2
    Mar -> 3
    Apr -> 4
    May -> 5
    Jun -> 6
    Jul -> 7
    Aug -> 8
    Sep -> 9
    Oct -> 10
    Nov -> 11
    Dec -> 12

posix_to_date z t = (fi <| Time.toDay z t, fi <| month_number <| Time.toMonth z t, fi <| Time.toYear z t)

validate_date : String -> Maybe Date
validate_date =
       String.split "-" 
    >> List.map String.toInt
    >> (\l -> case l of
        (Just year)::(Just month)::(Just day)::_ -> Just (day, month, year)
        _ -> Nothing
       )


is_valid_day (d,m,y) =
    let
        n = case m of
            9 -> 30
            4 -> 30
            6 -> 30
            11 -> 30
            2 -> if (modBy 4 y) == 0 && ((modBy 100 y) /= 0 || (modBy 400 y) == 0) then 29 else 28
            _ -> 31
    in
        d >= 1 && d <= n

make_square (d,m,year) =
    let
        y = modBy 100 year
        c = year // 100
    in
        [ [d,m,c,y]
        , [c,y,d,m]
        , [y,c,m,d]
        , [m,d,y,c]
        ]

adders =
    [ [ [0,0,0,0]
      , [0,0,-1,1]
      , [0,1,0,-1]
      , [0,-1,1,0]
      ]
    
    , [ [0,0,0,0]
      , [0,0,1,-1]
      , [1,0,-1,0]
      , [-1,0,0,1]
      ]

    , [ [0,0,0,0]
      , [-1,1,0,0]
      , [0,-1,0,1]
      , [1,0,0,-1]
      ]

    , [ [0,0,0,0]
      , [1,-1,0,0]
      , [-1,0,1,0]
      , [0,1,-1,0]
      ]
    ]
{-
a + b = [ 0   0   0   0  
          1   1  -1  -1  
         -1  -1   1   1
         ]
a + c = [-1   1  -1   1  
          0   0   0   0  
          1  -1   1  -1
          ]
a + d = [ 1  -1  -1   1  
         -1   1   1  -1  
          0   0   0   0
          ]
-}

add_squares : (MagicSquare, Int) -> MagicSquare -> MagicSquare
add_squares (b, n) a = List.map2 (List.map2 (\ca -> \cb -> ca + n*cb)) a b

find_square_fiddle : List (List Int) -> MagicSquare -> MagicSquare
find_square_fiddle all_tricks square = 
    let
        step tricks allow_negatives = case tricks of
            ns::rest ->
                let
                    z = Debug.log "consider" (ns,allow_negatives)
                    square2 = Debug.log "result" <| fiddle_square ns square
                in
                    if (not (has_repeats square2)) && (allow_negatives || not (has_negative square2)) then
                        square2
                    else
                        step rest allow_negatives

            [] -> 
                if not allow_negatives then 
                    step all_tricks True
                else
                    fiddle_square [1,2,3,4] square

    in
        step all_tricks False

fiddle_square : List Int -> MagicSquare -> MagicSquare
fiddle_square ns square =
    let
        bs = List.map2 pair adders ns
    in
        List.foldl add_squares square bs

fiddle_tricks = 
    [ [1,2,3,4]
    , [1,2,4,8]
    ]
    ++
    (cartesianProduct (List.repeat 4 (List.range 0 10))) -- You don't need a coefficient bigger than 10 for any date between 1AD and 3000AD. Checked by Python.

fiddle_options : List Int -> List (List Int)
fiddle_options ns = case ns of
    [] -> [[]]
    a::rest -> [ (a+1)::rest, (a-1)::rest ] ++ (List.map ((::) a) (fiddle_options rest))

has_repeats : MagicSquare -> Bool
has_repeats square = 
    let
        all_cells = List.concat square
        unique_top_row = all_cells |> List.take 4 |> Set.fromList |> Set.toList
        added_cells = List.drop 4 all_cells
        cells = unique_top_row ++ added_cells
        unique_cells = Set.fromList cells
    in
        (Set.size unique_cells) /= (List.length cells)

has_negative : MagicSquare -> Bool
has_negative = List.concat >> List.any ((>) 0)

subscriptions model = Sub.none

view model = case model.screen of
    EnterBirthdayScreen date_string -> view_enter_birthday_screen date_string
    ShowMagicNumberScreen date square -> view_magic_number_screen date square
    ShowMagicSquareScreen date square highlighting -> view_magic_square_screen date square highlighting

view_enter_birthday_screen date_str =
    let
        date = validate_date date_str
    in
        { title = "A Birthday Present For You"
        , body = [
            div
                [ HA.id "enter-date" ]
                [ p [] [ text "I'd like to make you a birthday present, if that's alright." ]
                , p [] [ text "If you don't mind me asking, what is your birth date?" ]
                , div [ HA.class "big-emoji", HA.id "emoji-left" ] [ text "🎁" ]
                , div [ HA.class "big-emoji", HA.id "emoji-right" ] [ text "🎁" ]

                , Html.form
                    [ HE.onSubmit SubmitDate
                    ]
                    [ Html.label
                        []
                        [ text "My birthday is"
                        , input
                            [ HE.onInput SetDate
                            , HA.value date_str
                            , HA.type_ "date"
                            ]
                            []
                        ]
                    , button
                        [ HA.type_ "submit" 
                        , HA.disabled (date == Nothing)
                        ]
                        [ text "I am ready for my birthday present" ]
                    ]

                {- Inputs to twiddle the fiddle coefficients
                , div
                    []
                    (List.indexedMap (\i -> \s -> number_input "" (SetFiddle i) (fi s)) fiddles)

                , p [] [text <| Debug.toString <| validate_date (day,month,year)]
                -}
                ]

            , footer
            ]
        }

view_magic_number_screen date square =
    let
        magic_number = square |> List.head |> Maybe.withDefault [] |> List.sum
    in
        { title = "Your Birthday Magic Number"
        , body = [ div
            [ HA.id "magic-number" ]
            [ p [] [ text "Here is a special gift made just for you.", Html.br [] [], text "It's a special number linked to your birthday." ]
            , div [ HA.class "big-emoji", HA.id "emoji-left" ] [ text "💝" ]
            , div [ HA.class "big-emoji", HA.id "emoji-right" ] [ text "💝" ]
            , p [ HA.id "magic-number" ] [ text <| fi <| magic_number ]
            , button
                [ HA.type_ "button"
                , HE.onClick GoToShowMagicSquareScreen
                ]
                [ text "OK..." ]
            ]

          , footer
          ]
        }

padded c = if c<0 then "-"++(padded (-c)) else String.padLeft 2 '0' <| fi c

view_magic_square_screen date square highlighting =
    let
        scale = 20

        magic_number = square |> List.head |> Maybe.withDefault [] |> List.sum

        lines = 
            (List.map (\i -> line [ SA.stroke "black", SA.strokeWidth "1" ] ((i+1)*scale) (0) ((i+1)*scale) (4*scale)) (List.range 0 2))
            ++
            (List.map (\i -> line [ SA.stroke "black", SA.strokeWidth "1" ] (0) ((i+1)*scale) (4*scale) ((i+1)*scale)) (List.range 0 2))

        should_highlight x y = case highlighting of
            NoHighlight -> False
            Finished -> False
            Row i -> y == i
            Column i -> x == i
            DownDiagonal -> x == y
            UpDiagonal -> x+y == 3
            DownBrokenDiagonal -> (modBy 4 (x+2)) == y
            UpBrokenDiagonal -> (modBy 4 (y+2)) + x == 3
            TopLeftQuadrant -> x < 2 && y < 2
            TopRightQuadrant -> x >= 2 && y<2
            BottomLeftQuadrant -> x < 2 && y >= 2
            BottomRightQuadrant -> x >= 2 && y >= 2

        highlighted_cells = 
            List.indexedMap (\y -> List.indexedMap (\x -> \c -> (x,y,c))) square
            |> List.concat
            |> List.filter (\(x,y,c) -> should_highlight x y)
            |> List.map (\(x,y,c) -> c)

        cell x y c =
            Svg.g
                [ classList
                    [ ("cell", True)
                    , ("highlight", should_highlight x y)
                    ]
                , SE.onMouseOver <| MouseOverCell x y
                , HA.attribute "tabindex" "0"
                , HE.onFocus <| MouseOverCell x y
                , HA.attribute "role" "gridcell"
                , HA.attribute "aria-label" <| "Cell in row "++(fi y)++" column "++(fi x)++" containing the number "++(fi c)++(if should_highlight x y then " (highlighted)" else "")
                ]

                [ rect [ SA.class (if should_highlight x y then "highlight" else "") ] (scale*x+1) (scale*y+1) (scale-2) (scale-2)
                , svg_text 
                    [ SA.fontSize (if y==0 then "7" else "5")
                    , SA.fill "black"
                    , SA.textAnchor "middle"
                    , SA.dominantBaseline "central" 
                    , SE.onMouseOver <| MouseOverCell x y
                    ] 
                    (scale*(tf x + 0.5)) (scale*(tf y + 0.5)) (padded c)
                ]

        cells = List.indexedMap (\y -> \row -> List.indexedMap (\x -> \c -> cell x y c) row) square |> List.concat

        bold_magic_number = Html.strong [] [ text <| fi magic_number ]

        explanation = case highlighting of
            NoHighlight -> [text "And here is a special birthday square to go with your special birthday number"]
            Row _ -> [ text "Each row adds up to ", bold_magic_number ]
            Column _ -> [ text "And each column adds up to ", bold_magic_number ]
            DownDiagonal -> [ text "And the diagonals add up to ", bold_magic_number ]
            UpDiagonal -> [ text "And the diagonals add up to ", bold_magic_number ]
            DownBrokenDiagonal -> [ text "And the broken diagonals add up to ", bold_magic_number ]
            UpBrokenDiagonal -> [ text "And the broken diagonals add up to ", bold_magic_number ]
            TopLeftQuadrant -> [ text "And the four quadrants add up to ", bold_magic_number ]
            TopRightQuadrant -> [ text "And the four quadrants add up to ", bold_magic_number ]
            BottomLeftQuadrant -> [ text "And the four quadrants add up to ", bold_magic_number ]
            BottomRightQuadrant -> [ text "And the four quadrants add up to ", bold_magic_number ]
            Finished -> [text "Isn't that nice?" ]

        equation = 
            if highlighting == NoHighlight || highlighting == Finished then 
                [] 
            else 
                [text ((List.foldl (\n -> \p -> if p == "" then (fi n) else if n<0 then p ++ " - "++ (fi <| abs n) else p ++ " + " ++ (fi n)) "" highlighted_cells )++ " = "), Html.strong [] [ text (fi magic_number) ]]

        button_text = case highlighting of
            NoHighlight -> "So?"
            Row _ -> "Nice!"
            Column _ -> "Golly!"
            DownDiagonal -> "Nifty!"
            UpDiagonal -> "Nifty!"
            DownBrokenDiagonal -> "No way!"
            UpBrokenDiagonal -> "No way!"
            TopLeftQuadrant -> "Thanks!"
            TopRightQuadrant -> "Thanks!"
            BottomLeftQuadrant -> "Thanks!"
            BottomRightQuadrant -> "Thanks!"
            Finished -> "Actually, I got my birthday wrong…"

        left_emoji = case highlighting of
            NoHighlight -> "🥳"
            Row _ -> "😯"
            Column _ -> "😮"
            DownDiagonal -> "😲"
            UpDiagonal -> "😲"
            DownBrokenDiagonal ->  "😳"
            UpBrokenDiagonal ->  "😳"
            TopLeftQuadrant -> "🤯"
            TopRightQuadrant -> "🤯"
            BottomLeftQuadrant -> "🤯"
            BottomRightQuadrant -> "🤯"
            Finished -> "🥰"

        right_emoji = left_emoji

        margin = (-scale//10)
        size = (4*scale+2*scale//10)
    in
        { title = "Your Birthday Magic Square"
        , body = [
            div
                [ HA.id "view-square" ]
                [ p [ HA.id "explanation" ] explanation
                , div [ HA.class "big-emoji", HA.id "emoji-left" ] [ text left_emoji ]
                , div [ HA.class "big-emoji", HA.id "emoji-right" ] [ text right_emoji ]
                , Svg.svg
                    [ SA.viewBox <| (fi margin)++" "++(fi margin)++" "++(fi size)++" "++(fi size)
                    , HA.attribute "aria-label" "A 4×4 grid of numbers"
                    ]

                    [ rect [ SA.id "grid-bg" ] margin margin size size
                    , Svg.g [ SA.id "cells" ] cells
                    ]
                , p [ HA.id "equation" ] equation
                , button
                    [ HA.type_ "button"
                    , HE.onClick NextHighlight
                    ]
                    [ text button_text ]
                ]

            , footer
            ]
        }

line attrs x1 y1 x2 y2 = 
    Svg.line 
        ([ SA.x1 <| fi <| x1
        , SA.y1 <| fi <| y1
        , SA.x2 <| fi <| x2
        , SA.y2 <| fi <| y2
        ]++attrs)
        []

rect attrs x y width height =
    Svg.rect
        ([ SA.x <| fi <| x
         , SA.y <| fi <| y
         , SA.width <| fi <| width
         , SA.height <| fi <| height
         ]++attrs
        )
        []

frect attrs x y width height =
    Svg.rect
        ([ SA.x <| ff <| x
         , SA.y <| ff <| y
         , SA.width <| ff <| width
         , SA.height <| ff <| height
         ]++attrs
        )
        []

svg_text attrs x y content = Svg.text_ (attrs++[ SA.x <| ff <| x, SA.y <| ff <| y ]) [ Svg.text content ]

classList : List (String,Bool) -> Svg.Attribute Msg
classList = List.filterMap (\(c,b) -> if b then Just c else Nothing) >> String.join " " >> SA.class


footer =
    Html.footer
        []
        [ text "Made by ", Html.a [ HA.href "https://somethingorotherwhatever.com" ] [ text "clp" ] ]
