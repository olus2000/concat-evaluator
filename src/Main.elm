module Main exposing (..)


import Browser
import Html exposing (Html, button, div, text, br, input, code, textarea, span,
                      table, caption, thead, tr, th, tbody, td, a)
import Html.Attributes exposing (class, style, value, placeholder, attribute,
                                 colspan, href)
import Html.Events exposing (onClick, onInput)
import Dict exposing (Dict)
import Array exposing (Array)
import String exposing (lines)
import Time exposing (Posix, now, posixToMillis)
import Task exposing (perform)
import Set exposing (Set)


words : String -> List String
words s =
  case String.trim s of
    "" -> []
    _ -> String.words s


-- MAIN


main =
  Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = always Sub.none
  }


-- TREE


type Tree a
  = Tree ( List (Tree a) )
  | Node a


treeMap : (a -> b) -> Tree a -> Tree b
treeMap f tree =
  case tree of
    Tree branches ->
      Tree (List.map (treeMap f) branches)

    Node v -> Node (f v)


-- PRESETS


defaultOperators : List OperatorModel
defaultOperators =
  [ { name = "dup",   arity = 1, id = 0
    , result = Ok [ Tree [ Node 1 ], Tree [ Node 1 ] ]
    , body = "[ 1 ] [ 1 ]"
    }
  , { name = "drop",  arity = 1, id = 1
    , result = Ok [ ]
    , body = ""
    }
  , { name = "swap",  arity = 2, id = 2
    , result = Ok [ Tree [ Node 1 ], Tree [ Node 2 ] ]
    , body = "[ 1 ] [ 2 ]"
    }
  , { name = "quote", arity = 1, id = 3
    , result = Ok [ Tree [ Tree [ Node 1 ] ] ]
    , body = "[ [ 1 ] ]"
    }
  , { name = "cat",   arity = 2, id = 4
    , result = Ok [ Tree [ Node 2, Node 1 ] ]
    , body = "[ 2 1 ]"
    }
  , { name = "call",  arity = 1, id = 5
    , result = Ok [ Node 1 ]
    , body = "1"
    }
  ]


cakeK : List OperatorModel
cakeK =
  [ { name = "cake", arity = 2, id = 0
    , result = Ok [ Tree [ Tree [ Node 2 ], Node 1 ]
                  , Tree [ Node 1, Tree [ Node 2 ] ]
                  ]
    , body = "[ [ 2 ] 1 ] [ 1 [ 2 ] ]"
    }
  , { name = "k",    arity = 2, id = 1
    , result = Ok [ Node 1 ]
    , body = "1"
    }
  ]


coupSap : List OperatorModel
coupSap =
  [ { name = "coup", arity = 3, id = 0
    , result = Ok [ Tree [ Tree [ Node 3 ], Node 2 ]
                  , Tree [ Node 1 ], Tree [ Node 1 ] ]
    , body = "[ [ 3 ] 2 ] [ 1 ] [ 1 ]"
    }
  , { name = "sap", arity = 2, id = 1
    , result = Ok [ Node 1, Node 2 ]
    , body = "1 2"
    }
  ]


consSap : List OperatorModel
consSap =
  [ { name = "cons", arity = 2, id = 0
    , result = Ok [ Tree [ Tree [ Node 2 ], Node 1 ] ]
    , body = "[ [ 2 ] 1 ]"
    }
  , { name = "sap", arity = 2, id = 1
    , result = Ok [ Node 1, Node 2 ]
    , body = "1 2"
    }
  ]


-- MODEL


type alias Skeleton = List ( Tree Int )
type alias Expression = List ( Tree String )


type alias Operator =
  { arity : Int
  , skeleton : Skeleton
  }


type alias Dictionary =
  { operators : Dict String Operator
  , words : Dict String Expression
  , zero : Maybe Expression
  , succ : Maybe Expression
  }


type alias OperatorModel =
  { arity : Int
  , name : String
  , body : String
  , result : Result Error Skeleton
  , id : Int
  }

newOperator : Posix -> OperatorModel
newOperator time =
  { arity = 0
  , name = ""
  , body = ""
  , result = Ok []
  , id = posixToMillis time
  }


type alias Model =
  { dictionary : Dictionary -- unuseddd
  , operators : List OperatorModel
  , expression : String
  , step : Int
  , result : Result Error Expression
  }


init : () -> ( Model, Cmd a )
init _ =
  ( { dictionary =
      { operators = Dict.empty
      , words = Dict.empty
      , zero = Nothing
      , succ = Nothing
      }
    , operators = defaultOperators
    , expression = ""
    , step = 0
    , result = Ok []
    }
  , Cmd.none
  )


-- PARSERS


type Error = Error ( List String )
errorHtml : Error -> List ( Html Msg )
errorHtml ( Error message ) =
  List.map text message
  |> (::) ( text "Error:" )
  |> List.intersperse ( br [] [] )


whitespace : Set Char
whitespace = Set.fromList ['\n', '\t', ' ', '\r']

keywords : Set String
keywords = Set.fromList ["[", "]", "(", ")", "--"]


parseOperator : OperatorModel -> OperatorModel
parseOperator operator =
  { operator | result = checkName operator.name
    |> Maybe.map Err
    |> Maybe.withDefault (parseSkeleton operator.arity operator.body)
  }


checkName : String -> Maybe Error
checkName name =
  if name == "" then
    Just (Error ["Operator name is empty."])
  else if name |> String.toList |> Set.fromList |> Set.intersect whitespace
    |> Set.isEmpty |> not then
      Just (Error ["Whitespace is not allowed in operator names."])
  else if Set.member name keywords then
    Just (Error ["Name \"" ++ name ++ "\" is a restricted keyword."])
  else Nothing


parseSkeleton : Int -> String -> Result Error Skeleton
parseSkeleton arity body =
  case parseSkeletonTree arity (words body) of
    Err e -> Err e
    Ok ( skeleton, [] ) -> Ok skeleton
    _ -> Err ( Error [ "Unexpected \"]\"" ] )


parseSkeletonTree : Int -> List String -> Result Error ( Skeleton, List String )
parseSkeletonTree arity tokens =
  case tokens of
    [] -> Ok ( [], [] )
    ( "]" :: _ ) -> Ok ( [], tokens ) 
    ( "[" :: tail ) ->
      case parseSkeletonTree arity tail of
        Err e -> Err e
        Ok ( skeleton, ( "]" :: rest ) ) ->
          case parseSkeletonTree arity rest of
            Err e -> Err e
            Ok ( l, unparsed ) -> Ok ( Tree skeleton :: l, unparsed )
        Ok ( _, _ ) -> Err ( Error [ "Unclosed \"[\"" ] )
    ( s :: tail ) ->
      case parseSkeletonNode arity s of
        Err e -> Err e
        Ok n ->
          case parseSkeletonTree arity tail of
            Err e -> Err e
            Ok ( l, unparsed ) -> Ok ( Node n :: l, unparsed )


parseSkeletonNode : Int -> String -> Result Error Int
parseSkeletonNode arity token =
  case String.toInt token of
    Just n ->
      if n > 0 && n <= arity then Ok n
      else Err ( Error [ "Only numbers refering to operator arguments can be used in operator body."
                              , "\"" ++ token ++ "\" doesn\"t refer to an operator argument."
                              ] )
    Nothing -> Err ( Error [ "Only numbers refering to operator arguments can be used in operator body."
                                  , "\"" ++ token ++ "\" is not a number."
                                  ] )


parseExpression : String -> Result Error Expression
parseExpression str =
  case parseExpressionTree (words str) of
    Err e -> Err e
    Ok ( expr, [] ) -> Ok expr
    _ -> Err ( Error [ "Unexpected \"]\"" ] )


parseExpressionTree : List String -> Result Error ( Expression, List String )
parseExpressionTree tokens =
  case tokens of
    [] -> Ok ( [], [] )
    ( "]" :: _ ) -> Ok ( [], tokens ) 
    ( "[" :: tail ) ->
      case parseExpressionTree tail of
        Err e -> Err e
        Ok ( expression, ( "]" :: rest ) ) ->
          case parseExpressionTree rest of
            Err e -> Err e
            Ok ( l, unparsed ) -> Ok ( Tree expression :: l, unparsed )
        Ok ( _, _ ) -> Err ( Error [ "Unclosed \"[\"" ] )
    ( s :: tail ) ->
      case parseExpressionNode s of
        Err e -> Err e
        Ok n ->
          case parseExpressionTree tail of
            Err e -> Err e
            Ok ( l, unparsed ) -> Ok ( Node n :: l, unparsed )


parseExpressionNode : String -> Result Error String
parseExpressionNode token =
  if Set.member token keywords then
    Err ( Error [ "Word \"" ++ token ++ "\" is a restricted keyword." ] )
  else Ok token


-- REDUCTION


reduce : Dict String Operator -> Int -> Expression -> Expression -> Expression
reduce operators limit expr stack =
  if limit <= 0 then (List.reverse stack) ++ expr
  else
    case expr of
      [] -> List.reverse stack
      ( Node op :: rest ) ->
        case Dict.get op operators of
          Nothing -> reduce operators limit rest ( Node op :: stack )
          Just operator ->
            case topQuotes operator.arity stack of
              Nothing -> reduce operators limit rest ( Node op :: stack )
              Just arguments ->
                let
                  new_stack = List.drop operator.arity stack
                  new_expr = ( instantiate arguments operator.skeleton ) ++ rest
                in
                  reduce operators (limit - 1) new_expr new_stack
      ( head :: tail ) -> reduce operators limit tail ( head :: stack )


instantiate : Array Expression -> Skeleton -> Expression
instantiate arguments skeleton =
  List.map
    ( instantiateElement arguments )
    skeleton
  |> List.foldr (++) []


instantiateElement : Array Expression -> Tree Int -> Expression
instantiateElement arguments element =
  case element of
    Tree l -> [ Tree ( instantiate arguments l ) ]
    Node n ->
      case Array.get (n - 1) arguments of
        Just expr -> expr
        Nothing -> []


topQuotes : Int -> List ( Tree a ) -> Maybe ( Array ( List ( Tree a ) ) )
topQuotes arity stack =
  if List.length stack < arity then Nothing else
    let tos = List.take arity stack in
      case List.foldr makeTree ( Just [] ) tos of
        Just l -> Just ( Array.fromList l )
        Nothing -> Nothing


makeTree : Tree a -> Maybe ( List ( List ( Tree a ) ) ) -> Maybe ( List ( List ( Tree a ) ) )
makeTree x rest =
  case rest of
    Nothing -> Nothing
    Just l ->
      case x of
        Tree a -> Just ( a :: l )
        Node _ -> Nothing


-- UPDATE


type Msg
  = UpdateOp OperatorModel (OperatorModel -> OperatorModel)
  | NewOp Posix
  | DelOp OperatorModel
  | Timed (Posix -> Msg)
  | UpdateExpr String
  | Step Int
  | Preset (List OperatorModel)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateOp operator transform ->
      ( { model | operators = List.map
        ( \x -> if x == operator then transform x |> parseOperator else x )
        model.operators
        }
      , Cmd.none
      )
    NewOp time ->
      ( { model | operators = newOperator time :: model.operators }
      , Cmd.none
      )
    DelOp operator ->
      ( { model | operators = List.filter ( (/=) operator ) model.operators }
      , Cmd.none
      )
    Timed makeMsg -> ( model, perform makeMsg now )
    UpdateExpr s ->
      ( { model
        | expression = s
        , result = parseExpression s
        , step = 0
      }, Cmd.none )
    Step n ->
      case model.result of
        Err _ -> ( model, Cmd.none )
        Ok expression ->
          if n < 0 then
            ( { model
              | result = reduce
                ( buildOpDict model.operators )
                ( model.step + n |> max 0 )
                ( parseExpression model.expression |> Result.withDefault [] )
                []
                |> Ok
              , step = model.step + n |> max 0
              }
            , Cmd.none
            )
          else
            ( { model
              | result = reduce ( buildOpDict model.operators ) n expression []
                |> Ok
              , step = model.step + n |> max 0
              }
            , Cmd.none
            )
    Preset operators ->
      ( { model | operators = operators }, Cmd.none )



buildOpDict : List OperatorModel -> Dict String Operator
buildOpDict operators =
  List.filterMap
  ( \x -> case x.result of
      Err _ -> Nothing
      Ok skeleton -> Just (x.name, { arity = x.arity, skeleton = skeleton })
  ) operators
  |> Dict.fromList


-- SETTERS


setArity : b -> { a | arity : b } -> { a | arity : b }
setArity new obj = { obj | arity = new }


setName : b -> { a | name : b } -> { a | name : b }
setName new obj = { obj | name = new }


setComment : b -> { a | comment : b } -> { a | comment : b }
setComment new obj = { obj | comment = new }


setBody : b -> { a | body : b } -> { a | body : b }
setBody new obj = { obj | body = new }


-- RENDERERS

skeletonToString : Skeleton -> String
skeletonToString = treeToString String.fromInt


expressionToString : Expression -> String
expressionToString = treeToString identity


treeToString : ( a -> String ) -> List ( Tree a ) -> String
treeToString toString tree = 
  List.map
    ( \x -> case x of
        Node n -> [ toString n ]
        Tree l -> [ "[", treeToString toString l, "]" ]
    ) tree
  |> List.foldr (++) []
  |> String.join " "


opToString : String -> Operator -> String
opToString name operator =
  [ operatorArguments operator.arity
  , name
  , "--"
  , skeletonToString operator.skeleton
  ] |> String.join " "


operatorArguments : Int -> String
operatorArguments arity =
  List.range 1 arity
  |> List.reverse
  |> List.map (\x -> "[ " ++ String.fromInt x ++ " ]")
  |> String.join " "


-- VIEW


ukCard : String
ukCard = "uk-margin uk-card uk-card-body uk-card-default uk-width-1-1"


view : Model -> Html Msg
view model =
  div [] 
  [ div [ class """uk-grid-small uk-child-width-1-6@m uk-child-width-1-3@s
                   uk-child-width-1-1 uk-grid"""
        , attribute "uk-grid" ""
        , attribute "uk-height-match" "target: > div > *"
        ]
          [ div []
            [ button
              [ class """uk-button uk-button-primary uk-button-small
                         uk-width-expand"""
              , onClick (Timed NewOp)
              ]
              [ text "New operator" ] ]
          , div []
            [ button
              [ class """uk-button uk-button-primary uk-button-small
                         uk-width-expand"""
              , onClick (Preset defaultOperators)
              ]
              [ text "Standard operators" ] ]
          , div []
            [ button
              [ class """uk-button uk-button-primary uk-button-small
                         uk-width-expand"""
              , onClick (Preset cakeK)
              ]
              [ text "Minmal base" ] ]
          , div []
            [ button
              [ class """uk-button uk-button-primary uk-button-small
                         uk-width-expand"""
              , onClick (Preset coupSap)
              ]
              [ text "Conservative base" ] ]
          , div []
            [ button
              [ class """uk-button uk-button-primary uk-button-small
                         uk-width-expand"""
              , onClick (Preset consSap)
              ]
              [ text "Linear base" ] ]
          , div []
            [ a
              [ href
              "https://github.com/olus2000/concat-evaluator/blob/main/README.rst"
              ]
              [ button [ class """uk-button uk-button-primary uk-button-small
                               uk-width-expand uk-height-1-1"""
              ]
              [ text "README" ] ] ]
          ]
  , div [ class ukCard ]
    [ div [ class "uk-overflow-auto" ]
      [ table [ class ( "uk-table uk-table-justify uk-table-small "
                     ++ "uk-table-divider" ) ]
        [ caption [] [ text "Operator definitions" ]
        , thead [] [ tr []
          [ th [ colspan 3 ] [ text "Arguments" ]
          , th [] [ text "Name" ]
          , th [] []
          , th [] [ text "Substitution" ]
          , th [] []
          ] ]
        , tbody []
          ( List.concatMap operatorRow model.operators  )
        ]
      ]
    ]
  , div [ class ukCard ]
    ( textarea
      [ class "uk-textarea"
      , placeholder "Expression..."
      , style "font-family" "monospace"
      , onInput UpdateExpr
      ] []
      :: 
      stepButtons
      ::
      case model.result of
         Err message -> [ div [ class "uk-text-danger" ]
                              ( errorHtml message ) ]
         Ok expr ->
           [ div [ style "font-family" "monospace", class "uk-text-emphasis" ]
             [ "Step " ++ String.fromInt model.step ++ ": "
               ++ expressionToString expr |> text ] ]
    )
  ]


operatorRow : OperatorModel -> List (Html Msg)
operatorRow operator = 
  [ tr []
    -- arity buttons
    [ td [ class "uk-table-shrink uk-padding-remove-horizontal" ]
        [ button
          [ class "uk-button uk-button-default uk-button-small"
          , operator.arity - 1 |> max 0 |> setArity
            |> UpdateOp operator |> onClick
          ]
          [ span [ attribute "uk-icon" "icon: minus; ratio: .5"
                 , class "uk-preserve-width" ] [] ]
        ]
    , td [ class "uk-table-shrink uk-padding-remove-horizontal" ]
      [ button
        [ class "uk-button uk-button-default uk-button-small"
        , operator.arity + 1 |> setArity |> UpdateOp operator |> onClick
        ]
        [ span [ attribute "uk-icon" "icon: plus; ratio: .5"
               , class "uk-preserve-width" ] [] ]
      ]
    , td [ class "uk-table-shrink uk-padding-remove-horizontal" ]
      [ div [ class "uk-flex uk-margin-auto-vertical" ]
        [ code [ class ( "uk-text-emphasis uk-background-default" )
               , style "font-size" "1.1rem" ]
               [ text ( operatorArguments operator.arity ) ]
        ]
      ]
      -- name
    , td [ class "uk-table-shrink" ]
      [ input [ class "uk-input uk-form-width-medium"
              , placeholder "name"
              , value operator.name
              , onInput ( setName >> UpdateOp operator )
              , style "font-family" "monospace"
              , style "height" "30px"
              ] []
      ]
    , td [ class "uk-table-shrink uk-padding-remove-horizontal" ] [ arrow ]
      -- body
    , td [ class "uk-table-expand" ]
      [ input [ class "uk-input uk-width-expand@m"
              , placeholder "Example: [ [ 2 ] 1 ] [ 1 [ 2 ] ]"
              , value operator.body
              , onInput ( setBody >> UpdateOp operator )
              , style "font-family" "monospace"
              , style "height" "30px"
              ] []
      ]
    , td []
      [ span
        [ attribute "uk-icon" "icon: trash"
        , class "uk-preserve-width"
        , DelOp operator |> onClick
        ] []
      ]
    ]
  ] ++
  -- error
  ( case operator.result of
      Err message -> [ tr [] [ td [ colspan 5, class "uk-text-danger" ]
                                  ( errorHtml message ) ] ]
      Ok skeleton -> []
  )


arrow : Html Msg
arrow = span [ attribute "uk-icon" "icon: arrow-right; ratio: 2.5"
             , class ( "uk-margin-auto-vertical uk-padding-remove"
                    ++ "uk-preserve-wodth" )
             ] []


stepButtons : Html Msg
stepButtons =
  div [ class ( "uk-grid uk-grid-collapse uk-margin-remove "
             ++ "uk-child-width-expand@s" ) ]
      [ stepButton -1000 "-1000"
      , stepButton -100 "-100"
      , stepButton -10 "-10"
      , stepButton -1 "-1"
      , stepButton 1 "+1"
      , stepButton 10 "+10"
      , stepButton 100 "+100"
      , stepButton 1000 "+1000"
      ]
--  [ div [ class "uk-column" ]
--    [ div [ class "uk-grid uk-grid-collapse uk-margin-remove" ]
--      [ stepButton -1000 "-1000"
--      , stepButton -100 "-100"
--      , stepButton -10 "-10"
--      , stepButton -1 "-1"
--      ]
--    ]
--  , div [ class "uk-column" ]
--    [ div [ class "uk-grid uk-grid-collapse uk-margin-remove" ]
--      [ stepButton 1 "+1"
--      , stepButton 10 "+10"
--      , stepButton 100 "+100"
--      , stepButton 1000 "+1000"
--      ]
--    ]
--  ]


stepButton : Int -> String -> Html Msg
stepButton value txt =
  div [ class "uk-column uk-margin-remove" ]
  [ button [ class "uk-button uk-button-default uk-width-expand"
           , onClick (Step value) ] [ text txt ]
  ]
