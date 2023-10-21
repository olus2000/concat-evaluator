module Main exposing (..)


import Browser
import Html exposing (Html, button, div, text, br, input, code, textarea, span,
                      table, caption, thead, tr, th, tbody, td, a, label)
import Html.Attributes exposing (class, style, value, placeholder, attribute,
                                 colspan, href, rel, target, type_, checked)
import Html.Events exposing (onClick, onInput, onCheck)
import Dict exposing (Dict)
import Array exposing (Array)
import String
import Time exposing (Posix, now, posixToMillis)
import Task exposing (perform)
import Set exposing (Set)
import File.Select as Select
import File.Download as Download
import File


words : String -> List String
words s =
  case String.trim s of
    "" -> []
    trimmed -> String.words trimmed


lines : String -> List String
lines s =
  case String.trim s of
    "" -> []
    trimmed -> String.lines trimmed


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


-- IMPORT


fileTuple a b c d =
  { ops = a
  , math = b
  , words = c
  , expr = d
  }


parseFile : String -> Model
parseFile s =
  let
      f = splitFile s
      (zero, succ) = parseMath f.math
  in
  { operators = parseOperators f.ops
  , words = parseWords f.words
  , expression = f.expr
  , step = 0
  , result = parseExpression f.expr
  , zero = zero
  , succ = succ
  , stepMult = 1
  , stepMultField = ""
  }


-- splitFile : String -> (String, String, String, String)
splitFile f = case String.split "\n\n" f of
  [] -> fileTuple "" "" "" ""
  [a] -> fileTuple a "" "" ""
  [a, b] -> fileTuple a b "" ""
  a :: b :: c :: d -> fileTuple a b c (String.join "\n\n" d)


parseOperators : String -> List OperatorModel
parseOperators = lines >> List.indexedMap parseOperatorLine


parseOperatorLine : Int -> String -> OperatorModel
parseOperatorLine id s =
  let
      (arity, name, body) = case words s of
        [] -> ("", "", "")
        [a] -> (a, "", "")
        a :: b :: c -> (a, b, String.join " " c)
  in
  { arity = String.toInt arity |> Maybe.withDefault 0
  , name = name
  , body = body
  , result = Ok []
  , id = id
  } |> parseOperator


parseWords : String -> List WordModel
parseWords = lines >> List.indexedMap parseWordLine


parseWordLine : Int -> String -> WordModel
parseWordLine id s =
  let
      (name, body) = case words s of
        [] -> ("", "")
        a :: b -> (a, String.join " " b)
  in
  { name = name
  , body = body
  , result = Ok []
  , id = id
  } |> parseWord


parseMath : String -> (NatModel, NatModel)
parseMath s =
  let
      (zero, succ) = case lines s of
        [] -> ("", "")
        [a] -> (a, "")
        a :: b :: _ -> (a, b)
  in
  (parseMathLine (String.trim zero), parseMathLine (String.trim succ))


parseMathLine : String -> NatModel
parseMathLine s = 
  { body = s
  , result = parseExpression s
  , enabled = s /= ""
  }


-- EXPORT

exportFile : Model -> String
exportFile m = String.join "\n\n"
  [ exportStuff exportOperator m.operators
  , exportStuff exportMath [m.zero, m.succ]
  , exportStuff exportWord m.words
  , m.expression
  ] |> String.trimRight


exportStuff : (a -> Maybe String) -> List a -> String
exportStuff f = List.filterMap f >> String.join "\n"


exportOperator : OperatorModel -> Maybe String
exportOperator op = String.join " "
  [ String.fromInt op.arity
  , if String.length op.name > 0 then op.name else "noname"
  , op.body
  ] |> Just


exportMath : NatModel -> Maybe String
exportMath math =
  if math.enabled then Just math.body else Nothing


exportWord : WordModel -> Maybe String
exportWord word = String.join " " [ word.name, word.body ] |> Just


-- PRESETS


defaultOperators : Model
defaultOperators = parseFile
  """
  1 dup   [ 1 ] [ 1 ]
  1 drop
  2 swap  [ 1 ] [ 2 ]
  1 quote [ [ 1 ] ]
  2 cat   [ 2 1 ]
  1 call  1

  [ drop ]
  [ dup quote cat call ] swap cat

  take ( [A] [B] -- [B[A]] ) swap quote cat
  dip  ( [A] [B] -- B [A] ) take call
  cons ( [A] [B] -- [[A]B] ) swap quote swap cat
  over ( a b -- a b a ) [ dup ] dip swap
  rot  ( a b c -- b c a ) [ swap ] dip swap
  -rot ( a b c -- c a b ) swap [ swap ] dip
  fix  ( ( A ( A -- B ) -- B ) -- ( A -- B ) ) [ dup cons ] swap cat dup cons
  """


cakeK : Model
cakeK = parseFile
  """
  2 cake [ [ 2 ] 1 ] [ 1 [ 2 ] ]
  2 k    1

  [ [ ] k ]
  [ [ dup dip ] dip call ] cons

  drop  [ ] k
  dip   cake k
  cons  cake [ ] k
  quote [ ] cake [ ] k
  swap  [ ] cake [ ] k cake k
  dup   [ ] cake cake k cake k
  call  [ ] cake cake k cake k k
  """


consSap : Model
consSap = parseFile
  """
  2 cons [ [ 2 ] 1 ]
  2 sap  1 2
  """


becc : Model
becc = parseFile
  """
  1 + [ 1 ] [ 1 ]
  2 - 1
  2 > [ [ 2 ] 1 ]
  2 < [ 1 [ 2 ] ]
  """


-- MODEL


type alias Skeleton = List ( Tree Int )
type alias Expression = List ( Tree String )


type alias Operator =
  { arity : Int
  , skeleton : Skeleton
  }


type Nats
  = NoNats
  | JustZero Expression
  | Both Expression Expression
  


type alias Dictionary =
  { operators : Dict String Operator
  , words : Dict String Expression
  , nats : Nats
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


type alias WordModel =
  { name : String
  , body : String
  , result : Result Error Expression
  , id : Int
  }


newWord : Posix -> WordModel
newWord time =
  { name = ""
  , body = ""
  , result = Ok []
  , id = posixToMillis time
  }


type alias NatModel =
  { body : String
  , result : Result Error Expression
  , enabled : Bool
  }


type alias Model =
  { operators : List OperatorModel
  , words : List WordModel
  , expression : String
  , step : Int
  , result : Result Error Expression
  , zero : NatModel
  , succ : NatModel
  , stepMult : Int
  , stepMultField : String
  }


init : () -> ( Model, Cmd a )
init _ = ( defaultOperators, Cmd.none )


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


parseWord : WordModel -> WordModel
parseWord word =
  { word | result = checkName word.name
    |> Maybe.map Err
    |> Maybe.withDefault (parseExpression word.body)
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
    ( "(" :: tail ) ->
      parseComment tail |> Result.andThen parseExpressionTree
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


parseComment : List String -> Result Error (List String)
parseComment tokens =
  case tokens of
    [] -> Err ( Error [ "Unclosed \"(\"" ] )
    ( "(" :: tail ) -> parseComment tail |> Result.andThen parseComment
    ( ")" :: tail ) -> Ok tail
    ( _ :: tail ) -> parseComment tail


-- REDUCTION


reduce : Dictionary -> Int -> Expression -> Expression -> Expression
reduce dictionary limit expr stack =
  if limit <= 0 then (List.reverse stack) ++ expr
  else
    case expr of
      [] -> List.reverse stack
      ( Node op :: rest ) ->
        case Dict.get op dictionary.operators of
          Nothing ->
            case Dict.get op dictionary.words of
              Nothing ->
                case dictionary.nats of
                  NoNats ->
                    reduce dictionary limit rest ( Node op :: stack )
                  JustZero zeroExpr ->
                    case String.toInt op of
                      Just 0 ->
                        reduce dictionary (limit - 1) ( zeroExpr ++ rest ) stack
                      _ -> reduce dictionary limit rest ( Node op :: stack )
                  Both zeroExpr succExpr ->
                    let n = String.toInt op |> Maybe.withDefault -1 in
                      if n < 0 then
                        reduce dictionary limit rest ( Node op :: stack )
                      else
                        reduce dictionary (limit - 1)
                        ( makeNat zeroExpr succExpr n ++ rest ) stack
              Just word ->
                reduce dictionary ( limit - 1 ) ( word ++ rest ) stack
          Just operator ->
            case topQuotes operator.arity stack of
              Nothing -> reduce dictionary limit rest ( Node op :: stack )
              Just arguments ->
                let
                  new_stack = List.drop operator.arity stack
                  new_expr = ( instantiate arguments operator.skeleton ) ++ rest
                in
                  reduce dictionary (limit - 1) new_expr new_stack
      ( head :: tail ) -> reduce dictionary limit tail ( head :: stack )


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


makeTree : Tree a -> Maybe ( List ( List ( Tree a ) ) )
                  -> Maybe ( List ( List ( Tree a ) ) )
makeTree x rest =
  case rest of
    Nothing -> Nothing
    Just l ->
      case x of
        Tree a -> Just ( a :: l )
        Node _ -> Nothing


makeNat : Expression -> Expression -> Int -> Expression
makeNat zero succ n =
  List.repeat n succ |> List.concat |> (++) zero


-- UPDATE


type Msg
  = UpdateOp OperatorModel (OperatorModel -> OperatorModel)
  | NewOp Posix
  | DelOp OperatorModel
  | UpdateWord WordModel (WordModel -> WordModel)
  | NewWord Posix
  | DelWord WordModel
  | Timed (Posix -> Msg)
  | UpdateExpr String
  | Step Int
  | ChangeStepMult String
  | LoadModel Model
  | UpdateZero String
  | ToggleZero Bool
  | UpdateSuccessor String
  | ToggleSuccessor Bool
  | Export
  | Import
  | LoadFile File.File


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
    UpdateWord word transform ->
      ( { model | words = List.map
        ( \x -> if x == word then transform x |> parseWord else x )
        model.words
        }
      , Cmd.none
      )
    NewWord time ->
      ( { model | words = newWord time :: model.words }
      , Cmd.none
      )
    DelWord word ->
      ( { model | words = List.filter ( (/=) word ) model.words }
      , Cmd.none
      )
    Timed makeMsg -> ( model, perform makeMsg now )
    UpdateExpr s ->
      ( { model
        | expression = s
        , result = parseExpression s
        , step = 0
      }, Cmd.none )
    UpdateZero s ->
      let zero = model.zero in
        ( { model | zero = { zero
          | body = s
          , result = parseExpression s
          } }, Cmd.none )
    ToggleZero b ->
      let zero = model.zero
          succ = model.succ
      in
        ( { model
          | zero = { zero | enabled = b }
          , succ = { succ | enabled = succ.enabled && b }
          }, Cmd.none )
    UpdateSuccessor s ->
      let succ = model.succ in
        ( { model | succ = { succ
          | body = s
          , result = parseExpression s
          } }, Cmd.none )
    ToggleSuccessor b ->
      let succ = model.succ
          zero = model.zero
      in
        ( { model
          | succ = { succ | enabled = b }
          , zero = { zero | enabled = zero.enabled || b }
          }, Cmd.none )
    Step m ->
      let n = m * model.stepMult in
      case model.result of
        Err _ -> ( model, Cmd.none )
        Ok expression ->
          if n < 0 then
            ( { model
              | result = reduce
                ( buildDict model )
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
              | result = reduce ( buildDict model ) n expression []
                |> Ok
              , step = model.step + n |> max 0
              }
            , Cmd.none
            )
    ChangeStepMult s ->
      let
          m = { model | stepMultField = s }
      in case String.toFloat s of
        Just n -> ( { m | stepMult = max 1 (floor n) }, Cmd.none )
        Nothing -> ( m, Cmd.none )
    LoadModel m -> ( m, Cmd.none )
    Export ->
      ( model, Download.string "preset.cce" "" (exportFile model) )
    Import -> ( model, Select.file ["text/*"] LoadFile )
    LoadFile f ->
      ( model, File.toString f |> perform (parseFile >> LoadModel) )


buildDict : Model -> Dictionary
buildDict model =
  { operators = buildOpDict model.operators
  , words = buildWordDict model.words
  , nats = buildNats model.zero model.succ
  }


buildNats : NatModel -> NatModel -> Nats
buildNats zero succ =
  if zero.enabled then
    case zero.result of
      Err _ -> NoNats
      Ok zeroExpr ->
        case if succ.enabled then Result.toMaybe succ.result else Nothing of
          Just succExpr -> Both zeroExpr succExpr
          Nothing -> JustZero zeroExpr
  else NoNats


buildOpDict : List OperatorModel -> Dict String Operator
buildOpDict operators =
  List.filterMap
  ( \x -> case x.result of
      Err _ -> Nothing
      Ok skeleton -> Just (x.name, { arity = x.arity, skeleton = skeleton })
  ) operators
  |> Dict.fromList


buildWordDict : List WordModel -> Dict String Expression
buildWordDict wordList =
  List.filterMap
  ( \x -> case x.result of
      Err _ -> Nothing
      Ok expr -> Just (x.name, expr)
  ) wordList
  |> Dict.fromList


-- SETTERS


setArity : b -> { a | arity : b } -> { a | arity : b }
setArity new obj = { obj | arity = new }


setName : b -> { a | name : b } -> { a | name : b }
setName new obj = { obj | name = new }


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
          [ presetButton "Standard operators" (LoadModel defaultOperators)
          , presetButton "Minimal base" (LoadModel cakeK)
          , presetButton "Linear base" (LoadModel consSap)
          , presetButton "Brainfuck Encoded" (LoadModel becc)
          , presetButton "Export" Export
          , presetButton "Import" Import
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
          ( tr [ onClick (Timed NewOp) ]
            [ td [ colspan 7 ]
                 [ text "Add operator" ] ]
          :: List.concatMap operatorRow model.operators 
          )
        ]
      ]
    ]
  , div [ class ukCard ]
    [ div [ class "uk-grid uk-grid-small" ]
      [ div [ class "uk-column uk-width-1-3@m" ]
        ( [ div [ class "uk-grid uk-grid-small"
                , attribute "uk-height-match" "target: > div > *" ]
            [ div [ class "uk-column uk-margin-auto-vertical" ]
              [ label []
                [ input
                  [ class "uk-checkbox"
                  , type_ "checkbox"
                  , checked model.zero.enabled
                  , onCheck ToggleZero
                  ] []
                , text " Zero"
                ]
              ]
            , div [ class "uk-column uk-width-expand" ]
              [ input
                [ class "uk-input uk-width-expand"
                , placeholder "Zero expression"
                , value model.zero.body
                , onInput UpdateZero
                , style "font-family" "monospace"
                --, style "height" "30px"
                ] []
              ]
            ]
          ] ++
          -- error
          ( case model.zero.result of
              Err message ->
                [ div [ class "uk-text-danger" ] ( errorHtml message ) ]
              Ok skeleton -> []
          )
        )
      , div [ class "uk-column uk-width-expand" ]
        ( [ div [ class "uk-grid uk-grid-small"
                , attribute "uk-height-match" "target: > div > *" ]
            [ div [ class "uk-column uk-margin-auto-vertical" ]
              [ label []
                [ input
                  [ class "uk-checkbox"
                  , type_ "checkbox"
                  , checked model.succ.enabled
                  , onCheck ToggleSuccessor
                  ] []
                , text " Successor"
                ]
              ]
            , div [ class "uk-column uk-width-expand" ]
              [ input
                [ class "uk-input uk-width-expand"
                , placeholder "Successor expression"
                , value model.succ.body
                , onInput UpdateSuccessor
                , style "font-family" "monospace"
                --, style "height" "30px"
                ] []
              ]
            ]
          ] ++
          -- error
          ( case model.succ.result of
              Err message ->
                [ div [ class "uk-text-danger" ] ( errorHtml message ) ]
              Ok skeleton -> []
          )
        )
      ]
    ]
  , div [ class ukCard ]
    [ div [ class "uk-overflow-auto" ]
      [ table [ class ( "uk-table uk-table-justify uk-table-small "
                     ++ "uk-table-divider" ) ]
        [ caption [] [ text "Word definitions" ]
        , thead [] [ tr []
          [ th [] [ text "Name" ]
          , th [] []
          , th [] [ text "Substitution" ]
          , th [] []
          ] ]
        , tbody []
          ( tr [ onClick (Timed NewWord) ]
            [ td [ colspan 4 ]
                 [ text "Add word" ] ]
          :: List.concatMap wordRow model.words
          )
        ]
      ]
    ]
  , div [ class ukCard ]
    ( textarea
      [ class "uk-textarea"
      , placeholder "Expression..."
      , style "font-family" "monospace"
      , value model.expression
      , onInput UpdateExpr
      ] []
      :: 
      stepButtons model.stepMultField
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


presetButton : String -> msg -> Html msg
presetButton name message =
  div []
  [ button
    [ class """uk-button uk-button-primary uk-button-small
               uk-width-expand"""
    , onClick message
    ]
    [ text name ]
  ]



wordRow : WordModel -> List (Html Msg)
wordRow word = 
  [ tr []
    -- name
    [ td [ class "uk-table-shrink" ]
      [ input [ class "uk-input uk-form-width-medium"
              , placeholder "name"
              , value word.name
              , onInput ( setName >> UpdateWord word )
              , style "font-family" "monospace"
              , style "height" "30px"
              ] []
      ]
    , td [ class "uk-table-shrink uk-padding-remove-horizontal" ] [ arrow ]
      -- body
    , td [ class "uk-table-expand" ]
      [ input [ class "uk-input uk-width-expand@m"
              , placeholder "Example: swap quote cat call"
              , value word.body
              , onInput ( setBody >> UpdateWord word )
              , style "font-family" "monospace"
              , style "height" "30px"
              ] []
      ]
    , td [ class "uk-table-shrink uk-padding-remove-horizontal" ]
      [ span
        [ attribute "uk-icon" "icon: trash"
        , class "uk-preserve-width"
        , DelWord word |> onClick
        ] []
      ]
    ]
  ] ++
  -- error
  ( case word.result of
      Err message -> [ tr [] [ td [ colspan 7, class "uk-text-danger" ]
                                  ( errorHtml message ) ] ]
      Ok skeleton -> []
  )


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
    , td [ class "uk-table-shrink uk-padding-remove-horizontal" ]
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
      Err message -> [ tr [] [ td [ colspan 7, class "uk-text-danger" ]
                                  ( errorHtml message ) ] ]
      Ok skeleton -> []
  )


arrow : Html Msg
arrow = span [ attribute "uk-icon" "icon: arrow-right; ratio: 2.5"
             , class ( "uk-margin-auto-vertical uk-padding-remove"
                    ++ "uk-preserve-wodth" )
             ] []


stepButtons : String -> Html Msg
stepButtons stepMult =
  div [ class ( "uk-grid uk-grid-collapse uk-margin-remove "
             ++ "uk-child-width-expand@s" ) ]      
      [ stepButton -5 "-5"
      , stepButton -1 "-1"
      , input [ class "uk-input uk-width-expand@s"
              , type_ "number"
              , placeholder "Steps"
              , value stepMult
              , onInput ChangeStepMult
              , style "font-family" "monospace"
              , style "height" "30px"
              ] []
      , stepButton 1 "+1"
      , stepButton 5 "+5"
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
  div [ class "uk-column uk-margin-remove uk-flex-none uk-width-auto@s" ]
  [ button [ class """uk-button uk-button-default uk-width-auto@s
                      uk-width-small@l uk-button-small"""
           , onClick (Step value) ] [ text txt ]
  ]
