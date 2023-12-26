! Copyright (C) 2023 Aleksander Sabak.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators combinators.short-circuit command-line continuations
  debugger hashtables interpolate kernel make math math.parser
  namespaces prettyprint splitting
  strings
  io io.encodings.utf8 io.files io.pathnames
  sequences sequences.extras sequences.repeating ;
IN: concat_eval.runner


ERROR: incorrect-arity operator arity ;

ERROR: unexpected-symbol symbol ;

ERROR: unclosed-quotation ;

ERROR: unclosed-comment ;

ERROR: non-integer-in-skeleton word ;

ERROR: no-operator-name ;


: (print-error) ( error -- )
  [ error. flush ] curry
  [ [ print-error ] with-global ]
  recover ;


: report-error ( error -- )
  error-stream get [ (print-error) ] with-output-stream* ;


TUPLE: concat-eval operators zero succ words expression stack ;

: <concat-eval> ( ops zero succ words expr -- concat-eval )
  V{ } clone concat-eval boa ;

: expression>string ( expression -- string )
  [ dup string? [ expression>string ] unless ] map reverse
  { "[" } { "]" } surround " " join ;

: concat-eval>string ( concat-eval -- string )
  [ stack>> ] [ expression>> reverse ] bi append
  [ dup string? [ expression>string ] unless ] map
  " " join ;


TUPLE: operator arity skeleton ;

: <operator> ( arity skeleton -- operator ) operator boa ;

: <skeleton> ( expression -- skeleton )
  [ dup string?
    [ dup string>number
      [ non-integer-in-skeleton ] unless* nip ]
    [ <skeleton> ] if ] map ;


: skip-comment ( strings -- rest )
  [ dup empty? [ unclosed-comment ] when unclip dup ")" = ]
  [ "(" = [ skip-comment ] when ] until drop ;


: (parse-expression) ( strings -- rest expression )
  V{ } clone swap
  [ dup empty? [ unclosed-quotation ] when unclip dup "]" = ]
  [ { { "" [ ] }
      { "[" [ (parse-expression) pick push ] }
      { "(" [ skip-comment ] }
      { ")" [ ")" unexpected-symbol ] }
      { "--" [ "--" unexpected-symbol ] }
      [ pick push ] } case ] until drop swap reverse ;


: parse-expression ( strings -- expression )
  V{ } clone swap [ dup empty? ]
  [ unclip
    { { "" [ ] }
      { "[" [ (parse-expression) pick push ] }
      { "]" [ "]" unexpected-symbol ] }
      { "(" [ skip-comment ] }
      { ")" [ ")" unexpected-symbol ] }
      { "--" [ "--" unexpected-symbol ] }
      [ pick push ] } case ] until drop reverse ;


: ?parse-operator ( string -- {name,operator}? )
  " " split harvest
  [ ?unclip dup string>number dup [ 0 >= swap and ] when*
    [ swap ?unclip rot incorrect-arity ] unless*
    nip swap ?unclip [ no-operator-name ] unless* -rot
    parse-expression <skeleton> <operator> 2array ]
  [ report-error drop f ] recover ;

: ?parse-word ( string -- {name,expression}? )
  " " split harvest [ ?unclip swap parse-expression 2array ]
  [ report-error drop f ] recover ;


: parse-operators ( strings -- operators )
  [ ?parse-operator ] map sift >hashtable ;


: parse-numbers ( strings -- zero succ )
  ?first2
  [ [ [ " " split parse-expression ]
      [ report-error drop f ] recover ] [ f ] if* ] bi@ ;


: parse-words ( strings -- words )
  [ ?parse-word ] map sift >hashtable ;


: ?parse-expression ( string -- expression )
  " " split harvest [ parse-expression ]
  [ report-error drop V{ } clone ] recover ;


: parse-concat-eval ( string -- concat-eval )
  "\n" split { "" } split1 [ parse-operators ] dip
  { "" } split1 [ parse-numbers ] dip
  { "" } split1 [ parse-words ] dip
  " " join ?parse-expression <concat-eval> ;


SYMBOL: args


: fill-skeleton ( skeleton -- expression )
  [ [ dup number? [ 1 - args get nth % ]
    [ fill-skeleton , ] if ] each ] V{ } make ;


: run-operator ( concat-eval name operator -- concat-eval )
  swapd over stack>> length over arity>> >=
  [ over stack>> over arity>> tail* dup [ string? not ] all?
    [ reverse args set
      [ arity>> over stack>> swap head* >>stack ] keep
      skeleton>> fill-skeleton over expression>> push-all nip ]
    [ 2drop tuck stack>> push ] if ]
  [ drop tuck stack>> push ] if ;


: step ( concat-eval -- concat-eval )
  dup expression>>
  [ pop
    { { [ dup string? not ] [ over stack>> push step ] }
      { [ over operators>> dupd at ]
        [ over operators>> dupd at run-operator ] }
      { [ over words>> dupd at ]
        [ over words>> at over expression>> push-all ] }
      { [ { [ dup string>number dup [ 0 >= and ] when* ]
            [ over zero>> ]
            [ over succ>> ] } 0&& ]
        [ string>number [ dup succ>> ] dip repeat
          over expression>> push-all
          dup zero>> over expression>> push-all ] }
      { [ { [ dup "0" = ] [ over zero>> ] } 0&& ]
        [ drop dup zero>> over expression>> push-all ] }
      [ over stack>> push ] } cond ] unless-empty ;


: run-infinite ( concat-eval -- concat-eval )
  [ dup expression>> empty? ] [ step ] until ;


: run-steps ( concat-eval steps -- concat-eval )
  [ step ] times ;


: run-file ( steps? file -- )
  utf8 [ read-contents ] with-file-reader parse-concat-eval
  swap [ run-steps ] [ run-infinite ] if*
  concat-eval>string write ;


: argv[0] ( -- string )
  (command-line) command-line get length head*
  unclip file-name swap " " join " " glue ;


: usage ( -- )
  argv[0] "Usage: ${0} [limits] script" interpolate>string print
  nl
  "    limits : the second to last argument will be used as a limit on the number"
  print
  "             of steps before terminating evaluation. If it's not a number or if"
  print
  "             only one parameter is provided the evaluation will run until full"
  print
  "             reduction."
  print nl
  "    script : path to a concatenative evaluator file you want to evaluate"
  print ;


: main ( -- )
  command-line get [ usage ]
  [ [ but-last [ f ]
      [ last string>number
        dup [ 0 >= swap and ] when* ] if-empty ]
    [ last ] bi run-file ] if-empty ;


MAIN: main
