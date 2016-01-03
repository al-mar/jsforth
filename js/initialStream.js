var initialStream = ("" + function dataStore() {
/*
1 word Initializing...1 type






1 word   *********************
         ****  Word "bls" ****
         *********************     1 drop
here                               1 word   Set HERE for settnig new LATEST   1 drop

1 drop  1 word bls1 ,              1 word   Name = "bls"                      1 drop
2 drop  0 ,                        1 word   Immediate = 0                     1 drop
3 drop  current @ @ ,              1 word   Prev = LATEST                     1 drop
4 drop  1 word lit1 find drop ,    1 word   LIT                               1 drop
5 drop  1 word (^\s+)|(\s)1 ,      1 word   (^\s+)|(\s)                       1 drop
6 drop  1 word exit1 find drop ,   1 word   EXIT                              1 drop

current @ !                        1 word   Set New LATEST                    1 drop






1 word   *********************
         ****   Word "("  ****
         *********************    1 drop
here                    bls word \| dup word   Add comment separator  | drop

1 drop  bls word ( ,                dup word   Name="("               | drop
2 drop  1 ,                         dup word   Immediate = 1          | drop
3 drop  current @ @ ,               dup word   Prev = LATEST          | drop
4 drop  bls word lit find drop ,    dup word   LIT                    | drop
5 drop  bls word \) ,               dup word   "\)"                   | drop
6 drop  bls word word find drop ,   dup word   WORD                   | drop
7 drop  bls word drop find drop ,   dup word   DROP                   | drop
8 drop  bls word exit find drop ,   dup word   EXIT                   | drop
                                        word   Drop comment separator | drop
current @ !







(        *********************
         ****   Word "["  ****
         *********************     )
here
1 drop  bls word [ ,               ( Name = "[" )
2 drop  1 ,                        ( Immediate = 1 )
3 drop  current @ @ ,              ( Prev = LATEST )
4 drop  bls word lit find drop ,   ( LIT )
5 drop  0 ,                        ( 0 )
6 drop  bls word state find drop , ( STATE )
7 drop  bls word ! find drop ,     ( ! )
8 drop  bls word exit find drop ,  ( EXIT )
current @ !








(        ***************************
         ****   Word "CREATE#"  ****
         ***************************     )
here

bls word create# , ( Name = "CREATE#" )
0 ,                ( Immediate = 0 )
current @ @ ,      ( Prev = #LATEST @ )
1 state !          ( Enter compilation mode )
  here             ( HERE )

  bls word ,       ( BLS WORD -> word "CREATE#" will read a word from the stream and put it to the vocabulary )
  0 ,              ( Immedate = 0 -> "CREATE#" will set Immediate = 0 for the new word )
  current @ @ ,    ( Prev = LATEST -> voc)

  current @ !
  exit
[                  ( Exit compilation mode )

current @ !







create# ]    ( Word "]" )
1 state !    ( Enter compilation mode )
  1 state !  ( The word "[" will set "STATE" to 1 )
  exit       ( Exit from "]" )
[            ( Exit compilation mode )






create# flags# ( ---> flagsAddr, flagsValue )
]
  current @ @ 1 + dup @ ( LATEST + 1, [Latest + 1] )
  exit
[

create# sflag ( flag ---> )
]
  flags# rot or swap ! ( set a new value to flags )
  exit
[

create# rflag ( flag ---> )
]
  invert
  flags# rot and swap ! ( set a new value to flags )
  exit
[

create# smudge ] 2 sflag exit [

create# unsmudge ] 2 rflag exit [


create# :              ( Word ":" )
]
  create#              ( Word ":" will create new words )
  smudge               ( Set the "smudge" flag for the new word )
  ]                    ( Enter compilation mode so that user can type the body )
  exit
[



: ;                          ( Word ";" )
  lit exit ,                 ( Add the address of exit to the compiled word )
  unsmudge                   ( Set the new word visible )
  [ bls word [ find drop , ] ( Stop compilation )
  exit
[ unsmudge
1 sflag                      ( Set Immediate flag )


: latest current @ @ ;




( Address transformations )

: nfa>ffa ( name --> flags ) 1 + ;
: nfa>lfa ( name --> link  ) 2 + ;
: nfa>cfa ( name --> code  ) 3 + ;

: cfa>nfa ( code --> name  ) 3 - ;
: cfa>ffa ( code --> flags ) 2 - ;
: cfa>lfa ( code --> link  ) 1 - ;

( Address transformation synonyms )
: name> nfa>cfa ;
: >name cfa>nfa ;
: name>c nfa>cfa ;
: name>f nfa>ffa ;
: name>l nfa>lfa ;

: >body ( code -> data ) 1 + ;




: sflag latest name>f dup @ rot or swap ! ;
: rflag latest name>f dup @ rot invert and swap ! ;

: immediate 1 sflag ;

: \ lit [ bls word [\n\r]+ , ] word drop ; immediate     \ Backslash comment
: \eof lit [ bls word $ , ] word drop ; immediate        \ Comment till the end of file



( Branching helpers )
: >mark here 0 , ;       \ marks the place where are we going to jump forward from (forward jumps)
: >resolve here swap ! ; \ marks the place where are we going to jump forward to   (forward jumps)
: <mark here ;           \ marks the place where are we going to return to         (backward jumps)
: <resolve , ;           \ marks the place where are we going to return from       (backward jumps)

: ' ( ---> A )
  bls word find
  not ?branch [ >mark ]
  lit [ bls word " word Word '" , ] swap +
  lit [ bls word " word ' is not found." , ] + .abort
  [ >resolve ]
;

: [compile] ' , ; immediate
: compile r> dup 1 + >r @ , ;
: compile, , ;

: state?compile                 \ compiles the next address if its compilation mode or goes to it otherwise
  state @ ?branch [ >mark ]
    r> dup 1 + >r @ ,
  [ >resolve ]
;

: ['] ( ---> ) ' compile lit , ; immediate

: (;create) ] r> exit ;   \ The default body for the words created by "CREATE" command

: create create# compile (;create) ;

: variable create , ;

: (;code) ( ---> )        \ Replaces the default body of the words created by "CREATE" command to the code part after "DOES>"
  r> latest name> !
;

: does>
  compile (;code)
  compile r>
; immediate

: constant create , does> @ ;

( Article flags constants )
1 constant &immediate
2 constant &smudge
4 constant &voc
8 constant &value

bls word " word  " constant bl  \ Constant "BL"

bls word " word 
" constant cr                   \ Constant "CR"

: . type bl type ;

1 constant cell

: cells cell * ;

: cell+ cell + ;

: execute lit [ >mark ] ! [ >resolve 0 , ] ;

: (;abort") r> dup cell + >r swap ?branch [ >mark ] @ .abort [ >resolve ] drop ;  \ Shows an error message if there's "true" on the top of the stack

: ?comp state @ not (;abort") [ bls word \" word Interpreting a compile-only word" , ] ;

: abort" ?comp compile (;abort") lit [ bls word " , ] word , ; immediate

: vocabulary
  create
  &voc sflag
  current @ ,
  latest , does> cell + context !
;

: definitions context @ current ! ;

: ?dest - abort" Expected dest" ; \ Checks a destination flag. The checked flag and the test value sould be on the top of the stack

: branch r> @ >r ;

1 constant <if>

: if ?comp compile ?branch >mark <if> ; immediate
: then ?comp <if> ?dest >resolve ; immediate
: else ?comp <if> ?dest compile branch >mark swap >resolve <if> ; immediate
: endif [compile] then ; immediate \ Synonym for "THEN"

: literal
  state @
  if compile lit , then
; immediate

: " lit [ bls word " , ] word [compile] literal ; immediate
: "( " \)" word [compile] literal ; immediate      \ Gets a string from the stream that ends with ")" (like " word)
: ," [compile] " state?compile , ; immediate       \ Compiles the string after this word (comp/run)
: ,( [compile] "( state?compile , ; immediate      \ Compiles the string after this word (comp/run)
: ." [compile] " state?compile type ; immediate       \ Prints the string after this word   (comp/run)
: .( [compile] "( state?compile type ; immediate      \ Prints the string after this word   (comp/run)

: (;flag") r> dup cell + >r rot cfa>ffa @ rot and not if @ .abort else drop then ;
: ?flag" ?comp compile (;flag") lit [ bls word " , ] word , ; immediate

: definitions<                 \ Changes "CURRENT" variable but leaves "CONTEXT" untouched. (ex. DEFINITIONS< myVocabulary )
  ' dup
  &voc ?flag" Not a word list"
  context @ swap execute
  definitions
  context !
;

: value create , &value sflag does> @ ;
: to ' dup &value ?flag" Not a value" cell + ! ;

\ ###########################################################################################################
\ #  Loops implementations is a slightly modified and extended code from the Baranov's and                  #
\ #  Nozdrunov's book "FORTH and its implementations" (http://books.google.ru/books?id=99xGAAAACAAJ&hl=ru)  #
\ ###########################################################################################################

2 constant <begin>  \ begin mark

: begin ?comp <mark <begin> ; immediate
: until ?comp <begin> ?dest compile ?branch <resolve ; immediate
: again ?comp <begin> ?dest compile  branch <resolve ; immediate
: while <begin> ?comp <begin> ?dest [compile] if ; immediate
: repeat >r >r [compile] again r> r> [compile] then ; immediate



vocabulary private


definitions< private   \ compile to "PRIVATE"

: (do) ( a2: limit, a1: first )
  r> ( a2, a1, r:return)
  dup @ >r ( for leave)
  rot >r ( limit )
  swap >r ( first )
  1 + >r ( add 1 to addr)
;

: (loop) ( ---> )
  r> r> r@ ( r:return, I:current, a2:limit)
  - 1 + dup not ( r, I-a2+1, f:shouldReturn )
  if ( finalize )
    drop
    r> r> drop drop
    1 + ( addr++ )
  else ( continue )
    r@ + >r ( I++ )
    @ ( body addr )
  then
  >r ( goto addr )
;

: (+loop) ( ---> )
  r> swap r> r@ ( r:return, s: step, I:current, a2:limit)
  - over + dup -1 > ( r, s, I-a2+1, f:shouldReturn )
  if ( finalize )
    drop drop
    r> r> drop drop
    1 + ( addr++ )
  else ( continue )
    r@ + >r ( I++ )
    swap
    @ ( body addr )
  then
  >r ( goto addr )
;

definitions< forth private     \ Compile to "FORTH", look from "PRIVATE"

3 constant <do>

: do ?comp compile (do) >mark <mark <do> ; immediate
: loop ?comp <do> ?dest compile (loop) <resolve >resolve ; immediate
: +loop <do> ?dest compile (+loop) <resolve >resolve ; immediate
: I r> r@ swap >r ;
: J r> r> r> r> r@ swap >r swap >r swap >r swap >r ;
: leave r> drop r> drop r> drop ;
: bleave r> drop ;

forth                \ Look from "FORTH"

\ ###################################
\ #   End of loops implementation   #
\ ###################################

: 0= 0 = ;
: 0<> ;
: 0> 0 > ;
: 0< 0 < ;
: 1- 1 - ;
: 1+ 1 + ;
: negate 0 swap - ;

: depth s0 @ sp@ - 1 - ;
: ?dup dup if dup then ;
: 2dup over over ;
: 2drop drop drop ;

: recurse latest name> , ; immediate
: space bl type ;
: spaces begin dup 0 > while space 1 - repeat drop ;
: 2>r r> rot >r swap >r >r ;
: 2r> r> r> r> rot >r swap ;
: 2r@ r> r> r> 2dup >r >r rot >r swap ;
: c@ @ ;
: c! @ ;

\ lshift, max, min, invert, rshift, 

: hide ' cfa>ffa dup @ &smudge or swap ! ; \ Hides words from the list

hide create#
hide (;create)
hide (;abort")
hide (;code)
hide private

\ ##############################
\ #       Debugging words      #
\ ##############################

: .s sp@ 1 - s0 @ 1 - begin over over < while dup @ . 1 - repeat drop drop ; \ Prints the stack
: .r rp@ 1 + r0 @ 1 - begin over over < while dup @ . 1 - repeat drop drop ; \ Prints the return stack for the caller of this word
: dumpLatest     \ 
  latest
  begin
    dup . lit [ " - " , ] . dup @ . lit [ " - " , ] . dup @ 3 - @ . cr .
    1 +
    dup here =
  until
;


: load"
  ." Loading..."
  null [compile] " " get" http
  200 = not abort" Can't load script"
  ." ok" cr type
  drop cr + >str drop
;

\ #########################################
\ #   Finish and show a welcome message   #
\ #########################################

page

" #####################################
##                                 ##
##         JS FORTH v1.0           ##
##   (c) Alexander Martyanov 2015  ##
##                                 ##
#####################################

To see all available words type 'words' in the input field and press 'Send' button or Ctrl+Enter

" html


*/
});

initialStream = initialStream.substr(0, initialStream.lastIndexOf("*/")).substr(initialStream.indexOf("/*") + 2).trim() + "\n";
