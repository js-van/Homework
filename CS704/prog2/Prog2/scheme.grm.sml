functor SchemeLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Scheme_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure SchemeAST = SchAST;

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\004\000\000\000\
\\001\000\002\000\007\000\000\000\
\\001\000\002\000\011\000\000\000\
\\001\000\002\000\025\000\008\000\024\000\009\000\023\000\010\000\022\000\
\\011\000\021\000\012\000\020\000\000\000\
\\001\000\002\000\051\000\005\000\050\000\006\000\049\000\007\000\039\000\
\\008\000\048\000\009\000\023\000\010\000\022\000\011\000\021\000\
\\012\000\020\000\014\000\038\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\019\000\033\000\020\000\032\000\
\\021\000\031\000\022\000\030\000\023\000\029\000\000\000\
\\001\000\002\000\051\000\005\000\050\000\006\000\049\000\008\000\048\000\
\\009\000\023\000\010\000\022\000\011\000\021\000\012\000\020\000\
\\014\000\038\000\015\000\037\000\016\000\036\000\017\000\035\000\
\\018\000\034\000\019\000\033\000\020\000\032\000\021\000\031\000\
\\022\000\030\000\023\000\029\000\000\000\
\\001\000\003\000\008\000\000\000\
\\001\000\003\000\015\000\008\000\014\000\000\000\
\\001\000\003\000\016\000\000\000\
\\001\000\003\000\027\000\000\000\
\\001\000\003\000\042\000\000\000\
\\001\000\003\000\054\000\000\000\
\\001\000\003\000\056\000\000\000\
\\001\000\003\000\061\000\000\000\
\\001\000\003\000\063\000\000\000\
\\001\000\003\000\065\000\000\000\
\\001\000\004\000\010\000\000\000\
\\001\000\005\000\041\000\006\000\040\000\007\000\039\000\014\000\038\000\
\\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\000\000\
\\001\000\008\000\012\000\000\000\
\\001\000\008\000\052\000\000\000\
\\067\000\000\000\
\\068\000\000\000\
\\069\000\000\000\
\\070\000\000\000\
\\071\000\002\000\007\000\000\000\
\\072\000\000\000\
\\073\000\000\000\
\\074\000\000\000\
\\075\000\000\000\
\\076\000\000\000\
\\077\000\000\000\
\\078\000\002\000\025\000\008\000\024\000\009\000\023\000\010\000\022\000\
\\011\000\021\000\012\000\020\000\000\000\
\\079\000\000\000\
\\080\000\000\000\
\\081\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\008\000\014\000\000\000\
\\092\000\000\000\
\\093\000\002\000\051\000\005\000\050\000\006\000\049\000\008\000\048\000\
\\009\000\023\000\010\000\022\000\011\000\021\000\012\000\020\000\
\\014\000\038\000\015\000\037\000\016\000\036\000\017\000\035\000\
\\018\000\034\000\019\000\033\000\020\000\032\000\021\000\031\000\
\\022\000\030\000\023\000\029\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\"
val actionRowNumbers =
"\001\000\021\000\002\000\007\000\
\\025\000\017\000\022\000\026\000\
\\003\000\019\000\008\000\009\000\
\\045\000\004\000\004\000\046\000\
\\027\000\010\000\036\000\035\000\
\\037\000\034\000\028\000\018\000\
\\011\000\023\000\004\000\058\000\
\\057\000\056\000\055\000\054\000\
\\053\000\052\000\051\000\050\000\
\\049\000\006\000\020\000\004\000\
\\024\000\012\000\032\000\041\000\
\\013\000\039\000\040\000\043\000\
\\042\000\005\000\004\000\004\000\
\\031\000\033\000\038\000\014\000\
\\047\000\015\000\004\000\044\000\
\\048\000\030\000\016\000\029\000\
\\000\000"
val gotoT =
"\
\\001\000\064\000\002\000\001\000\000\000\
\\000\000\
\\003\000\004\000\004\000\003\000\000\000\
\\000\000\
\\003\000\004\000\004\000\007\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\011\000\000\000\
\\000\000\
\\011\000\015\000\000\000\
\\005\000\017\000\007\000\016\000\000\000\
\\005\000\024\000\007\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\026\000\000\000\
\\000\000\
\\000\000\
\\005\000\042\000\006\000\041\000\007\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\045\000\008\000\044\000\010\000\043\000\000\000\
\\000\000\
\\005\000\051\000\007\000\016\000\000\000\
\\000\000\
\\000\000\
\\005\000\042\000\006\000\053\000\007\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\045\000\008\000\056\000\009\000\055\000\010\000\043\000\000\000\
\\005\000\042\000\006\000\057\000\007\000\016\000\000\000\
\\005\000\058\000\007\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\045\000\008\000\056\000\009\000\060\000\010\000\043\000\000\000\
\\000\000\
\\005\000\062\000\007\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 65
val numrules = 38
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NULL of unit ->  (string) | CDR of unit ->  (string)
 | CAR of unit ->  (string) | CONS of unit ->  (string)
 | MOD of unit ->  (string) | DIV of unit ->  (string)
 | TIMES of unit ->  (string) | EQL of unit ->  (string)
 | MINUS of unit ->  (string) | PLUS of unit ->  (string)
 | NUM of unit ->  (int) | ID of unit ->  (string)
 | VARLIST of unit ->  (string list) | OP of unit ->  (string)
 | QUOTABLELIST of unit ->  (SchemeAST.constant list)
 | QUOTABLE of unit ->  (SchemeAST.constant)
 | CONST of unit ->  (SchemeAST.constant)
 | EXPLIST of unit ->  (SchemeAST.expr list)
 | EXP of unit ->  (SchemeAST.expr)
 | EQNLIST of unit ->  ( ( string list ) * ( (string list * SchemeAST.expr) list ) )
 | EQN of unit ->  (string* ( string list * SchemeAST.expr ) )
 | PROG of unit ->  (SchemeAST.program)
 | START of unit ->  (SchemeAST.program)
end
type svalue = MlyValue.svalue
type result = SchemeAST.program
end
structure EC=
struct
open LrTable
val is_keyword =
fn _ => false
val preferred_change = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "LPAREN"
  | (T 2) => "RPAREN"
  | (T 3) => "DEFINE"
  | (T 4) => "IF"
  | (T 5) => "CALL"
  | (T 6) => "QUOTE"
  | (T 7) => "ID"
  | (T 8) => "NUM"
  | (T 9) => "NIL"
  | (T 10) => "TRUE"
  | (T 11) => "FALSE"
  | (T 12) => "DOT"
  | (T 13) => "PLUS"
  | (T 14) => "MINUS"
  | (T 15) => "EQL"
  | (T 16) => "TIMES"
  | (T 17) => "DIV"
  | (T 18) => "MOD"
  | (T 19) => "CONS"
  | (T 20) => "CAR"
  | (T 21) => "CDR"
  | (T 22) => "NULL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms = (T 0) :: (T 1) :: (T 2) :: (T 3) :: (T 4) :: (T 5) :: (T 6
) :: (T 9) :: (T 10) :: (T 11) :: (T 12) :: nil
end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of (0,(_,(MlyValue.PROG PROG1,PROG1left,PROG1right))::rest671) => let 
val result=MlyValue.START(fn _ => let val PROG as PROG1=PROG1 ()
 in (PROG) end
)
 in (LrTable.NT 0,(result,PROG1left,PROG1right),rest671) end
| (1,(_,(_,_,RPAREN1right))::(_,(MlyValue.EQNLIST EQNLIST1,_,_))::(_,(
_,LPAREN1left,_))::rest671) => let val result=MlyValue.PROG(fn _ => 
let val EQNLIST as EQNLIST1=EQNLIST1 ()
 in (EQNLIST) end
)
 in (LrTable.NT 1,(result,LPAREN1left,RPAREN1right),rest671) end
| (2,(_,(_,_,RPAREN2right))::(_,(MlyValue.EXP EXP1,_,_))::_::(_,(
MlyValue.ID ID1,_,_))::_::_::(_,(_,LPAREN1left,_))::rest671) => let 
val result=MlyValue.EQN(fn _ => let val ID as ID1=ID1 ()
val EXP as EXP1=EXP1 ()
 in (ID, (nil,EXP)) end
)
 in (LrTable.NT 2,(result,LPAREN1left,RPAREN2right),rest671) end
| (3,(_,(_,_,RPAREN2right))::(_,(MlyValue.EXP EXP1,_,_))::_::(_,(
MlyValue.VARLIST VARLIST1,_,_))::(_,(MlyValue.ID ID1,_,_))::_::_::(_,(
_,LPAREN1left,_))::rest671) => let val result=MlyValue.EQN(fn _ => 
let val ID as ID1=ID1 ()
val VARLIST as VARLIST1=VARLIST1 ()
val EXP as EXP1=EXP1 ()
 in (ID,(VARLIST,EXP)) end
)
 in (LrTable.NT 2,(result,LPAREN1left,RPAREN2right),rest671) end
| (4,(_,(MlyValue.EQN EQN1,EQN1left,EQN1right))::rest671) => let val 
result=MlyValue.EQNLIST(fn _ => let val EQN as EQN1=EQN1 ()
 in (let val (fname,body) = EQN in (fname::nil,body::nil) end) end
)
 in (LrTable.NT 3,(result,EQN1left,EQN1right),rest671) end
| (5,(_,(MlyValue.EQNLIST EQNLIST1,_,EQNLIST1right))::(_,(MlyValue.EQN
 EQN1,EQN1left,_))::rest671) => let val result=MlyValue.EQNLIST(fn _
 => let val EQN as EQN1=EQN1 ()
val EQNLIST as EQNLIST1=EQNLIST1 ()
 in (
let val (flist,blist) = EQNLIST; val (fname,body) = EQN in (fname::flist,body::blist) end
) end
)
 in (LrTable.NT 3,(result,EQN1left,EQNLIST1right),rest671) end
| (6,(_,(MlyValue.CONST CONST1,CONST1left,CONST1right))::rest671) => 
let val result=MlyValue.EXP(fn _ => let val CONST as CONST1=CONST1 ()
 in (SchemeAST.Const(CONST)) end
)
 in (LrTable.NT 4,(result,CONST1left,CONST1right),rest671) end
| (7,(_,(MlyValue.ID ID1,ID1left,ID1right))::rest671) => let val 
result=MlyValue.EXP(fn _ => let val ID as ID1=ID1 ()
 in (SchemeAST.Variable(ID)) end
)
 in (LrTable.NT 4,(result,ID1left,ID1right),rest671) end
| (8,(_,(_,_,RPAREN1right))::(_,(MlyValue.EXP EXP3,_,_))::(_,(
MlyValue.EXP EXP2,_,_))::(_,(MlyValue.EXP EXP1,_,_))::_::(_,(_,
LPAREN1left,_))::rest671) => let val result=MlyValue.EXP(fn _ => let 
val EXP1=EXP1 ()
val EXP2=EXP2 ()
val EXP3=EXP3 ()
 in (SchemeAST.If(EXP1,EXP2,EXP3)) end
)
 in (LrTable.NT 4,(result,LPAREN1left,RPAREN1right),rest671) end
| (9,(_,(_,_,RPAREN1right))::(_,(MlyValue.EXPLIST EXPLIST1,_,_))::(_,(
MlyValue.ID ID1,_,_))::_::(_,(_,LPAREN1left,_))::rest671) => let val 
result=MlyValue.EXP(fn _ => let val ID as ID1=ID1 ()
val EXPLIST as EXPLIST1=EXPLIST1 ()
 in (SchemeAST.Call(ID,EXPLIST)) end
)
 in (LrTable.NT 4,(result,LPAREN1left,RPAREN1right),rest671) end
| (10,(_,(_,_,RPAREN1right))::(_,(MlyValue.EXPLIST EXPLIST1,_,_))::(_,
(MlyValue.OP OP1,_,_))::(_,(_,LPAREN1left,_))::rest671) => let val 
result=MlyValue.EXP(fn _ => let val OP as OP1=OP1 ()
val EXPLIST as EXPLIST1=EXPLIST1 ()
 in (SchemeAST.Op(OP,EXPLIST)) end
)
 in (LrTable.NT 4,(result,LPAREN1left,RPAREN1right),rest671) end
| (11,(_,(MlyValue.EXP EXP1,EXP1left,EXP1right))::rest671) => let val 
result=MlyValue.EXPLIST(fn _ => let val EXP as EXP1=EXP1 ()
 in (EXP::nil) end
)
 in (LrTable.NT 5,(result,EXP1left,EXP1right),rest671) end
| (12,(_,(MlyValue.EXPLIST EXPLIST1,_,EXPLIST1right))::(_,(
MlyValue.EXP EXP1,EXP1left,_))::rest671) => let val result=
MlyValue.EXPLIST(fn _ => let val EXP as EXP1=EXP1 ()
val EXPLIST as EXPLIST1=EXPLIST1 ()
 in (EXP::EXPLIST) end
)
 in (LrTable.NT 5,(result,EXP1left,EXPLIST1right),rest671) end
| (13,(_,(MlyValue.NUM NUM1,NUM1left,NUM1right))::rest671) => let val 
result=MlyValue.CONST(fn _ => let val NUM as NUM1=NUM1 ()
 in (SchemeAST.Numeral NUM) end
)
 in (LrTable.NT 6,(result,NUM1left,NUM1right),rest671) end
| (14,(_,(_,TRUE1left,TRUE1right))::rest671) => let val result=
MlyValue.CONST(fn _ => (SchemeAST.Boolean true))
 in (LrTable.NT 6,(result,TRUE1left,TRUE1right),rest671) end
| (15,(_,(_,FALSE1left,FALSE1right))::rest671) => let val result=
MlyValue.CONST(fn _ => (SchemeAST.Boolean false))
 in (LrTable.NT 6,(result,FALSE1left,FALSE1right),rest671) end
| (16,(_,(_,NIL1left,NIL1right))::rest671) => let val result=
MlyValue.CONST(fn _ => (SchemeAST.NIL))
 in (LrTable.NT 6,(result,NIL1left,NIL1right),rest671) end
| (17,(_,(_,_,RPAREN1right))::(_,(MlyValue.QUOTABLE QUOTABLE1,_,_))::_
::(_,(_,LPAREN1left,_))::rest671) => let val result=MlyValue.CONST(fn 
_ => let val QUOTABLE as QUOTABLE1=QUOTABLE1 ()
 in (SchemeAST.Quote(QUOTABLE)) end
)
 in (LrTable.NT 6,(result,LPAREN1left,RPAREN1right),rest671) end
| (18,(_,(MlyValue.CONST CONST1,CONST1left,CONST1right))::rest671) => 
let val result=MlyValue.QUOTABLE(fn _ => let val CONST as CONST1=
CONST1 ()
 in (CONST) end
)
 in (LrTable.NT 7,(result,CONST1left,CONST1right),rest671) end
| (19,(_,(MlyValue.ID ID1,ID1left,ID1right))::rest671) => let val 
result=MlyValue.QUOTABLE(fn _ => let val ID as ID1=ID1 ()
 in (SchAST.Symbol ID) end
)
 in (LrTable.NT 7,(result,ID1left,ID1right),rest671) end
| (20,(_,(MlyValue.OP OP1,OP1left,OP1right))::rest671) => let val 
result=MlyValue.QUOTABLE(fn _ => let val OP as OP1=OP1 ()
 in (SchAST.Symbol OP) end
)
 in (LrTable.NT 7,(result,OP1left,OP1right),rest671) end
| (21,(_,(_,IF1left,IF1right))::rest671) => let val result=
MlyValue.QUOTABLE(fn _ => (SchAST.Symbol "if"))
 in (LrTable.NT 7,(result,IF1left,IF1right),rest671) end
| (22,(_,(_,CALL1left,CALL1right))::rest671) => let val result=
MlyValue.QUOTABLE(fn _ => (SchAST.Symbol "call"))
 in (LrTable.NT 7,(result,CALL1left,CALL1right),rest671) end
| (23,(_,(_,_,RPAREN1right))::(_,(MlyValue.QUOTABLELIST QUOTABLELIST1,
_,_))::(_,(_,LPAREN1left,_))::rest671) => let val result=
MlyValue.QUOTABLE(fn _ => let val QUOTABLELIST as QUOTABLELIST1=
QUOTABLELIST1 ()
 in (List.foldr SchemeAST.List SchemeAST.NIL QUOTABLELIST) end
)
 in (LrTable.NT 7,(result,LPAREN1left,RPAREN1right),rest671) end
| (24,(_,(MlyValue.ID ID1,ID1left,ID1right))::rest671) => let val 
result=MlyValue.VARLIST(fn _ => let val ID as ID1=ID1 ()
 in (ID::nil) end
)
 in (LrTable.NT 10,(result,ID1left,ID1right),rest671) end
| (25,(_,(MlyValue.VARLIST VARLIST1,_,VARLIST1right))::(_,(MlyValue.ID
 ID1,ID1left,_))::rest671) => let val result=MlyValue.VARLIST(fn _ => 
let val ID as ID1=ID1 ()
val VARLIST as VARLIST1=VARLIST1 ()
 in (ID::VARLIST) end
)
 in (LrTable.NT 10,(result,ID1left,VARLIST1right),rest671) end
| (26,(_,(MlyValue.QUOTABLE QUOTABLE1,QUOTABLE1left,QUOTABLE1right))::
rest671) => let val result=MlyValue.QUOTABLELIST(fn _ => let val 
QUOTABLE as QUOTABLE1=QUOTABLE1 ()
 in (QUOTABLE::nil) end
)
 in (LrTable.NT 8,(result,QUOTABLE1left,QUOTABLE1right),rest671) end
| (27,(_,(MlyValue.QUOTABLELIST QUOTABLELIST1,_,QUOTABLELIST1right))::
(_,(MlyValue.QUOTABLE QUOTABLE1,QUOTABLE1left,_))::rest671) => let 
val result=MlyValue.QUOTABLELIST(fn _ => let val QUOTABLE as QUOTABLE1
=QUOTABLE1 ()
val QUOTABLELIST as QUOTABLELIST1=QUOTABLELIST1 ()
 in (QUOTABLE::QUOTABLELIST) end
)
 in (LrTable.NT 8,(result,QUOTABLE1left,QUOTABLELIST1right),rest671)
 end
| (28,(_,(MlyValue.PLUS PLUS1,PLUS1left,PLUS1right))::rest671) => let 
val result=MlyValue.OP(fn _ => let val PLUS as PLUS1=PLUS1 ()
 in (PLUS) end
)
 in (LrTable.NT 9,(result,PLUS1left,PLUS1right),rest671) end
| (29,(_,(MlyValue.MINUS MINUS1,MINUS1left,MINUS1right))::rest671) => 
let val result=MlyValue.OP(fn _ => let val MINUS as MINUS1=MINUS1 ()
 in (MINUS) end
)
 in (LrTable.NT 9,(result,MINUS1left,MINUS1right),rest671) end
| (30,(_,(MlyValue.EQL EQL1,EQL1left,EQL1right))::rest671) => let val 
result=MlyValue.OP(fn _ => let val EQL as EQL1=EQL1 ()
 in (EQL) end
)
 in (LrTable.NT 9,(result,EQL1left,EQL1right),rest671) end
| (31,(_,(MlyValue.TIMES TIMES1,TIMES1left,TIMES1right))::rest671) => 
let val result=MlyValue.OP(fn _ => let val TIMES as TIMES1=TIMES1 ()
 in (TIMES) end
)
 in (LrTable.NT 9,(result,TIMES1left,TIMES1right),rest671) end
| (32,(_,(MlyValue.DIV DIV1,DIV1left,DIV1right))::rest671) => let val 
result=MlyValue.OP(fn _ => let val DIV as DIV1=DIV1 ()
 in (DIV) end
)
 in (LrTable.NT 9,(result,DIV1left,DIV1right),rest671) end
| (33,(_,(MlyValue.MOD MOD1,MOD1left,MOD1right))::rest671) => let val 
result=MlyValue.OP(fn _ => let val MOD as MOD1=MOD1 ()
 in (MOD) end
)
 in (LrTable.NT 9,(result,MOD1left,MOD1right),rest671) end
| (34,(_,(MlyValue.CONS CONS1,CONS1left,CONS1right))::rest671) => let 
val result=MlyValue.OP(fn _ => let val CONS as CONS1=CONS1 ()
 in (CONS) end
)
 in (LrTable.NT 9,(result,CONS1left,CONS1right),rest671) end
| (35,(_,(MlyValue.CAR CAR1,CAR1left,CAR1right))::rest671) => let val 
result=MlyValue.OP(fn _ => let val CAR as CAR1=CAR1 ()
 in (CAR) end
)
 in (LrTable.NT 9,(result,CAR1left,CAR1right),rest671) end
| (36,(_,(MlyValue.CDR CDR1,CDR1left,CDR1right))::rest671) => let val 
result=MlyValue.OP(fn _ => let val CDR as CDR1=CDR1 ()
 in (CDR) end
)
 in (LrTable.NT 9,(result,CDR1left,CDR1right),rest671) end
| (37,(_,(MlyValue.NULL NULL1,NULL1left,NULL1right))::rest671) => let 
val result=MlyValue.OP(fn _ => let val NULL as NULL1=NULL1 ()
 in (NULL) end
)
 in (LrTable.NT 9,(result,NULL1left,NULL1right),rest671) end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Scheme_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun DEFINE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun CALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun QUOTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.PLUS (fn () => i),p1,p2))
fun MINUS (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.MINUS (fn () => i),p1,p2))
fun EQL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.EQL (fn () => i),p1,p2))
fun TIMES (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.TIMES (fn () => i),p1,p2))
fun DIV (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.DIV (fn () => i),p1,p2))
fun MOD (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.MOD (fn () => i),p1,p2))
fun CONS (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.CONS (fn () => i),p1,p2))
fun CAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.CAR (fn () => i),p1,p2))
fun CDR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.CDR (fn () => i),p1,p2))
fun NULL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.NULL (fn () => i),p1,p2))
end
end
