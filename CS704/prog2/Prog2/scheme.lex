structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val pos = ref 1
val eof = fn () => Tokens.EOF(!pos, !pos)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat["line ", (Int.toString l), ": ", e, "\n"])

%%
%header (functor SchemeLexFun(structure Tokens : Scheme_TOKENS));
ws = [\ \t];
alpha = [a-z];
num = [0-9];
alphanum = [a-z,A-Z,0-9];
%%
"\n"		=> (pos := (!pos) + 1; lex());
{ws}+		=> (lex());
"("		=> (Tokens.LPAREN(!pos, !pos));
")"		=> (Tokens.RPAREN(!pos, !pos));
"define"	=> (Tokens.DEFINE(!pos, !pos));
{num}+          => (Tokens.NUM(valOf (Int.fromString yytext), !pos,!pos));
"+"             => (Tokens.PLUS(yytext,!pos,!pos));
"-"             => (Tokens.MINUS(yytext,!pos,!pos));
"="             => (Tokens.EQL(yytext,!pos,!pos));
"*"             => (Tokens.TIMES(yytext,!pos,!pos));
"div"           => (Tokens.DIV(yytext,!pos,!pos));
"mod"           => (Tokens.DIV(yytext,!pos,!pos));
"cons"             => (Tokens.CONS(yytext,!pos,!pos));
"car"             => (Tokens.CAR(yytext,!pos,!pos));
"cdr"             => (Tokens.CDR(yytext,!pos,!pos));
"null?"         => (Tokens.NULL(yytext,!pos,!pos));
"if"            => (Tokens.IF(!pos,!pos));
"call"            => (Tokens.CALL(!pos,!pos));
"quote"         => (Tokens.QUOTE(!pos,!pos));
"NIL"         => (Tokens.NIL(!pos,!pos));
"#t"          => (Tokens.TRUE(!pos,!pos));
"#f"          => (Tokens.FALSE(!pos,!pos));
"."           => (Tokens.DOT(!pos,!pos));
{alphanum}+     => (Tokens.ID(yytext,!pos,!pos));
.		=> (error ("Ignoring bad character "^yytext,!pos,!pos); lex());

