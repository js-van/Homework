signature Scheme_TOKENS =
sig
type ('a,'b) token
type svalue
val NULL: (string) *  'a * 'a -> (svalue,'a) token
val CDR: (string) *  'a * 'a -> (svalue,'a) token
val CAR: (string) *  'a * 'a -> (svalue,'a) token
val CONS: (string) *  'a * 'a -> (svalue,'a) token
val MOD: (string) *  'a * 'a -> (svalue,'a) token
val DIV: (string) *  'a * 'a -> (svalue,'a) token
val TIMES: (string) *  'a * 'a -> (svalue,'a) token
val EQL: (string) *  'a * 'a -> (svalue,'a) token
val MINUS: (string) *  'a * 'a -> (svalue,'a) token
val PLUS: (string) *  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val NIL:  'a * 'a -> (svalue,'a) token
val NUM: (int) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val QUOTE:  'a * 'a -> (svalue,'a) token
val CALL:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val DEFINE:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Scheme_LRVALS=
sig
structure Tokens : Scheme_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
