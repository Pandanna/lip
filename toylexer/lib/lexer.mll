{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let capitals = ['A' - 'Z']
let lower_vowels = ['a' 'e' 'i' 'o' 'u']
let upper_vowels = ['A' 'E' 'I' 'O' 'U'] 
let lower_cons = ['b'-'d' 'f'-'h' 'j'-'n' 'p'-'t' 'v'-'z']
let upper_cons = ['B'-'D' 'F'-'H' 'J'-'N' 'P'-'T' 'V'-'Z']
let hex = ['0'-'9' 'A'-'F' 'a'-'f']

let atok = capitals chr*
let btok = lower_vowels+
let ctok = (lower_cons|upper_cons)* (lower_vowels|upper_vowels)? (lower_cons|upper_cons)*
let dtok = ['-']? num* (['.'] num*)?
let etok = ("0x" | "0X") hex+


rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ } 
  | atok { ATOK (Lexing.lexeme lexbuf) }
  | btok { BTOK (Lexing.lexeme lexbuf) } 
  | ctok { CTOK (Lexing.lexeme lexbuf) } 
  | dtok { DTOK (Lexing.lexeme lexbuf) } 
  | etok { ETOK (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }
  
