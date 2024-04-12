{
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = lexbuf.lex_curr_pos }

  let filename lexbuf = lexbuf.lex_curr_p.pos_fname
  let line lexbuf = lexbuf.lex_curr_p.pos_lnum
  let metainfo lexbuf= (filename lexbuf, line lexbuf)

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let integer = '-'? digit+
let identifier = (alpha | '_' ) (alpha | digit | '_')*

rule read = parse
  | white              { read lexbuf }
  | "--"               { read_single_line_comment lexbuf }
  | "{-"               { read_multi_line_comment lexbuf }
  | '('                { LPAREN }
  | ')'                { RPAREN }
  | '['                { LBRACKET }
  | ']'                { RBRACKET }
  | ','                { COMMA (metainfo lexbuf) }
  | '.'                { DOT (metainfo lexbuf) }
  | ':'                { COLON (metainfo lexbuf) }
  | ';'                { SEMICOLON (metainfo lexbuf) }
  | '+'                { PLUS }
  | '-'                { MINUS }
  | '*'                { TIMES }
  | '/'                { DIV }
  | "&&"               { AND }
  | "||"               { OR }
  | "="                { EQ }
  | "!="               { NEQ }
  | "<"                { LT }
  | "<="               { LEQ }
  | ">"                { GT }
  | ">="               { GEQ }
  | '|'                { VERTICAL }
  | '_'                { UNDERSCORE }
  | ":="               { COLONEQ }
  | "->"               { ARROW }
  | "~>"               { TILDE_ARROW }
  | "unit"             { UNIT_T }
  | "int"              { INT_T }
  | "string"           { STRING_T }
  | "bool"             { BOOL_T }
  | "fun"              { FUN }
  | "type"             { TYPE }
  | "true"             { TRUE }
  | "false"            { FALSE }
  | "if"               { IF }
  | "then"             { THEN }
  | "else"             { ELSE }
  | "match"            { MATCH }
  | "with"             { WITH }
  | "let"              { LET }
  | "in"               { IN }
  | "fst"              { FST }
  | "snd"              { SND }
  | "left"             { LEFT }
  | "right"            { RIGHT }
  | integer as s       { INT (int_of_string s) }
  | identifier as s    { ID (s, metainfo lexbuf) }
  | '"'                { read_string (Buffer.create 17) lexbuf }
  | newline            { next_line lexbuf; read lexbuf }
  | _                  { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof                { EOF (metainfo lexbuf) }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' ('/' | '\\' | 'b' | 'f' | 'n' | 'r' | 't' as esc)
    { let c = match esc with
        | '/'  -> '/'
        | '\\' -> '\\'
        | 'b'  -> '\b'
        | 'f'  -> '\012'
        | 'n'  -> '\n'
        | 'r'  -> '\r'
        | 't'  -> '\t'
        | _    -> assert false
      in Buffer.add_char buf c;
      read_string buf lexbuf
    }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _   { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError "String is not terminated") }

and read_single_line_comment = parse
  | newline { next_line lexbuf; read lexbuf }
  | _       { read_single_line_comment lexbuf }
  | eof     { EOF (metainfo lexbuf) }

and read_multi_line_comment = parse
  | "-}"    { read lexbuf }
  | newline { next_line lexbuf; read_multi_line_comment lexbuf }
  | _       { read_multi_line_comment lexbuf }
  | eof     { raise (SyntaxError "Comment is not terminated") }
