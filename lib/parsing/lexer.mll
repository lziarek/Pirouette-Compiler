{
  open Lexing
  open Parser

  exception SyntaxError of string

  (** [next_line lexbuf] advances the lexer to the next line in the input buffer.
    
    - This function increments the line number in the lexer buffer's current position
      and sets the beginning of line marker to the current position in the buffer.
    - It is typically called when a newline character is encountered in the input.
  *)
  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = lexbuf.lex_curr_pos }

  let filename lexbuf = lexbuf.lex_curr_p.pos_fname
  let line lexbuf = lexbuf.lex_curr_p.pos_lnum

  (** [metainfo lexbuf] retrieves the current file name and line number from the lexer buffer.
    
    - Returns: A tuple containing the file name and line number of the current position
      in the lexer buffer.
    - This function is useful for error reporting and debugging, providing context
      about where in the source file the lexer is currently operating.
  *)
  let metainfo lexbuf= (filename lexbuf, line lexbuf)

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let integer = '-'? digit+
let identifier = (alpha | '_' ) (alpha | digit | '_')*

(** [read lexbuf] is the main lexer function that tokenizes the input based on the
    defined rules and patterns.
    
    - This function reads characters from the input buffer and matches them against
      predefined patterns to identify tokens such as identifiers, literals, operators,
      and keywords.
    - Returns: It returns a token each time it is called, until it reaches the end
      of the file, at which point it returns EOF.
    - Note: This function handles whitespace, comments, and newline characters
      internally, often recursively calling itself to skip over non-token characters.
*)
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
  | '+'                { PLUS (metainfo lexbuf) }
  | '-'                { MINUS (metainfo lexbuf) }
  | '*'                { TIMES (metainfo lexbuf) }
  | '/'                { DIV (metainfo lexbuf) }
  | "&&"               { AND (metainfo lexbuf) }
  | "||"               { OR (metainfo lexbuf) }
  | "="                { EQ (metainfo lexbuf) }
  | "!="               { NEQ (metainfo lexbuf) }
  | "<"                { LT (metainfo lexbuf) }
  | "<="               { LEQ (metainfo lexbuf) }
  | ">"                { GT (metainfo lexbuf) }
  | ">="               { GEQ (metainfo lexbuf) }
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
