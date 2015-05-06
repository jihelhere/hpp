{
open Core.Std
open Lexing
open Ptbparser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}


let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '`' ''' '*' '+' '.' ',' '&' '$' ':' '%' '\\' '/' '?' ';' '=' '#' '!' '|' '@']+


rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | id  {ID (Lexing.lexeme lexbuf)}
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
