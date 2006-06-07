(* file : ticlex.mll *)
(* $Id: ticlex.mll,v 1.30 2006/06/07 20:02:09 luca Exp $ *)

{
  open Ticparse (* Assumes the parser file is "ticparse.mly".*)
  open Lexing 

  type lex_pos = {
    mutable lex_line: int; 
    mutable lex_col: int;
    mutable tok: string; 
  }
  
  let curr_pos : lex_pos = {
    lex_line = 0; 
    lex_col = 0; 
    tok = ""; 
  }

  (* This stores the position of the last token read, and also the 
     last token, for error reporting.  I wish this was done 
     automatically... *)
  let getpos lexbuf = 
    let pos = lexbuf.Lexing.lex_curr_p in
    curr_pos.lex_line <- pos.Lexing.pos_lnum; 
    curr_pos.lex_col  <- (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
    curr_pos.tok <- Lexing.lexeme lexbuf
    
  (* Apparently the lexer doesn't know about newlines, so we have to
     reset the position by hand. *) 
  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

let digit  = ['0'-'9']
let alpha  = ['a'-'z''A'-'Z']
rule token = parse
  | [' ' '\t'] 		{getpos lexbuf; token lexbuf}
 | "\n" 		{getpos lexbuf; incr_linenum lexbuf; token lexbuf}
 | "//"[^'\n']*'\n' 	{getpos lexbuf; incr_linenum lexbuf; token lexbuf}
 | "true"    		{getpos lexbuf; TRUE}
 | "TRUE"    		{getpos lexbuf; TRUE}
 | "false"   		{getpos lexbuf; FALSE}
 | "FALSE"   		{getpos lexbuf; FALSE}
 | "bool"    		{getpos lexbuf; BOOL}
 | "var"     		{getpos lexbuf; VAR}
 | "module"  		{getpos lexbuf; MODULE}
 | "stateless"          {getpos lexbuf; STATELESS}
 | "oinv"    		{getpos lexbuf; OINV}
 | "iinv"    		{getpos lexbuf; IINV}
 | "endmodule" 		{getpos lexbuf; ENDMODULE}
 | "input"   		{getpos lexbuf; INPUT}
 | "output"  		{getpos lexbuf; OUTPUT}
 | "global"  		{getpos lexbuf; GLOBAL}
 | "local"   		{getpos lexbuf; LOCAL}
 | "clock"   		{getpos lexbuf; CLOCK}
(* | "endinput" 		{getpos lexbuf; ENDINPUT}
   | "endoutput" 		{getpos lexbuf; ENDOUTPUT}
   | "endlocal" 		{getpos lexbuf; ENDLOCAL} *)
 | "==>"      		{getpos lexbuf; TO}
 | "else"    		{getpos lexbuf; ELSE}
 | "mod"                {getpos lexbuf; MOD}    
 | "not"                {getpos lexbuf; NOT}    
 | "and"                {getpos lexbuf; AND}        
 | "or"                 {getpos lexbuf; OR}    
 | "impl"               {getpos lexbuf; IMPLIES}
 | "nondet"             {getpos lexbuf; NONDET}     
 | "stateset"           {getpos lexbuf; STATESET}    
 | "initial"            {getpos lexbuf; INITIAL}    
 | '['       		{getpos lexbuf; LB}
 | ']'       		{getpos lexbuf; RB} 
 | '('                  {getpos lexbuf; LPAR} 
 | ')'                  {getpos lexbuf; RPAR} 
 | '{'                  {getpos lexbuf; BEGBLOCK} 
 | '}'                  {getpos lexbuf; ENDBLOCK} 
 | '&'       		{getpos lexbuf; AND}
 | '|'       		{getpos lexbuf; OR}
 | "->"      		{getpos lexbuf; IMPLIES}
 | ';'      		{getpos lexbuf; SEMICOL}
 | ':'       		{getpos lexbuf; COL}
 | ','       		{getpos lexbuf; COMMA}
 | '+'       		{getpos lexbuf; PLUS}
 | '-'       		{getpos lexbuf; MINUS}
 | ".."     		{getpos lexbuf; DOTS}
 | "<="      		{getpos lexbuf; LE}
 | ">="      		{getpos lexbuf; GE}
 | '='       		{getpos lexbuf; EQ}
 | "!="      		{getpos lexbuf; NE}
 | '>'       		{getpos lexbuf; GT} 
 | '<'       		{getpos lexbuf; LT}   
 | '~'       		{getpos lexbuf; NOT}
 | ":="                 {getpos lexbuf; COLEQ}
 | '\''                 {getpos lexbuf; PRIME}    
 | digit+ as num 	{getpos lexbuf; NUM (int_of_string num)}
 | alpha(alpha|digit|'_'|'.')* as word 	 {getpos lexbuf; ID word}
 | alpha(alpha|digit|'_')*'*' as word 	 {getpos lexbuf; WILDID word}
 | eof       				 {getpos lexbuf; EOF (* raise
							    End_of_file *)}
 | _         				 {getpos lexbuf; token lexbuf}   

