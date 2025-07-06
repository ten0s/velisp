/**
 *   This file is part of VeLisp
 *
 *   Copyright (C) 2022-2024 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

/* SPDX-License-Identifier: GPL-3.0-or-later */

grammar VeLisp;

// Parser rules

file : expr* ;

expr :
       '(' AND expr* ')'                                                          # and
     | '(' COND condTestResult* ')'                                               # cond
     | '(' DEFUN funName '(' funParam* ( ' / ' funLocal* )? ')' expr+ ')'         # defun
     | '(' FOREACH foreachName foreachList expr* ')'                              # foreach
     | '(' FUNCTION expr ')'                                                      # function
     | '(' IF ifTest ifThen ifElse? ')'                                           # if
     | '(' LAMBDA '(' funParam* ( ' / ' funLocal* )? ')' expr+ ')'                # lambda
     | '(' OR expr* ')'                                                           # or
     | '(' PROGN expr* ')'                                                        # progn
     | '(' QUOTE expr ')'                                                         # quote
     | '(' REPEAT repeatNum expr* ')'                                             # repeat
     | '(' SETQ setqNameExpr* ')'                                                 # setQ
     | '(' WHILE whileTest expr* ')'                                              # while

     | '(' listExpr+ '.' listExpr ')'                                             # dotList
     | '(' listExpr* ')'                                                          # list

     | NIL                                                                        # nil
     | TRU                                                                        # tru
     | INT                                                                        # int
     | REAL                                                                       # real
     | STR                                                                        # str
     | ID                                                                         # id

     | '\'' expr                                                                  # quote
     ;

condTestResult : '(' condTest condResult* ')' ;
condTest : expr ;
condResult : expr ;

funName : ID ;
funParam : ID ;
funLocal : ID ;

foreachName : ID ;
foreachList : expr ;

ifTest : expr ;
ifThen : expr ;
ifElse : expr ;

repeatNum : expr ;

setqNameExpr : ID expr ;

whileTest : expr ;

listExpr : expr ;

// Lexer rules

AND : A N D ;
COND : C O N D ;
DEFUN : D E F U N ;
FOREACH : F O R E A C H ;
FUNCTION : F U N C T I O N ;
IF : I F ;
LAMBDA : L A M B D A ;
OR : O R ;
PROGN : P R O G N ;
QUOTE : Q U O T E ;
REPEAT : R E P E A T ;
SETQ : S E T Q ;
WHILE : W H I L E ;
NIL : N I L ;
TRU : T ;

INT : '-'?DIGIT+ ;
REAL : '-'?DIGIT+'.'DIGIT+ ;
STR : '"' CHAR* '"' ;
ID : [a-zA-Z0-9!$%*/\-+=<>~:_]+ ;

COMMENT : ';|' .*? '|;' -> skip ; // TODO: inline inside expr doesn't work
LINE_COMMENT : ';'+ .*? NEWLINE -> skip ;

NEWLINE : '\r'? '\n' -> skip ;
WHITESPACE : [ \t]+ -> skip ;

fragment CHAR
         : ~[\\"\r\n]
         | '\\' ESCAPE_SEQ
         | NEWLINE
         ;

fragment ESCAPE_SEQ
         : [\\"rnte0] // See VeUtil.js::unescape
         ;

fragment A : [aA] ;
fragment B : [bB] ;
fragment C : [cC] ;
fragment D : [dD] ;
fragment E : [eE] ;
fragment F : [fF] ;
fragment G : [gG] ;
fragment H : [hH] ;
fragment I : [iI] ;
fragment J : [jJ] ;
fragment K : [kK] ;
fragment L : [lL] ;
fragment M : [mM] ;
fragment N : [nN] ;
fragment O : [oO] ;
fragment P : [pP] ;
fragment Q : [qQ] ;
fragment R : [rR] ;
fragment S : [sS] ;
fragment T : [tT] ;
fragment U : [uU] ;
fragment V : [vV] ;
fragment W : [wW] ;
fragment X : [xX] ;
fragment Y : [yY] ;
fragment Z : [zZ] ;

fragment LETTER : [a-zA-Z] ;
fragment DIGIT  : [0-9] ;
