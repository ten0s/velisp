/**
 *   This file is part of VeLisp
 *
 *   Copyright (C) 2020-2025 Dmitry Klionsky aka ten0s <dm.klionsky@gmail.com>
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

grammar VeDcl;

// Parser rules

file : (includeFile | defineTile)* ;

includeFile : '@include' fileName ';'? ;

defineTile : ID ':' clusterTile '{' entry* '}'     ';'?             # defineClusterTile
           | ID ':' simpleTile  '{' attribute* '}' ';'?             # defineSimpleTile
           ;

innerTile :     ':' clusterTile '{' entry* '}'     ';'?             # innerClusterTile
          |     ':' simpleTile  '{' attribute* '}' ';'?             # innerSimpleTile
          |     ':' deriveTile  '{' attribute* '}' ';'?             # innerDeriveTile
          |         aliasTile                      ';'              # innerAliasTile
          ;

clusterTile : 'dialog'
            | 'row'
            | 'column'
            | 'boxed_row'
            | 'boxed_column'
            | 'concatenation'
            | 'paragraph'
            | 'radio_row'
            | 'radio_column'
            | 'boxed_radio_row'
            | 'boxed_radio_column'
            ;

simpleTile : 'button'
           | 'edit_box'
           | 'image'
           | 'image_button'
           | 'list_box'
           | 'popup_list'
           | 'radio_button'
           | 'slider'
           | 'spacer'
           | 'text'
           | 'text_part'
           | 'toggle'
           ;

deriveTile : ID ;

aliasTile : ID ;

entry : attribute
      | innerTile
      ;

fileName : STR ;

attribute : attributeName '=' attributeValue ';' ;

attributeName : ID ;
attributeValue : BOOL
               | INT
               | REAL
               | STR
               | ALIGN
               | LAYOUT
               ;

// Lexer rules

BOOL : 'true'
     | 'false'
     ;

INT : '-'?DIGIT+ ;
REAL : '-'?DIGIT+'.'DIGIT+ ;
STR : '"' CHAR* '"' ;

ALIGN : 'left'
      | 'right'
      | 'top'
      | 'bottom'
      | 'centered'
      | 'filled'
      ;

LAYOUT : 'horizontal'
       | 'vertical'
       ;

ID : LETTER+(DIGIT | LETTER | '_')* ;

COMMENT : '/*' .*? '*/' -> skip ;
LINE_COMMENT : '//'+ .*? NEWLINE -> skip ;

NEWLINE : '\r'? '\n' -> skip ;
WHITESPACE : [ \t]+ -> skip ;

fragment CHAR
         : ~["\\\r\n]
         | '\\' ESCAPE_SEQ
         | NEWLINE
         ;

fragment ESCAPE_SEQ
         : ["\\rnte]
         ;

fragment LETTER : LOWER_LETTER | UPPER_LETTER ;
fragment LOWER_LETTER : [a-z] ;
fragment UPPER_LETTER : [A-Z] ;
fragment DIGIT  : [0-9] ;
