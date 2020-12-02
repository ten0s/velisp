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
            | 'radio_row'
            | 'radio_column'
            | 'boxed_radio_row'
            | 'boxed_radio_column'
            ;

simpleTile : 'button'
           | 'edit_box'
           | 'radio_button'
           | 'slider'
           | 'spacer'
           | 'text'
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
         : ["\\rnt]
         ;

fragment LETTER : LOWER_LETTER | UPPER_LETTER ;
fragment LOWER_LETTER : [a-z] ;
fragment UPPER_LETTER : [A-Z] ;
fragment DIGIT  : [0-9] ;
