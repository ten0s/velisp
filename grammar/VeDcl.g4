grammar VeDcl;

// Parser rules

file : (includeFile | defineTile)* ;

includeFile : '@include' fileName ';'? ;

defineTile : ID ':' clusterTile '{' entry* '}' ';'?                # defineClusterTile
           | ID ':' simpleTile  '{' attribute* '}' ';'?            # defineSimpleTile
           ;

innerTile : ':' clusterTile    '{' entry* '}' ';'?                 # innerClusterTile
          | ':' simpleTile     '{' attribute* '}' ';'?             # innerSimpleTile
          | ':' deriveTile     '{' attribute* '}' ';'?             # innerDeriveTile
          ;

clusterTile : 'dialog'
            | 'row'
            | 'column'
            ;

simpleTile : 'text'
           | 'button'
           | 'edit_box'
           ;

deriveTile : ID ;

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
