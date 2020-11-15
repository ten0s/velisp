grammar VeDcl;

// Parser rules

file : dialog* ;

dialog : ID ':' 'dialog' '{' entry* '}' ';'? ;

entry : attribute
      | control
      ;

control : ID? ':' 'row'      '{' entry* '}' ';'?               # row
        | ID? ':' 'column'   '{' entry* '}' ';'?               # column
        | ID? ':' 'text'     '{' attribute* '}' ';'?           # text
        | ID? ':' 'button'   '{' attribute* '}' ';'?           # button
        | ID? ':' 'edit_box' '{' attribute* '}' ';'?           # editBox
        | ID? ':' 'errtile'  '{' attribute* '}' ';'?           # errTile
        ;

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
