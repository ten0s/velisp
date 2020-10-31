grammar VeDcl;

// Parser rules

file : dialog* ;

dialog : ID ':' 'dialog' '{' entry* '}' ';'? ;

entry : attribute
      | control
      ;

control : ID? ':' 'text'     '{' attribute* '}' ';'?           # text
        | ID? ':' 'button'   '{' attribute* '}' ';'?           # button
        | ID? ':' 'edit_box' '{' attribute* '}' ';'?           # editBox
        ;

attribute : attributeName '=' attributeValue ';' ;

attributeName : ID ;
attributeValue : STRING
               | BOOL
               ;

// Lexer rules

STRING : '"' CHAR* '"' ;
BOOL : 'true'
     | 'false'
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
