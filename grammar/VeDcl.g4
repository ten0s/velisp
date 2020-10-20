grammar VeDcl;

// Parser rules

file : dialog* ;

dialog: ID ':' 'dialog' '{' (attribute | control)* '}' ';'? ;

control : ID? ':' CONTROL_NAME '{' attribute* '}' ';'? ;

attribute : ID '=' STRING ';' ;

// Lexer rules

CONTROL_NAME : 'text'
             | 'button'
             ;

ID : LETTER+(DIGIT | LETTER | '_')* ;

STRING : '"' CHAR* '"' ;

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
