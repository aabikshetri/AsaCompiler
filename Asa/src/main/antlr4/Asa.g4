
grammar Asa ;

  /*****************
    Main Program
  *****************/

  program
    : PROGRAM IDENT SEMICOLON block PERIOD
    ;

  block
    :  (CONST ( constant_definition SEMICOLON )+)?
       (TYPE ( type_definition SEMICOLON )+)?     
       (VAR ( variable_declaration SEMICOLON )+)? 
       compound_statement
    ;

  /****************************
        Declarations
  ****************************/
  
  constant_definition
    : IDENT EQUALS literal
    ;

  type_definition
    : IDENT EQUALS atype
    ;

  variable_declaration
    : IDENT ( COMMA IDENT )* COLON atype
    ;

  literal
    : discreteLiteral      # DiscreteLiteralAlt
    | STRINGLITERAL        # StringLiteralAlt
    | setLiteral           # SetLiteralAlt
    ;

  setLiteral
    : LBRACKET discreteLiteral ( COMMA discreteLiteral )* RBRACKET
    ;

  discreteLiteral
    : integerLiteral       # IntegerLiteralAlt
    | booleanLiteral       # BooleanLiteralAlt
    | CHARLITERAL          # CharLiteralAlt
    ;

  integerLiteral
    : DECIMALINTEGERLITERAL      # DecimalIntegerLiteralAlt
    | HEXADECIMALINTEGERLITERAL  # HexadecimalIntegerLiteralAlt
    | OCTALINTEGERLITERAL        # OctalIntegerLiteralAlt
    ;  
  
  booleanLiteral
    : 'true'
    | 'false'
    ;

  atype
    : namedType 
    | rangeType
    | arrayType
    | setType
    | enumType
    ;

  namedType
    : IDENT
    ;

  rangeType
    : discreteLiteral DOTDOT discreteLiteral
    ;

  arrayType
    : ARRAY LBRACKET rangeType RBRACKET OF atype
    ;

  setType
    : SET OF rangeType
    ;

  enumType
    : LPAREN IDENT ( COMMA IDENT )* RPAREN
    ;

  /***************************
      Statements
  ****************************/

  statement
    : assignment_statement 
    | compound_statement 
    | while_statement 
    | repeat_statement 
    | for_statement
    | if_statement 
    | case_statement
    | printf_statement
    ; 

  assignment_statement
    : lhsreference ASSIGN logicalexpression SEMICOLON
    ;

  lhsreference
    : IDENT ( LBRACKET simpleexpression RBRACKET )?
    ;

  rhsvalue
    : IDENT ( LBRACKET simpleexpression RBRACKET )?
    ;  

  compound_statement
    : BEGIN statement* END
    ;

  while_statement
    : WHILE logicalexpression DO statement
    ;

  repeat_statement
    : REPEAT statement* UNTIL logicalexpression SEMICOLON
    ;

  for_statement
    : FOR IDENT ASSIGN simpleexpression dir=(TO | DOWNTO) simpleexpression DO statement
    ;

  if_statement
    : IF logicalexpression THEN statement ( ELSE statement )?
    ;

  printf_statement
    : PRINTF LPAREN simpleexpression ( COMMA simpleexpression )* RPAREN SEMICOLON
    ;

  case_statement
    : CASE simpleexpression OF case_limb+ END
    ;
  
  case_limb
    : integerLiteral( COMMA integerLiteral )* COLON statement
    ;

  /** 
    Add a production here for case_limb to complete the case statement. The labels on a case are integer literals; if there is more than one then they are separated by commas. A colon separates the labels from the statement that is executed when one of the case labels matches.
  **/

  /*************************
    Expressions
  *************************/

  logicalexpression
    : left=relationalexpression (op=(AND | OR) right=relationalexpression)?
    
    /* Replace this comment with the definition of a logicalexpression, which consists of 
       either a single relationalexpression or two relationalexpressions separated by one of and/or
    */
    ;

  relationalexpression
    : simpleexpression (op=(EQUALS | NOTEQUALTO | LESSTHAN | LESSTHANOREQUALTO | GREATERTHAN | GREATERTHANOREQUALTO | IN) simpleexpression )?
    ;

  simpleexpression
    : term (op+=(PLUS | MINUS) term)*
    ;

  term
    : factor (op+=(TIMES | DIVIDE | DIV | MOD | LSHIFT | RSHIFT) factor)*
    ;

  factor
    : LPAREN fle=logicalexpression RPAREN
    | fl=literal
    | fi=rhsvalue
    | fn=negation
    ;

  negation
    : NOT factor
    ;

  /********************
    Lexical Defs
  *********************/

  AND
    : 'and'
    ;

  ASSIGN
    : ':='
    ;

  ARRAY
    : 'array'
    ;

  BEGIN
    : 'begin'
    ;

  CASE
    : 'case'
    ;

  COLON
    : ':'
    ;

  COMMA
    : ','
    ;

  CONST
    : 'const'
    ;

  DIV
    : 'div'
    ;

  DIVIDE 
    : '/'
    ;

  DO
    : 'do'
    ;

  DOTDOT
    : '..'
    ;

  DOWNTO
    : 'downto'
    ;

  ELSE
    : 'else'
    ;

  END
    : 'end'
    ;

  EQUALS
    : '='
    ;

  FOR
    : 'for'
    ;

  GREATERTHAN 
    : '>'
    ;

  GREATERTHANOREQUALTO 
    : '>='
    ;

  IF
    : 'if'
    ;

  IN
    : 'in'
    ;

  LBRACE
    : '{'
    ;

  LESSTHAN
    : '<'
    ;

  LBRACKET
    : '['
    ;

  LESSTHANOREQUALTO 
    : '<='
    ;

  LPAREN
    : '('
    ;

  LSHIFT
    : '<<'
    ;

  MINUS
    : '-'
    ;

  MOD 
    : 'mod'
    ;
  
  NOTEQUALTO
    : '<>'
    ;

  OF
    : 'of'
    ;

  OR
    : 'or'
    ;

  NOT
    : 'not'
    ;

  PERIOD
    : '.'
    ;

  PLUS
    : '+'
    ;

  PRINTF
    : 'printf'
    ;

  PROGRAM
    : 'program'
    ;

  RBRACE
    : '}'
    ;

  RBRACKET
    : ']'
    ;

  REPEAT
    : 'repeat'
    ;

  RPAREN
    : ')'
    ;

  RSHIFT
    : '>>'
    ;

  SEMICOLON
    : ';'
    ;

  SET
    : 'set'
    ;

  SYMMETRIC_DIFFERENCE
    : '><'
    ;

  THEN
    : 'then'
    ;

  TIMES 
    : '*'
    ;

  TO
    : 'to'
    ;

  TYPE
    : 'type'
    ;

  UNTIL
    : 'until'
    ;

  VAR
    : 'var'
    ;

  WHILE
    : 'while'
    ;

  CHARLITERAL
	  :	'\'' SingleCharacter '\''
	  |	'\'' EscapeSequence '\''
	  ;

  fragment 
  SingleCharacter
	  :	~['\\\r\n]
	  ;

  fragment 
  EscapeSequence
	  : '\\' [btnfr"'\\]
    ;

  DECIMALINTEGERLITERAL
    : [1-9][0-9]*
    ;

  HEXADECIMALINTEGERLITERAL
    : '0' [xX] [0-9a-fA-F]+
 
    /* Replace this comment with the definition for a hexadecimal literal which
       starts with a 0,
       followed by either an upper or lower case x
       followed by 1 or more of (either 0-9 or upper case a-f or lower case a-f) */
    ;

  OCTALINTEGERLITERAL
    : '0' [0-7]+
    /* Replace this comment with the definition for an octal literal which
       starts with a 0,
       followed by 0 or more digits from 0-7 */
    ;
    
  STRINGLITERAL
    : UnterminatedStringLiteral '"'
    ;

  IDENT
    : [a-zA-Z] [a-zA-Z0-9_]*
    /* Replace this comment with the definition for an IDENT (identifier) which
       starts with an upper or lower case letter
       followed by 0 or more letters (upper or lower) digits or underscores */    
    ;

  UnterminatedStringLiteral
    : '"' (~["\\\r\n] | '\\' (. | EOF))*
    ;

  COMMENT 
    : '{' ~[}]* '}' -> skip 
    ;

  WS
    : [ \r\n\t] + -> skip
    ;
