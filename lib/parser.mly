%token EOF
%token BOOL
%token BREAK
%token CONTINUE
%token ELSE
%token FALSE
%token FOR
%token IF
%token INT
%token NULL
%token RETURN
%token SIZEOF
%token TRUE
%token VOID
%token WHILE
%token <string> IDENT
%token <int> LIT
%token <string> INCLUDE
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token COMMA
%token SEMICOLON
%token PLUS_PLUS
%token PLUS
%token MINUS_MINUS
%token MINUS
%token STAR
%token SLASH
%token PERCENT
%token AMPERSAND_AMPERSAND
%token AMPERSAND
%token BAR_BAR
%token EXCLAMATION_EQUALS
%token EXCLAMATION
%token EQUALS_EQUALS
%token EQUALS
%token LESS_EQUALS
%token LESS
%token GREATER_EQUALS
%token GREATER

%start <unit> p

%%

p:
  | EOF { () }
