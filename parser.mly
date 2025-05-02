%{
  open LMJ
  let swap = List.map (fun (x, y) -> (y, x))
%}

%token <int32> INT_CONST
%token <string> STRING_CONST
%token <bool> BOOL_CONST
%token <float> FLOAT_CONST
%token INTEGER BOOLEAN FLOAT
%token <string Location.t> IDENT
%token CLASS PUBLIC STATIC VOID MAIN STRING EXTENDS RETURN BREAK
%token PLUS MINUS TIMES DIV NOT LT OR AND GT EQUALS MOD NEQUALS BITAND BITOR BITXOR LSHIFT RSHIFT BITCOMP
%token COMMA SEMICOLON
%token ASSIGN
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token THIS NEW DOT LENGTH
%token SYSO
%token IF ELSE WHILE FOR DO
%token EOF

%nonassoc NOELSE
%nonassoc ELSE 
%left EQUALS NEQUALS
%left OR
%left AND
%nonassoc BITOR
%nonassoc BITXOR
%nonassoc BITAND
%nonassoc LT GT
%nonassoc LSHIFT RSHIFT
%left PLUS MINUS
%left TIMES DIV MOD

%nonassoc NOT BITCOMP
%nonassoc DOT LBRACKET

%start program

%type <LMJ.program> program

%%

program:
| m = main_class d = defs EOF
   {
     let c, a, i = m in
     {
       name = c;
       defs = d;
       main_args = a;
       main = i
     }
   }

main_class:
| CLASS c = IDENT
   LBRACE
   PUBLIC STATIC VOID MAIN LPAREN STRING LBRACKET RBRACKET a = IDENT RPAREN
   LBRACE
   i = list(instruction)
   RBRACE
   RBRACE
   { (c, a, IBlock i) }

defs:
| c = list(clas)
   { c }

clas:
| CLASS name = IDENT e = option(preceded(EXTENDS, IDENT))
   LBRACE
   a = list(pair(typ, terminated(IDENT, SEMICOLON)))
   m = list(metho)
   RBRACE
   {
     name,
     {
       extends = e;
       attributes = swap a;
       methods = m;
     }
   }

metho:
| PUBLIC t = typ name = IDENT
   LPAREN
   f = separated_list(COMMA, pair(typ, IDENT))
   RPAREN
   LBRACE
   ds = declarations_and_statements
   RETURN e = expression SEMICOLON
   RBRACE
   {
     let d, s = fst ds, snd ds in
     name,
     {
       formals = swap f;
       result  = t;
       locals  = d;
       body    = s;
       return  = e;
     }
   }

declarations_and_statements:
| t = typ id = IDENT SEMICOLON r = declarations_and_statements
   {
     let d, s = r in
     ((id, t) :: d, s)
   }
| t = typ id = IDENT ASSIGN e = expression SEMICOLON r = declarations_and_statements
   {
     let d, s = r in
     ((id, t) :: d, ISetVar(id, e) :: s)
   }
| s = list(instruction)
   { ([], s) }

expression:
|  e = raw_expression
   { Location.make $startpos $endpos e }
| LPAREN e = expression RPAREN
   { e }

raw_expression:
| i = INT_CONST
   { EConst (ConstInt i) }

| f = FLOAT_CONST
   { EConst (ConstFloat f) }

| s = STRING_CONST
   { EConst (ConstString s)}

| b = BOOL_CONST
   { EConst (ConstBool b) }

| id = IDENT
   { EGetVar id }

| e1 = expression op = binop e2 = expression
   { EBinOp (op, e1, e2) }

| o = expression DOT c = IDENT LPAREN actuals = separated_list(COMMA, expression) RPAREN
   { EMethodCall (o, c, actuals) }

| a = expression LBRACKET i = expression RBRACKET
   { EArrayGet (a, i) }

| NEW INTEGER LBRACKET e = expression RBRACKET
   { EArrayAlloc e }

| NEW STRING LBRACKET e = expression RBRACKET
  { EStringArrayAlloc e }

| a = expression DOT LENGTH
   { EArrayLength a }

| THIS
   { EThis }

| NEW id = IDENT LPAREN RPAREN
   { EObjectAlloc id }

| NOT e = expression
   { EUnOp (UOpNot, e) }
| BITCOMP e = expression
   { EUnOp (UOpBitComp, e) }

%inline binop:
| EQUALS { OpEq }
| PLUS  { OpAdd }
| MINUS { OpSub }
| DIV   { OpDiv }
| MOD   { OpMod }
| TIMES { OpMul }
| LT    { OpLt }
| GT    { OpGt }
| OR    { OpOr }
| AND   { OpAnd }
| NEQUALS { OpNeq }
| BITAND { OpBitAnd }
| BITOR  { OpBitOr }
| BITXOR { OpBitXor }
| LSHIFT { OpLShift }
| RSHIFT { OpRShift }

expr_pair:
  | e1 = expression ASSIGN e2 = expression { (e1, e2) }

instruction:
| b = block
   { b }

| id = IDENT ASSIGN e = expression SEMICOLON
   { ISetVar (id, e) }

| a = IDENT LBRACKET i = expression RBRACKET ASSIGN e = expression SEMICOLON
   { IArraySet (a, i, e) }

| SYSO LPAREN e = expression RPAREN SEMICOLON
   { ISyso e }

| IF LPAREN c = expression RPAREN i1 = instruction ELSE i2 = instruction
   { IIf (c, i1, i2) }

| IF LPAREN c = expression RPAREN i = instruction %prec NOELSE
   { IIfS (c, i) }

| WHILE LPAREN c = expression RPAREN i = instruction
   { IWhile (c, i) }

| FOR LPAREN id1 = IDENT ASSIGN e1 = expression SEMICOLON c = expression SEMICOLON id2 = IDENT ASSIGN e2 = expression RPAREN i3 = instruction
    { IFor (ISetVar (id1, e1), c, ISetVar (id2, e2), i3) }

| DO i = instruction WHILE LPAREN c = expression RPAREN SEMICOLON
   { IDoWhile (i, c) }

| BREAK SEMICOLON
   { IBreak }

block:
| LBRACE is = list(instruction) RBRACE
   { IBlock is }

typ:
| INTEGER
   { TypInt }
| STRING
   { TypString }
| FLOAT
   { TypFloat }
| BOOLEAN
   { TypBool }
| INTEGER LBRACKET RBRACKET
   { TypIntArray }
| STRING LBRACKET RBRACKET
   { TypStringArray }
| id = IDENT
   { Typ id }
