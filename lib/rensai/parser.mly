%token EOF
%token <int> INT
%token <int32> INT32
%token <int64> INT64
%token <float> FLOAT
%token <string> STRING
%token <string> ATOM
%token TRUE
%token FALSE
%token NULL
%token OPEN_OBJ
%token CLOSE_OBJ
%token OPEN_LIST
%token CLOSE_LIST
%token OPEN_PARENS
%token CLOSE_PARENS
%token COLON
%token SEMICOLON
%token COMMA
%start <Ast.t option> main
%%

main:
  | v = value { Some v }
  | EOF       { None   }
;

value:
  | i = INT32                               { Ast.int32 i    }
  | i = INT64                               { Ast.int64 i    }
  | i = INT                                 { Ast.int i      }
  | f = FLOAT                               { Ast.float f    }
  | s = STRING                              { Ast.string s   }
  | TRUE                                    { Ast.bool true  }
  | FALSE                                   { Ast.bool false }
  | NULL                                    { Ast.null ()    }
  | OPEN_PARENS; CLOSE_PARENS               { Ast.unit ()    }
  | OPEN_OBJ; obj = obj_fields; CLOSE_OBJ   { Ast.record obj }
  | OPEN_LIST; l = list_fields; CLOSE_LIST  { Ast.hlist l    }
  | OPEN_PARENS; a = value; COMMA; b = value; CLOSE_PARENS { Ast.lpair a b }
;

obj_fields:
  | obj = separated_list(SEMICOLON, obj_field) { obj }
;

obj_field:
  | k = ATOM; COLON; v = value { (k, v) }
;

list_fields:
  | v = separated_list(COMMA, value) { v }
;
