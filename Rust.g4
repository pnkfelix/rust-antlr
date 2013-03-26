grammar Rust;

start : item ;

Ident            : Letter (Letter | JavaIDDigit)* ;

fragment
XID_start : Letter ;
fragment
XID_continue : Letter | JavaIDDigit ;

// Delimiter-restricted productions

Non_null         : ~'\u0000' ;
Non_eol          : ~'\n'     ;
Non_star         : ~'*'      ;
Non_slash        : ~'/'      ;
Non_single_quote : ~'\''     ;
Non_double_quote : ~'\"'     ;

// Comments

Comment : Block_comment | Line_comment ;
Block_comment : '/*' Block_comment_body * '*/' ;
fragment
Block_comment_body : Non_star * | '*' Non_slash ;
Line_comment : '//' Non_eol * ;

// Whitespace

Whitespace_char : '\u0020' | '\u0009' | '\u000a' | '\u000d' ;
Whitespace : ( Whitespace_char | Comment ) + ;

// Tokens

Simple_token : Keyword | Unop | Binop ;
Token : Simple_token | Ident | Literal | Symbol | Whitespace Token ;

// See "Macros" section
Sep_token : Keyword
    | '-' | '!' | '@' | '~' | '&' // unop except * and +
    | '/' | '%' | 'as' | '-'      // binop except * and +
    | '<<' | '>>' | '&' | '^'
    | '|' | '<' | '>' | '<=' | '>='
    | '==' | '!=' | '&&' | '||'
    | '=' | '<->'
    | Compound_assignment 
    | Ident | Literal | Symbol ;

Non_special_token : Keyword
    | Unop | Binop
    | Ident | Literal | Symbol ;

Keyword : 'as' | 'break' | 'const' | 'copy' | 'do' | 'drop'
    | 'else' | 'enum' | 'extern' | 'false' | 'fn' | 'for'
    | 'if' | 'impl' | 'let' | 'log' | 'loop' | 'match'
    | 'mod' | 'mut' | 'priv' | 'pub'| 'pure' | 'ref'
    | 'return' | 'self' | 'static' | 'struct' | 'super'
    | 'true' | 'trait' | 'type' | 'unsafe' | 'use' | 'while' ;


/// Literals

Literal : String_lit | Char_lit | Num_lit ;

//// Character and string literals

Char_lit : '\u0027' Char_body '\u0027' ;
String_lit : '\"' String_body * '\"' ;

Char_body : Non_single_quote
          | '\u005c' ( '\u0027' | Common_escape ) ;

String_body : Non_double_quote
            | '\u005c' ( '\u0022' | Common_escape ) ;

Common_escape : '\u005c'
    | 'n' | 'r' | 't'
    | 'x' Hex_digit Hex_digit                     // 2
    | 'u' Hex_digit Hex_digit Hex_digit Hex_digit // 4
    | 'U' Hex_digit Hex_digit Hex_digit Hex_digit 
          Hex_digit Hex_digit Hex_digit Hex_digit // 8 
    ;

Hex_digit : 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
          | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
          | Dec_digit ;
Dec_digit : '0' | Nonzero_dec ;
Nonzero_dec: '1' | '2' | '3' | '4'
           | '5' | '6' | '7' | '8' | '9' ;

//// Number literals

Num_lit : Nonzero_dec ( Dec_digit | '_' ) * Num_suffix ?
        | '0' (       ( Dec_digit | '_' ) + Num_suffix ?
              | 'b'   ( '1' | '0' | '_' ) + Int_suffix ?
              | 'x'   ( Hex_digit | '_' ) + Int_suffix ? ) ;

Num_suffix : Int_suffix | Float_suffix ;

Int_suffix : 'u' Int_suffix_size ?
           | 'i' Int_suffix_size ;
Int_suffix_size : ( '8' | '1' '6' | '3' '2' | '6' '4' ) ;

Float_suffix : ( Exponent | '.' Dec_lit Exponent ? ) Float_suffix_ty ? ;
Float_suffix_ty : 'f' ( '3' '2' | '6' '4' ) ;
Exponent : ('E' | 'e') ('-' | '+' ) ? Dec_lit ;
Dec_lit : ( Dec_digit | '_' ) + ;

///// Unit and boolean literals

Symbol : '::' '->'
       | '#' | '[' | ']' | '(' | ')' | '{' | '}'
       | ',' | ';' ;

expr_path : Ident ( '::' expr_path_tail ) + ;
expr_path_tail : '<' type_path ( ',' type_path ) + '>'
               | expr_path ;

type_path : Ident ( type_path_tail ) + ;
type_path_tail : '<' type_path ( ',' type_path ) + '>'
               | '::' type_path ;

type : type_expr ;
type_expr : type_path ;

// Macros

expr_macro_rules : 'macro_rules' '!' Ident '(' macro_rule * ')' ;
macro_rule : '(' matcher * ')' '=>' '(' transcriber * ')' ';' ;
matcher : '(' matcher * ')' | '[' matcher * ']'
        | '{' matcher * '}' | '$' Ident ':' Ident
        | '$' '(' matcher * ')' Sep_token? ( '*' | '+' )
        | Non_special_token ;
transcriber : '(' transcriber * ')' | '[' transcriber * ']'
            | '{' transcriber * '}' | '$' Ident
            | '$' '(' transcriber * ')' Sep_token? ( '*' | '+' )
            | Non_special_token ;

// Items

item : mod_item | fn_item | type_item | struct_item | enum_item
     | const_item | trait_item | impl_item | foreign_mod_item ;

/// Modules

mod_item : 'mod' Ident ( ';' | '{' mod '}' ) ;
mod : ( view_item | item ) * ;

//// View items

view_item : extern_mod_decl | use_decl ;

///// Extern mod declarations

extern_mod_decl : 'extern' 'mod' Ident ( '(' link_attrs ')' ) ? ;
link_attrs : link_attr ( ',' link_attrs ) + ;
link_attr : Ident '=' Literal ;


///// Use declarations

use_decl : 'pub'? 'use' Ident ( '=' path_glob
                          | '::' path_glob ) ;

path_glob : Ident ( '::' path_glob ) ?
          | '*'
          | '{' Ident ( ',' Ident ) * '}' ;

/// Functions

fn_item : fn_decl | fn_defn ;

fn_decl : fn_header ';' ;
fn_defn : fn_header '{' block '}' ;

fn_header : 'fn' Ident '(' ( Ident ':' type
        ( ',' Ident ':' type ) * )? ')'
        ( '->' type ) ? ;

/// Type definitions

type_item : 'type' Ident '=' type_path ';' ;

/// Structures

struct_item : 'struct' Ident
        '{' Ident ':' type_path
        ( ',' Ident ':' type_path )* '}' ;

/// Enumerations

enum_item : 'enum' Ident
        '{' enum_variant ( ',' enum_variant )* '}' ;
enum_variant : Ident '(' type_path (',' type_path )* ')'
    | Ident '{' Ident ':' type_path
        (',' Ident ':' type_path ) '}' ;

/// Constants

const_item : 'const' Ident ':' type '=' expr ';' ;

/// Traits

trait_item : 'trait' Ident '{' fn_decl + '}' ;

/// Implementations

impl_item : 'impl' ( '<' Ident (',' Ident)* '>' )?
        Ident      ( '<' type_path (',' type_path)* '>' )?
        'for'
        Ident      ( '<' type_path (',' type_path)* '>' )?
        '{' fn_defn+ '}' ;

/// Foreign modules

foreign_mod_item : 'extern' 'mod' Ident '{' foreign_mod '}' ;
foreign_mod : ( foreign_fn ) * ;
foreign_fn : fn_decl ;

// Attributes

attribute : '#' '[' attr_list ']' ;
attr_list : attr ( ',' attr_list )* ;
attr : Ident ( '=' Literal
             | '(' attr_list ')' ) ? ;

// Statements

block : statement* ;

statement : decl_stmt | expr_stmt ;

/// Declaration statements

decl_stmt : item_decl | let_decl ;

//// Item declarations

item_decl : item ;

//// Slot declarations

let_decl : 'let' pat (':' type ) ? ( init ) ? ';' ;
init : ( '=' ) expr ;

expr_stmt : expr ';' ;

expr : // lvalue_expr
        expr_path
    | expr '.' Ident    // field_expr
    | expr '[' expr ']' // idx_expr
    | Unop expr         // unop_expr
    | expr Binop expr   // binop_expr 
        // rvalue_expr  
    | path_expr '=' expr // path_expr assign_expr
    | expr '.' Ident '=' expr // field_expr assign_expr
    | expr '[' expr ']' '=' expr // idx_expr assign_expr
    | literal_expr
    | tuple_expr
    | struct_expr
    | rec_expr
    | expr '.' Ident paren_expr_list // method_call_expr
    | vec_expr
    | path_expr
    ;

/// Literal expressions

literal_expr : Literal ;

/// Path expressions

path_expr : expr_path ;

/// Tuple expressions

tuple_expr : '(' expr ( ',' expr )* ')' ;

/// Structure expressions

struct_expr : expr_path '{' Ident ':' expr
        (',' Ident ':' expr ) *
        ( '..' expr ) '}' 
    | expr_path '(' expr 
        ( ',' expr ) * ')' 
    | expr_path ;

/// Record expressions

rec_expr : '{' Ident ':' expr
        ( ',' Ident ':' expr ) *
        ( '..' expr ) '}' ;

/// Method-call expressions

method_call_expr : expr '.' Ident paren_expr_list ;

/// Field expressions

/// Vector expressions

vec_expr : '[' 'mut'? vec_elems? ']' ;

vec_elems : (expr (',' expr)* ) | ( expr ',' '..' expr ) ;

/// Index expressions

/// Unary operator expressions

Unop : '-' | '*' | '!' | '@' | '~' | '&' ;

unop_expr : Unop expr ;

/// Binary operator expressions

//// Arithmetic operators

Binop : '*' | '/' | '%'
    | 'as' 
    | '+' | '-'
    | '<<' | '>>'
    | '&'
    | '^'
    | '|'
    | '<'
    | '>'
    | '<='
    | '>='
    | '=='
    | '!='
    | '&&'
    | '||'
    | '='
    | '<->'
    | Compound_assignment
    ;

//// Bitwise operators
//// Lazy boolean operators
//// Comparison operators
//// Type cast expressions
//// Swap expressions
//// Assignment expressions

//// Compound assignment expressions

Compound_assignment : '+=' | '-=' | '*=' | '/=' | '%='
    | '&=' | '|=' | '^=' | '<<=' | '>>=' ;

// The precedence of Rust binary operators is ordered as follows,
// going from strong to weak:
// 
// ~~~~ {.precedence}
// * / %
// as
// + -
// << >>
// &
// ^
// |
// < > <= >=
// == !=
// &&
// ||
// = <->
// ~~~~


/// Grouped expressions

paren_expr : '(' expr ')' ;

/// Unary copy expressions

copy_expr : 'copy' expr ;

/// Unary move expressions

move_expr : 'move' expr ;

/// Call expressions

expr_list : ( expr ( ',' expr )* ) ? ;
paren_expr_list : '(' expr_list ')' ;
call_expr : expr paren_expr_list ;

/// Lambda expressions

ident_list : ( Ident ( ',' Ident )* ) ? ;
lambda_expr : '|' ident_list '|' expr ;

/// While loops

while_expr : 'while' expr '{' block '}' ;

/// Infinite loops

loop_expr : 'loop' ( Ident ':' ) ? '{' block '}' ;

/// Break expressions

break_expr : 'break' ( Ident ) ?;

/// Continue expressions

continue_expr : 'loop' ( Ident ) ?;

/// Do expressions

do_expr : 'do' expr ( '|' ident_list '|' ) ? '{' block '}' ;

/// For expressions

for_expr : 'for' expr ( '|' ident_list '|' ) ? '{' block '}' ;

/// If expressions

if_expr : 'if' expr '{' block '}'
          else_tail ? ;

else_tail : 'else' ( if_expr
                   | '{' block '}' ) ;


/// Match expressions

match_expr : 'match' expr '{' match_arm ( '|' match_arm ) * '}' ;

match_arm : match_pat '=>' ( expr ',' | '{' block '}' ) ;

match_pat : pat ( '..' pat ) ? ( 'if' expr ) ;

pat : Literal | '_' | '*'
    | Ident '(' pat (',' pat )* ')'
    | Ident '{' Ident ':' pat ( ',' Ident ':' pat )* '}'
    | '{' Ident ':' pat ( ',' Ident ':' pat )* '}'
    ;

/// Return expressions

return_expr : 'return' expr ? ;

/// Log expressions

log_expr : 'log' '(' level ',' expr ')' ;

level : expr_path | Num_lit ;

// Below taken from Java.g4 on ANTLR examples (on github?)
// Someone should review/verify it for consistency with XID_start XID_continue

// In particular, FSK sees '$' in Letter and says: really???
// In fact, FSK is going to just remove that.

/**I found this char range in JavaCC's grammar, but Letter and Digit overlap.
   Still works, but...
 */
  // '\u0024' |               // $
fragment
Letter
    : '\u0041'..'\u005a' |     // A-Z
       '\u005f' |               // _  
       '\u0061'..'\u007a' |     // a-z
       '\u00c0'..'\u00d6' |     // Latin Capital Letter A with grave - Latin Capital letter O with diaeresis
       '\u00d8'..'\u00f6' |     // Latin Capital letter O with stroke - Latin Small Letter O with diaeresis
       '\u00f8'..'\u00ff' |     // Latin Small Letter O with stroke - Latin Small Letter Y with diaeresis
       '\u0100'..'\u1fff' |     // Latin Capital Letter A with macron - Latin Small Letter O with stroke and acute
       '\u3040'..'\u318f' |     // Hiragana
       '\u3300'..'\u337f' |     // CJK compatibility
       '\u3400'..'\u3d2d' |     // CJK compatibility
       '\u4e00'..'\u9fff' |     // CJK compatibility
       '\uf900'..'\ufaff'       // CJK compatibility
    ;

fragment
JavaIDDigit
    :  '\u0030'..'\u0039' |     // 0-9
       '\u0660'..'\u0669' |     // Arabic-Indic Digit 0-9
       '\u06f0'..'\u06f9' |     // Extended Arabic-Indic Digit 0-9
       '\u0966'..'\u096f' |     // Devanagari 0-9
       '\u09e6'..'\u09ef' |     // Bengali 0-9
       '\u0a66'..'\u0a6f' |     // Gurmukhi 0-9
       '\u0ae6'..'\u0aef' |     // Gujarati 0-9
       '\u0b66'..'\u0b6f' |     // Oriya 0-9
       '\u0be7'..'\u0bef' |     // Tami 0-9
       '\u0c66'..'\u0c6f' |     // Telugu 0-9
       '\u0ce6'..'\u0cef' |     // Kannada 0-9
       '\u0d66'..'\u0d6f' |     // Malayala 0-9
       '\u0e50'..'\u0e59' |     // Thai 0-9
       '\u0ed0'..'\u0ed9' |     // Lao 0-9
       '\u1040'..'\u1049'       // Myanmar 0-9?
   ;

WS  :  [ \r\t\u000C\n]+ -> channel(HIDDEN)
    ;

COMMENT
    :   '/*' .*? '*/' -> channel(HIDDEN)
    ;

LINE_COMMENT
    : '//' ~[\r\n]* ('\r'? '\n' | EOF) -> channel(HIDDEN)
    ;
