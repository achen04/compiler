import java_cup.runtime.*;
%%
%class Lexer
%unicode
%cup
%line
%column


%{
  StringBuffer string = new StringBuffer();

  private Symbol symbol(int type) {
    return new Symbol(type, yyline, yycolumn);
  }
  private Symbol symbol(int type, Object value) {
    return new Symbol(type, yyline, yycolumn, value);
  }
%}

LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]
WhiteSpace     = {LineTerminator} | [ \t\f]

// Comments
LineComment = "#".* {LineTerminator}
MultiLineComment = "/#".*{LineTerminator}*"#/"
Comment = {LineComment} | {MultiLineComment}

// Identifiers
Letter = [a-zA-Z] // Matches one letter from A-Z in lowercase and uppercase
Digit = [0-9]
Identifier = {Letter}({Letter}|{Digit}|"_")*

// Character
Space = \s
ASCII = [!\#$%&\'()\*\+,-\.\\/:;<=>?@[]\^_`{}~|] // check again later
Character = "'"({Letter}|{Digit}|{Space}|{ASCII})"'"

// String literals
charsString = {Letter}|{Digit}|{Space}|{ASCII}
StringLit = "\""{charsString}*"\""


// Boolean
Boolean = ("T"|"F")

// Numbers
Zero = 0
PositiveInteger = [1-9][0-9]*
NegativeInteger = "-"{PositiveInteger}
Integer = {PositiveInteger} | {NegativeInteger} | {Zero}
Rational = ({Integer}"/"{PositiveInteger}) | (({PositiveInteger}|{NegativeInteger})"_"{Integer}"/"{PositiveInteger})
FloatLit = ({Digit}+"."{Digit}+)|("-"{Digit}+"."{Digit}+)


// Lambda expressions TODO: PUT IN PARSER INSTEAD
//Lambda ="|"{Identifier}+(","{Identifier}+)*"|->"

%state STRING

%%

<YYINITIAL> {
    // Primitive data types
    "char"      { return symbol(sym.CHAR); }
    "bool"      { return symbol(sym.BOOL); }
    "int"       { return symbol(sym.INT); }
    "rat"       { return symbol(sym.RAT); }
    "float"     { return symbol(sym.FLOAT); }
    "top"       { return symbol(sym.TOP); }
    "string"    { return symbol(sym.STRING); }

    // Aggregate data types
    "dict"      { return symbol(sym.DICT); }
    "seq"       { return symbol(sym.SEQ); }
    "set"       { return symbol(sym.SET); }

    // Other data types
    "thread"    { return symbol(sym.THREAD); }
    "function"  { return symbol(sym.FUNCTION); }


    // Declarations
    "tdef"      { return symbol(sym.TDEF); }
    "alias"     { return symbol(sym.ALIAS); }
    "fdef"      { return symbol(sym.FDEF); }
    "main"      { return symbol(sym.MAIN); }

    // {Lambda}    { return symbol(sym.LAMBDA); } I think this is in the parser

    // Boolean Operators
    "!"         { return symbol(sym.NOT); }
    "&&"        { return symbol(sym.AND); }
    "||"        { return symbol(sym.OR); }

    // Numeric Operators
    "+"         { return symbol(sym.PLUS); }
    "-"         { return symbol(sym.MINUS); }
    "*"         { return symbol(sym.TIMES); }
    "/"         { return symbol(sym.DIVIDE); }
    "^"         { return symbol(sym.POWER); }


    // Aggregate data types operators
    "len"       { return symbol(sym.LEN); }
    "in"        { return symbol(sym.IN); }
    "::"        { return symbol(sym.CONCAT); }

    // Set operators
    "|"         { return symbol(sym.UNION); }
    "&"         { return symbol(sym.INTERSECTION); }
    "\\"         { return symbol(sym.DIFFERENCE); }

    // Comparison operators
    "<"         { return symbol(sym.LESSTHAN); }
    "<="        { return symbol(sym.LESSTHANEQ); }
    ">"         { return symbol(sym.GREATERTHAN); }
    ">="        { return symbol(sym.GREATERTHANEQ); }
    "=="        { return symbol(sym.EQEQ); }
    "!="        { return symbol(sym.NOTEQ); }

    // Assignment
    ":="        { return symbol(sym.ASSIGN); }

    // Input and output
    "read"      { return symbol(sym.READ); }
    "print"     { return symbol(sym.PRINT); }

    // Control flow
    "if"        { return symbol(sym.IF); }
    "then"      { return symbol(sym.THEN); }
    "fi"        { return symbol(sym.FI); }
    "else"      { return symbol(sym.ELSE); }
    "elif"      { return symbol(sym.ELSEIF); }
    "while"     { return symbol(sym.WHILE); }
    "do"        { return symbol(sym.DO); }
    "od"        { return symbol(sym.OD); }
    "break"     { return symbol(sym.BREAK); }
    "forall"    { return symbol(sym.FORALL); }
    "return"    { return symbol(sym.RETURN); }

    // Literals
    {Boolean}       { return symbol(sym.BOOLEAN); }
    {Identifier}    { return symbol(sym.IDENTIFIER); }
    {Character}     { return symbol(sym.CHARACTER); }
    {Integer}       { return symbol(sym.INTEGER); }
    {Rational}      { return symbol(sym.RATIONAL); }
    {FloatLit}      { return symbol(sym.FLOAT_LIT); }
    {StringLit}     { return symbol(sym.STRING_LIT); }


    // Other characters
    "("         { return symbol(sym.LPAREN); }
    ")"         { return symbol(sym.RPAREN); }
    "["         { return symbol(sym.LBRACKET); }
    "]"         { return symbol(sym.RBRACKET); }
    "."         { return symbol(sym.DOT); }
    ","         { return symbol(sym.COMMA); }
    ":"         { return symbol(sym.COLON); }
    ";"         { return symbol(sym.SEMICOLON); }
    "{"         { return symbol(sym.LCURLY); }
    "}"         { return symbol(sym.RCURLY); }


    // Comments
    {Comment}                      { /* ignore */ }

    // White space
    {WhiteSpace}                   { /* ignore */ }

}
/*
<STRING> {
  \"                             { yybegin(YYINITIAL);
                                   return symbol(sym.STRING_LITERAL,
                                   string.toString()); }
  [^\n\r\"\\]+                   { string.append( yytext() ); }
  \\t                            { string.append('\t'); }
  \\n                            { string.append('\n'); }

  \\r                            { string.append('\r'); }
  \\\"                           { string.append('\"'); }
  \\                             { string.append('\\'); }
} */

/* error fallback */
[^]                              { throw new Error("Line " + yyline+1 + ", Column " + yycolumn + " => Illegal character <" + yytext() + ">"); }

