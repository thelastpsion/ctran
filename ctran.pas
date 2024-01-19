{$mode objfpc}{$H+}{$J-}
program ctran;

// uses fgl;
uses
    sysutils;

type
    TokenType = string;
    Token = record
        TType : TokenType;
        Literal : string;
    end;
    // TokenDict = specialize TFPGmap<TokenType, string>;
    TokenArray = array of Token;

    TTokenType = (
        // Breaks
        tknEOF,
        tknNewline,
        tknSemicolon,

        // Symbols
        tknParenLeft,
        tknParenRight,
        tknBraceLeft,
        tknBraceRight,
        tknBang,

        // Category type keywords
        tknImage,
        tknLibrary,

        // External file inclusion keywords
        tknExternal,
        tknInclude,
        tknRequire,

        // Class keyword
        tknClass,

        // Method declaration keywords
        tknAdd,
        tknReplace,
        tknDefer,

        // Other class-related keywords
        tknProperty,
        tknTypes,
        tknConstants
    );

    TMyLexer = class
        LineNo, curpos : Integer;
        content : String;
        flagEOF : Boolean;
        curchar : string[1];
        constructor Create(str : string);
        procedure FindStartOfLine;
        procedure GetNextGlyph;
    end;

const
    EOF = 'EOF';
    NEWLINE = 'NEWLINE';
    SEMICOLON = ';';

    ILLEGAL = 'ILLEGAL';
    IDENT = 'IDENT';
    PROPERTYTEXT = 'PROPERTYTEXT';

    LPAREN = '(';
    RPAREN = ')';
    LBRACE = '{';
    RBRACE = '}';
    BANG = '!';

    IMAGE = 'IMAGE';
    LIBR = 'LIBRARY';
    INCEXT = 'EXTERNAL';
    INCLUDE = 'INCLUDE';
    PCLASS = 'CLASS';
    REQUIRE = 'REQUIRE';

    ADD = 'ADD';
    REPLACE = 'REPLACE';
    DEFER = 'DEFER';

    PROP = 'PROPERTY';
    
    TYPES = 'TYPES';
    CONSTANTS = 'CONSTANTS';

// procedure MakeDict();
// begin
//     dict := TokenDict.Create;

//     dict.add('EOF',aur 'EOF');
//     dict.add('NEWLINE', 'NEWLINE');
//     Writeln('Called fine.');
// end;

constructor TMyLexer.Create(str : String);
begin
    inherited Create;
    LineNo := 0;
    curpos := 0;
    content := str;
    flagEOF := false;
    curchar := '';
end;

//TODO: Should this be a function that returns a char, or should it just put values into variables inside the class?
procedure TMyLexer.GetNextGlyph();
var
    nextpos : Integer;
    nextchar : char;
begin
    nextpos := curpos + 1;

    if curpos > length(content) then
    begin
        flagEOF := true;
        curchar := '';
        Exit();
    end;

    nextchar := content[nextpos];
    inc(curpos);
end;

procedure TMyLexer.FindStartOfLine();
var
    ch : Char;
begin
    ch := content[curpos];
end;

procedure PrintTestArray(tokenArray: TokenArray);
var
    i: Integer;
begin
    Writeln(' Token      | Literal');
    Writeln('------------|---------');
    for i := 0 to Length(tokenArray) - 1 do
    begin
        Writeln(' ' + tokenArray[i].TType.PadRight(10) + ' | ' + tokenArray[i].Literal);
    end;
    Writeln;
    Writeln('Length: ', Length(tokenArray));
end;

// TODO: Use a pointer to the array?
// procedure AddToken(var tokenArray: TokenArray; newTokenType: TokenType; newTokenLiteral: String);
// var
//     newToken: Token;
// begin
//     newToken.TType := newTokenType;
//     newToken.Literal := newTokenLiteral;
//     tokenArray := concat(tokenArray, [newToken]);
// end;

// Takes a TokenType and a String and puts it into a Token record
function NewToken(newTokenType: TokenType; newTokenLiteral: String): Token;
begin
    NewToken.TType := newTokenType;
    NewToken.Literal := newTokenLiteral;
end;

function IsValidLetter(ch: Char): Boolean;
begin
    //Result := (((ord(ch) >= 97) and (ord(ch) <= 122)) or ((ord(ch) >= 65) and (ord(ch) <= 90)) or (ch = '_'));
    Result := ((LowerCase(ch) in ['a' .. 'z']) or (ch = '_'));
end;

procedure TestTokeniser();
var
    input: String = '(){}!';
    tests: TokenArray;
    i: Integer;
    subject: String;
begin
    tests := [NewToken(LPAREN, '('),
              NewToken(RPAREN, ')'),
              NewToken(LBRACE, '{'),
              NewToken(RBRACE, '}'),
              NewToken(BANG, '!'),
              NewToken(EOF, '')
    ];

    PrintTestArray(tests);

    for i := 0 to Length(input) - 1 do
    begin
        subject := input.Substring(i, 1);
        if subject = tests[i].Literal then
        begin
            Writeln('OK');
        end
        else
        begin
            Writeln('Nope!')
        end;
    end;
    
    i := length(input);
    // Writeln('i is now ' + i.ToString());

    if i > Length(tests) then
    begin
        Writeln('Reached end before EOF - i = ' + i.ToString());
    end;

    if tests[i].Literal = '' then
    begin
        Writeln('EOF found.');
    end
    else
    begin
        Writeln('EOF not found.')
    end;

    WriteLn(IsValidLetter('_'));
    WriteLn(IsValidLetter('a'));
    WriteLn(IsValidLetter('1'));

end;

begin
    TestTokeniser();
end.

