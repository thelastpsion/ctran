{$mode objfpc}{$H+}{$J-}
program ctran;

// uses fgl;
uses
    sysutils, Classes;

type
//    TokenType = string;
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
        tknConstants,

        tknString
    );

//    TokenDict = specialize TDictionary<TTokenType, string>;

    TToken = record
        TType : TTokenType;
        Literal : string;
        LineNum : Integer;
    end;
    TTokenArray = array of TToken;


    TLexerState = (
        stateInitial,
        stateSeekExtIncClass,
        stateClassSeekStart,
        stateClass,
        stateClassProperty,
        stateClassTypes,
        stateClassConstants,
        stateClassConstantsSeekStart,
        stateClassTypesSeekStart,
        stateClassPropertySeekStart,
        stateSeekClassOrRequire,
        stateSeekRequire
    );

//    TMyLexer = class
//        LineNo, curpos : Integer;
//        content : String;
//        flagEOF : Boolean;
//        curchar : string[1];
//        constructor Create(str : string);
//        procedure FindStartOfLine;
//        procedure GetNextGlyph;
//    end;

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

//constructor TMyLexer.Create(str : String);
//begin
//    inherited Create;
//    LineNo := 0;
//    curpos := 0;
//    content := str;
//    flagEOF := false;
//    curchar := '';
//end;

//TODO: Should this be a function that returns a char, or should it just put values into variables inside the class?
//procedure TMyLexer.GetNextGlyph();
//var
//    nextpos : Integer;
//    nextchar : char;
//begin
//    nextpos := curpos + 1;
//
//    if curpos > length(content) then
//    begin
//        flagEOF := true;
//        curchar := '';
//        Exit();
//    end;
//
//    nextchar := content[nextpos];
//    inc(curpos);
//end;
//
//procedure TMyLexer.FindStartOfLine();
//var
//    ch : Char;
//begin
//    ch := content[curpos];
//end;

var
    boolExternal : Boolean = false;
    strExternal : String;
    boolGenG : Boolean = false;
    strGenG : String;
    strFilename : String;
    Tokenised : TTokenArray;

procedure PrintTestArray(tokenArray: TTokenArray);
var
    i: Integer;
    s: String;
begin
    Writeln(' Line | Token Type    | Literal');
    Writeln('------+---------------+-------------');
    for i := 0 to Length(tokenArray) - 1 do
    begin
        Str(tokenArray[i].TType, s);
        Writeln(format(' %4d | %-13s | %s', [tokenArray[i].LineNum, s, tokenArray[i].Literal]));
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
function NewToken(newTokenLineNum: Integer; newTokenType: TTokenType; newTokenLiteral: String): TToken;
begin
    NewToken.TType := newTokenType;
    NewToken.Literal := newTokenLiteral;
    NewToken.LineNum := newTokenLineNum;
end;

function IsValidLetter(ch: Char): Boolean;
begin
    //Result := (((ord(ch) >= 97) and (ord(ch) <= 122)) or ((ord(ch) >= 65) and (ord(ch) <= 90)) or (ch = '_'));
    Result := ((LowerCase(ch) in ['a' .. 'z']) or (ch = '_'));
end;

function GetNextToken(curline: String; var StartPos: Integer): String;
var
    curpos : Integer;
    flgFoundText : Boolean = false;
begin
    GetNextToken := '';

    for curpos := StartPos to length(curline) do
    begin
        if curline[curpos] = ' ' then begin
            if flgFoundText then begin
                startpos := curpos;
                exit;
            end;
        end else begin
            GetNextToken := concat(GetNextToken, curline[curpos]);
            flgFoundText := true;
        end;
    end;
    startpos := StartPos + length(GetNextToken);
end;

//procedure TestTokeniser();
//var
//    input: String = '(){}!';
//    tests: TokenArray;
//    i: Integer;
//    subject: String;
//begin
//    tests := [NewToken(LPAREN, '('),
//              NewToken(RPAREN, ')'),
//              NewToken(LBRACE, '{'),
//              NewToken(RBRACE, '}'),
//              NewToken(BANG, '!'),
//              NewToken(EOF, '')
//    ];
//
//    PrintTestArray(tests);
//
//    for i := 0 to Length(input) - 1 do
//    begin
//        subject := input.Substring(i, 1);
//        if subject = tests[i].Literal then
//        begin
//            Writeln('OK');
//        end
//        else
//        begin
//            Writeln('Nope!')
//        end;
//    end;
//    
//    i := length(input);
//    // Writeln('i is now ' + i.ToString());
//
//    if i > Length(tests) then
//    begin
//        Writeln('Reached end before EOF - i = ' + i.ToString());
//    end;
//
//    if tests[i].Literal = '' then
//    begin
//        Writeln('EOF found.');
//    end
//    else
//    begin
//        Writeln('EOF not found.')
//    end;
//
//    WriteLn(IsValidLetter('_'));
//    WriteLn(IsValidLetter('a'));
//    WriteLn(IsValidLetter('1'));
//end;

// Removes everything in a string after the first semicolon
procedure TrimAfterSemicolon(var s: String);
begin
    if ansipos(';', s) > 0 then s := copy(s, 1, ansipos(';', s));
end;

procedure LoadThatFile();
var
    slCategoryFile : TStringList;
    i, x : Integer;
    linepos : Integer;
    curtoken : String;
    grabbedline : String;
    status : TLexerState;
    bracelevel: Integer = 0;
begin
    status := stateInitial;
    slCategoryFile := TStringList.Create;

    slCategoryFile.LoadFromFile(strFilename);

    for i := 0 to slCategoryFile.Count - 1 do
    begin
        linepos := 0;
        curtoken := '';
        grabbedline := slCategoryFile[i].Trim;
        WriteLn(format('%.3d', [i + 1]), ':', grabbedline);

        if length(grabbedline) = 0 then
            Writeln('>>> Empty line')
        else if grabbedline[1] = '!' then
            Writeln('>>> Explicit comment, line skipped')
        else begin
            linepos := 1;
            curtoken := '';
    
            case status of
                stateInitial: begin
                    curtoken := GetNextToken(grabbedline, linepos);
                    case UpCase(curtoken) of 
                        'IMAGE': begin
                            Writeln('>>> IMAGE found!');
                            status := stateSeekExtIncClass;
                            Tokenised := [NewToken(i, tknImage, curtoken)];
                        end;
                        'LIBRARY': begin
                            Writeln('>>> LIBRARY found!');
                            status := stateSeekExtIncClass;
                            Tokenised := [NewToken(i, tknLibrary, curtoken)];
                        end;
                    end;
                    if status = stateSeekExtIncClass then
                    begin
                        WriteLn('>>>   Now in stateSeekExtIncClass');
                        curtoken := GetNextToken(grabbedline, linepos);
                        Writeln('>>> Token grabbed: ', curtoken);
                        Tokenised := concat(Tokenised, [NewToken(i, tknString, curtoken)]);
                    end;
                end;

                stateSeekExtIncClass: begin
                    curtoken := GetNextToken(grabbedline, linepos);
                    case UpCase(curtoken) of
                        'EXTERNAL': begin
                            Writeln('>>> EXTERNAL found!');
                            Tokenised := concat(Tokenised, [NewToken(i, tknExternal, curtoken)]);
                            curtoken := GetNextToken(grabbedline, linepos);
                            Writeln('>>> Token grabbed: ', curtoken);
                            Tokenised := concat(Tokenised, [NewToken(i, tknString, curtoken)]);
                            Writeln('>>>   Do something with EXTERNAL here');
                        end;
                        'INCLUDE': begin
                            Writeln('>>> INCLUDE found!');
                            Tokenised := concat(Tokenised, [NewToken(i, tknInclude, curtoken)]);
                            curtoken := GetNextToken(grabbedline, linepos);
                            Writeln('>>> Token grabbed: ', curtoken);
                            Tokenised := concat(Tokenised, [NewToken(i, tknString, curtoken)]);
                            Writeln('>>>   Do something with INCLUDE here');
                        end;
                        'CLASS': begin
                            Writeln('>>> CLASS found!');
                            Tokenised := concat(Tokenised, [NewToken(i, tknClass, curtoken)]);
                            curtoken := GetNextToken(grabbedline, linepos);
                            Writeln('>>> Token grabbed: ', curtoken);
                            Tokenised := concat(Tokenised, [NewToken(i, tknString, curtoken)]);
                            curtoken := GetNextToken(grabbedline, linepos);
                            Writeln('>>> Token grabbed: ', curtoken);
                            Tokenised := concat(Tokenised, [NewToken(i, tknString, curtoken)]);
                            status := stateClassSeekStart;
                            Writeln('>>>   Now in stateClassSeekStart (looking for brace)');
                        end;
                        'REQUIRE': begin
                            Writeln('!!! REQUIRE found before first CLASS declaration');
                            exit;
                        end;
                    end;
                end;
    
                stateSeekClassOrRequire: begin
                    curtoken := GetNextToken(grabbedline, linepos);
                    case UpCase(curtoken) of
                        'REQUIRE': begin
                            Writeln('>>> REQUIRE found!');
                            Tokenised := concat(Tokenised, [NewToken(i, tknRequire, curtoken)]);
                            curtoken := GetNextToken(grabbedline, linepos);
                            Writeln('>>> Token grabbed: ', curtoken);
                            Tokenised := concat(Tokenised, [NewToken(i, tknString, curtoken)]);
                            Writeln('>>>   Do something with REQUIRE here');
                            status := stateSeekRequire; // After the first REQUIRE, don't allow any more CLASSes
                            Writeln('>>> Now in stateSeekRequire');
                        end;
                        'INCLUDE': begin
                            Writeln('!!! INCLUDE found after first CLASS declaration');
                            exit;
                        end;
                        'EXTERNAL': begin
                            Writeln('!!! EXTERNAL found after first CLASS declaration');
                            exit;
                        end;
                        'CLASS': begin
                            Writeln('>>> CLASS found!');
                            Tokenised := concat(Tokenised, [NewToken(i, tknClass, curtoken)]);
                            curtoken := GetNextToken(grabbedline, linepos);
                            Writeln('>>> Token grabbed: ', curtoken);
                            Tokenised := concat(Tokenised, [NewToken(i, tknString, curtoken)]);
                            curtoken := GetNextToken(grabbedline, linepos);
                            Writeln('>>> Token grabbed: ', curtoken);
                            Tokenised := concat(Tokenised, [NewToken(i, tknString, curtoken)]);
                            status := stateClassSeekStart;
                            Writeln('>>>   Now in stateClassSeekStart (looking for brace)');
                        end;
                    end;
                end;
    
                stateSeekRequire: begin
                    curtoken := GetNextToken(grabbedline, linepos);
                    case UpCase(curtoken) of
                        'REQUIRE': begin
                            Writeln('>>> REQUIRE found!');
                            Tokenised := concat(Tokenised, [NewToken(i, tknRequire, curtoken)]);
                            curtoken := GetNextToken(grabbedline, linepos);
                            Writeln('>>> Token grabbed: ', curtoken);
                            Tokenised := concat(Tokenised, [NewToken(i, tknString, curtoken)]);
                            Writeln('>>>   Do something with REQUIRE here');
                        end;
                        'INCLUDE': begin
                            Writeln('!!! INCLUDE found after REQUIRE declaration');
                            exit;
                        end;
                        'EXTERNAL': begin
                            Writeln('!!! EXTERNAL found after REQUIRE declaration');
                            exit;
                        end;
                        'CLASS': begin
                            Writeln('!!! CLASS found after REQUIRE declaration');
                            exit;
                        end;
                    end;
                end;
    
                stateClassSeekStart: begin
                    if grabbedline[1] = '{' then begin
                        Writeln('>>> Start of CLASS section found!');
                            Tokenised := concat(Tokenised, [NewToken(i, tknBraceLeft, '{')]);
                        status := stateClass;
                        Writeln('>>>   Now in stateClass');
                        inc(bracelevel);
                        Writeln('>>>   Brace level: ', bracelevel);
                    end;
                end;

                stateClass: begin
                    if grabbedline[1] = '}' then begin
                        Tokenised := concat(Tokenised, [NewToken(i, tknBraceRight, '}')]);
                        Writeln('>>> End of CLASS section found!');
                        dec(bracelevel);
                        Writeln('>>>   Brace level: ', bracelevel);
                        status := stateSeekClassOrRequire;
                        Writeln('>>> Now in stateSeekClassOrRequire');
                    end else begin
                        curtoken := GetNextToken(grabbedline, linepos);
                        case curtoken of
                            'ADD': begin
                                Writeln('>>> ADD found!');
                                Tokenised := concat(Tokenised, [NewToken(i, tknAdd, curtoken)]);
                                curtoken := GetNextToken(grabbedline, linepos);
                                Writeln('>>> Token grabbed: ', curtoken);
                                Tokenised := concat(Tokenised, [NewToken(i, tknString, curtoken)]);
                                Writeln('>>>   Do something with ADD here');
                            end;
                            'REPLACE': begin
                                Writeln('>>> REPLACE found!');
                                Tokenised := concat(Tokenised, [NewToken(i, tknReplace, curtoken)]);
                                curtoken := GetNextToken(grabbedline, linepos);
                                Writeln('>>> Token grabbed: ', curtoken);
                                Tokenised := concat(Tokenised, [NewToken(i, tknString, curtoken)]);
                                Writeln('>>>   Do something with REPLACE here');
                            end;
                            'DEFER': begin
                                Writeln('>>> DEFER found!');
                                Tokenised := concat(Tokenised, [NewToken(i, tknDefer, curtoken)]);
                                curtoken := GetNextToken(grabbedline, linepos);
                                Writeln('>>> Token grabbed: ', curtoken);
                                Tokenised := concat(Tokenised, [NewToken(i, tknString, curtoken)]);
                                Writeln('>>>   Do something with DEFER here');
                            end;
                            'CONSTANTS': begin
                                Writeln('>>> CONSTANTS found!');
                                Tokenised := concat(Tokenised, [NewToken(i, tknConstants, curtoken)]);
                                status := stateClassConstantsSeekStart;
                                Writeln('>>>   Now in stateClassConstantsSeekStart');
                            end;
                            'TYPES': begin
                                Writeln('>>> TYPES found!');
                                Tokenised := concat(Tokenised, [NewToken(i, tknTypes, curtoken)]);
                                status := stateClassTypesSeekStart;
                                Writeln('>>>   Now in stateClassTypesSeekStart');
                            end;
                            'PROPERTY': begin
                                Writeln('>>> PROPERTY found!');
                                Tokenised := concat(Tokenised, [NewToken(i, tknProperty, curtoken)]);
                                curtoken := GetNextToken(grabbedline, linepos);
                                Writeln('>>> Token grabbed: ', curtoken);
                                if TryStrToInt(curtoken, x) then begin
                                    Writeln('>>> Number found!');
                                    Tokenised := concat(Tokenised, [NewToken(i, tknString, curtoken)]);
                                end;
                                status := stateClassPropertySeekStart;
                                Writeln('>>>   Now in stateClassPropertySeekStart');
                            end;
                        end;
                    end;
                end;

                stateClassConstantsSeekStart: begin
                    if grabbedline[1] = '{' then begin
                        Writeln('>>> Start of CONSTANTS section found!');
                        Tokenised := concat(Tokenised, [NewToken(i, tknBraceLeft, '{')]);
                        status := stateClassConstants;
                        Writeln('>>>   Now in stateClassConstants');
                        inc(bracelevel);
                        Writeln('>>>   Brace level: ', bracelevel);
                    end;
                end;

                stateClassConstants: begin
                    if grabbedline[1] = '}' then begin
                        Writeln('>>> End of CONSTANTS section found!');
                        Tokenised := concat(Tokenised, [NewToken(i, tknBraceRight, '}')]);
                        dec(bracelevel);
                        Writeln('>>>   Brace level: ', bracelevel);
                        status := stateClass;
                        Writeln('>>> Now in stateClass');
                    end else if grabbedline[1] = '{' then begin
                        Writeln('!!! Too many curly braces');
                        exit;
                    end else begin
                        curtoken := GetNextToken(grabbedline, linepos);
                        Writeln('>>> Token grabbed: ', curtoken);
                        Tokenised := concat(Tokenised, [NewToken(i, tknString, curtoken)]);
                        curtoken := GetNextToken(grabbedline, linepos);
                        Writeln('>>> Token grabbed: ', curtoken);
                        Tokenised := concat(Tokenised, [NewToken(i, tknString, curtoken)]);
                    end;
                end;

                stateClassTypesSeekStart: begin
                    if grabbedline[1] = '{' then begin
                        Writeln('>>> Start of TYPES section found!');
                        Tokenised := concat(Tokenised, [NewToken(i, tknBraceLeft, '{')]);
                        status := stateClassTypes;
                        Writeln('>>>   Now in stateClassTypes');
                        inc(bracelevel);
                        Writeln('>>>   Brace level: ', bracelevel);
                    end;
                end;

                stateClassTypes: begin
                    if grabbedline[1] = '{' then begin
                        inc(bracelevel);
                        Writeln('>>>   Brace level: ', bracelevel);
                    end else if grabbedline[1] = '}' then begin
                        dec(bracelevel);
                        Writeln('>>>   Brace level: ', bracelevel);
                        if bracelevel = 1 then begin
                            Writeln('>>> End of TYPES section found!');
                            Tokenised := concat(Tokenised, [NewToken(i, tknBraceRight, '}')]);
                            status := stateClass;
                            Writeln('>>> Now in stateClass');
                        end;
                    end;
                    if bracelevel > 1 then begin
                        TrimAfterSemicolon(grabbedline);
                        Writeln ('>>> Found string: ', grabbedline);
                        Tokenised := concat(Tokenised, [NewToken(i, tknString, grabbedline)]);
                    end;
                end;

                stateClassPropertySeekStart: begin
                    if grabbedline[1] = '{' then begin
                        Writeln('>>> Start of PROPERTY section found!');
                        Tokenised := concat(Tokenised, [NewToken(i, tknBraceLeft, '{')]);
                        status := stateClassProperty;
                        Writeln('>>>   Now in stateClassProperty');
                        inc(bracelevel);
                        Writeln('>>>   Brace level: ', bracelevel);
                    end;
                end;

                stateClassProperty: begin
                    if grabbedline[1] = '}' then begin
                        Writeln('>>> End of PROPERTY section found!');
                        Tokenised := concat(Tokenised, [NewToken(i, tknBraceRight, '}')]);
                        dec(bracelevel);
                        Writeln('>>>   Brace level: ', bracelevel);
                        status := stateClass;
                        Writeln('>>> Now in stateClass');
                    end else begin
                        TrimAfterSemicolon(grabbedline);
                        Writeln ('>>> Found string: ', grabbedline);
                        Tokenised := concat(Tokenised, [NewToken(i, tknString, grabbedline)]);
                    end;
                end;
            end;
        end;
    end;

    if bracelevel <> 0 then begin
        WriteLn('Error with braces: Somehow at brace level ', bracelevel);
        exit;
    end;

    // TODO: Check for braces inside lines?

    Writeln;
    PrintTestArray(Tokenised);
end;

procedure GetParams();
var
    cur, tot : Integer;
    thisParam : String;
    flgFoundName: Boolean = false;
begin
    tot := paramCount();
    cur := 1;
    
    while cur <= tot do
    begin
        thisParam := paramStr(cur);
        if thisParam[1] = '-' then begin
            case UpCase(thisParam[2]) of
                'E': begin
                    writeln('external includes found');
                    boolExternal := true;
                end;
                'G': begin
                    writeln('generate .G files');
                    boolGenG := true;
                end;
                else begin
                    writeln('Nope!');
                    exit();
                end;
            end;
            if length(thisParam) = 2 then begin
                inc(cur);
                thisParam := paramStr(cur);
            end else begin
                thisParam := copy(thisParam, 3);
            end;
            writeln('Found value: ', thisParam);
        end else begin
            if flgFoundName then begin
                writeln('Too many items without a switch!');
                exit();
            end;
            writeln('Found name of file to process: ', thisParam);
            flgFoundName := true;
            strFilename := thisParam;
        end;
        inc(cur);
        // TODO: Avoid incrementing if a -thing is followed by another -thing
        // TODO: Check for too many items without a switch
        // TODO: is it worth catering for `-s value` as well as `-svalue`? 
    end;
end;

begin
    //TestTokeniser();

    GetParams();
    
    if length(strFilename) = 0 then begin
        WriteLn('No filename given.');
        exit;
    end;

    if (length(strFilename) < 5) or ((length(strFilename) > 4) and (AnsiPos('.', UpCase(strFilename)) < 2)) then
        strfilename += '.cat';
    WriteLn('Filename: ', strFilename);
    WriteLn;

    LoadThatFile();
end.

