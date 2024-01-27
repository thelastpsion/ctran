{$mode objfpc}{$H+}{$J-}
unit PsionOOLexer;
{ *** Psion Object Oriented Lexer ***

The tokeniser/lexer part of the reverse-engineered CTRAN.

}

interface

uses
    sysutils, Classes;

type
//    TokenType = string;
    TTokenType = (
        // Breaks
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
        tknName,

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

        // External reference file (.EXT) keywords
        tknHasMethod,
        tknHasProperty,
        tknDeclare,

        tknString,
        tknEOF
    );

    TToken = record
        TType : TTokenType;
        Literal : string;
        LineNum : Integer;
    end;
    TTokenArray = array of TToken;

    TLexerState = (
        stateInitial,
        stateSeekKeyword,
        stateClassSeekStart,
        stateClass,
        stateClassProperty,
        stateClassTypes,
        stateClassConstants,
        stateClassConstantsSeekStart,
        stateClassTypesSeekStart,
        stateClassPropertySeekStart
    );

    TPsionOOLexer = class
        strict private
            var
                _curLineNum, _curLinePos : Integer;
                _slCategoryFile : TStringList;
                _strFilename : String;
                _strCurLine : String;
                _LexerState : TLexerState;
                _BraceLevel : Integer;
                _TokenArray : TTokenArray;
                _CurToken : Integer;
            function _GetNextLiteral() : String;
            function _NewToken(newTokenLineNum: Integer; newTokenType: TTokenType; newTokenLiteral: String): TToken;
            procedure _AddToken(newTokenType: TTokenType; newTokenLiteral: String);
            procedure _ProcessCLine();
            procedure _GrabAndAddStringTokens(count : Integer);
        public
            constructor Create();
            procedure LoadFile(strFilename : String);
            procedure PrintArray();
            function GetNextToken() : TToken;
            procedure Reset();
    end;

var
    boolExternal : Boolean = false;
//    strExternal : String;
    boolGenG : Boolean = false;
//    strGenG : String;

implementation

// Removes everything in a string after the first semicolon
procedure TrimAfterSemicolon(var s: String);
begin
    if ansipos(';', s) > 0 then s := copy(s, 1, ansipos(';', s));
end;

//function IsValidLetter(ch: Char): Boolean;
//begin
    //Result := (((ord(ch) >= 97) and (ord(ch) <= 122)) or ((ord(ch) >= 65) and (ord(ch) <= 90)) or (ch = '_'));
//    Result := ((LowerCase(ch) in ['a' .. 'z']) or (ch = '_'));
//end;

constructor TPsionOOLexer.Create();
begin
    inherited Create;
    _curLineNum := 0;
    _curLinePos := 0;
    _strCurLine := '';
    _strFilename := '';
    _LexerState := stateInitial;
    _BraceLevel := 0;
    _slCategoryFile := TStringList.Create;
    _CurToken := -1;
    _BraceLevel := 0;
end;

//TODO: Should this be a function that returns a char, or should it just put values into variables inside the class?
//procedure TPsionOOLexer.GetNextGlyph();
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
//procedure TPsionOOLexer.FindStartOfLine();
//var
//    ch : Char;
//begin
//    ch := content[curpos];
//end;


procedure TPsionOOLexer.PrintArray();
var
    s: String;
    recToken : TToken;
begin
    recToken := _NewToken(0, tknString, ''); // Just an empty token, so that the variable is initialised

    Writeln(' Line | Token Type     | Literal');
    Writeln('------+----------------+-------------');

    while recToken.TType <> tknEOF do
    begin
        recToken := GetNextToken();
        Str(recToken.TType, s); // Because you can't simply use an enum in format()
        Writeln(format(' %4d | %-14s | %s', [recToken.LineNum, s, recToken.Literal]));
    end;

    Writeln;
    Writeln('Length: ', Length(_TokenArray));
end;

// Takes a TokenType and a String and puts it into a Token record
function TPsionOOLexer._NewToken(newTokenLineNum: Integer; newTokenType: TTokenType; newTokenLiteral: String): TToken;
begin
    _NewToken.TType := newTokenType;
    _NewToken.Literal := newTokenLiteral;
    _NewToken.LineNum := newTokenLineNum;
end;

procedure TPsionOOLexer._AddToken(newTokenType: TTokenType; newTokenLiteral: String);
var
    newToken: TToken;
begin
    newToken.TType := newTokenType;
    newToken.Literal := newTokenLiteral;
    newToken.LineNum := _curLineNum;
    _TokenArray := concat(_TokenArray, [newToken]);
end;

function TPsionOOLexer._GetNextLiteral() : String;
var
    curpos : Integer;
    flgFoundText : Boolean = false;
begin
    _GetNextLiteral := '';

    for curpos := _curLinePos to length(_strCurLine) do

    begin
        if _strCurLine[curpos] = ' ' then begin
            if flgFoundText then begin
                _curLinePos := curpos + 1;
                exit;
            end;
        end else begin
            _GetNextLiteral := concat(_GetNextLiteral, _strCurLine[curpos]);
            flgFoundText := true;
        end;
    end;
    _curLinePos += length(_GetNextLiteral);
end;

procedure TPsionOOLexer._GrabAndAddStringTokens(count : Integer);
var
    i : Integer;
    curtoken : String;
begin
    WriteLn('>>> Fetching up to ', count, ' token(s)');
    for i := 1 to count do
    begin
        curtoken := _GetNextLiteral();
        if curtoken = '' then begin
            Writeln('>>>   No more tokens on line ', _curLineNum);
            exit;
        end;
        Writeln(format('>>>   String token %d grabbed: %s', [i, curtoken]));
        _AddToken(tknString, curtoken);
    end;
end;

procedure TPsionOOLexer._ProcessCLine();
begin
    if (_LexerState <> stateClassTypes) and (_LexerState <> stateClassProperty) then begin
        WriteLn('_ProcessCBlock: Why am I here?');
        halt;
    end;
    if _strCurLine[1] = '{' then begin
        inc(_BraceLevel);
        Writeln('>>>   Brace level: ', _BraceLevel);
    end else if _strCurLine[1] = '}' then begin
        dec(_BraceLevel);
        Writeln('>>>   Brace level: ', _BraceLevel);
        if _BraceLevel = 1 then begin
            case _LexerState of
                stateClassTypes:     Writeln('>>> End of TYPES section found!');
                stateClassProperty:  Writeln('>>> End of PROPERTY section found!');
            end;
            _AddToken(tknBraceRight, '}');
            _LexerState := stateClass;
            Writeln('>>> Now in stateClass');
        end;
    end;
    if _BraceLevel > 1 then begin
        TrimAfterSemicolon(_strCurLine);
        Writeln ('>>> Found string: ', _strCurLine);
        _AddToken(tknString, _StrCurLine);
    end;
end;

// TODO: Check for braces inside lines?
procedure TPsionOOLexer.LoadFile(strFilename : String);
var
    x : LongInt;
    curtoken : String;
begin
    _LexerState := stateInitial;
    _slCategoryFile := TStringList.Create;

    _slCategoryFile.LoadFromFile(strFilename);

    _curLineNum := 0;
    while _CurLineNum < _slCategoryFile.Count do
    begin
        curtoken := '';
        inc(_curLineNum);
        _curLinePos := 1;

        _strCurLine := _slCategoryFile[_curLineNum - 1].Trim;

        WriteLn(format('%.3d:%s', [_curLineNum, _strCurLine]));

        if length(_strCurLine) = 0 then
            Writeln('>>> Empty line')
        else if _strCurLine[1] = '!' then
            Writeln('>>> Explicit comment, line skipped')
        else begin
            curtoken := '';
    
            case _LexerState of
                stateInitial: begin
                    curtoken := _GetNextLiteral();
                    case UpCase(curtoken) of 
                        'IMAGE': begin
                            Writeln('>>> IMAGE found!');
                            _AddToken(tknImage, curtoken);
                            _LexerState := stateSeekKeyword;
                        end;
                        'LIBRARY': begin
                            Writeln('>>> LIBRARY found!');
                            _AddToken(tknLibrary, curtoken);
                            _LexerState := stateSeekKeyword;
                        end;
                        'NAME': begin
                            Writeln('>>> NAME found!');
                            _AddToken(tknName, curtoken);
                            _LexerState := stateSeekKeyword;
                        end;
                    end;
                    if _LexerState = stateSeekKeyword then
                    begin
                        WriteLn('>>>   Now in stateSeekKeyword');
                        _GrabAndAddStringTokens(1);
                    end;
                end;

                stateSeekKeyword: begin
                    curtoken := _GetNextLiteral();
                    case UpCase(curtoken) of
                        'EXTERNAL': begin
                            Writeln('>>> EXTERNAL found!');
                            _AddToken(tknExternal, curtoken);
                            _GrabAndAddStringTokens(1);
                        end;
                        'INCLUDE': begin
                            Writeln('>>> INCLUDE found!');
                            _AddToken(tknInclude, curtoken);
                            _GrabAndAddStringTokens(1);
                        end;
                        'CLASS': begin
                            Writeln('>>> CLASS found!');
                            _AddToken(tknClass, curtoken);
                            _GrabAndAddStringTokens(2);
                            _LexerState := stateClassSeekStart;
                            Writeln('>>>   Now in stateClassSeekStart (looking for brace)');
                        end;
                        'REQUIRE': begin
                            Writeln('>>> REQUIRE found!');
                            _AddToken(tknRequire, curtoken);
                            _GrabAndAddStringTokens(1);
                        end;
                    end;
                end;

                stateClassSeekStart: begin
                    if _strCurLine[1] = '{' then begin
                        Writeln('>>> Start of CLASS section found!');
                        _AddToken(tknBraceLeft, '{');
                        _LexerState := stateClass;
                        Writeln('>>>   Now in stateClass');
                        inc(_BraceLevel);
                        Writeln('>>>   Brace level: ', _BraceLevel);
                    end;
                end;

                stateClass: begin
                    if _strCurLine[1] = '}' then begin
                        _AddToken(tknBraceRight, '}');
                        Writeln('>>> End of CLASS section found!');
                        dec(_BraceLevel);
                        Writeln('>>>   Brace level: ', _BraceLevel);
                        _LexerState := stateSeekKeyword;
                        Writeln('>>> Now in stateSeekKeyword');
                    end else begin
                        curtoken := _GetNextLiteral();
                        case curtoken of
                            'ADD': begin
                                Writeln('>>> ADD found!');
                                _AddToken(tknAdd, curtoken);
                                _GrabAndAddStringTokens(1);
                            end;
                            'REPLACE': begin
                                Writeln('>>> REPLACE found!');
                                _AddToken(tknReplace, curtoken);
                                _GrabAndAddStringTokens(1);
                            end;
                            'DEFER': begin
                                Writeln('>>> DEFER found!');
                                _AddToken(tknDefer, curtoken);
                                _GrabAndAddStringTokens(1);
                            end;
                            'CONSTANTS': begin
                                Writeln('>>> CONSTANTS found!');
                                _AddToken(tknConstants, curtoken);
                                _LexerState := stateClassConstantsSeekStart;
                                Writeln('>>>   Now in stateClassConstantsSeekStart');
                            end;
                            'TYPES': begin
                                Writeln('>>> TYPES found!');
                                _AddToken(tknTypes, curtoken);
                                _LexerState := stateClassTypesSeekStart;
                                Writeln('>>>   Now in stateClassTypesSeekStart');
                            end;
                            'PROPERTY': begin
                                Writeln('>>> PROPERTY found!');
                                _AddToken(tknProperty, curtoken);
                                curtoken := _GetNextLiteral();
                                Writeln('>>> Token grabbed: ', curtoken);
                                if TryStrToInt(curtoken, x) then begin
                                    Writeln('>>> Number found!');
                                    _AddToken(tknString, curtoken);
                                end;
                                _LexerState := stateClassPropertySeekStart;
                                Writeln('>>>   Now in stateClassPropertySeekStart');
                            end;
                            // External reference (.EXT) keywords
                            'DECLARE': begin
                                Writeln('>>> DECLARE found!');
                                _AddToken(tknDeclare, curtoken);
                                _GrabAndAddStringTokens(1);
                            end;
                            'HAS_METHOD': begin
                                Writeln('>>> HAS_METHOD found!');
                                _AddToken(tknHasMethod, curtoken);
                            end;
                            'HAS_PROPERTY': begin
                                Writeln('>>> HAS_PROPERTY found!');
                                _AddToken(tknHasProperty, curtoken);
                            end;
                        end;
                    end;
                end;

                stateClassConstantsSeekStart: begin
                    if _strCurLine[1] = '{' then begin
                        Writeln('>>> Start of CONSTANTS section found!');
                        _AddToken(tknBraceLeft, '{');
                        _LexerState := stateClassConstants;
                        Writeln('>>>   Now in stateClassConstants');
                        inc(_BraceLevel);
                        Writeln('>>>   Brace level: ', _BraceLevel);
                    end;
                end;

                stateClassConstants: begin
                    if _strCurLine[1] = '}' then begin
                        Writeln('>>> End of CONSTANTS section found!');
                        _AddToken(tknBraceRight, '}');
                        dec(_BraceLevel);
                        Writeln('>>>   Brace level: ', _BraceLevel);
                        _LexerState := stateClass;
                        Writeln('>>> Now in stateClass');
                    end else if _strCurLine[1] = '{' then begin
                        Writeln('!!! Too many curly braces');
                        exit;
                    end else begin
                        _GrabAndAddStringTokens(2);
                    end;
                end;

                stateClassTypesSeekStart: begin
                    if _strCurLine[1] = '{' then begin
                        Writeln('>>> Start of TYPES section found!');
                        _AddToken(tknBraceLeft, '{');
                        _LexerState := stateClassTypes;
                        Writeln('>>>   Now in stateClassTypes');
                        inc(_BraceLevel);
                        Writeln('>>>   Brace level: ', _BraceLevel);
                    end;
                end;

                stateClassTypes: begin
                    _ProcessCLine;
                end;

                stateClassPropertySeekStart: begin
                    if _strCurLine[1] = '{' then begin
                        Writeln('>>> Start of PROPERTY section found!');
                        _AddToken(tknBraceLeft, '{');
                        _LexerState := stateClassProperty;
                        Writeln('>>>   Now in stateClassProperty');
                        inc(_BraceLevel);
                        Writeln('>>>   Brace level: ', _BraceLevel);
                    end;
                end;

                stateClassProperty: begin
                    _ProcessCLine;
                end;
            end;
        end;
    end;

    if _BraceLevel <> 0 then begin
        WriteLn('Error with braces: Somehow at brace level ', _BraceLevel);
        exit;
    end;

    _AddToken(tknEOF, '');
end;

function TPsionOOLexer.GetNextToken() : TToken;
begin
    if length(_TokenArray) = 0 then begin
        GetNextToken := _NewToken(0, tknEOF, '');
        exit;
    end;

    if _CurToken < length(_TokenArray) then inc(_CurToken);
    GetNextToken := _TokenArray[_CurToken];
end;

procedure TPsionOOLexer.Reset();
begin
    _CurToken := -1;
end;

end.

