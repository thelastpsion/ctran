{$mode objfpc}{$H+}{$J-}
unit PsionOOLexer;
{ *** Psion Object Oriented Lexer ***

The tokeniser/lexer part of the reverse-engineered CTRAN.

}

interface

uses
    sysutils, Classes;

type
    // TokenType = string;
    TTokenType = (
        // Breaks
        tknNewline,

        // Symbols
        tknBraceLeft,
        tknBraceRight,

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
        LinePos : Integer;
    end;

    TTokenArray = array of TToken;

    // TODO: Rename this, as it's used by the lexer and parser
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

    // Parser-specific types
    TFileType = (
        ooCategory,
        ooSubCat,
        ooExternal
    );

    TCategoryType = (
        catImage,
        catLibrary,
        catName
    );

    TElementType = (
        incInclude,
        incExternal,
        incRequire,
        incClass
    );

    TMethodType = (
        methodAdd,
        methodReplace,
        methodDefer,
        methodDeclare
    );

    TPsionOOMethodEntry = record
        MethodType : TMethodType;
        Name : String;
    end;

    TPsionOOConstantEntry = record
        Name : String;
        Value : String;
    end;

    TPsionOOConstants = array of TPsionOOConstantEntry;

    TPsionOOProperty = array of string;

    TPsionOOTypes = array of string;

    TPsionOOClass = record
        Name : String;
        Inherits : String;
        Methods : array of TPsionOOMethodEntry;
        ClassProperty : TPsionOOProperty;
        ClassTypes : TPsionOOTypes;
        ClassConstants : TPsionOOConstants;
        HasMethod : Boolean;
        HasProperty : Boolean;
        PropertyAutodestroyCount : Integer;
    end;

    TPsionOOFileElement = record
        ElementType : TElementType;
        index : integer;
    end;

    TTokenisedLine = record
        LineNum : Integer;
        Tokens : array of TToken;
    end;

    TPsionOOLexer = class
        strict private
            // Fields: File Information
            _slCategoryFile : TStringList;
            _FileType : TFileType;
            _ModuleName : String;
            // Fields: Lexing
            _curLineNum, _curLinePos : Integer;
            _strFilename : String;
            _strCurLine : String;
            _LexerState : TLexerState;
            _BraceLevel : Integer;
            _TokenArray : TTokenArray;

            // Fields: Token Processing
            _CurTokenIndex : Integer;
            _CurToken : TToken;

            // Fields: Tokenised Line Builder
            _nextTLBTokenIndex : Integer;

            // Fields: Parser
            _ClassList : array of TPsionOOClass;
            _RequireList : array of string;
            _IncludeList : array of string;
            _ExternalList : array of string;
            _ElementList : array of TPsionOOFileElement;
            _CategoryType : TCategoryType;

            // Methods: Lexing
            procedure _DetectFileType(strFilename : String);
            procedure _DetectModuleName(strFilename : String);
            function _NewToken(newTokenLineNum: Integer; newTokenType: TTokenType; newTokenLiteral: String): TToken;
            procedure _AddToken(newTokenType: TTokenType; newTokenLiteral: String);
            procedure _AddToken(newTokenType: TTokenType; tok : TToken);
            procedure _ProcessCLine();
            procedure _GrabAndAddStringTokens(count : Integer);
            function _GetNextToken() : TToken;
            procedure _SeekStartOfSection(NextLexerState : TLexerState);

            // Methods: Token Processing
            function _getToken() : TToken;

            // Methods: Tokenised Line Builder
            function _GetNextLine() : TTokenisedLine;
            procedure _ResetTLB();

            // Methods: Parser
            procedure _CheckLine(tokline : TTokenisedLine; args : Integer; compulsary_args : Integer; toktypes : array of TTokenType);
            function _GetClass(tokline_class : TTokenisedLine) : TPsionOOClass;
            function _GetConstants() : TPsionOOConstants;
            function _BuildConstant(tokline : TTokenisedLine) : TPsionOOConstantEntry;
            function _GetTypes() : TPsionOOTypes;
            function _GetProperty() : TPsionOOProperty;
            procedure _CheckForBrace();
            function _TokenValidForFiletypes(toktype: TTokenType; valid_filetypes: array of TFileType) : boolean;

            // Methods: Misc
            procedure _ErrShowLine(linenum : Integer; linepos : Integer);

        public
            constructor Create();
            procedure LoadFile(strFilename : String);
            procedure Lex();
            procedure PrintArray();
            function GetNextToken() : TToken;
            procedure NextToken();
            procedure Reset();
            property token : TToken read _getToken;
            procedure PrintTokenisedLines();

            // Methods: Parser
            procedure Parse();
            procedure ShowTree();

    end;

implementation

// Removes everything in a string after the first semicolon
procedure TrimAfterSemicolon(var s: String);
begin
    if ansipos(';', s) > 0 then s := copy(s, 1, ansipos(';', s));
    s := s.Trim;
end;

function RepeatString(s: String; c: integer) : String;
var
    i : Integer;
begin
    Result := '';

    for i := 1 to c do
    begin
        Result := Result + s;
    end;
end;

// function IsValidLetter(ch: Char): Boolean;
// begin
//     Result := (((ord(ch) >= 97) and (ord(ch) <= 122)) or ((ord(ch) >= 65) and (ord(ch) <= 90)) or (ch = '_'));
//     Result := ((LowerCase(ch) in ['a' .. 'z']) or (ch = '_'));
// end;

constructor TPsionOOLexer.Create();
begin
    inherited Create;
    _slCategoryFile := TStringList.Create;

    Reset();

    // Tokenised Line Builder
    _resetTLB();
end;

procedure TPsionOOLexer.Reset();
begin
    _LexerState := stateInitial;
    _curLineNum := 0;
    _curLinePos := 0;
    _strCurLine := '';
    _strFilename := '';
    _CurTokenIndex := -1;
    _BraceLevel := 0;
end;

procedure TPsionOOLexer._ResetTLB();
begin
    _nextTLBTokenIndex := 0;
end;

//
// TOKENISED LINE BUILDER
//

function TPsionOOLexer._GetNextLine() : TTokenisedLine;
begin
    Result.Tokens := nil; // Because it remembers what was here before!

    if _nextTLBTokenIndex >= length(_TokenArray) then begin
        _nextTLBTokenIndex := length(_TokenArray);
        Result.LineNum := 0;
        Result.Tokens := [_NewToken(0, tknEOF, '')];
        exit;
    end;

    Result.LineNum := _TokenArray[_nextTLBTokenIndex].LineNum;
    while _nextTLBTokenIndex < length(_TokenArray) do
    begin
        if _TokenArray[_nextTLBTokenIndex].LineNum <> Result.LineNum then break;
        Result.Tokens := concat(Result.Tokens, [_TokenArray[_nextTLBTokenIndex]]);
        inc(_nextTLBTokenIndex);
    end;
end;

function TPsionOOLexer._GetToken(): TToken;
begin
    result := _TokenArray[_CurTokenIndex];
end;

procedure TPsionOOLexer.NextToken();
begin
    if _CurTokenIndex < length(_TokenArray) then inc(_CurTokenIndex);
    _CurToken := _TokenArray[_CurTokenIndex];
end;

procedure TPsionOOLexer._ErrShowLine(linenum : Integer; linepos : Integer);
begin
    Writeln(format('%.3d: %s', [LineNum, _slCategoryFile[LineNum - 1]]));
    Writeln('    ', RepeatString(' ', linepos), '^');
    halt;
end;

//
// OUTPUT (should really be in testing)
//

procedure TPsionOOLexer.PrintArray();
var
    s: String;
    recToken : TToken;
begin
    recToken := _NewToken(0, tknString, ''); // Just an empty token, so that the variable is initialised

    Writeln(' Line | Pos | Token Type     | Literal');
    Writeln('------+-----+----------------+-------------');

    while recToken.TType <> tknEOF do
    begin
        recToken := GetNextToken();
        Str(recToken.TType, s); // Because you can't simply use an enum in format()
        Writeln(format(' %4d | %3d | %-14s | %s', [recToken.LineNum, recToken.LinePos, s, recToken.Literal]));
    end;

    Writeln;
    Writeln('Length: ', Length(_TokenArray));
end;

procedure TPsionOOLexer.PrintTokenisedLines();
var
    // i : Integer;
    tokline : TTokenisedLine;
    tok : TToken;
begin
    WriteLn;
    WriteLn('Tokenised Line Parser!');

    tokline := _GetNextLine();
    while tokline.Tokens[0].TType <> tknEOF do
    begin
        Writeln('Line ', tokline.LineNum, ': (returned ', length(tokline.Tokens), ' tokens)');
        for tok in tokline.Tokens do
        begin
            WriteLn('   > ', tok.TType, ' ''', tok.Literal, ''' (', tok.LinePos, ')');
        end;
        Writeln('   O: ', _slCategoryFile[tokline.LineNum - 1]);
        Write('   G:');
        for tok in tokline.Tokens do
        begin
            Write(' ', tok.Literal);
        end;
        Writeln;
        tokline := _GetNextLine();
    end;
end;

//
// TOKEN CREATION
//

// Takes a TokenType and a String and puts it into a Token record
function TPsionOOLexer._NewToken(newTokenLineNum: Integer; newTokenType: TTokenType; newTokenLiteral: String): TToken;
begin
    _NewToken.TType := newTokenType;
    _NewToken.Literal := newTokenLiteral;
    _NewToken.LineNum := newTokenLineNum;
end;

procedure TPsionOOLexer._AddToken(newTokenType: TTokenType; tok: TToken);
var
    newToken: TToken;
begin
    newToken := tok;
    newToken.TType := newTokenType;
    _TokenArray := concat(_TokenArray, [newToken]);
end;

procedure TPsionOOLexer._AddToken(newTokenType: TTokenType; newTokenLiteral: String);
var
    newToken: TToken;
begin
    with newToken do begin
        TType := newTokenType;
        Literal := newTokenLiteral;
        LineNum := _curLineNum;
        LinePos := _curLinePos;
    end;
    _TokenArray := concat(_TokenArray, [newToken]);
end;

//
// LINE PROCESSING
//

function TPsionOOLexer._GetNextToken() : TToken;
var
    pos : Integer;
    flgFoundText : Boolean = false;
    startpos : Integer;
begin
    Result.Literal := '';
    Result.TType := tknEOF;
    Result.LineNum := 0;
    Result.LinePos := 0;

    for pos := _curLinePos to length(_strCurLine) do
    begin
        if _strCurLine[pos] = ' ' then begin
            if flgFoundText then begin
                _curLinePos := pos + 1;
                exit;
            end;
        end else begin
            Result.Literal += _strCurLine[pos];
            if not(flgFoundText) then begin
                Result.LineNum := _curLineNum;
                Result.LinePos := pos;
                flgFoundText := true;
            end;
        end;
    end;
    _curLinePos += length(Result.Literal);
end;

procedure TPsionOOLexer._GrabAndAddStringTokens(count : Integer);
var
    i : Integer;
    tok : TToken;
begin
    WriteLn('>>> Fetching up to ', count, ' token(s)');
    for i := 1 to count do
    begin
        tok := _GetNextToken();
        if tok.Literal = '' then begin
            Writeln('>>>   No more tokens on line ', _curLineNum);
            exit;
        end;
        Writeln(format('>>>   String token %d grabbed: %s', [i, tok.Literal]));
        _AddToken(tknString, tok);
    end;
end;

procedure TPsionOOLexer._ProcessCLine();
begin
    if (_LexerState <> stateClassTypes) and (_LexerState <> stateClassProperty) then begin
        WriteLn('_ProcessCLine: Why am I here?');
        halt;
    end;
    if _strCurLine[_curLinePos] = '{' then begin
        inc(_BraceLevel);
        Writeln('>>>   Brace level: ', _BraceLevel);
    end else if _strCurLine[_curLinePos] = '}' then begin
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

procedure TPsionOOLexer._SeekStartOfSection(NextLexerState : TLexerState);
begin
    if _strCurLine[_curLinePos] = '{' then begin
        Writeln('>>> Start of section found!');
        _AddToken(tknBraceLeft, '{');

        _LexerState := NextLexerState;
        Writeln('>>>   Now in ', _LexerState);

        inc(_BraceLevel);
        Writeln('>>>   Brace level: ', _BraceLevel);
    end;
end;


procedure TPsionOOLexer._DetectFileType(strFilename : String);
var
    ext : String;
begin
    ext := UpCase(ExtractFileExt(strFilename));

    case ext of
        '', '.': begin
            _FileType := ooCategory;
        end;
        '.CAT': begin
            _FileType := ooCategory;
        end;
        '.CL': begin
            _FileType := ooSubCat;
        end;
        '.EXT': begin
            _FileType := ooExternal;
        end;
        else begin
            WriteLn('ERROR: Unknown file type.');
            halt;
        end;
    end;
    WriteLn('File is ', _FileType);
end;

procedure TPsionOOLexer._DetectModuleName(strFilename : String);
var
    s : String;
begin
    s := ExtractFileName(strFilename);
    _ModuleName := Copy(UpCase(s), 1, Length(s) - length(ExtractFileExt(s)));

    Writeln('Module name: ', _ModuleName);
end;

procedure TPsionOOLexer.LoadFile(strFilename : String);
begin
    _slCategoryFile := TStringList.Create;

    _slCategoryFile.LoadFromFile(strFilename);
    _DetectFileType(strFilename);
    _DetectModuleName(strFilename);
end;

// TODO: Check for braces inside lines?
procedure TPsionOOLexer.Lex();
var
    x : LongInt;
    tok : TToken;
begin
    _LexerState := stateInitial;
    _curLineNum := 0;

    while _CurLineNum < _slCategoryFile.Count do
    begin
        tok.Literal := '';
        tok.TType := tknEOF;
        tok.LineNum := _curLineNum;
        tok.LinePos := 0;

        inc(_curLineNum);
        _curLinePos := 1;

        _strCurLine := _slCategoryFile[_curLineNum - 1]; //.Trim;

        while _curLinePos <= length(_strCurLine) do
        begin
            case _strCurLine[_curLinePos] of
                ' ', #1: inc(_curLinePos);
                else break;
            end;
        end;

        WriteLn(format('%.3d:%s', [_curLineNum, _strCurLine]));

        if length(_strCurLine.Trim) = 0 then
            Writeln('>>> Empty line')
        else if _strCurLine[_curLinePos] = '!' then
            Writeln('>>> Explicit comment, line skipped')
        else begin
    
            case _LexerState of
                stateInitial: begin
                    tok := _GetNextToken();
                    case UpCase(tok.Literal) of
                        'IMAGE': begin
                            Writeln('>>> IMAGE found!');
                            _AddToken(tknImage, tok);
                            _LexerState := stateSeekKeyword;
                        end;
                        'LIBRARY': begin
                            Writeln('>>> LIBRARY found!');
                            _AddToken(tknLibrary, tok);
                            _LexerState := stateSeekKeyword;
                        end;
                        'NAME': begin
                            Writeln('>>> NAME found!');
                            _AddToken(tknName, tok);
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
                    tok := _GetNextToken();
                    case UpCase(tok.Literal) of
                        'EXTERNAL': begin
                            Writeln('>>> EXTERNAL found!');
                            _AddToken(tknExternal, tok);
                            _GrabAndAddStringTokens(1);
                        end;
                        'INCLUDE': begin
                            Writeln('>>> INCLUDE found!');
                            _AddToken(tknInclude, tok);
                            _GrabAndAddStringTokens(1);
                        end;
                        'CLASS': begin
                            Writeln('>>> CLASS found!');
                            _AddToken(tknClass, tok);
                            _GrabAndAddStringTokens(2);
                            _LexerState := stateClassSeekStart;
                            Writeln('>>>   Now in stateClassSeekStart (looking for brace)');
                        end;
                        'REQUIRE': begin
                            Writeln('>>> REQUIRE found!');
                            _AddToken(tknRequire, tok);
                            _GrabAndAddStringTokens(1);
                        end;
                        else begin
                            WriteLn('!!! Invalid string literal found: ', tok.Literal);
                            halt;
                        end;
                    end;
                end;

                stateClassSeekStart: _SeekStartOfSection(stateClass);

                stateClass: begin
                    if _strCurLine[_curLinePos] = '}' then begin
                        _AddToken(tknBraceRight, '}');
                        Writeln('>>> End of CLASS section found!');
                        dec(_BraceLevel);
                        Writeln('>>>   Brace level: ', _BraceLevel);
                        _LexerState := stateSeekKeyword;
                        Writeln('>>> Now in stateSeekKeyword');
                    end else begin
                        tok := _GetNextToken();
                        case UpCase(tok.Literal) of
                            'ADD': begin
                                Writeln('>>> ADD found!');
                                _AddToken(tknAdd, tok);
                                _GrabAndAddStringTokens(1);
                            end;
                            'REPLACE': begin
                                Writeln('>>> REPLACE found!');
                                _AddToken(tknReplace, tok);
                                _GrabAndAddStringTokens(1);
                            end;
                            'DEFER': begin
                                Writeln('>>> DEFER found!');
                                _AddToken(tknDefer, tok);
                                _GrabAndAddStringTokens(1);
                            end;
                            'CONSTANTS': begin
                                Writeln('>>> CONSTANTS found!');
                                _AddToken(tknConstants, tok);
                                _LexerState := stateClassConstantsSeekStart;
                                Writeln('>>>   Now in stateClassConstantsSeekStart');
                            end;
                            'TYPES': begin
                                Writeln('>>> TYPES found!');
                                _AddToken(tknTypes, tok);
                                _LexerState := stateClassTypesSeekStart;
                                Writeln('>>>   Now in stateClassTypesSeekStart');
                            end;
                            'PROPERTY': begin
                                Writeln('>>> PROPERTY found!');
                                _AddToken(tknProperty, tok);
                                tok := _GetNextToken();
                                Writeln('>>> Literal grabbed: ', tok.Literal);
                                if TryStrToInt(tok.Literal, x) then begin
                                    Writeln('>>> Number found!');
                                    _AddToken(tknString, tok);
                                end;
                                _LexerState := stateClassPropertySeekStart;
                                Writeln('>>>   Now in stateClassPropertySeekStart');
                            end;
                            // External reference (.EXT) keywords
                            'DECLARE': begin
                                Writeln('>>> DECLARE found!');
                                _AddToken(tknDeclare, tok);
                                _GrabAndAddStringTokens(1);
                            end;
                            'HAS_METHOD': begin
                                Writeln('>>> HAS_METHOD found!');
                                _AddToken(tknHasMethod, tok);
                            end;
                            'HAS_PROPERTY': begin
                                Writeln('>>> HAS_PROPERTY found!');
                                _AddToken(tknHasProperty, tok);
                            end;
                        end;
                    end;
                end;

                stateClassConstantsSeekStart: _SeekStartOfSection(stateClassConstants);

                stateClassConstants: begin
                    case _strCurLine[_curLinePos] of
                        '}': begin
                            Writeln('>>> End of CONSTANTS section found!');
                            _AddToken(tknBraceRight, '}');
                            dec(_BraceLevel);
                            Writeln('>>>   Brace level: ', _BraceLevel);
                            _LexerState := stateClass;
                            Writeln('>>> Now in stateClass');
                        end;
                        '{': begin
                            Writeln('!!! Too many curly braces');
                            exit;
                        end else begin
                            _GrabAndAddStringTokens(2);
                        end;
                    end;
                end;

                stateClassTypesSeekStart: _SeekStartOfSection(stateClassTypes);

                stateClassTypes: _ProcessCLine;

                stateClassPropertySeekStart: _SeekStartOfSection(stateClassProperty);

                stateClassProperty: _ProcessCLine;
            end;
        end;
    end;

    if _BraceLevel <> 0 then begin
        WriteLn('Error with braces: Somehow at brace level ', _BraceLevel);
        exit;
    end;

    _curLinePos := 0;
    _AddToken(tknEOF, '');
end;

procedure TPsionOOLexer._CheckLine(tokline : TTokenisedLine; args : Integer; compulsary_args : Integer; toktypes : array of TTokenType);
var
    i : integer;
begin
    if args < compulsary_args then begin
        WriteLn('_CheckLine: args is less than compulsary_args');
        halt;
    end;
    if length(toktypes) <> args then begin
        Writeln('_CheckLine: args doesn''t equal the number of token types provided');
        halt;
    end;

    if length(tokline.Tokens) - 1 < compulsary_args then begin
        Writeln('ERROR: Current line has too few arguments');
        _ErrShowLine(tokline.LineNum, tokline.Tokens[length(tokline.Tokens)].LinePos);
    end;

    if length(tokline.Tokens) - 1 > args then begin
        Writeln('ERROR: Current line has too many arguments');
        _ErrShowLine(tokline.LineNum, tokline.Tokens[args].LinePos);
    end;

    for i := 1 to length(tokline.Tokens) - 1 do
    begin
        if (tokline.Tokens[i].TType <> toktypes[i-1]) then begin
            WriteLn('ERROR: Incorrect token type (', tokline.LineNum, ') Expected ', toktypes[i-1], ' but got ', tokline.Tokens[i].TType);
            _ErrShowLine(tokline.LineNum, tokline.Tokens[i].LinePos);
            halt;
        end;
    end;
end;

function TPsionOOLexer._TokenValidForFiletypes(toktype: TTokenType; valid_filetypes: array of TFileType) : boolean;
var
    filetype: TFileType;
begin
    Result := false;

    for filetype in valid_filetypes do
    begin
        if filetype = _FileType then exit(true);
    end;
end;

function TPsionOOLexer._BuildConstant(tokline : TTokenisedLine) : TPsionOOConstantEntry;
begin
    Result.Name := tokline.Tokens[0].Literal;
    Result.Value := tokline.Tokens[1].Literal;
end;

function TPsionOOLexer._GetConstants() : TPsionOOConstants;
var
    tokline : TTokenisedLine;
begin
    tokline := _GetNextLine();
    Result := nil;

    while tokline.Tokens[0].TType <> tknEOF do
    begin
        case tokline.Tokens[0].TType of
            tknBraceRight: begin
                exit;
            end;
            tknString: begin
                _CheckLine(tokline, 1, 1, [tknString]);
                Result := concat(Result, [_BuildConstant(tokline)]);
            end;
            else begin
                WriteLn('ERROR: Incorrect token, found ', tokline.Tokens[0].TType);
                _ErrShowLine(tokline.LineNum, 1);
            end;
        end;
        tokline := _GetNextLine();
    end;
end;

function TPsionOOLexer._GetTypes() : TPsionOOTypes;
var
    tokline : TTokenisedLine;
begin
    tokline := _GetNextLine();
    Result := nil;

    while tokline.Tokens[0].TType <> tknEOF do
    begin
        case tokline.Tokens[0].TType of
            tknBraceRight: begin
                exit;
            end;
            tknString: begin
                _CheckLine(tokline, 0, 0, []);
                Result := concat(Result, [tokline.Tokens[0].Literal]);
            end;
            else begin
                WriteLn('ERROR: Incorrect token, found ', tokline.Tokens[0].TType);
                _ErrShowLine(tokline.LineNum, 1);
            end;
        end;
        tokline := _GetNextLine();
    end;
end;

function TPsionOOLexer._GetProperty() : TPsionOOProperty;
var
    tokline : TTokenisedLine;
begin
    Result := nil;
    tokline := _GetNextLine();

    while tokline.Tokens[0].TType <> tknEOF do
    begin
        case tokline.Tokens[0].TType of
            tknBraceRight: begin
                exit;
            end;
            tknString: begin
                _CheckLine(tokline, 0, 0, []);
                Result := concat(Result, [tokline.Tokens[0].Literal]);
            end;
            else begin
                WriteLn('ERROR: Incorrect token, found ', tokline.Tokens[0].TType);
                _ErrShowLine(tokline.LineNum, 1);
            end;
        end;
        tokline := _GetNextLine();
    end;
end;

procedure TPsionOOLexer._CheckForBrace();
var
    tokline : TTokenisedLine;
begin
    tokline := _GetNextLine();
    if tokline.Tokens[0].TType <> tknBraceLeft then begin
        Writeln('ERROR: Expected tknBraceLeft, got ', tokline.Tokens[0].TType);
        _ErrShowLine(tokline.LineNum, 1);
    end;
    _CheckLine(tokline, 0, 0, []);
end;

// procedure TPsionOOLexer._AddMethodEntry(method_type: TMethodType, s: String);
// var
//     curMethodEntry : TPsionOOMethodEntry;
// begin
//     curMethodEntry.MethodType := method_type;
//     curMethodEntry.Name := s;
//     Result.Methods := concat(Result.Methods, [curMethodEntry]);
// end;

function TPsionOOLexer._GetClass(tokline_class : TTokenisedLine) : TPsionOOClass;
var
    tokline : TTokenisedLine;
    curMethodEntry : TPsionOOMethodEntry;
begin
    if tokline_class.Tokens[0].TType <> tknClass then begin
        WriteLn('_GetClass: Been sent the wrong line.');
        _ErrShowLine(tokline_class.LineNum, 1);
    end;

    _CheckLine(tokline_class, 2, 1, [tknString, tknString]);

    Result.Name := tokline_class.Tokens[1].Literal;
    if length(tokline_class.Tokens) = 3 then begin
        Result.Inherits := tokline_class.Tokens[2].Literal;
    end else begin
        Result.Inherits := '';
    end;

    Result.HasMethod := false;
    Result.HasProperty := false;
    Result.ClassConstants := nil;
    Result.ClassProperty := nil;
    Result.ClassTypes := nil;
    Result.Methods := nil;
    Result.PropertyAutodestroyCount := 0;

    tokline := _GetNextLine();

    while tokline.Tokens[0].TType <> tknEOF do
    begin
        case tokline.Tokens[0].TType of
            tknAdd: begin
                _CheckLine(tokline, 1, 1, [tknString]);
                // WriteLn('ADD ', tokline.Tokens[1].Literal);
                curMethodEntry.MethodType := methodAdd;
                curMethodEntry.Name := tokline.Tokens[1].Literal;
                Result.Methods := concat(Result.Methods, [curMethodEntry]);
            end;

            tknReplace: begin
                _CheckLine(tokline, 1, 1, [tknString]);
                // WriteLn('REPLACE ', tokline.Tokens[1].Literal);
                curMethodEntry.MethodType := methodReplace;
                curMethodEntry.Name := tokline.Tokens[1].Literal;
                Result.Methods := concat(Result.Methods, [curMethodEntry]);
            end;

            tknDefer: begin
                _CheckLine(tokline, 1, 1, [tknString]);
                // WriteLn('DEFER ', tokline.Tokens[1].Literal);
                curMethodEntry.MethodType := methodDefer;
                curMethodEntry.Name := tokline.Tokens[1].Literal;
                Result.Methods := concat(Result.Methods, [curMethodEntry]);
            end;

            tknDeclare: begin
                if _FileType <> ooExternal then begin
                    WriteLn('ERROR: tknDeclare only valid in External files');
                    _ErrShowLine(tokline.LineNum, 1);
                end;
                _CheckLine(tokline, 1, 1, [tknString]);
                // WriteLn('DECLARE ', tokline.Tokens[1].Literal);
                curMethodEntry.MethodType := methodDeclare;
                curMethodEntry.Name := tokline.Tokens[1].Literal;
                Result.Methods := concat(Result.Methods, [curMethodEntry]);
            end;

            tknTypes: begin
                _CheckLine(tokline, 0, 0, []);
                WriteLn('Found TYPES');
                _CheckForBrace();
                Result.ClassTypes := _GetTypes();
            end;

            tknProperty: begin
                _CheckLine(tokline, 1, 0, [tknString]);
                WriteLn('Found PROPERTY');
                if length(tokline.Tokens) = 2 then begin
                    if not TryStrToInt(tokline.Tokens[1].Literal, Result.PropertyAutodestroyCount) then begin
                        WriteLn('ERROR: Expected a number, got something else.');
                        _ErrShowLine(tokline.LineNum, tokline.Tokens[2].LinePos);
                    end;
                    WriteLn('>>> Property has Autodestroy Count of ', Result.PropertyAutodestroyCount);
                end;
                _CheckForBrace();
                Result.ClassProperty := _GetProperty();
            end;

            tknConstants: begin
                _CheckLine(tokline, 0, 0, []);
                WriteLn('Found CONSTANTS');
                _CheckForBrace();
                Result.ClassConstants := _GetConstants();
            end;

            tknHasMethod: begin
                WriteLn('Detected HAS_METHOD');
                // if _FileType <> ooExternal then begin
                if not _TokenValidForFiletypes(tknHasMethod, [ooExternal]) then begin
                    WriteLn('ERROR: tknHasMethod only valid in External files');
                    _ErrShowLine(tokline.LineNum, 1);
                end;
                Result.HasMethod := true;
            end;

            tknHasProperty : begin
                WriteLn('Detected HAS_PROPERTY');
                // if _FileType <> ooExternal then begin
                if not _TokenValidForFiletypes(tknHasProperty, [ooExternal]) then begin
                    WriteLn('ERROR: tknHasProperty only valid in External files');
                    _ErrShowLine(tokline.LineNum, 1);
                end;
                Result.HasProperty := true;
            end;

            tknBraceRight: begin
                exit;
            end;
            else begin
                Writeln('ERROR: Invalid token. Found ', tokline.Tokens[0].TType);
                _ErrShowLine(tokline.LineNum, 1);
            end;
        end;
        tokline := _GetNextLine();
    end;
end;

procedure TPsionOOLexer.Parse();
var
    tokline : TTokenisedLine;
    tok : TToken;
    curElement : TPsionOOFileElement;
begin
    _ResetTLB();

    // First line check

    tokline := _GetNextLine();

    case tokline.Tokens[0].TType of
        tknEOF: begin
            Writeln('INFO: EOF found in initial parser state. (Empty file, or no starter token?)');
            halt;
        end;
        tknName:    _CategoryType := catName;
        tknImage:   _CategoryType := catImage;
        tknLibrary: _CategoryType := catLibrary;
        else begin
            Writeln('ERROR: First token isn''t a valid starter token. (Is there a bug in the lexer?)');
            _ErrShowLine(tokline.LineNum, tokline.Tokens[0].LinePos);
        end;
    end;

    case _FileType of
        ooCategory: begin
            if tokline.Tokens[0].TType = tknName then begin
                WriteLn('ERROR: Category file can''t start with a NAME token');
                _ErrShowLine(tokline.LineNum, tokline.Tokens[0].LinePos);
            end;
        end;
        ooSubCat: begin
            if tokline.Tokens[0].TType <> tknName then begin
                WriteLn('ERROR: Sub-category file can only start with a NAME token');
                _ErrShowLine(tokline.LineNum, tokline.Tokens[0].LinePos);
            end;
        end;
        ooExternal: begin
            if tokline.Tokens[0].TType = tknName then begin
                WriteLn('ERROR: External file can''t start with a NAME token');
                _ErrShowLine(tokline.LineNum, tokline.Tokens[0].LinePos);
            end;
        end;
        else begin
            WriteLn('ERROR: File type is undefined.');
            halt;
        end;
    end;

    if length(tokline.Tokens) = 1 then begin
        Writeln('ERROR: Starter token found, but nothing following it on the line.');
        _ErrShowLine(tokline.LineNum, length(_slCategoryFile[tokline.LineNum - 1]));
    end;
    if length(tokline.Tokens) > 2 then begin
        Writeln('ERROR: Too many tokens on this line. (Is there a bug in the lexer?)');
        _ErrShowLine(tokline.LineNum, tokline.Tokens[2].LinePos);
    end;
    if tokline.Tokens[1].TType <> tknString then begin
        Writeln('ERROR: Incorrect token type. Expected tknString but got ', tokline.Tokens[1].TType, '. (Is there a bug in the lexer?)');
        _ErrShowLine(tokline.LineNum, tokline.Tokens[1].LinePos);
    end;
    
    if _ModuleName <> UpCase(tokline.Tokens[1].Literal) then begin
        WriteLn('ERROR: Token ', tokline.Tokens[1].Literal, ' doesn''t match module name ', _ModuleName);
        _ErrShowLine(tokline.LineNum, tokline.Tokens[1].LinePos);
    end;

    Writeln('Found ', tokline.Tokens[0].TType, ' in ', _FileType, ' file with name ', _ModuleName);

    // Check the rest of the file

    tokline := _GetNextLine();

    while tokline.Tokens[0].TType <> tknEOF do
    begin
        case tokline.Tokens[0].TType of
            tknInclude: begin
                _CheckLine(tokline, 1, 1, [tknString]);
                WriteLn('Found INCLUDE with value ', tokline.Tokens[1].Literal);
                curElement.index := length(_IncludeList);
                curElement.ElementType := incInclude;
                _ElementList := concat(_ElementList, [curElement]);
                _IncludeList := concat(_IncludeList, [tokline.Tokens[1].Literal]);
            end;
            tknExternal: begin
                _CheckLine(tokline, 1, 1, [tknString]);
                WriteLn('Found EXTERNAL with value ', tokline.Tokens[1].Literal);
                curElement.index := length(_ExternalList);
                curElement.ElementType := incExternal;
                _ElementList := concat(_ElementList, [curElement]);
                _ExternalList := concat(_ExternalList, [tokline.Tokens[1].Literal]);
            end;
            tknRequire: begin
                _CheckLine(tokline, 1, 1, [tknString]);
                WriteLn('Found REQUIRE with value ', tokline.Tokens[1].Literal);
                curElement.index := length(_RequireList);
                curElement.ElementType := incRequire;
                _ElementList := concat(_ElementList, [curElement]);
                _RequireList := concat(_RequireList, [tokline.Tokens[1].Literal]);
            end;
            tknClass: begin
                _CheckLine(tokline, 2, 1, [tknString, tknString]);
                Write('Found CLASS with name ', tokline.Tokens[1].Literal);
                if length(tokline.Tokens) = 3 then begin
                    Writeln(', inheriting ', tokline.Tokens[2].Literal);
                end else begin
                    Writeln(' (does not inherit)');
                end;

                _CheckForBrace();
                curElement.index := length(_ClassList);
                curElement.ElementType := incClass;
                _ElementList := concat(_ElementList, [curElement]);
                _ClassList := concat(_ClassList, [_GetClass(tokline)]);
            end;
        end;
        tokline := _GetNextLine();
    end;
end;

function TPsionOOLexer.GetNextToken() : TToken;
begin
    if length(_TokenArray) = 0 then begin
        GetNextToken := _NewToken(0, tknEOF, '');
        exit;
    end;

    if _CurTokenIndex < length(_TokenArray) then inc(_CurTokenIndex);
    GetNextToken := _TokenArray[_CurTokenIndex];
end;

procedure TPsionOOLexer.ShowTree();
var
    i, j : integer;
    element : TPsionOOFileElement;
    method: TPsionOOMethodEntry;
    s: String;
    constant_entry: TPsionOOConstantEntry;
begin
    Writeln;
    Writeln('INCLUDEs');
    Writeln('--------');
    for i := 0 to length(_IncludeList) - 1 do
    begin
        Writeln(_Includelist[i]);
    end;

    Writeln;
    Writeln('EXTERNALs');
    Writeln('---------');
    for i := 0 to length(_ExternalList) - 1 do
    begin
        Writeln(_ExternalList[i]);
    end;

    Writeln;
    Writeln('REQUIREs');
    Writeln('--------');
    for i := 0 to length(_RequireList) - 1 do
    begin
        Writeln(_RequireList[i]);
    end;

    Writeln;
    Writeln('CLASSes');
    Writeln('-------');

    for i := 0 to length(_ClassList) - 1 do
    begin
        Write('Name: ', _ClassList[i].Name);
        if _ClassList[i].Inherits = '' then begin
            Writeln(' (root class)');
        end else begin
            Writeln(' (inherits from ', _classList[i].Inherits, ')');
        end;
        for j := 0 to length(_ClassList[i].Methods) - 1 do
        begin
            Writeln('  ', _ClassList[i].Methods[j].MethodType, ' ', _ClassList[i].Methods[j].Name);
        end;
        Writeln('  Types:');
        for j := 0 to length(_ClassList[i].ClassTypes) - 1 do
        begin
            Writeln('    ', _ClassList[i].ClassTypes[j]);
        end;
        Writeln('  Constants:');
        for j := 0 to length(_ClassList[i].ClassConstants) - 1 do
        begin
            Writeln('    ', _ClassList[i].ClassConstants[j].Name, ' ', _ClassList[i].ClassConstants[i].Value);
        end;
        Writeln('  Property:');
        for j := 0 to length(_ClassList[i].ClassProperty) - 1 do
        begin
            Writeln('    ', _ClassList[i].ClassProperty[j]);
        end;
        if _ClassList[i].HasMethod then WriteLn('  HAS_METHOD set');
        if _ClassList[i].HasProperty then WriteLn('  HAS_PROPERTY set');
        
    end;

    Writeln;
    Writeln('Element List');
    Writeln('------------');

    WriteLn(' Element | Type        | Index');
    WriteLn('---------+-------------+-------');

    for i := 0 to length(_ElementList) - 1 do
    begin
        Str(_ElementList[i].ElementType, s);
        WriteLn(format('    %04d | %-11s | %04d', [i, s, _ElementList[i].index]));
    end;

    Writeln;
    Writeln('Full Tree');
    Writeln('---------');

    case _CategoryType of
        catName:    Write('NAME');
        catImage:   Write('IMAGE');
        catLibrary: Write('LIBRARY');
        else        Write(_CategoryType);
    end;
    WriteLn(' ', LowerCase(_ModuleName));

    for element in _ElementList do
    begin
        case element.ElementType of
            incExternal: begin
                WriteLn('EXTERNAL ', _ExternalList[element.index]);
            end;
            incInclude: begin
                WriteLn('INCLUDE ', _IncludeList[element.index]);
            end;
            incRequire: begin
                WriteLn('REQUIRE ', _RequireList[element.index]);
            end;
            incClass: begin
                Write('CLASS ', _ClassList[element.index].Name, ' ');
                if _ClassList[element.index].Inherits <> '' then
                    Write(_ClassList[element.index].Inherits);
                WriteLn;
                WriteLn('{');
                for method in _ClassList[element.index].Methods do
                begin
                    case method.MethodType of
                        methodReplace: Write('REPLACE ');
                        methodDefer:   Write('DEFER ');
                        methodAdd:     Write('ADD ');
                        methodDeclare: Write('DECLARE ');
                        else           Write(method.MethodType, ' ');
                    end;
                    WriteLn(method.Name);
                end;
                if _ClassList[element.index].HasMethod then WriteLn('HAS_METHOD');
                if _ClassList[element.index].HasProperty then WriteLn('HAS_PROPERTY');
                if length(_ClassList[element.index].ClassConstants) > 0 then begin
                    WriteLn('CONSTANTS');
                    Writeln('{');
                    for constant_entry in _ClassList[element.index].ClassConstants do
                    begin
                        WriteLn(constant_entry.Name, ' ', constant_entry.Value);
                    end;
                    Writeln('}');
                end;
                if length(_ClassList[element.index].ClassTypes) > 0 then begin
                    WriteLn('TYPES');
                    Writeln('{');
                    for s in _ClassList[element.index].ClassTypes do
                    begin
                        WriteLn(s);
                    end;
                    Writeln('}');
                end;
                if length(_ClassList[element.index].ClassProperty) > 0 then begin
                    Write('PROPERTY');
                    if _ClassList[element.index].PropertyAutodestroyCount > 0 then
                        Write(' ', _ClassList[element.index].PropertyAutodestroyCount);
                    WriteLn;
                    Writeln('{');
                    for s in _ClassList[element.index].ClassProperty do
                    begin
                        WriteLn(s);
                    end;
                    Writeln('}');
                end;
                WriteLn('}');
            end;
        end;
    end;

end;

end.

