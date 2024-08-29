{$mode objfpc}{$H+}{$J-}
{$ModeSwitch typehelpers}
unit PsionOOParser;
{ *** Psion Object Oriented Lexer/Parser ***

A lexer/parser class for Psion SIBO C category files, including external (.EXT)
and internal (.CAT and .CL) types.

}

interface

uses
    sysutils, Classes, StringThings;

type
    // TokenType = string;
    TTokenType = (
        tknString,

        // Symbols
        tknBraceLeft,
        tknBraceRight,
        tknEquals,

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

        tknEOF
    );

    TTokenTypeHelper = Type Helper for TTokenType
        function ToString() : String;
    end;

    TToken = record
        TType : TTokenType;
        Literal : string;
        LineNum : Integer;
        LinePos : Integer;
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
        ForwardRef : String;
    end;

    TPsionOOConstantEntry = record
        Name : String;
        Value : String;
    end;

    TPsionOOConstants = array of TPsionOOConstantEntry;
    TPsionOOTypes = TStringArray;
    TPsionOOProperty = TStringArray;

    TPsionOOClass = record
        Name : String;
        Parent : String;
        Methods : array of TPsionOOMethodEntry;
        ClassProperty : TPsionOOProperty;
        ClassTypes : TPsionOOTypes;
        ClassConstants : TPsionOOConstants;
        HasMethod : Boolean;
        HasProperty : Boolean;
        PropertyAutodestroyCount : Integer;
    end;

    TPsionOOClassList = array of TPsionOOClass;

    TPsionOOFileElement = record
        ElementType : TElementType;
        index : integer;
    end;

    TPsionOOFileElementList = array of TPsionOOFileElement;

    TTokenisedLine = record
        LineNum : Integer;
        Tokens : array of TToken;
    end;

    TPsionOOParser = class
        strict private
            // Fields: File Information
            _slCategoryFile : TStringList;
            _FileType : TFileType;
            _FileLocation : String;
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
            // _CurToken : TToken;

            // Fields: Tokenised Line Builder
            _nextTLBTokenIndex : Integer;

            // Fields: Parser
            _CategoryType : TCategoryType;
            _ElementList : array of TPsionOOFileElement;
            _ExternalList : TStringArray;
            _IncludeList : TStringArray;
            _ClassList : array of TPsionOOClass;
            _RequireList : TStringArray;

            // Methods: Lexing
            procedure _DetectFileType(strFilename : String);
            procedure _DetectModuleName(strFilename : String);
            function _NewToken(newTokenLineNum: Integer; newTokenType: TTokenType; newTokenLiteral: String): TToken;
            procedure _AddToken(toktype: TTokenType; tokliteral: String);
            procedure _AddToken(toktype: TTokenType; part_tok : TToken);
            procedure _ProcessCLine();
            procedure _GrabAndAddStringTokens(count : Integer);
            function _GrabNextToken() : TToken;
            procedure _SeekStartOfSection(NextLexerState : TLexerState);

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
            // function _TokenValidForFiletypes(toktype: TTokenType; valid_filetypes: array of TFileType) : boolean;

            // Methods: Misc
            procedure _ErrShowLine(tokline : TTokenisedLine ; toknum : Integer ; message : String);

        public
            Verbose : boolean;
            constructor Create();
            procedure LoadFile(strFilename : String);
            procedure Lex();
            procedure Reset();
            procedure PrintTokenisedLines();

            // Methods: Parser
            procedure Parse();

            property FileType : TFileType read _FileType;
            property FileLocation : String read _FileLocation;
            property ModuleName : String read _ModuleName;
            property CategoryType : TCategoryType read _CategoryType;
            property Tokens : TTokenArray read _TokenArray;

            property ElementList : TPsionOOFileElementList read _ElementList;
            property RequireList : TStringArray read _RequireList;
            property IncludeList : TStringArray read _IncludeList;
            property ClassList : TPsionOOClassList read _ClassList;
            property ExternalList : TStringArray read _ExternalList;
    end;

implementation

function TTokenTypeHelper.ToString() : String;
begin
    WriteStr(Result, self);
end;

constructor TPsionOOParser.Create();
begin
    inherited Create;
    _slCategoryFile := TStringList.Create;

    Reset();

    // Tokenised Line Builder
    _resetTLB();
end;

procedure TPsionOOParser.Reset();
begin
    _LexerState := stateInitial;
    _curLineNum := 0;
    _curLinePos := 0;
    _strCurLine := '';
    _strFilename := '';
    _CurTokenIndex := -1;
    _BraceLevel := 0;
end;

procedure TPsionOOParser._ResetTLB();
begin
    _nextTLBTokenIndex := 0;
end;

//
// TOKENISED LINE BUILDER
//

function TPsionOOParser._GetNextLine() : TTokenisedLine;
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

procedure TPsionOOParser._ErrShowLine(tokline : TTokenisedLine ; toknum : Integer ; message : String);
var
    spaces : Integer;
    line : String;
begin
    WriteLn('Error: ', message);
    line := _slCategoryFile[tokline.LineNum - 1];

    Writeln(format('%.3d: %s', [tokline.LineNum, line]));
    if toknum = -1 then spaces := length(line) else spaces := tokline.Tokens[toknum].LinePos;

    Write('    ', RepeatStr(' ', spaces), '^');
    if toknum > -1 then Write(RepeatStr('~', length(tokline.Tokens[toknum].Literal) - 1));
    WriteLn;
    halt(-1);
end;

//
// OUTPUT (should really be in testing)
//

procedure TPsionOOParser.PrintTokenisedLines();
var
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
function TPsionOOParser._NewToken(newTokenLineNum: Integer; newTokenType: TTokenType; newTokenLiteral: String): TToken;
begin
    Result.TType := newTokenType;
    Result.Literal := newTokenLiteral;
    Result.LineNum := newTokenLineNum;
end;

procedure TPsionOOParser._AddToken(toktype: TTokenType; part_tok: TToken);
var
    tok: TToken;
begin
    // TODO: Add checks here (e.g. valid line number and line position)
    tok := part_tok;
    tok.TType := toktype;
    _TokenArray := concat(_TokenArray, [tok]);
end;

procedure TPsionOOParser._AddToken(toktype: TTokenType; tokliteral: String);
var
    tok: TToken;
begin
    tok.TType := toktype;
    tok.Literal := tokliteral;
    tok.LineNum := _curLineNum;
    tok.LinePos := _curLinePos;

    _TokenArray := concat(_TokenArray, [tok]);
end;

//
// LINE PROCESSING
//

function TPsionOOParser._GrabNextToken() : TToken;
var
    pos : Integer;
    flgFoundText : Boolean = false;
begin
    Result.Literal := '';
    Result.TType := tknEOF;
    Result.LineNum := 0;
    Result.LinePos := 0;

    for pos := _curLinePos to length(_strCurLine) do
    begin
        if trim(_strCurLine[pos]) = '' then begin
            if flgFoundText then begin
                _curLinePos := pos + 1;
                exit;
            end;
        end else if (ansipos(_strCurLine[pos], '={}') > 0) then begin
            if flgFoundText then begin
                _curLinePos := pos;
            end else begin
                Result.Literal := _strCurLine[pos];
                Result.LineNum := _curLineNum;
                Result.LinePos := pos;
                _curLinePos := pos + 1;
            end;
            exit;
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

procedure TPsionOOParser._GrabAndAddStringTokens(count : Integer);
var
    i : Integer;
    tok : TToken;
begin
    if Verbose then WriteLn('>>> Fetching up to ', count, ' token(s)');
    for i := 1 to count do
    begin
        tok := _GrabNextToken();
        if tok.Literal = '' then begin
            if Verbose then Writeln('>>>   No more tokens on line ', _curLineNum);
            exit;
        end;
        if Verbose then Writeln(format('>>>   String token %d grabbed: %s', [i, tok.Literal]));
        _AddToken(tknString, tok);
    end;
end;

procedure TPsionOOParser._ProcessCLine();
var
    tok: TToken;
begin
    if (_LexerState <> stateClassTypes) and (_LexerState <> stateClassProperty) then begin
        WriteLn('_ProcessCLine: Why am I here?');
        halt(-1);
    end;

    tok := _GrabNextToken();

    case tok.Literal of
        '{': begin
            inc(_BraceLevel);
            if Verbose then Writeln('>>>   Brace level: ', _BraceLevel);
        end;
        '}': begin
            dec(_BraceLevel);
            if Verbose then Writeln('>>>   Brace level: ', _BraceLevel);
            if _BraceLevel = 1 then begin
                if Verbose then case _LexerState of
                    stateClassTypes:     Writeln('>>> End of TYPES section found!');
                    stateClassProperty:  Writeln('>>> End of PROPERTY section found!');
                end;
                _AddToken(tknBraceRight, tok);
                _LexerState := stateClass;
                if Verbose then Writeln('>>> Now in stateClass');
            end;
        end;
    end;

    _curLinePos := tok.LinePos;

    if _BraceLevel > 1 then begin
        TrimAfterSemicolon(_strCurLine);
        if Verbose then Writeln ('>>> Found string: ', _strCurLine);
        _AddToken(tknString, _StrCurLine);
    end;
end;

// _SeekStartOfSection()
// Looks for a tknBraceLeft. If found, it changes the current lexer state to NextLexerState.
// It also increments the brace level.
procedure TPsionOOParser._SeekStartOfSection(NextLexerState : TLexerState);
var
    tok: TToken;
begin
    tok := _GrabNextToken();

    if tok.Literal = '{' then begin
        if Verbose then Writeln('>>> Start of section found!');
        _AddToken(tknBraceLeft, tok);

        _LexerState := NextLexerState;
        if Verbose then Writeln('>>>   Now in ', _LexerState);

        inc(_BraceLevel);
        if Verbose then Writeln('>>>   Brace level: ', _BraceLevel);
    end;
end;

// _DetectFileType()
// Identifies the type of category file (regular, sub-category, external) based on the
// file extension. If no file extension is provided, it assumes a regular category file.
procedure TPsionOOParser._DetectFileType(strFilename : String);
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
            WriteLn('Error: Unknown file type.');
            halt(-1);
        end;
    end;
    if Verbose then WriteLn('File is ', _FileType);
end;

procedure TPsionOOParser._DetectModuleName(strFilename : String);
var
    s : String;
begin
    s := ExtractFileName(strFilename);
    _ModuleName := Copy(UpCase(s), 1, Length(s) - length(ExtractFileExt(s)));

    if Verbose then Writeln('Module name: ', _ModuleName);
end;

procedure TPsionOOParser.LoadFile(strFilename : String);
begin
    _slCategoryFile := TStringList.Create;

    _slCategoryFile.LoadFromFile(strFilename);
    _DetectFileType(strFilename);
    _DetectModuleName(strFilename);
    _FileLocation := ExpandFileName(strFilename);
end;

// TODO: Check for braces inside lines?
procedure TPsionOOParser.Lex();
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

        if Verbose then WriteLn;

        while LeftStr(_slCategoryFile[_curLineNum - 1], 1) = #12 do
            _slCategoryFile[_curLineNum - 1] := copy(_slCategoryFile[_curLineNum - 1], 2);

        _strCurLine := _slCategoryFile[_curLineNum - 1];

        if Verbose then WriteLn(format('%.3d:%s', [_curLineNum, _strCurLine]));

        if length(_strCurLine.Trim) = 0 then begin
            if Verbose then Writeln('>>> Empty line');
            continue;
        end;

        if _strCurLine.Trim[1] = '!' then begin
            if Verbose then Writeln('>>> Explicit comment, line skipped');
            continue;
        end;

        case _LexerState of
            stateInitial: begin
                tok := _GrabNextToken();
                case UpCase(tok.Literal) of
                    'IMAGE': begin
                        if Verbose then Writeln('>>> IMAGE found!');
                        _AddToken(tknImage, tok);
                        _LexerState := stateSeekKeyword;
                    end;
                    'LIBRARY': begin
                        if Verbose then Writeln('>>> LIBRARY found!');
                        _AddToken(tknLibrary, tok);
                        _LexerState := stateSeekKeyword;
                    end;
                    'NAME': begin
                        if Verbose then Writeln('>>> NAME found!');
                        _AddToken(tknName, tok);
                        _LexerState := stateSeekKeyword;
                    end;
                end;
                if _LexerState = stateSeekKeyword then
                begin
                    if Verbose then WriteLn('>>>   Now in stateSeekKeyword');
                    _GrabAndAddStringTokens(1);
                end;
            end;

            stateSeekKeyword: begin
                tok := _GrabNextToken();
                case UpCase(tok.Literal) of
                    'EXTERNAL': begin
                        if Verbose then Writeln('>>> EXTERNAL found!');
                        _AddToken(tknExternal, tok);
                        _GrabAndAddStringTokens(1);
                    end;
                    'INCLUDE': begin
                        if Verbose then Writeln('>>> INCLUDE found!');
                        _AddToken(tknInclude, tok);
                        _GrabAndAddStringTokens(1);
                    end;
                    'CLASS': begin
                        if Verbose then Writeln('>>> CLASS found!');
                        _AddToken(tknClass, tok);
                        _GrabAndAddStringTokens(2);
                        _LexerState := stateClassSeekStart;
                        if Verbose then Writeln('>>>   Now in stateClassSeekStart (looking for brace)');
                    end;
                    'REQUIRE': begin
                        if Verbose then Writeln('>>> REQUIRE found!');
                        _AddToken(tknRequire, tok);
                        _GrabAndAddStringTokens(1);
                    end;
                    else begin
                        WriteLn('!!! Invalid string literal found: ', tok.Literal);
                        halt(-1);
                    end;
                end;
            end;

            stateClassSeekStart: _SeekStartOfSection(stateClass);

            stateClass: begin
                tok := _GrabNextToken();
                case UpCase(tok.Literal) of
                    '}': begin
                        _AddToken(tknBraceRight, tok);
                        if Verbose then Writeln('>>> End of CLASS section found!');
                        dec(_BraceLevel);
                        if Verbose then Writeln('>>>   Brace level: ', _BraceLevel);
                        _LexerState := stateSeekKeyword;
                        if Verbose then Writeln('>>> Now in stateSeekKeyword');
                    end;
                    'ADD': begin
                        if Verbose then Writeln('>>> ADD found!');
                        _AddToken(tknAdd, tok);
                        _GrabAndAddStringTokens(1);
                        tok := _GrabNextToken();
                        if tok.Literal = '=' then begin
                            _AddToken(tknEquals, tok);
                            _GrabAndAddStringTokens(1);
                        end;
                    end;
                    'REPLACE': begin
                        if Verbose then Writeln('>>> REPLACE found!');
                        _AddToken(tknReplace, tok);
                        _GrabAndAddStringTokens(1);
                        tok := _GrabNextToken();
                        if tok.Literal = '=' then begin
                            _AddToken(tknEquals, tok);
                            _GrabAndAddStringTokens(1);
                        end;
                    end;
                    'DEFER': begin
                        if Verbose then Writeln('>>> DEFER found!');
                        _AddToken(tknDefer, tok);
                        _GrabAndAddStringTokens(1);
                    end;
                    'CONSTANTS': begin
                        if Verbose then Writeln('>>> CONSTANTS found!');
                        _AddToken(tknConstants, tok);
                        _LexerState := stateClassConstantsSeekStart;
                        if Verbose then Writeln('>>>   Now in stateClassConstantsSeekStart');
                    end;
                    'TYPES': begin
                        if Verbose then Writeln('>>> TYPES found!');
                        _AddToken(tknTypes, tok);
                        _LexerState := stateClassTypesSeekStart;
                        if Verbose then Writeln('>>>   Now in stateClassTypesSeekStart');
                    end;
                    'PROPERTY': begin
                        if Verbose then Writeln('>>> PROPERTY found!');
                        _AddToken(tknProperty, tok);
                        tok := _GrabNextToken();
                        if Verbose then Writeln('>>> Literal grabbed: ', tok.Literal);
                        if TryStrToInt(tok.Literal, x) then begin
                            if Verbose then Writeln('>>> Number found!');
                            _AddToken(tknString, tok);
                        end;
                        _LexerState := stateClassPropertySeekStart;
                        if Verbose then Writeln('>>>   Now in stateClassPropertySeekStart');
                    end;
                    // External reference (.EXT) keywords
                    'DECLARE': begin
                        if Verbose then Writeln('>>> DECLARE found!');
                        _AddToken(tknDeclare, tok);
                        _GrabAndAddStringTokens(1);
                    end;
                    'HAS_METHOD': begin
                        if Verbose then Writeln('>>> HAS_METHOD found!');
                        _AddToken(tknHasMethod, tok);
                    end;
                    'HAS_PROPERTY': begin
                        if Verbose then Writeln('>>> HAS_PROPERTY found!');
                        _AddToken(tknHasProperty, tok);
                    end;
                    else begin
                        WriteLn('!!! Invalid string literal found: ', tok.Literal);
                        halt(-1);
                    end;
                end;
            end;

            stateClassConstantsSeekStart: _SeekStartOfSection(stateClassConstants);

            stateClassConstants: begin
                tok := _GrabNextToken();
                case tok.Literal of
                    '}': begin
                        _AddToken(tknBraceRight, tok);
                        dec(_BraceLevel);
                        _LexerState := stateClass;
                        if Verbose then begin
                            Writeln('>>> End of CONSTANTS section found!');
                            Writeln('>>>   Brace level: ', _BraceLevel);
                            Writeln('>>> Now in stateClass');
                        end;
                    end;
                    '{': begin
                        Writeln('!!! Too many curly braces');
                        exit;
                    end else begin
                        _curLinePos := tok.LinePos;
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

    if _BraceLevel <> 0 then begin
        WriteLn('Error: [Lex] Somehow at brace level ', _BraceLevel);
        exit;
    end;

    _curLinePos := 0;
    _AddToken(tknEOF, '');
end;

procedure TPsionOOParser._CheckLine(tokline : TTokenisedLine; args : Integer; compulsary_args : Integer; toktypes : array of TTokenType);
var
    i : Integer;
    tokline_argcount : Integer;
begin
    tokline_argcount := length(tokline.Tokens) - 1;

    // Make sure that, if the last token is tknEOF, it isn't classed as an argument
    if tokline.Tokens[tokline_argcount].TType = tknEOF then dec(tokline_argcount);

    if args < compulsary_args then begin
        WriteLn('[_CheckLine] args is less than compulsary_args');
        halt(-1);
    end;
    if length(toktypes) <> args then begin
        WriteLn('[_CheckLine] args doesn''t equal the number of token types provided');
        halt(-1);
    end;

    if tokline_argcount < compulsary_args then begin
        _ErrShowLine(tokline, -1, format('Current line has too few (%d) arguments', [tokline_argcount]));
    end;

    if tokline_argcount - 1 > args then begin
        _ErrShowLine(tokline, args + 1, format('Current line has too many (%d) arguments', [tokline_argcount]))
    end;

    for i := 1 to tokline_argcount - 1 do
    begin
        if (tokline.Tokens[i].TType <> toktypes[i-1]) then begin
            _ErrShowLine(tokline, 1, format('Incorrect token type. Expected %s but got %s', [toktypes[i-1], tokline.Tokens[i].TType.ToString()]));
        end;
    end;
end;

// function TPsionOOParser._TokenValidForFiletypes(toktype: TTokenType; valid_filetypes: array of TFileType) : boolean;
// var
//     ft: TFileType;
// begin
//     Result := false;
//
//     for ft in valid_filetypes do
//     begin
//         if ft = self.FileType then exit(true);
//     end;
// end;

function TPsionOOParser._BuildConstant(tokline : TTokenisedLine) : TPsionOOConstantEntry;
begin
    Result.Name := tokline.Tokens[0].Literal;
    Result.Value := tokline.Tokens[1].Literal;
end;

function TPsionOOParser._GetConstants() : TPsionOOConstants;
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
                _ErrShowLine(tokline, 0, format('Incorrect token, found %s', [tokline.Tokens[0].TType.ToString()]));
            end;
        end;
        tokline := _GetNextLine();
    end;
end;

function TPsionOOParser._GetTypes() : TPsionOOTypes;
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
                _ErrShowLine(tokline, 0, format('Incorrect token, found %s', [tokline.Tokens[0].TType.ToString()]));
            end;
        end;
        tokline := _GetNextLine();
    end;
end;

function TPsionOOParser._GetProperty() : TPsionOOProperty;
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
                _ErrShowLine(tokline, 0, format('Incorrect token, found %s', [tokline.Tokens[0].TType.ToString()]));
            end;
        end;
        tokline := _GetNextLine();
    end;
end;

procedure TPsionOOParser._CheckForBrace();
var
    tokline : TTokenisedLine;
begin
    tokline := _GetNextLine();
    if tokline.Tokens[0].TType <> tknBraceLeft then begin
        _ErrShowLine(tokline, 0, format('Expected tknBraceLeft, got %s', [tokline.Tokens[0].TType.ToString()]));
    end;
    _CheckLine(tokline, 0, 0, []);
end;

// procedure TPsionOOParser._AddMethodEntry(method_type: TMethodType, s: String);
// var
//     curMethodEntry : TPsionOOMethodEntry;
// begin
//     curMethodEntry.MethodType := method_type;
//     curMethodEntry.Name := s;
//     Result.Methods := concat(Result.Methods, [curMethodEntry]);
// end;

function TPsionOOParser._GetClass(tokline_class : TTokenisedLine) : TPsionOOClass;
var
    tokline : TTokenisedLine;
    curMethodEntry : TPsionOOMethodEntry;
begin
    if tokline_class.Tokens[0].TType <> tknClass then begin
        _ErrShowLine(tokline_class, 0, '[_GetClass] Been sent the wrong line.');
    end;

    _CheckLine(tokline_class, 2, 1, [tknString, tknString]);

    Result.Name := tokline_class.Tokens[1].Literal;
    if length(tokline_class.Tokens) = 3 then begin
        Result.Parent := tokline_class.Tokens[2].Literal;
    end else begin
        Result.Parent := '';
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
                if _FileType = ooExternal then begin
                    _ErrShowLine(tokline, 0, 'tknAdd not valid in External files');
                end;
                _CheckLine(tokline, 3, 1, [tknString, tknEquals, tknString]);
                curMethodEntry.MethodType := methodAdd;
                curMethodEntry.Name := tokline.Tokens[1].Literal;
                if length(tokline.Tokens) = 4 then begin
                    curMethodEntry.ForwardRef := tokline.Tokens[3].Literal;
                end else begin
                    curMethodEntry.ForwardRef := '';
                end;
                Result.Methods := concat(Result.Methods, [curMethodEntry]);
            end;

            tknReplace: begin
                if _FileType = ooExternal then begin
                    _ErrShowLine(tokline, 0, 'tknReplace not valid in External files');
                end;
                _CheckLine(tokline, 3, 1, [tknString, tknEquals, tknString]);
                curMethodEntry.MethodType := methodReplace;
                curMethodEntry.Name := tokline.Tokens[1].Literal;
                if length(tokline.Tokens) = 4 then begin
                    curMethodEntry.ForwardRef := tokline.Tokens[3].Literal;
                end else begin
                    curMethodEntry.ForwardRef := '';
                end;
                Result.Methods := concat(Result.Methods, [curMethodEntry]);
            end;

            tknDefer: begin
                if _FileType = ooExternal then begin
                    _ErrShowLine(tokline, 0, 'tknDefer not valid in External files');
                end;
                _CheckLine(tokline, 1, 1, [tknString]);
                curMethodEntry.MethodType := methodDefer;
                curMethodEntry.Name := tokline.Tokens[1].Literal;
                Result.Methods := concat(Result.Methods, [curMethodEntry]);
            end;

            tknDeclare: begin
                if _FileType <> ooExternal then begin
                    _ErrShowLine(tokline, 0, 'tknDeclare only valid in External files');
                end;
                _CheckLine(tokline, 1, 1, [tknString]);
                curMethodEntry.MethodType := methodDeclare;
                curMethodEntry.Name := tokline.Tokens[1].Literal;
                Result.Methods := concat(Result.Methods, [curMethodEntry]);
            end;

            tknTypes: begin
                if _FileType = ooExternal then begin
                    _ErrShowLine(tokline, 0, 'tknTypes not valid in External files');
                end;
                _CheckLine(tokline, 0, 0, []);
                if Verbose then WriteLn('Found TYPES');
                _CheckForBrace();
                Result.ClassTypes := _GetTypes();
            end;

            tknProperty: begin
                if _FileType = ooExternal then begin
                    _ErrShowLine(tokline, 0, 'tknProperty not valid in External files');
                end;
                _CheckLine(tokline, 1, 0, [tknString]);
                if Verbose then WriteLn('Found PROPERTY');
                if length(tokline.Tokens) = 2 then begin
                    if not TryStrToInt(tokline.Tokens[1].Literal, Result.PropertyAutodestroyCount) then begin
                        _ErrShowLine(tokline, 2, 'Expected a number, got something else.');
                    end;
                    if Verbose then WriteLn('>>> Property has Autodestroy Count of ', Result.PropertyAutodestroyCount);
                end;
                _CheckForBrace();
                Result.ClassProperty := _GetProperty();
            end;

            tknConstants: begin
                if _FileType = ooExternal then begin
                    _ErrShowLine(tokline, 1, 'tknConstants not valid in External files');
                end;
                _CheckLine(tokline, 0, 0, []);
                if Verbose then WriteLn('Found CONSTANTS');
                _CheckForBrace();
                Result.ClassConstants := _GetConstants();
            end;

            tknHasMethod: begin
                if _FileType <> ooExternal then begin
                    _ErrShowLine(tokline, 0, 'tknHasMethod only valid in External files');
                end;
                if Verbose then WriteLn('Detected HAS_METHOD');
                Result.HasMethod := true;
            end;

            tknHasProperty : begin
                if _FileType <> ooExternal then begin
                    _ErrShowLine(tokline, 0, 'tknHasProperty only valid in External files');
                end;
                if Verbose then WriteLn('Detected HAS_PROPERTY');
                Result.HasProperty := true;
            end;

            tknBraceRight: begin
                exit;
            end;
            else begin
                _ErrShowLine(tokline, 0, format('Invalid token. Found %s', [tokline.Tokens[0].TType.ToString()]));
            end;
        end;
        tokline := _GetNextLine();
    end;
end;

procedure TPsionOOParser.Parse();
var
    tokline : TTokenisedLine;
    curElement : TPsionOOFileElement;
begin
    _ResetTLB();

    // First line check

    tokline := _GetNextLine();

    case tokline.Tokens[0].TType of
        tknEOF: begin
            Writeln('INFO: EOF found in initial parser state. (Empty file, or no starter token?)');
            halt(-1);
        end;
        tknName:    _CategoryType := catName;
        tknImage:   _CategoryType := catImage;
        tknLibrary: _CategoryType := catLibrary;
        else begin
            _ErrShowLine(tokline, 0, 'First token isn''t a valid starter token. (Is there a bug in the lexer?)')
        end;
    end;

    case _FileType of
        ooCategory: begin
            if tokline.Tokens[0].TType = tknName then begin
                _ErrShowLine(tokline, 0, 'Category file can''t start with a NAME token');
            end;
        end;
        ooSubCat: begin
            if tokline.Tokens[0].TType <> tknName then begin
                _ErrShowLine(tokline, 0, 'Sub-category file can only start with a NAME token');
            end;
        end;
        ooExternal: begin
            if tokline.Tokens[0].TType = tknName then begin
                _ErrShowLine(tokline, 0, 'External file can''t start with a NAME token');
            end;
        end;
        else begin
            WriteLn('Error: File type is undefined.');
            halt(-1);
        end;
    end;

    if length(tokline.Tokens) = 1 then begin
        _ErrShowLine(tokline, -1, 'Starter token found, but nothing following it on the line.');
    end;
    if length(tokline.Tokens) > 2 then begin
        _ErrShowLine(tokline, 2, 'Too many tokens on this line. (Is there a bug in the lexer?)');
    end;
    if tokline.Tokens[1].TType <> tknString then begin
        _ErrShowLine(tokline, 1, format('Incorrect token type. Expected tknString but got %s. (Is there a bug in the lexer?)', [ tokline.Tokens[1].TType.ToString()]));
    end;

    if _ModuleName <> UpCase(tokline.Tokens[1].Literal) then begin
        _ErrShowLine(tokline, 1, format('Token %s doesn''t match module name %s', [tokline.Tokens[1].Literal, _ModuleName]));
    end;

    if Verbose then Writeln('Found ', tokline.Tokens[0].TType, ' in ', _FileType, ' file with name ', _ModuleName);

    // Check the rest of the file

    tokline := _GetNextLine();

    while tokline.Tokens[0].TType <> tknEOF do
    begin
        case tokline.Tokens[0].TType of
            tknInclude: begin
                _CheckLine(tokline, 1, 1, [tknString]);
                if Verbose then WriteLn('Found INCLUDE with value ', tokline.Tokens[1].Literal);
                curElement.index := length(_IncludeList);
                curElement.ElementType := incInclude;
                _ElementList := concat(_ElementList, [curElement]);
                _IncludeList := concat(_IncludeList, [tokline.Tokens[1].Literal]);
            end;
            tknExternal: begin
                if _FileType <> ooCategory then begin
                    _ErrShowLine(tokline, 1, 'EXTERNAL can only be used in category files (not external or sub-category files)');
                end;
                _CheckLine(tokline, 1, 1, [tknString]);
                if Verbose then WriteLn('Found EXTERNAL with value ', tokline.Tokens[1].Literal);
                curElement.index := length(_ExternalList);
                curElement.ElementType := incExternal;
                _ElementList := concat(_ElementList, [curElement]);
                _ExternalList := concat(_ExternalList, [tokline.Tokens[1].Literal]);
            end;
            tknRequire: begin
                _CheckLine(tokline, 1, 1, [tknString]);
                if Verbose then WriteLn('Found REQUIRE with value ', tokline.Tokens[1].Literal);
                curElement.index := length(_RequireList);
                curElement.ElementType := incRequire;
                _ElementList := concat(_ElementList, [curElement]);
                _RequireList := concat(_RequireList, [tokline.Tokens[1].Literal]);
            end;
            tknClass: begin
                _CheckLine(tokline, 2, 1, [tknString, tknString]);
                if Verbose then begin
                    Write('Found CLASS with name ', tokline.Tokens[1].Literal);
                    if length(tokline.Tokens) = 3 then begin
                        Writeln(', inheriting ', tokline.Tokens[2].Literal);
                    end else begin
                        Writeln(' (does not inherit)');
                    end;
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

end.

