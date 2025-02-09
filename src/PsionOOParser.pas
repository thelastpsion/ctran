{$mode objfpc}{$H+}{$J-}
{$ModeSwitch typehelpers}
unit PsionOOParser;
{ *** Psion Object Oriented Lexer/Parser ***

A lexer/parser class for Psion SIBO C category files, including external (.EXT)
and internal (.CAT and .CL) types.

}

interface

uses
  sysutils,
  Classes,
  Generics.Collections,
  StringThings;

type
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
    function ToString(): String;
  end;

  TFilePosition = record
    Line: Integer;
    Column: Integer;
  end;

  TToken = record
    TType: TTokenType;
    Literal: String;
    Position: TFilePosition;
  end;

  TTokenList = specialize TList<TToken>;

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
    methodNull, // For Null entries when dealing with inheritance
    methodAdd,
    methodReplace,
    methodDefer,
    methodDeclare
  );

  TPsionOOMethodEntry = record
    MethodType: TMethodType;
    Name: String;
    ForwardRef: String;
  end;

  TPsionOOConstantEntry = record
    Name: String;
    Value: String;
  end;

  TPsionOOConstantList = specialize TList<TPsionOOConstantEntry>;
  TPsionOOMethodList = specialize TList<TPsionOOMethodEntry>;
  TPsionOOCLines = TStringList;

  TPsionOOClass = record
    Name: String;
    Parent: String;
    Methods: TPsionOOMethodList;
    ClassProperty: TStringList;
    ClassTypes: TStringList;
    ClassConstants: TPsionOOConstantList;
    HasMethod: Boolean;
    HasProperty: Boolean;
    PropertyAutodestroyCount: Integer;
  end;

  TPsionOOClassList = array of TPsionOOClass;

  TPsionOOFileElement = record
    ElementType: TElementType;
    index: integer;
  end;

  TPsionOOFileElementList = array of TPsionOOFileElement;

  TTokenisedLine = record
    LineNum: Integer;
    Tokens: TTokenList;
  end;

  TPsionOOParser = class
    strict protected
      // Fields: File Information
      _slCategoryFile: TStringList;
      _FileType: TFileType;
      _FileLocation: String;
      _ModuleName: String;

      // Fields: Lexing
      _Position: TFilePosition;
      _strCurLine: String;
      _LexerState: TLexerState;
      _BraceLevel: Integer;
      _TokenList: TTokenList;

      // Fields: Tokenised Line Builder
      _NextTLBTokenIndex: Integer;

      // Fields: Parser
      _CategoryType: TCategoryType;
      _ElementList: array of TPsionOOFileElement;
      _ExternalList: TStringList;
      _IncludeList: TStringList;
      _ClassList: array of TPsionOOClass;
      _RequireList: TStringList;

      // Methods: Lexing
      procedure _SetLexerState(NewLexerState: TLexerState);
      procedure _DetectFileType(const Filename: String);
      procedure _DetectModuleName(const Filename: String);
      function _NewToken(newTokenLineNum: Integer; newTokenType: TTokenType; newTokenLiteral: String): TToken;
      procedure _AddToken(toktype: TTokenType; tokliteral: String);
      procedure _AddToken(toktype: TTokenType; part_tok: TToken);
      procedure _ProcessCLine();
      procedure _GrabAndAddStringTokens(const ACount: Integer);
      function _GrabNextToken(): TToken;
      procedure _SeekStartOfSection(const NextLexerState: TLexerState);
      procedure _DecBraceLevel();
      procedure _IncBraceLevel();

      // Methods: Lexing (State Machine States)
      procedure _LexStateInitial();
      procedure _LexStateSeekKeyword();
      procedure _LexStateClass();
      procedure _LexStateClassConstants();

      // Methods: Tokenised Line Builder
      function _GetNextLine(): TTokenisedLine;
      procedure _ResetTLB();

      // Methods: Parser
      procedure _CheckLine(tokline: TTokenisedLine; const ATokTypes: array of TTokenType; const AMandatoryArgs: Integer = -1);
      function _GetClass(tokline_class: TTokenisedLine): TPsionOOClass;
      function _GetConstants(): TPsionOOConstantList;
      function _BuildConstant(tokline: TTokenisedLine): TPsionOOConstantEntry;
      function _GetCLines(): TStringList;
      procedure _CheckForBrace();

      // Methods: Misc
      procedure _ErrShowLine(tok: TToken; message: String);
      procedure _ErrShowTokLine(tokline: TTokenisedLine ; toknum: Integer ; message: String);

    public
      Verbose: boolean;
      constructor Create();
      procedure LoadFile(const Filename: String);
      procedure Lex();
      procedure Reset();
      procedure PrintTokenisedLines();

      // Methods: Parser
      procedure Parse();

      property FileType: TFileType read _FileType;
      property FileLocation: String read _FileLocation;
      property ModuleName: String read _ModuleName;
      property CategoryType: TCategoryType read _CategoryType;
      property Tokens: TTokenList read _TokenList;

      property ElementList: TPsionOOFileElementList read _ElementList;
      property RequireList: TStringList read _RequireList;
      property IncludeList: TStringList read _IncludeList;
      property ClassList: TPsionOOClassList read _ClassList;
      property ExternalList: TStringList read _ExternalList;
  end;

implementation

function TTokenTypeHelper.ToString(): String;
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
  _Position.Line := 0;
  _Position.Column := 0;
  _strCurLine := '';
  _BraceLevel := 0;
  if assigned(_ExternalList) then FreeAndNil(_ExternalList);
  _ExternalList := TStringList.Create();
  If assigned(_IncludeList) then FreeAndNil(_IncludeList);
  _IncludeList := TStringList.Create();
  If assigned(_RequireList) then FreeAndNil(_RequireList);
  _RequireList := TStringList.Create();
  If assigned(_TokenList) then FreeAndNil(_TokenList);
  _TokenList := TTokenList.Create();
end;

procedure TPsionOOParser._ResetTLB();
begin
  _NextTLBTokenIndex := 0;
end;

procedure TPsionOOParser._SetLexerState(NewLexerState: TLexerState);
begin
  _LexerState := NewLexerState;
  if Verbose then begin
    Writeln('>>> Now in ', _LexerState);
    if _LexerState in [stateClassSeekStart, stateClassTypesSeekStart, stateClassPropertySeekStart, stateClassConstantsSeekStart] then
      WriteLn('>>>   (looking for brace)');
  end;
end;

//
// TOKENISED LINE BUILDER
//

function TPsionOOParser._GetNextLine(): TTokenisedLine;
begin
  Result.Tokens := TTokenList.Create(); // Because it remembers what was here before!

  if _NextTLBTokenIndex >= _TokenList.Count then begin
    _NextTLBTokenIndex := _TokenList.Count;
    Result.LineNum := 0;
    Result.Tokens.Add(_NewToken(0, tknEOF, ''));
    exit;
  end;

  Result.LineNum := _TokenList[_NextTLBTokenIndex].Position.Line;
  while _NextTLBTokenIndex < _TokenList.Count do
  begin
    if _TokenList[_NextTLBTokenIndex].Position.Line <> Result.LineNum then break;
    Result.Tokens.Add(_TokenList[_NextTLBTokenIndex]);
    inc(_NextTLBTokenIndex);
  end;
end;

procedure TPsionOOParser._ErrShowLine(tok: TToken; message: String);
begin
  WriteLn('ERROR: ', message);
  WriteLn(format('%.3d: %s', [_Position.Line, _strCurLine]));
  Write('    ', RepeatStr(' ', tok.Position.Column), '^');
  Write(RepeatStr('~', length(tok.Literal) - 1));
  WriteLn;
  halt(-1);
end;

procedure TPsionOOParser._ErrShowTokLine(tokline: TTokenisedLine; toknum: Integer; message: String);
var
  spaces: Integer;
  line: String;
begin
  WriteLn('ERROR: ', message);
  line := _slCategoryFile[tokline.LineNum - 1];

  Writeln(format('%.3d: %s', [tokline.LineNum, line]));
  if toknum = -1 then spaces := length(line) + 1 else spaces := tokline.Tokens[toknum].Position.Line;

  Write('    ', RepeatStr(' ', spaces), '^');
  if toknum > -1 then Write(RepeatStr('~', length(tokline.Tokens[toknum].Literal) - 1));
  WriteLn;
  halt(-1);
end;

//
// OUTPUT (should really be in testing)
//

// TODO: Move this into CatDiagnostics (might need to expose _GetNextLine()?)

// Uses _GetNextLine() to display tokenised lines one at a time, showing the original line compared to the next line
procedure TPsionOOParser.PrintTokenisedLines();
var
  tokline: TTokenisedLine;
  tok: TToken;
begin
  WriteLn;
  WriteLn('Tokenised Line Parser!');

  tokline := _GetNextLine();
  while tokline.Tokens[0].TType <> tknEOF do
  begin
    Writeln('Line ', tokline.LineNum, ': (returned ', tokline.Tokens.Count, ' tokens)');
    for tok in tokline.Tokens do
    begin
      WriteLn('   > ', tok.TType, ' ''', tok.Literal, ''' (', tok.Position.Column, ')');
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

// TODO: Is _NewToken() still needed? It's only used to create EOF tokens for adding to tokenised lines.

// Takes a TokenType and a String and puts it into a Token record
function TPsionOOParser._NewToken(newTokenLineNum: Integer; newTokenType: TTokenType; newTokenLiteral: String): TToken;
begin
  Result.TType := newTokenType;
  Result.Literal := newTokenLiteral;
  Result.Position.Line := newTokenLineNum;
  Result.Position.Column := 0; // TODO: I don't think this is right - it wasn't being set before!
end;

// Takes a part-constructed token (with literal, line and column) and adds the provided token type. It then
// adds the finished token to the main token list.
procedure TPsionOOParser._AddToken(toktype: TTokenType; part_tok: TToken);
var
  tok: TToken;
begin
  // TODO: Add checks here (e.g. valid line number and line position)
  tok := part_tok;
  tok.TType := toktype;
  _TokenList.Add(tok);
end;

procedure TPsionOOParser._AddToken(toktype: TTokenType; tokliteral: String);
var
  tok: TToken;
begin
  tok.TType := toktype;
  tok.Literal := tokliteral;
  tok.Position := _Position;

  _TokenList.Add(tok);
end;

//
// CHANGE BRACE LEVEL
//

procedure TPsionOOParser._DecBraceLevel();
begin
  Dec(_BraceLevel);
  if Verbose then Writeln('>>>   Brace level: ', _BraceLevel);
end;

procedure TPsionOOParser._IncBraceLevel();
begin
  Inc(_BraceLevel);
  if Verbose then Writeln('>>>   Brace level: ', _BraceLevel);
end;

//
// LINE PROCESSING
//

// The actual tokeniser. Not very complicated, but it doesn't need to be.
function TPsionOOParser._GrabNextToken(): TToken;
var
  column: Integer;
  flgFoundText: Boolean = false;
  curChar: Char;
begin
  Result.Literal := '';
  Result.TType := tknEOF;
  Result.Position.Line := 0;
  Result.Position.Column := 0;

  for column := _Position.Column to length(_strCurLine) do
  begin
    curChar := _strCurLine[column];
    if Trim(curChar) = '' then begin
      if flgFoundText then begin
        _Position.Column := column + 1;
        exit;
      end;
    end else if curChar in ['=', '{', '}'] then begin
      if flgFoundText then begin
        _Position.Column := column;
      end else begin
        Result.Literal := curChar;
        Result.Position.Line := _Position.Line;
        Result.Position.Column := column;
        _Position.Column := column + 1;
      end;
      exit;
    end else begin
      Result.Literal += curChar;
      if not(flgFoundText) then begin
        Result.Position.Line := _Position.Line;
        Result.Position.Column := column;
        flgFoundText := true;
      end;
    end;
  end;
  _Position.Column += Length(Result.Literal);
end;

// For the rest of the line, find up to `ACount` string tokens and add them to the token list. Note "up to" - the tokens
// do not have to be there, as it just adds as many as it can find.
procedure TPsionOOParser._GrabAndAddStringTokens(const ACount: Integer);
var
  i: Integer;
  tok: TToken;
begin
  if Verbose then WriteLn('>>> Fetching up to ', ACount, ' token(s)');
  for i := 1 to ACount do
  begin
    tok := _GrabNextToken();
    if tok.Literal = '' then begin
      if Verbose then WriteLn('>>>   No more tokens on line ', _Position.Line);
      exit;
    end;
    if Verbose then WriteLn(format('>>>   String token %d grabbed: %s', [i, tok.Literal]));
    _AddToken(tknString, tok);
  end;
end;

procedure TPsionOOParser._ProcessCLine();
var
  tok: TToken;
begin
  if not (_LexerState in [stateClassTypes, stateClassProperty]) then begin
    raise Exception.Create('_ProcessCLine called when not processing a TYPES or PROPERTY block');
  end;

  // This only checks the first token on the line, just like the original CTRAN
  // TODO: Check for curly braces in the middle of lines (e.g. `typedef struct {`)
  tok := _GrabNextToken();

  case tok.Literal of
    '{': _IncBraceLevel();

    '}': begin
      _DecBraceLevel();
      if _BraceLevel = 1 then begin
        if Verbose then case _LexerState of
          stateClassTypes:     Writeln('>>> End of TYPES section found!');
          stateClassProperty:  Writeln('>>> End of PROPERTY section found!');
        end;
        _AddToken(tknBraceRight, tok);
        _SetLexerState(stateClass);
      end;
    end;
  end;

  _Position.Column := tok.Position.Column;

  if _BraceLevel > 1 then begin
      TrimAfterSemicolon(_strCurLine);
      if Verbose then Writeln ('>>> Found string: ', _strCurLine);
      _AddToken(tknString, _StrCurLine);
  end;
end;

// _DetectFileType()
// Identifies the type of category file (regular, sub-category, external) based on the
// file extension. If no file extension is provided, it assumes a regular category file.
procedure TPsionOOParser._DetectFileType(const Filename: String);
var
  ext: String;
begin
  ext := UpCase(ExtractFileExt(Filename));

  case ext of
    '', '.': _FileType := ooCategory;
    '.CAT':  _FileType := ooCategory;
    '.CL':   _FileType := ooSubCat;
    '.EXT':  _FileType := ooExternal;
    else begin
      WriteLn('Error: Unknown file type.');
      halt(-1);
    end;
  end;
  if Verbose then WriteLn('File is ', _FileType);
end;

procedure TPsionOOParser._DetectModuleName(const Filename: String);
var
  s: String;
begin
  s := ExtractFileName(Filename);
  _ModuleName := Copy(UpCase(s), 1, Length(s) - Length(ExtractFileExt(s)));

  if Verbose then Writeln('Module name: ', _ModuleName);
end;

procedure TPsionOOParser.LoadFile(const Filename: String);
begin
  _slCategoryFile := TStringList.Create;

  _slCategoryFile.LoadFromFile(Filename);
  _DetectFileType(Filename);
  _DetectModuleName(Filename);
  _FileLocation := ExpandFileName(Filename);
end;

//
// STATE MACHINE METHODS
//

procedure TPsionOOParser._LexStateInitial();
var
  part_tok: TToken;
  TokLiteral: String;
  TokType: TTokenType;
begin
  part_tok := _GrabNextToken();
  TokLiteral := UpCase(part_tok.Literal);

  case TokLiteral of
    'IMAGE':   TokType := tknImage;
    'LIBRARY': TokType := tknLibrary;
    'NAME':    TokType := tknName;
    // If no match is found above, just exit the method and go back to lexing the next line
    else exit;
  end;

  if Verbose then Writeln('>>> ', TokLiteral, ' found!');
  _AddToken(TokType, part_tok);
  _GrabAndAddStringTokens(1); // There should always be a string token after one of these keywords
  _SetLexerState(stateSeekKeyword);
end;

procedure TPsionOOParser._LexStateSeekKeyword();
var
  part_tok: TToken;
  TokLiteral: String;
  TokType: TTokenType;
begin
  part_tok := _GrabNextToken();
  TokLiteral := UpCase(part_tok.Literal);

  case TokLiteral of
    'EXTERNAL': TokType := tknExternal;
    'INCLUDE':  TokType := tknInclude;
    'CLASS':    TokType := tknClass;
    'REQUIRE':  TokType := tknRequire;
    // If no match is found above, we have a problem so we need to halt
    else begin
      _ErrShowLine(part_tok, 'Invalid string literal ''' + part_tok.Literal + '''');
    end;
  end;

  if Verbose then Writeln('>>> ', TokLiteral, ' found!');
  _AddToken(TokType, part_tok);
  _GrabAndAddStringTokens(1); // There should always be a string token after one of these keywords

  if TokType = tknClass then begin
    _GrabAndAddStringTokens(1); // Grab the name of the class's parent, too (if it's there)
    _SetLexerState(stateClassSeekStart);
  end;
end;

// _SeekStartOfSection()
// Looks for a tknBraceLeft. If found, it changes the current lexer state to NextLexerState.
// It also increments the brace level.
procedure TPsionOOParser._SeekStartOfSection(const NextLexerState: TLexerState);
var
  part_tok: TToken;
begin
  part_tok := _GrabNextToken();

  if part_tok.Literal = '{' then begin
    if Verbose then Writeln('>>> Start of section found!');
    _AddToken(tknBraceLeft, part_tok);
    _SetLexerState(NextLexerState);
    _IncBraceLevel();
  end;
end;

procedure TPsionOOParser._LexStateClass();
var
  part_tok: TToken;
  x: LongInt; // Only used for TryStrToInt()

  procedure CheckForForwardRef();
  begin
    part_tok := _GrabNextToken();
    if part_tok.Literal = '=' then begin
      _AddToken(tknEquals, part_tok);
      _GrabAndAddStringTokens(1);
    end;
  end;

begin
  part_tok := _GrabNextToken();
  case UpCase(part_tok.Literal) of
    '}': begin
      if Verbose then Writeln('>>> End of CLASS section found!');
      _AddToken(tknBraceRight, part_tok);
      _DecBraceLevel();
      _SetLexerState(stateSeekKeyword);
    end;
    'ADD': begin
      if Verbose then Writeln('>>> ADD found!');
      _AddToken(tknAdd, part_tok);
      _GrabAndAddStringTokens(1);
      CheckForForwardRef();
    end;
    'REPLACE': begin
      if Verbose then Writeln('>>> REPLACE found!');
      _AddToken(tknReplace, part_tok);
      _GrabAndAddStringTokens(1);
      CheckForForwardRef();
    end;
    'DEFER': begin
      if Verbose then Writeln('>>> DEFER found!');
      _AddToken(tknDefer, part_tok);
      _GrabAndAddStringTokens(1);
    end;
    'CONSTANTS': begin
      if Verbose then Writeln('>>> CONSTANTS found!');
      _AddToken(tknConstants, part_tok);
      _SetLexerState(stateClassConstantsSeekStart);
    end;
    'TYPES': begin
      if Verbose then Writeln('>>> TYPES found!');
      _AddToken(tknTypes, part_tok);
      _SetLexerState(stateClassTypesSeekStart);
    end;
    'PROPERTY': begin
      if Verbose then Writeln('>>> PROPERTY found!');
      _AddToken(tknProperty, part_tok);
      part_tok := _GrabNextToken();
      if Verbose then Writeln('>>> Literal grabbed: ', part_tok.Literal);
      if TryStrToInt(part_tok.Literal, x) then begin
        if Verbose then Writeln('>>> Number found!');
        _AddToken(tknString, part_tok);
      end;
      _SetLexerState(stateClassPropertySeekStart);
    end;
    // External reference (.EXT) keywords
    'DECLARE': begin
      if Verbose then Writeln('>>> DECLARE found!');
      _AddToken(tknDeclare, part_tok);
      _GrabAndAddStringTokens(1);
    end;
    'HAS_METHOD': begin
      if Verbose then Writeln('>>> HAS_METHOD found!');
      _AddToken(tknHasMethod, part_tok);
    end;
    'HAS_PROPERTY': begin
      if Verbose then Writeln('>>> HAS_PROPERTY found!');
      _AddToken(tknHasProperty, part_tok);
    end;
    else begin
      _ErrShowLine(part_tok, 'Invalid string literal ''' + part_tok.Literal + '''');
    end;
  end;
end;


procedure TPsionOOParser._LexStateClassConstants();
var
  part_tok: TToken;
begin
  part_tok := _GrabNextToken();
  case part_tok.Literal of
    '}': begin
      _AddToken(tknBraceRight, part_tok);
      _DecBraceLevel();
      if Verbose then Writeln('>>> End of CONSTANTS section found!');
      _SetLexerState(stateClass);
    end;
    '{': begin
      _ErrShowLine(part_tok, 'Too many curly braces');
      exit;
    end else begin
      _Position.Column := part_tok.Position.Column;
      _GrabAndAddStringTokens(2);
    end;
  end;
end;

//
// MAIN LEXER STATE MACHINE
//

// TODO: Check for braces inside lines?
procedure TPsionOOParser.Lex();
begin
  _LexerState := stateInitial;
  _Position.Line := 0;

  while _Position.Line < _slCategoryFile.Count do
  begin
    Inc(_Position.Line);
    _Position.Column := 1;

    if Verbose then WriteLn();

    while LeftStr(_slCategoryFile[_Position.Line - 1], 1) = #12 do
      _slCategoryFile[_Position.Line - 1] := Copy(_slCategoryFile[_Position.Line - 1], 2);

    _strCurLine := _slCategoryFile[_Position.Line - 1];

    if Verbose then WriteLn(format('%.3d:%s', [_Position.Line, _strCurLine]));

    if Length(_strCurLine.Trim) = 0 then begin
      if Verbose then Writeln('>>> Empty line');
      continue;
    end;

    if _strCurLine.Trim[1] = '!' then begin
      if Verbose then Writeln('>>> Explicit comment, line skipped');
      continue;
    end;

    case _LexerState of
      stateInitial:                 _LexStateInitial();
      stateSeekKeyword:             _LexStateSeekKeyword();
      stateClassSeekStart:          _SeekStartOfSection(stateClass);
      stateClass:                   _LexStateClass(); 
      stateClassConstantsSeekStart: _SeekStartOfSection(stateClassConstants);
      stateClassConstants:          _LexStateClassConstants();
      stateClassTypesSeekStart:     _SeekStartOfSection(stateClassTypes);
      stateClassTypes:              _ProcessCLine;
      stateClassPropertySeekStart:  _SeekStartOfSection(stateClassProperty);
      stateClassProperty:           _ProcessCLine;
    end;
  end;

  if _BraceLevel <> 0 then begin
    WriteLn('Error: [Lex] Somehow at brace level ', _BraceLevel); // TODO: Should this be an exception?
    exit;
  end;

  _Position.Column := 0;
  _AddToken(tknEOF, '');
end;

procedure TPsionOOParser._CheckLine(tokline: TTokenisedLine; const ATokTypes: array of TTokenType; const AMandatoryArgs: Integer = -1);
var
  i: Integer;
  tokline_ArgCount: Integer;
  ArgCheckCount: Integer;
  MaxMandatoryArgs: Integer;
begin
  ArgCheckCount := Length(ATokTypes);
  tokline_ArgCount := tokline.Tokens.Count - 1;

  if AMandatoryArgs < 0 then // 0 mandatory arguments is valid when all args are optional
    MaxMandatoryArgs := ArgCheckCount // This is the default, but it also just ignores when a negative number is specified
  else
    MaxMandatoryArgs := AMandatoryArgs;

  // Make sure that, if the last token is tknEOF, it isn't classed as an argument
  if tokline.Tokens[tokline_argcount].TType = tknEOF then dec(tokline_argcount);

  if ArgCheckCount < MaxMandatoryArgs then begin
    raise Exception.Create('_CheckLine: The number of mandatory arguments requested is higher than the number of tokens given in toktypes');
  end;

  if tokline_ArgCount < MaxMandatoryArgs then begin
    _ErrShowTokLine(tokline, -1, format('Current line has too few (%d) arguments', [tokline_argcount]));
  end;

  if tokline_ArgCount - 1 > ArgCheckCount then begin
    _ErrShowTokLine(tokline, ArgCheckCount + 1, format('Current line has too many (%d) arguments', [tokline_argcount]))
  end;

  for i := 1 to tokline_ArgCount - 1 do
  begin
    if (tokline.Tokens[i].TType <> ATokTypes[i-1]) then begin
      _ErrShowTokLine(tokline, 1, format('Incorrect token type. Expected %s but found %s', [ATokTypes[i-1], tokline.Tokens[i].TType.ToString()]));
    end;
  end;
end;

function TPsionOOParser._BuildConstant(tokline: TTokenisedLine): TPsionOOConstantEntry;
begin
  Result.Name := tokline.Tokens[0].Literal;
  Result.Value := tokline.Tokens[1].Literal;
end;

function TPsionOOParser._GetConstants(): TPsionOOConstantList;
var
  tokline: TTokenisedLine;
begin
  tokline := _GetNextLine();
  Result := TPsionOOConstantList.Create();

  while tokline.Tokens[0].TType <> tknEOF do
  begin
    case tokline.Tokens[0].TType of
      tknBraceRight: begin
        exit;
      end;
      tknString: begin
        _CheckLine(tokline, [tknString]);
        Result.Add(_BuildConstant(tokline));
      end;
      else begin
        _ErrShowTokLine(tokline, 0, format('Incorrect token, found %s', [tokline.Tokens[0].TType.ToString()]));
      end;
    end;
    tokline := _GetNextLine();
  end;
end;

function TPsionOOParser._GetCLines(): TStringList;
var
  tokline: TTokenisedLine;
begin
  tokline := _GetNextLine();
  Result := TStringList.Create();

  while tokline.Tokens[0].TType <> tknEOF do
  begin
    case tokline.Tokens[0].TType of
      tknBraceRight: begin
        exit;
      end;
      tknString: begin
        _CheckLine(tokline, []);
        Result.Add(tokline.Tokens[0].Literal);
      end;
      else begin
        _ErrShowTokLine(tokline, 0, format('Incorrect token, found %s', [tokline.Tokens[0].TType.ToString()]));
      end;
    end;
    tokline := _GetNextLine();
  end;
end;

procedure TPsionOOParser._CheckForBrace();
var
  tokline: TTokenisedLine;
begin
  tokline := _GetNextLine();
  if tokline.Tokens[0].TType <> tknBraceLeft then begin
    _ErrShowTokLine(tokline, 0, format('Expected tknBraceLeft, found %s', [tokline.Tokens[0].TType.ToString()]));
  end;
  _CheckLine(tokline, []);
end;

// procedure TPsionOOParser._AddMethodEntry(method_type: TMethodType; s: String);
// var
//   curMethodEntry: TPsionOOMethodEntry;
// begin
//   curMethodEntry.MethodType := method_type;
//   curMethodEntry.Name := s;
//   Result.Methods := concat(Result.Methods, [curMethodEntry]);
// end;

function TPsionOOParser._GetClass(tokline_class: TTokenisedLine): TPsionOOClass;
var
  tokline: TTokenisedLine;
  curMethodEntry: TPsionOOMethodEntry;

  procedure StopIfEXT();
  begin
    if _FileType = ooExternal then begin
      _ErrShowTokLine(tokline, 0, format('%s not valid in External files', [tokline.Tokens[0].Literal]));
    end;
  end;

  procedure StopIfNotEXT();
  begin
    if _FileType <> ooExternal then begin
      _ErrShowTokLine(tokline, 0, format('%s only valid in External files', [tokline.Tokens[0].Literal]));
    end;
  end;

begin
  if tokline_class.Tokens[0].TType <> tknClass then begin
    _ErrShowTokLine(tokline_class, 0, '[_GetClass] Been sent the wrong line.');
  end;

  _CheckLine(tokline_class, [tknString, tknString], 1);

  Result.Name := tokline_class.Tokens[1].Literal;
  if tokline_class.Tokens.Count = 3 then begin
    Result.Parent := tokline_class.Tokens[2].Literal;
  end else begin
    Result.Parent := '';
  end;

  Result.HasMethod := false;
  Result.HasProperty := false;
  Result.ClassConstants := TPsionOOConstantList.Create();
  Result.ClassProperty := TStringList.Create();
  Result.ClassTypes := TStringList.Create();
  Result.Methods := TPsionOOMethodList.Create();
  Result.PropertyAutodestroyCount := 0;

  tokline := _GetNextLine();

  while tokline.Tokens[0].TType <> tknEOF do
  begin
    curMethodEntry.ForwardRef := '';

    case tokline.Tokens[0].TType of
      tknAdd: begin
        StopIfEXT();
        _CheckLine(tokline, [tknString, tknEquals, tknString], 1);
        curMethodEntry.MethodType := methodAdd;
        curMethodEntry.Name := tokline.Tokens[1].Literal;
        if tokline.Tokens.Count = 4 then begin
          curMethodEntry.ForwardRef := tokline.Tokens[3].Literal;
        end;
        Result.Methods.Add(curMethodEntry);
      end;

      tknReplace: begin
        StopIfEXT();
        _CheckLine(tokline, [tknString, tknEquals, tknString], 1);
        curMethodEntry.MethodType := methodReplace;
        curMethodEntry.Name := tokline.Tokens[1].Literal;
        if tokline.Tokens.Count = 4 then begin
          curMethodEntry.ForwardRef := tokline.Tokens[3].Literal;
        end;
        Result.Methods.Add(curMethodEntry);
      end;

      tknDefer: begin
        StopIfEXT();
        _CheckLine(tokline, [tknString]);
        curMethodEntry.MethodType := methodDefer;
        curMethodEntry.Name := tokline.Tokens[1].Literal;
        Result.Methods.Add(curMethodEntry);
      end;

      tknDeclare: begin
        StopIfNotEXT();
        _CheckLine(tokline, [tknString]);
        curMethodEntry.MethodType := methodDeclare;
        curMethodEntry.Name := tokline.Tokens[1].Literal;
        Result.Methods.Add(curMethodEntry);
      end;

      tknTypes: begin
        StopIfEXT();
        _CheckLine(tokline, []);
        if Verbose then WriteLn('Found TYPES');
        _CheckForBrace();
        Result.ClassTypes := _GetCLines();
      end;

      tknProperty: begin
        StopIfEXT();
        _CheckLine(tokline, [tknString], 0);
        if Verbose then WriteLn('Found PROPERTY');
        if tokline.Tokens.Count = 2 then begin
          if not TryStrToInt(tokline.Tokens[1].Literal, Result.PropertyAutodestroyCount) then begin
            _ErrShowTokLine(tokline, 2, 'Expected a number, found something else.');
          end;
          if Verbose then WriteLn('>>> Property has Autodestroy Count of ', Result.PropertyAutodestroyCount);
        end;
        _CheckForBrace();
        Result.ClassProperty := _GetCLines();
      end;

      tknConstants: begin
        StopIfEXT();
        _CheckLine(tokline, []);
        if Verbose then WriteLn('Found CONSTANTS');
        _CheckForBrace();
        Result.ClassConstants := _GetConstants();
      end;

      tknHasMethod: begin
        StopIfNotEXT();
        if Verbose then WriteLn('Found HAS_METHOD');
        Result.HasMethod := true;
      end;

      tknHasProperty: begin
        StopIfNotEXT();
        if Verbose then WriteLn('Found HAS_PROPERTY');
        Result.HasProperty := true;
      end;

      tknBraceRight: begin
        exit;
      end;
      else begin
        _ErrShowTokLine(tokline, 0, format('Invalid token. Found %s', [tokline.Tokens[0].TType.ToString()]));
      end;
    end;
    tokline := _GetNextLine();
  end;
end;

procedure TPsionOOParser.Parse();
var
  tokline: TTokenisedLine;

  procedure AddElement(AElementIndex: Integer; AElementType: TElementType);
  var
    curElement: TPsionOOFileElement;
  begin
    curElement.index := AElementIndex;
    curElement.ElementType := AElementType;
    _ElementList := concat(_ElementList, [curElement]);
  end;

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
      _ErrShowTokLine(tokline, 0, 'First token isn''t a valid starter token. (Is there a bug in the lexer?)')
    end;
  end;

  case _FileType of
    ooCategory: begin
      if tokline.Tokens[0].TType = tknName then begin
        _ErrShowTokLine(tokline, 0, 'Category file can''t start with a NAME token');
      end;
    end;
    ooSubCat: begin
      if tokline.Tokens[0].TType <> tknName then begin
        _ErrShowTokLine(tokline, 0, 'Sub-category file can only start with a NAME token');
      end;
    end;
    ooExternal: begin
      if tokline.Tokens[0].TType = tknName then begin
        _ErrShowTokLine(tokline, 0, 'External file can''t start with a NAME token');
      end;
    end;
    else begin
      WriteLn('Error: File type is undefined.'); // TODO: Should this be an exception?
      halt(-1);
    end;
  end;

  _CheckLine(tokline, [tknString]);

  if _ModuleName <> UpCase(tokline.Tokens[1].Literal) then begin
      _ErrShowTokLine(tokline, 1, format('Token %s doesn''t match module name %s', [tokline.Tokens[1].Literal, _ModuleName]));
  end;

  if Verbose then Writeln('Found ', tokline.Tokens[0].TType, ' in ', _FileType, ' file with name ', _ModuleName);

  // Check the rest of the file

  tokline := _GetNextLine();

  while tokline.Tokens[0].TType <> tknEOF do
  begin
    case tokline.Tokens[0].TType of
      tknInclude: begin
        _CheckLine(tokline, [tknString]);
        if Verbose then WriteLn('Found INCLUDE with value ', tokline.Tokens[1].Literal);
        AddElement(_IncludeList.Count, incInclude);
        _IncludeList.Add(tokline.Tokens[1].Literal);
      end;
      tknExternal: begin
        if _FileType <> ooCategory then begin
          _ErrShowTokLine(tokline, 1, 'EXTERNAL can only be used in category files (not external or sub-category files)');
        end;
        _CheckLine(tokline, [tknString]);
        if Verbose then WriteLn('Found EXTERNAL with value ', tokline.Tokens[1].Literal);
        AddElement(_ExternalList.Count, incExternal);
        _ExternalList.Add(tokline.Tokens[1].Literal);
      end;
      tknRequire: begin
        _CheckLine(tokline, [tknString]);
        if Verbose then WriteLn('Found REQUIRE with value ', tokline.Tokens[1].Literal);
        AddElement(_RequireList.Count, incRequire);
        _RequireList.Add(tokline.Tokens[1].Literal);
      end;
      tknClass: begin
        _CheckLine(tokline, [tknString, tknString], 1);
        if Verbose then begin
          Write('Found CLASS with name ', tokline.Tokens[1].Literal);
          if tokline.Tokens.Count = 3 then begin
            Writeln(', inheriting ', tokline.Tokens[2].Literal);
          end else begin
            Writeln(' (does not inherit)');
          end;
        end;

        _CheckForBrace();
        AddElement(length(_ClassList), incClass);
        _ClassList := concat(_ClassList, [_GetClass(tokline)]);
      end;
    end;
    tokline := _GetNextLine();
  end;
end;

end.

