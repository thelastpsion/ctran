{$mode objfpc}{$H+}{$J-}
unit PsionOOParser;

interface

uses PsionOOLexer;

type
    TElementType = (
        incInclude,
        incExternal,
        incRequire,
        incClass
    );

//    TIncludedFile = record
//        FileIdentifier : String;
//        IncludeType : TIncludeType;
//    end;

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

    TPsionOOClass = record
        Name : String;
        Inherits : String;
        Methods : array of TPsionOOMethodEntry;
        ClassProperty : array of String;
        ClassTypes : array of String;
        ClassConstants : array of TPsionOOConstantEntry;
        HasMethod : Boolean;
        HasProperty : Boolean;
    end;

    TPsionOOFileElement = record
        ElementType : TElementType;
        index : integer;
    end;

    TPsionOOParser = class 
        strict private
            _ClassList : array of TPsionOOClass;
            _RequireList : array of string;
            _IncludeList : array of string;
            _ExternalList : array of string;
            _FileList : array of TPsionOOFileElement;
            _TheLexer : TPsionOOLexer;
            procedure _GetClass;
            function _SeekNonNewlineToken(): TToken;
            function _IsToken(curtoken : TToken; TestTokenType: TTokenType) : Boolean;
            function _IsToken(curtoken : TToken; PossibleTokenTypes: array of TTokenType) : Boolean;
        public
            constructor Create(Lexer : TPsionOOLexer);
            

    end;



implementation

constructor TPsionOOParser.Create(Lexer : TPsionOOLexer);
var
    curtoken : TToken;
    i : Integer;
    curElement : TPsionOOFileElement;

begin
    inherited Create;

    _TheLexer := Lexer;

    _TheLexer.Reset();

//    curtoken := _TheLexer.GetNextToken();
    curtoken := _SeekNonNewlineToken();
    
    case curtoken.TType of
        tknName, tknImage, tknLibrary: begin end; // do nothing
        else begin
            Writeln(curtoken.LineNum, ': Bad token found. (Expected tknName, tknImage or tknLibrary, but got ', curtoken.TType, '.)');
            halt;
        end;
    end;

    Writeln('File type is ', curtoken.TType);

    while curtoken.TType <> tknEOF do
    begin
        case curtoken.TType of
            tknNewline: begin end; // do nothing
            tknInclude: begin
                curtoken := _TheLexer.GetNextToken();
                if curtoken.TType <> tknString then begin
                    Writeln(curtoken.LineNum, ': INCLUDE statement, bad token found. (Expected tknString but got ', curtoken.TType, '.)');
                    halt;
                end;
                _IncludeList := concat(_IncludeList, [curtoken.Literal]);
                curElement.index := length(_IncludeList);
                curElement.ElementType := incInclude;
                _FileList := concat(_FileList, [curElement]);
            end;
            tknExternal: begin
                curtoken := _TheLexer.GetNextToken();
                if curtoken.TType <> tknString then begin
                    Writeln(curtoken.LineNum, ': EXTERNAL statement, bad token found. (Expected tknString but got ', curtoken.TType, '.)');
                    halt;
                end;
                _ExternalList := concat(_ExternalList, [curtoken.Literal]);
                curElement.index := length(_ExternalList);
                curElement.ElementType := incExternal;
                _FileList := concat(_FileList, [curElement]);
            end;
            tknRequire: begin
                curtoken := _TheLexer.GetNextToken();
                if curtoken.TType <> tknString then begin
                    Writeln(curtoken.LineNum, ': REQUIRE statement, bad token found. (Expected tknString but got ', curtoken.TType, '.)');
                    halt;
                end;
                _RequireList := concat(_RequireList, [curtoken.Literal]);
                curElement.index := length(_RequireList);
                curElement.ElementType := incRequire;
                _FileList := concat(_FileList, [curElement]);
            end;

            tknClass: begin
                _GetClass();
            end;
        end;


        curtoken := _TheLexer.GetNextToken();
    end;

    Writeln;
    Writeln('Includes:');
    for i := 0 to length(_IncludeList) - 1 do
    begin
        Writeln(_Includelist[i]);
    end;

    Writeln;
    Writeln('Externals:');
    for i := 0 to length(_ExternalList) - 1 do
    begin
        Writeln(_ExternalList[i]);
    end;

    Writeln;
    Writeln('Requires:');
    for i := 0 to length(_RequireList) - 1 do
    begin
        Writeln(_RequireList[i]);
    end;

end;

procedure TPsionOOParser._GetClass();
var
    curtoken : TToken;
    curClass : TPsionOOClass;
    curMethodEntry : TPsionOOMethodEntry;
    i : Integer;

begin
    curtoken := _TheLexer.GetNextToken();
    //if curtoken.TType <> tknString then begin
    if not(_IsToken(curtoken, tknString)) then begin
        Writeln(curtoken.LineNum, ': CLASS statement, bad token found. (Expected tknString but got ', curtoken.TType, '.)');
        halt;
    end;

    curClass.Name := curtoken.Literal;
    Writeln('Found class: ', curClass.Name);

    curtoken := _TheLexer.GetNextToken();
    if not(_IsToken(curtoken, [tknString, tknNewline])) then begin
        Writeln(curtoken.LineNum, ': CLASS statement, bad token found. (Expected tknString or tknNewLine but got ', curtoken.TType, '.)');
        halt;
    end;

    curClass.Inherits := curtoken.Literal;
    Write('Class ', curClass.Name, ' inherits ');
    case curClass.Inherits of
        '': Writeln('nothing');
        else Writeln(curClass.Inherits);
    end;

    if curtoken.TType = tknNewline then 
    curtoken := _TheLexer.GetNextToken();

    while curtoken.TType <> tknBraceleft do
    begin
        curtoken := _TheLexer.GetNextToken();
    end;
    
    while not(_IsToken(curtoken,[tknBraceRight, tknEOF])) do
    begin
        curtoken := _TheLexer.GetNextToken();

        case curtoken.TType of
            tknAdd: begin
                curMethodEntry.MethodType := methodAdd;
                curtoken := _TheLexer.GetNextToken();
                curMethodEntry.Name := curtoken.Literal;
            end;
            tknReplace: begin
                curMethodEntry.MethodType := methodReplace;
                curtoken := _TheLexer.GetNextToken();
                curMethodEntry.Name := curtoken.Literal;
            end;
            tknDefer: begin
                curMethodEntry.MethodType := methodDefer;
                curtoken := _TheLexer.GetNextToken();
                curMethodEntry.Name := curtoken.Literal;
            end;
        end;
        
    end;

end;


function TPsionOOParser._SeekNonNewlineToken(): TToken;
var
    curtoken : TToken;
begin
    while true do begin
        curtoken := _TheLexer.GetNextToken();
        if curtoken.TType <> tknNewline then begin
            result := curtoken;
            exit;
        end;
    end;
end;


function TPsionOOParser._IsToken(curtoken : TToken; TestTokenType: TTokenType) : Boolean;
begin
    result := (TestTokenType = curtoken.TType)
end;

function TPsionOOParser._IsToken(curtoken : TToken; PossibleTokenTypes: array of TTokenType) : Boolean;
var
    TestTokenType : TTokenType;
begin
    result := false;
    for TestTokenType in PossibleTokenTypes do
    begin
        if TestTokenType = curtoken.TType then begin
            result := true;
            exit;
        end;
    end;
end;

end.
