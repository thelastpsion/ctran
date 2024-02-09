{$mode objfpc}{$H+}{$J-}
unit PsionOOParser;

interface

uses sysutils, PsionOOLexer;

type
    TPsionOOCatType = (
        catName,
        catImage,
        catLibrary
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

    TPsionOOClass = record
        Name : String;
        Inherits : String;
        Methods : array of TPsionOOMethodEntry;
        ClassProperty : array of String;
        ClassTypes : array of String;
        ClassConstants : array of TPsionOOConstantEntry;
        HasMethod : Boolean;
        HasProperty : Boolean;
        PropertyAutodestroyCount : Integer;
    end;

    TPsionOOFileElement = record
        ElementType : TElementType;
        index : integer;
    end;

    TPsionOOParser = class 
        strict private
            // Lexer class (handed to class from Create()
            _lex : TPsionOOLexer;

            // Top-level info about the parsed file
            _Name : String;
            _Filename : String;
            _ElementList : array of TPsionOOFileElement;
            _CategoryType : TPsionOOCatType;

            // Separated lists of things that make up the Element List
            _ClassList : array of TPsionOOClass;
            _RequireList : array of string;
            _IncludeList : array of string;
            _ExternalList : array of string;

            // Various methods
            function _GetClass : TPsionOOClass;
            function _SeekNonNewlineToken() : TToken;
            function _IsToken(curtoken : TToken; TestTokenType: TTokenType) : Boolean;
            function _IsToken(curtoken : TToken; PossibleTokenTypes: array of TTokenType) : Boolean;

        public
            constructor Create(Lexer : TPsionOOLexer);

//        published
//            property Name read _Name;
//            property Filename read _Filename;
    end;


implementation

constructor TPsionOOParser.Create(Lexer : TPsionOOLexer);
var
    curtoken : TToken;
    i, j : Integer;
    curElement : TPsionOOFileElement;

begin
    inherited Create;

    _lex := Lexer;

    _lex.Reset();

//    curtoken := _lex.GetNextToken();
    curtoken := _SeekNonNewlineToken();
    
    case curtoken.TType of
        tknName:     _CategoryType := catName;
        tknImage:    _CategoryType := catImage;
        tknLibrary:  _CategoryType := catLibrary;
        else begin
            Writeln(curtoken.LineNum, ': Bad token found. (Expected tknName, tknImage or tknLibrary, but got ', curtoken.TType, '.)');
            halt;
        end;
    end;

    Writeln('File type is ', _CategoryType);

    while curtoken.TType <> tknEOF do
    begin
        case curtoken.TType of
            tknNewline: begin end; // do nothing

            tknInclude: begin
                curtoken := _lex.GetNextToken();
                if curtoken.TType <> tknString then begin
                    Writeln(curtoken.LineNum, ': INCLUDE statement, bad token found. (Expected tknString but got ', curtoken.TType, '.)');
                    halt;
                end;
                _IncludeList := concat(_IncludeList, [curtoken.Literal]);
                curElement.index := length(_IncludeList);
                curElement.ElementType := incInclude;
                _ElementList := concat(_ElementList, [curElement]);
            end;

            tknExternal: begin
                curtoken := _lex.GetNextToken();
                if curtoken.TType <> tknString then begin
                    Writeln(curtoken.LineNum, ': EXTERNAL statement, bad token found. (Expected tknString but got ', curtoken.TType, '.)');
                    halt;
                end;
                _ExternalList := concat(_ExternalList, [curtoken.Literal]);
                curElement.index := length(_ExternalList);
                curElement.ElementType := incExternal;
                _ElementList := concat(_ElementList, [curElement]);
            end;

            tknRequire: begin
                curtoken := _lex.GetNextToken();
                if curtoken.TType <> tknString then begin
                    Writeln(curtoken.LineNum, ': REQUIRE statement, bad token found. (Expected tknString but got ', curtoken.TType, '.)');
                    halt;
                end;
                _RequireList := concat(_RequireList, [curtoken.Literal]);
                curElement.index := length(_RequireList);
                curElement.ElementType := incRequire;
                _ElementList := concat(_ElementList, [curElement]);
            end;

            tknClass: begin
                _ClassList := concat(_ClassList, [_GetClass()]);
            end;
        end;


        curtoken := _lex.GetNextToken();
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

    Writeln;
    Writeln('Classes:');

    for i := 0 to length(_ClassList) - 1 do
    begin
        Writeln('Name: ', _ClassList[i].Name);
        Writeln('Inherits from: ', _ClassList[i].Inherits);
        for j := 0 to length(_ClassList[i].Methods) - 1 do
        begin
            Writeln('  ', _ClassList[i].Methods[j].MethodType, ' ', _ClassList[i].Methods[j].Name);
        end;
//        for j := 0 to length(_ClassList[i].ClassProperty) - 1 do
//        begin
//            Writeln('  ', _ClassList[i].ClassProperty[j]);
//        end;
    end;
end;

function TPsionOOParser._GetClass() : TPsionOOClass;
var
    curtoken : TToken;
    curMethodEntry : TPsionOOMethodEntry;

begin
    Result.HasMethod := false;
    Result.HasProperty := false;

    curtoken := _lex.GetNextToken();
    //if curtoken.TType <> tknString then begin
    if not(_IsToken(curtoken, tknString)) then begin
        Writeln(curtoken.LineNum, ': CLASS statement, bad token found. (Expected tknString but got ', curtoken.TType, '.)');
        halt;
    end;

    Result.Name := curtoken.Literal;
    Writeln('Found class: ', Result.Name);

    curtoken := _lex.GetNextToken();
    if not(_IsToken(curtoken, [tknString, tknNewline])) then begin
        Writeln(curtoken.LineNum, ': CLASS statement, bad token found. (Expected tknString or tknNewLine but got ', curtoken.TType, '.)');
        halt;
    end;

    Result.Inherits := curtoken.Literal;
    Write('Class ', Result.Name, ' inherits ');
    case Result.Inherits of
        '': Writeln('nothing');
        else Writeln(Result.Inherits);
    end;

//    if curtoken.TType = tknNewline then 
//    curtoken := _lex.GetNextToken();
    curtoken := _SeekNonNewlineToken();

    while curtoken.TType <> tknBraceleft do
    begin
        curtoken := _lex.GetNextToken();
    end;
    
    if not(_IsToken(curtoken, tknBraceLeft)) then begin
        Writeln(curtoken.LineNum, ': CLASS statement, bad token found. (Expected tknBraceLeft but got ', curtoken.TType, '.)');
        halt;
    end;

    while not(_IsToken(curtoken,[tknBraceRight, tknEOF])) do
    begin
        curtoken := _lex.GetNextToken();

        case curtoken.TType of
            tknNewline: begin end;

            tknAdd: begin
                curMethodEntry.MethodType := methodAdd;
                curtoken := _lex.GetNextToken();
                curMethodEntry.Name := curtoken.Literal;
                Result.Methods := concat(Result.Methods, [curMethodEntry]);
            end;

            tknReplace: begin
                curMethodEntry.MethodType := methodReplace;
                curtoken := _lex.GetNextToken();
                curMethodEntry.Name := curtoken.Literal;
                Result.Methods := concat(Result.Methods, [curMethodEntry]);
            end;

            tknDefer: begin
                curMethodEntry.MethodType := methodDefer;
                curtoken := _lex.GetNextToken();
                curMethodEntry.Name := curtoken.Literal;
                Result.Methods := concat(Result.Methods, [curMethodEntry]);
            end;

//            tknProperty: begin
//                curtoken := _lex.GetNextToken();
//                if curtoken.TType = tknString then begin
//                    TryStrToInt(curtoken.Literal, Result.PropertyAutodestroyCount);
//                    curtoken := _lex.GetNextToken();
//                end;
//                
//                curtoken := _SeekNonNewlineToken();
//                Writeln('We''re in the Property section...');
//                if not(_IsToken(curtoken, tknBraceLeft)) then begin
//                    Writeln(curtoken.LineNum, ': CLASS-PROPERTY statement, bad token found. (Expected tknBraceLeft but got ', curtoken.TType, '.)');
//                    halt;
//                end;
//                while not(_IsToken(curtoken, [tknEOF, tknBraceRight])) do
//                begin
//                    case curtoken.TType of
//                        tknNewline: begin end;
//                        tknString: begin
//                            Result.ClassProperty := concat(Result.ClassProperty, [curtoken.Literal]);
//                        end;
//                        else begin
//                            Writeln(curtoken.LineNum, ': CLASS-PROPERTY statement, bad token found. (Expected tknString but got ', curtoken.TType, '.)');
//                            halt;
//                        end;
//                    end;    
//                end;
//            end;

            tknHasMethod: begin
                Result.HasMethod := true;
            end;

            tknHasProperty : begin
                Result.HasProperty := true;
            end;
        end;

    end;


end;


function TPsionOOParser._SeekNonNewlineToken(): TToken;
var
    curtoken : TToken;
begin
    while true do begin
        curtoken := _lex.GetNextToken();
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

