{$mode objfpc}{$H+}{$J-}
program ctran;

uses
    sysutils, PsionOOLexer;

var
    strFilename : String;
    CatLexer : TPsionOOLexer;
    boolExternal : Boolean;
    boolGenG : Boolean;

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

procedure PrintArray(lex : TPsionOOLexer);
var
    s: String;
    tok : TToken;
begin
    Writeln(' Line | Pos | Token Type     | Literal');
    Writeln('------+-----+----------------+-------------');

    for tok in lex.Tokens do
    begin
        Str(tok.TType, s); // Because you can't simply use an enum in format()
        Writeln(format(' %4d | %3d | %-14s | %s', [tok.LineNum, tok.LinePos, s, tok.Literal]));
    end;

    Writeln;
    Writeln('Length: ', Length(lex.Tokens));
end;

procedure ShowTree(lex : TPsionOOLexer);
var
    i, j : integer;
    s: String;
begin
    Writeln;
    Writeln('EXTERNALs');
    Writeln('---------');
    for i := 0 to length(lex.ExternalList) - 1 do
    begin
        Writeln(lex.ExternalList[i]);
    end;

    Writeln;
    Writeln('INCLUDEs');
    Writeln('--------');
    for i := 0 to length(lex.IncludeList) - 1 do
    begin
        Writeln(lex.Includelist[i]);
    end;

    Writeln;
    Writeln('REQUIREs');
    Writeln('--------');
    for i := 0 to length(lex.RequireList) - 1 do
    begin
        Writeln(lex.RequireList[i]);
    end;

    Writeln;
    Writeln('CLASSes');
    Writeln('-------');

    for i := 0 to length(lex.ClassList) - 1 do
    begin
        Write('Name: ', lex.ClassList[i].Name);
        if lex.ClassList[i].Inherits = '' then begin
            Writeln(' (root class)');
        end else begin
            Writeln(' (inherits from ', lex.classList[i].Inherits, ')');
        end;
        for j := 0 to length(lex.ClassList[i].Methods) - 1 do
        begin
            Writeln('  ', lex.ClassList[i].Methods[j].MethodType, ' ', lex.ClassList[i].Methods[j].Name);
        end;
        Writeln('  Types:');
        for j := 0 to length(lex.ClassList[i].ClassTypes) - 1 do
        begin
            Writeln('    ', lex.ClassList[i].ClassTypes[j]);
        end;
        Writeln('  Constants:');
        for j := 0 to length(lex.ClassList[i].ClassConstants) - 1 do
        begin
            Writeln('    ', lex.ClassList[i].ClassConstants[j].Name, ' ', lex.ClassList[i].ClassConstants[i].Value);
        end;
        Writeln('  Property:');
        for j := 0 to length(lex.ClassList[i].ClassProperty) - 1 do
        begin
            Writeln('    ', lex.ClassList[i].ClassProperty[j]);
        end;
        if lex.ClassList[i].HasMethod then WriteLn('  HAS_METHOD set');
        if lex.ClassList[i].HasProperty then WriteLn('  HAS_PROPERTY set');
        
    end;

    Writeln;
    Writeln('Element List');
    Writeln('------------');

    WriteLn(' Element | Type        | Index');
    WriteLn('---------+-------------+-------');

    for i := 0 to length(lex.ElementList) - 1 do
    begin
        Str(lex.ElementList[i].ElementType, s);
        WriteLn(format('    %04d | %-11s | %04d', [i, s, lex.ElementList[i].index]));
    end;
end;


procedure Reconstruct(lex: TPsionOOLexer);
var
    s: String;
    element : TPsionOOFileElement;
    method: TPsionOOMethodEntry;
    constant_entry: TPsionOOConstantEntry;
begin
    Writeln;
    Writeln('Reconstructed File');
    Writeln('------------------');

    case lex.CategoryType of
        catName:    Write('NAME');
        catImage:   Write('IMAGE');
        catLibrary: Write('LIBRARY');
        else        Write(lex.CategoryType);
    end;
    WriteLn(' ', LowerCase(lex.ModuleName));

    for element in lex.ElementList do
    begin
        case element.ElementType of
            incExternal: begin
                WriteLn('EXTERNAL ', lex.ExternalList[element.index]);
            end;
            incInclude: begin
                WriteLn('INCLUDE ', lex.IncludeList[element.index]);
            end;
            incRequire: begin
                WriteLn('REQUIRE ', lex.RequireList[element.index]);
            end;
            incClass: begin
                Write('CLASS ', lex.ClassList[element.index].Name, ' ');
                if lex.ClassList[element.index].Inherits <> '' then
                    Write(lex.ClassList[element.index].Inherits);
                WriteLn;
                WriteLn('{');
                for method in lex.ClassList[element.index].Methods do
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
                if lex.ClassList[element.index].HasMethod then WriteLn('HAS_METHOD');
                if lex.ClassList[element.index].HasProperty then WriteLn('HAS_PROPERTY');
                if length(lex.ClassList[element.index].ClassConstants) > 0 then begin
                    WriteLn('CONSTANTS');
                    Writeln('{');
                    for constant_entry in lex.ClassList[element.index].ClassConstants do
                    begin
                        WriteLn(constant_entry.Name, ' ', constant_entry.Value);
                    end;
                    Writeln('}');
                end;
                if length(lex.ClassList[element.index].ClassTypes) > 0 then begin
                    WriteLn('TYPES');
                    Writeln('{');
                    for s in lex.ClassList[element.index].ClassTypes do
                    begin
                        WriteLn(s);
                    end;
                    Writeln('}');
                end;
                if length(lex.ClassList[element.index].ClassProperty) > 0 then begin
                    Write('PROPERTY');
                    if lex.ClassList[element.index].PropertyAutodestroyCount > 0 then
                        Write(' ', lex.ClassList[element.index].PropertyAutodestroyCount);
                    WriteLn;
                    Writeln('{');
                    for s in lex.ClassList[element.index].ClassProperty do
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

begin
    GetParams();

    if length(strFilename) = 0 then begin
        WriteLn('No filename given.');
        exit;
    end;

    case ExtractFileExt(strFilename) of
        '.', '': strfilename += '.cat';
    end;

    WriteLn('Filename: ', strFilename);
    WriteLn;

    Try
    begin
        CatLexer := TPsionOOLexer.Create;
        CatLexer.LoadFile(strFilename);
        CatLexer.Lex();

        WriteLn;
        PrintArray(CatLexer);
        WriteLn;
        CatLexer.Parse();

        ShowTree(CatLexer);
        Reconstruct(CatLexer);

    end
    finally begin
        FreeAndNil(CatLexer);
    end;
end;

end.

