{$mode objfpc}{$H+}{$J-}
unit PsionOOCatDiagnostics;

interface
uses sysutils, PsionOOLexer;

procedure PrintArray(lex : TPsionOOLexer);
procedure ShowTree(lex : TPsionOOLexer);
procedure Reconstruct(lex: TPsionOOLexer);

implementation

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
    i : integer;
    s: String;
    constant_entry: TPsionOOConstantEntry;
    method_entry: TPsionOOMethodEntry;
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

        for method_entry in lex.ClassList[i].Methods do
        begin
            Writeln('  ', method_entry.MethodType, ' ', method_entry.Name);
        end;

        Writeln('  Types (',  length(lex.ClassList[i].ClassTypes), '):');
        for s in lex.ClassList[i].ClassTypes do
        begin
            Writeln('    ', s);
        end;


        Writeln('  Constants (', length(lex.ClassList[i].ClassConstants), '):');
        for constant_entry in lex.ClassList[i].ClassConstants do
        begin
            Writeln('    ', constant_entry.Name, ' ', constant_entry.Value);
        end;

        Writeln('  Property (', length(lex.ClassList[i].ClassProperty),'):');
        for s in lex.ClassList[i].ClassProperty do
        begin
            Writeln('    ', s);
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

end.
