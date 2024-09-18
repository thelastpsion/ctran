{$mode objfpc}{$H+}{$J-}
unit PsionOOCatDiagnostics;

interface
uses sysutils, PsionOOParser;

procedure PrintArray(par: TPsionOOParser);
procedure ShowTree(par: TPsionOOParser);
procedure Reconstruct(par: TPsionOOParser);

implementation

procedure PrintArray(par: TPsionOOParser);
var
    s: String;
    tok : TToken;
begin
    WriteLn;
    Writeln(' Line | Pos | Token Type     | Literal');
    Writeln('------+-----+----------------+-------------');

    for tok in par.Tokens do
    begin
        Str(tok.TType, s); // Because you can't simply use an enum in format()
        Writeln(format(' %4d | %3d | %-14s | %s', [tok.LineNum, tok.LinePos, s, tok.Literal]));
    end;

    Writeln;
    Writeln('Length: ', Length(par.Tokens));
end;

procedure ShowTree(par: TPsionOOParser);
var
    i : integer;
    s: String;
    class_item: TPsionOOClass;
    constant_entry: TPsionOOConstantEntry;
    method_entry: TPsionOOMethodEntry;
begin
    Writeln;
    Writeln('EXTERNALs');
    Writeln('---------');
    for s in par.ExternalList do WriteLn(s);

    Writeln;
    Writeln('INCLUDEs');
    Writeln('--------');
    for s in par.IncludeList do WriteLn(s);

    Writeln;
    Writeln('REQUIREs');
    Writeln('--------');
    for s in par.RequireList do WriteLn(s);

    Writeln;
    Writeln('CLASSes');
    Writeln('-------');

    for class_item in par.ClassList do
    begin
        Write('Name: ', class_item.Name);
        if class_item.Parent = '' then begin
            Writeln(' (root class)');
        end else begin
            Writeln(' (inherits from ', class_item.Parent, ')');
        end;

        for method_entry in class_item.Methods do
        begin
            Write('  ', method_entry.MethodType, ' ', method_entry.Name);
            if method_entry.ForwardRef <> '' then
            begin
                Write(' ( = ', method_entry.ForwardRef, ')');
            end;
            WriteLn;
        end;

        Writeln('  Types (',  class_item.ClassTypes.Count, '):');
        for s in class_item.ClassTypes do Writeln('    ', s);

        Writeln('  Constants (', length(class_item.ClassConstants), '):');
        for constant_entry in class_item.ClassConstants do
        begin
            Writeln('    ', constant_entry.Name, ' ', constant_entry.Value);
        end;

        Writeln('  Property (', class_item.ClassProperty.Count,'):');
        for s in class_item.ClassProperty do Writeln('    ', s);

        if class_item.HasMethod then WriteLn('  HAS_METHOD set');
        if class_item.HasProperty then WriteLn('  HAS_PROPERTY set');
    end;

    Writeln;
    Writeln('Element List');
    Writeln('------------');

    WriteLn(' Element | Type        | Index');
    WriteLn('---------+-------------+-------');

    for i := 0 to length(par.ElementList) - 1 do
    begin
        Str(par.ElementList[i].ElementType, s);
        WriteLn(format('    %04d | %-11s | %04d', [i, s, par.ElementList[i].index]));
    end;
end;

//
// Reconstruct Category File
//

procedure ReconstructClass(cls: TPsionOOClass);
var
    s: String;
    mtd: TPsionOOMethodEntry;
    cst: TPsionOOConstantEntry;
begin
    Write('CLASS ', cls.Name, ' ');
    if cls.Parent <> '' then Write(cls.Parent);
    WriteLn;
    WriteLn('{');

    for mtd in cls.Methods do
    begin
        case mtd.MethodType of
            methodReplace:  Write('REPLACE ');
            methodDefer:    Write('DEFER ');
            methodAdd:      Write('ADD ');
            methodDeclare:  Write('DECLARE ');
            else            Write(mtd.MethodType, ' ');
        end;
        Write(mtd.Name);
        if mtd.ForwardRef <> '' then begin
            Write(' = ', mtd.ForwardRef);
        end;
        WriteLn;
    end;

    if cls.HasMethod then WriteLn('HAS_METHOD');
    if cls.HasProperty then WriteLn('HAS_PROPERTY');
    if length(cls.ClassConstants) > 0 then begin
        WriteLn('CONSTANTS');
        Writeln('{');
        for cst in cls.ClassConstants do WriteLn(cst.Name, ' ', cst.Value);
        Writeln('}');
    end;

    if cls.ClassTypes.Count > 0 then begin
        WriteLn('TYPES');
        Writeln('{');
        for s in cls.ClassTypes do WriteLn(s);
        Writeln('}');
    end;

    if cls.ClassProperty.Count > 0 then begin
        Write('PROPERTY');
        if cls.PropertyAutodestroyCount > 0 then
            Write(' ', cls.PropertyAutodestroyCount);
        WriteLn;
        Writeln('{');
        for s in cls.ClassProperty do WriteLn(s);
        Writeln('}');
    end;

    WriteLn('}');
end;

procedure Reconstruct(par: TPsionOOParser);
var
    element : TPsionOOFileElement;
begin
    Writeln;
    Writeln('Reconstructed File');
    Writeln('------------------');

    case par.CategoryType of
        catName:    Write('NAME');
        catImage:   Write('IMAGE');
        catLibrary: Write('LIBRARY');
        else        Write(par.CategoryType);
    end;
    WriteLn(' ', LowerCase(par.ModuleName));

    for element in par.ElementList do
    begin
        case element.ElementType of
            incExternal: begin
                WriteLn('EXTERNAL ', par.ExternalList[element.index]);
            end;
            incInclude: begin
                WriteLn('INCLUDE ', par.IncludeList[element.index]);
            end;
            incRequire: begin
                WriteLn('REQUIRE ', par.RequireList[element.index]);
            end;
            incClass: begin
                ReconstructClass(par.ClassList[element.index]); // INFO: Spun off to make it more readable and hopefully faster
            end;
        end;
    end;
end;

end.
