{$mode objfpc}{$H+}{$J-}
unit PsionOOCatDiagnostics;

interface
uses
  sysutils,
  PsionOOParser;

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
  WriteLn(' Line | Pos | Token Type     | Literal');
  WriteLn('------+-----+----------------+-------------');

  for tok in par.Tokens do
  begin
    Str(tok.TType, s); // Because you can't simply use an enum in Format()
    WriteLn(Format(' %4d | %3d | %-14s | %s', [tok.Position.Line, tok.Position.Column, s, tok.Literal]));
  end;

  WriteLn;
  WriteLn('Length: ', par.Tokens.Count);
end;

procedure ShowTree(par: TPsionOOParser);
var
  i: integer;
  s: String;
  class_item: TPsionOOClass;
  constant_entry: TPsionOOConstantEntry;
  method_entry: TPsionOOMethodEntry;
begin
  WriteLn;
  WriteLn('EXTERNALs');
  WriteLn('---------');
  for s in par.ExternalList do WriteLn(s);

  WriteLn;
  WriteLn('INCLUDEs');
  WriteLn('--------');
  for s in par.IncludeList do WriteLn(s);

  WriteLn;
  WriteLn('REQUIREs');
  WriteLn('--------');
  for s in par.RequireList do WriteLn(s);

  WriteLn;
  WriteLn('CLASSes');
  WriteLn('-------');

  for class_item in par.ClassList do
  begin
    Write('Name: ', class_item.Name);
    if class_item.Parent = '' then begin
      WriteLn(' (root class)');
    end else begin
      WriteLn(' (inherits from ', class_item.Parent, ')');
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

    WriteLn('  Types (',  class_item.ClassTypes.Count, '):');
    for s in class_item.ClassTypes do WriteLn('    ', s);

    WriteLn('  Constants (', class_item.ClassConstants.Count, '):');
    for constant_entry in class_item.ClassConstants do
    begin
        WriteLn('    ', constant_entry.Name, ' ', constant_entry.Value);
    end;

    WriteLn('  Property (', class_item.ClassProperty.Count,'):');
    for s in class_item.ClassProperty do WriteLn('    ', s);

    if class_item.HasMethod then WriteLn('  HAS_METHOD set');
    if class_item.HasProperty then WriteLn('  HAS_PROPERTY set');
  end;

  WriteLn;
  WriteLn('Element List');
  WriteLn('------------');

  WriteLn(' Element | Type        | Index');
  WriteLn('---------+-------------+-------');

  for i := 0 to Length(par.ElementList) - 1 do
  begin
    Str(par.ElementList[i].ElementType, s);
    WriteLn(Format('    %04d | %-11s | %04d', [i, s, par.ElementList[i].index]));
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
  if cls.ClassConstants.Count > 0 then begin
    WriteLn('CONSTANTS');
    WriteLn('{');
    for cst in cls.ClassConstants do WriteLn(cst.Name, ' ', cst.Value);
    WriteLn('}');
  end;

  if cls.ClassTypes.Count > 0 then begin
    WriteLn('TYPES');
    WriteLn('{');
    for s in cls.ClassTypes do WriteLn(s);
    WriteLn('}');
  end;

  if cls.ClassProperty.Count > 0 then begin
    Write('PROPERTY');
    if cls.PropertyAutodestroyCount > 0 then
      Write(' ', cls.PropertyAutodestroyCount);
    WriteLn;
    WriteLn('{');
    for s in cls.ClassProperty do WriteLn(s);
    WriteLn('}');
  end;

  WriteLn('}');
end;

procedure Reconstruct(par: TPsionOOParser);
var
  element : TPsionOOFileElement;
begin
  WriteLn;
  WriteLn('Reconstructed File');
  WriteLn('------------------');

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
