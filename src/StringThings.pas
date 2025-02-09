{$mode objfpc}{$H+}{$J-}
{$ModeSwitch typehelpers}
unit StringThings;

interface

uses
  sysutils,
  classes;

type
  TStringListHelper = Class Helper for TStringList
    Public
      Function Reverse: TStringList;
      function FormatAll(fmt_main: String; fmt_final: String = ''): TStringList;
      procedure AddStringsFormat(sl: TStringList; fmt_main: String; fmt_final: String = '');
  end;
  TStringArrayHelper = Type Helper for TStringArray
    Public
      function FormatAll(fmt_main: String; fmt_final: String = ''): TStringArray;
  end;

function RepeatStr(s: String; c: integer): String;
function ExtractFileStem(s: String): String;
procedure TrimAfterSemicolon(var s: String);
function DelimitStr(s: String; delimiter: String; step: Integer = 1): String;
function ProperCase(s: String): String;
implementation

// String

// Creates a string where s is repeated c times.
function RepeatStr(s: String; c: integer): String;
var
  i: Integer;
  l: Integer;
begin
  Result := '';
  if c > 0 then begin
    l := Length(s);
    if l > 0 then begin
      SetLength(Result, l * c);
      for i := 0 to c - 1 do
      begin
        Move(s[1], Result[l * i + 1], l);
      end;
    end;
  end;
end;

// Gets the stem of a filename (i.e. without the extension). If it has no
// extension, it just returns the filename.
function ExtractFileStem(s: String): String;
var
  file_ext: String;
  file_name: String;
begin
  file_ext := ExtractFileExt(s);
  if file_ext = '' then begin
    Result := ExtractFileName(s)
  end else begin
    file_name := ExtractFileName(s);
    Result := copy(file_name, 1, AnsiPos(file_ext, file_name) - 1);
  end;
end;

function DelimitStr(s: String; delimiter: String; step: Integer = 1): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to length(s) do
  begin
    if (i > 1) and ((i - 1) mod step = 0) then begin
      Result += delimiter;
    end;
    Result += copy(s, i, 1);
  end;
end;

// Removes everything in a string after the first semicolon
procedure TrimAfterSemicolon(var s: String);
begin
  if ansipos(';', s) > 0 then s := copy(s, 1, ansipos(';', s));
  s := s.Trim;
end;

// TStringList

// Reverses the order of elements in a TStringList
function TStringListHelper.Reverse(): TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create();
  for i := Self.Count - 1 downto 0 do
  begin
    Result.Add(Self[i]);
  end;
end;

// Takes a TStringArray and formats every element using format_main. If
// fmt_final exists, the last element in the TStringList will be formatted
// with it instead.
// Useful for generating output for a file where the last line needs to be
// different, such as a comma separated list.
function TStringArrayHelper.FormatAll(fmt_main: String; fmt_final: String = ''): TStringArray;
var
  i: Integer;
begin
  SetLength(Result, Length(Self));

  // TODO: For both fmt_main and fmt_final, if there is more than one %s, repeat sl[i] in the array

  if AnsiPos('%s', fmt_main) = 0 then
  begin
    WriteLn('FormatStringArray: Missing %s from format_main.');
    halt(-1);
  end;
  if (fmt_final <> '') and (AnsiPos('%s', fmt_main) = 0) then
  begin
    WriteLn('FormatStringArray: Missing %s from fmt_final.');
    halt(-1);
  end;

  for i := 0 to Length(Self) - 2 do
  begin
    Result[i] := Format(fmt_main, [Self[i]]);
  end;
  if fmt_final = '' then
    Result[Length(Result) - 1] := Format(fmt_main, [Self[Length(Self) - 1]])
  else
    Result[Length(Result) - 1] := Format(fmt_final, [Self[Length(Self) - 1]]);
end;

// Takes a TStringList and formats every element using format_main. If
// fmt_final exists, the last element in the TStringList will be formatted
// with it instead.
// Useful for generating output for a file where the last line needs to be
// different, such as a comma separated list.
function TStringListHelper.FormatAll(fmt_main: String; fmt_final: String = ''): TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create();

  // TODO: For both fmt_main and fmt_final, if there is more than one %s, repeat sl[i] in the array

  if AnsiPos('%s', fmt_main) = 0 then
  begin
    WriteLn('FormatStringList: Missing %s from format_main.');
    Halt(-1);
  end;
  if (fmt_final <> '') and (AnsiPos('%s', fmt_main) = 0) then
  begin
    WriteLn('FormatStringList: Missing %s from fmt_final.');
    Halt(-1);
  end;

  for i := 0 to Self.Count - 2 do
  begin
    Result.Add(fmt_main, [Self[i]]);
  end;
  if fmt_final = '' then
    Result.Add(fmt_main, [Self[Self.Count - 1]])
  else
    Result.Add(fmt_final, [Self[Self.Count - 1]]);
end;

procedure TStringListHelper.AddStringsFormat(sl: TStringList; fmt_main: String; fmt_final: String = '');
var
  i: Integer;
begin
  if Self.Count + sl.Count > Self.Capacity then
    Self.Capacity := Self.Count + sl.Count;
  for i := 0 to sl.Count - 2 do
    Self.AddObject(Format(fmt_main, [sl[i]]), sl.Objects[i]);
  if fmt_final = '' then
    Self.AddObject(Format(fmt_main, [sl[sl.Count - 1]]), sl.Objects[sl.Count - 1])
  else
    Self.AddObject(Format(fmt_final, [sl[sl.Count - 1]]), sl.Objects[sl.Count - 1]);
end;

// General Functions

function ProperCase(s: String): String;
begin
  if length(s) > 0 then
    Result := UpCase(s[1]) + LowerCase(Copy(s, 2))
  else
    Result := '';
end;

end.
