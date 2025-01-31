unit fpcunit_parser;
{$mode objfpc}{$H+}{$J-}

interface

uses
  fpcunit,
  testregistry,
  PsionOOParser,
  SysUtils,
  TypInfo,
  Classes;

type
  TTestParser = class(TTestCase)
  published
    procedure TestHookup;
  end;

  TPsionOOParserWrap = class(TPsionOOParser)
    private
      procedure _AddToken(toktype: TTokenType; tokliteral: String);
      procedure _AddTokensFromFile(filename: String);
  end;

var
  par: TPsionOOParserWrap;

implementation


procedure TTestParser.TestHookUp;
begin
  par := TPsionOOParserWrap.Create();
  par._AddTokensFromFile('test1.csv');

  WriteLn(par.Tokens[1].TType);

  Check(par.Tokens[1].TType = tknString, 'Incorrect token found.');
  CheckEquals(par.Tokens.Count, 3, 'Incorrect number of tokens.');
  FreeAndNil(par);
end;

procedure TPsionOOParserWrap._AddToken(toktype: TTokenType; tokliteral: String);
begin
  inherited;
end;

procedure TPsionOOParserWrap._AddTokensFromFile(filename : String);
// TODO: Return error properly (somehow)
var
  csv: TStringList;
  s: String;
  STokType, STokLteral: String;
  toktype: TTokenType;
  toktypenum: Integer;
  commapos: Integer;
begin
  csv := TStringList.Create();
  csv.LoadFromFile(filename);

  for s in csv do
  begin
    commapos := Pos(',', s);
    STokType := LeftStr(s, commapos - 1);
    toktypenum := GetEnumValue(TypeInfo(TTokenType), STokType);
    if toktypenum = -1 then
    begin
      WriteLn(format('Invalid token found (%s)', [STokType]));
      halt(-1);
    end;

    toktype := TTokenType(toktypenum);
    literal := Copy(s, commapos + 1);
    self._AddToken(toktype, STokLiteral);
  end;
end;

initialization

  RegisterTests([TTestParser]);

end.
