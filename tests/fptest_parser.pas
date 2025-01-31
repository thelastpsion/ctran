unit fptest_parser;
{$mode objfpc}{$H+}{$J-}

interface

uses
  TestFramework,
  PsionOOParser,
  SysUtils;

type
  TTestParser = class(TTestCase)
  published
    procedure TestHookUp;
  end;

  TPsionOOParserWrap = class(TPsionOOParser)
    private
      procedure _AddToken(toktype: TTokenType; tokliteral: String);
  end;

procedure RegisterTests;

var
  par: TPsionOOParserWrap;



implementation

// fptest_parser

procedure TTestParser.TestHookUp;
begin
  par := TPsionOOParserWrap.Create();
  par._AddToken(tknString, 'Hello');
  par._AddToken(tknEOF, '');

  Check(par.Tokens[0].TType = tknString, 'Incorrect token found.');
  CheckEquals(par.Tokens.Count, 2, 'Incorrect number of tokens.');
  FreeAndNil(par);
end;

procedure RegisterTests;
begin
  RegisterTest(TTestParser.Suite);
end;

//
// Parser wrapper methods
//

procedure TPsionOOParserWrap._AddToken(toktype: TTokenType; tokliteral: String);
begin
  Inherited;
end;

end.

