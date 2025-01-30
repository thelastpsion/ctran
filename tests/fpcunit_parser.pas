unit fpcunit_parser;
{$mode objfpc}{$H+}{$J-}

interface

uses
    fpcunit,
    testregistry,
    PsionOOParser,
    SysUtils,
    Classes;

type
    TTestParser = class(TTestCase)
    published
       procedure TestHookup;
    end;

    TPsionOOParserWrap = class(TPsionOOParser)
        private
            procedure _AddToken(toktype: TTokenType; tokliteral: String);
    end;

var
    par : TPsionOOParserWrap;

implementation


procedure TTestParser.TestHookUp;
var
    csv : TStringList;
    s : String;
begin
    csv := TStringList.Create();
    csv.Delimiter := ',';
    csv.QuoteChar := '"';
    csv.LoadFromFile('test1.csv');

    for s in csv do
    begin
        WriteLn(s);
    end;

    par := TPsionOOParserWrap.Create();
    par._AddToken(tknString, 'Hello');
    par._AddToken(tknEOF, '');

    Check(par.Tokens[0].TType = tknString, 'Incorrect token found.');
    CheckEquals(par.Tokens.Count, 2, 'Incorrect number of tokens.');
    // AssertEquals('Incorrect number of tokens.', par.Tokens.Count, 1);
    FreeAndNil(par);
end;

procedure TPsionOOParserWrap._AddToken(toktype: TTokenType; tokliteral: String);
begin
    Inherited;
end;

initialization

    RegisterTests([TTestParser]);

end.
