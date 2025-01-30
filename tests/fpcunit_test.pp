program fpcunit_test;
{$mode objfpc}{$H+}{$J-}

uses
    consoletestrunner,
    PsionOOParser,
    fpcunit_parser;

var
  App: TTestRunner;

begin
  WriteLn('Psion Parser Test');
  WriteLn;
  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FPCUnit Psion Parser runner.';
  App.Run;
  App.Free;
end.
