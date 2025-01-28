program fptest_test;

{$mode objfpc}{$H+}

uses
    Classes,
    TextTestRunner,
    fptest_parser;

begin
    fptest_parser.RegisterTests;

    RunRegisteredTests;
end.
