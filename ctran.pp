{$mode objfpc}{$H+}{$J-}
program ctran;

uses
    sysutils, PsionOOLexer, PsionOOCatDiagnostics, PsionSDKApp;

var
    strFilename : String;
    CatLexer : TPsionOOLexer;
    boolExternal : Boolean;
    boolGenG : Boolean;
    params : TPsionSDKAppParams;

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
                'X': begin
                    writeln('generate .EXT files');
                    boolGenG := true;
                end;
                'V': begin
                    writeln('verbose');
                end;
                else begin
                    writeln('Nope!');
                    exit();
                end;
            end;
            if length(thisParam) > 2 then begin
                thisParam := copy(thisParam, 3);
            end else begin
                thisParam := '';
            end;
            writeln('Value: ', thisParam);
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

procedure HelpText();
var
    s : String;
begin
    s := 'CTRAN Version x.xx (C) xxx' + LineEnding +
         'Parameters: <name> [-e<dir>] [-x[<dir>] -g[<dir>] -a[<dir>] -i[<dir>] -l[<dir>] -c[<dir>] -s -k -v]' + LineEnding +
         '<name>       Category source input file' + LineEnding +
         '-e<dir>      Input externals directory' + LineEnding +
         '-x<dir>      Output .EXT file' + LineEnding +
         '-c<dir>      Output .C code file' + LineEnding +
         '-g<dir>      Output .G include file' + LineEnding +
         '-a<dir>      Output .ASM code file' + LineEnding +
         '-i<dir>      Output .ING code file' + LineEnding +
         '-l<dir>      Output .LIS file' + LineEnding +
         '-s           SDK output' + LineEnding +
         '-k           Output skeleton source files' + LineEnding +
         '-v           Verbose output';
    WriteLn(s);
end;

procedure MakeEXT(lex: TPsionOOLexer);
var
    element : TPsionOOFileElement;
    method: TPsionOOMethodEntry;
    flgHasMethod: boolean;
begin
    WriteLn('Generate by Ctran from ', lex.ModuleName, '.cat'); // TODO: Use real filename here
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
            incExternal, incInclude: begin end;

            incRequire: begin
                WriteLn('REQUIRE ', lex.RequireList[element.index]);
            end;

            incClass: begin
                flgHasMethod := false;
                Write('CLASS ', lex.ClassList[element.index].Name, ' ');
                if lex.ClassList[element.index].Inherits <> '' then
                    Write(lex.ClassList[element.index].Inherits);
                WriteLn;
                WriteLn('{');
                for method in lex.ClassList[element.index].Methods do
                begin
                    case method.MethodType of
                        methodReplace: flgHasMethod := true;
                        methodDefer, methodAdd: begin
                            WriteLn('DECLARE ', method.Name);
                            flgHasMethod := true;
                        end;
                        methodDeclare: begin
                            WriteLn('MakeEXT: Can''t have a DECLARE in a Category file... What''s happened?');
                            exit;
                        end;
                        else begin
                            WriteLn('MakeEXT: Unknown token in a Category file... What''s happened?');
                            exit;
                        end;
                    end;
                end;
                if flgHasMethod then WriteLn('HAS_METHOD');
                if length(lex.ClassList[element.index].ClassProperty) > 0 then begin
                    WriteLn('HAS_PROPERTY');
                end;
                WriteLn('}');
            end;
        end;
    end;
    WriteLn;
end;


begin
    params := TPsionSDKAppParams.Create;
    params.Grab;

    if length(params.Filename) = 0 then begin
        HelpText();
        exit;
    end;

    if params.Filename = '' then begin
        HelpText();
        exit;
    end;

    strFilename := params.Filename;
    case ExtractFileExt(strFilename) of
        '.', '': strfilename += '.cat';
    end;

    WriteLn('Filename: ', strFilename);

    Try
    begin
        CatLexer := TPsionOOLexer.Create;
        CatLexer.LoadFile(strFilename);

        CatLExer.Verbose := params.InSwitch('V', 'L');
        CatLexer.Lex();

        if params.InSwitch('V', 'T') then PrintArray(CatLexer);

        CatLexer.Verbose := params.InSwitch('V', 'P');
        CatLexer.Parse();

        if params.InSwitch('V', 'A') then ShowTree(CatLexer);
        if params.InSwitch('V', 'R') then Reconstruct(CatLexer);
        if params.Params[24].Exists then MakeEXT(CatLexer);

    end
    finally begin
        FreeAndNil(CatLexer);
    end;
end;

end.

