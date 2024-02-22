{$mode objfpc}{$H+}{$J-}
program ctran;

uses
    sysutils, PsionOOLexer, PsionOOCatDiagnostics;

var
    strFilename : String;
    CatLexer : TPsionOOLexer;
    boolExternal : Boolean;
    boolGenG : Boolean;

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

begin
    GetParams();

    if length(strFilename) = 0 then begin
        HelpText();
        exit;
    end;

    case ExtractFileExt(strFilename) of
        '.', '': strfilename += '.cat';
    end;

    WriteLn('Filename: ', strFilename);
    WriteLn;

    Try
    begin
        CatLexer := TPsionOOLexer.Create;
        CatLexer.LoadFile(strFilename);
        CatLexer.Lex();

        WriteLn;
        PrintArray(CatLexer);
        WriteLn;
        CatLexer.Parse();

        ShowTree(CatLexer);
        Reconstruct(CatLexer);

    end
    finally begin
        FreeAndNil(CatLexer);
    end;
end;

end.

