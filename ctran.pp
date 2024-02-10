{$mode objfpc}{$H+}{$J-}
program ctran;

uses
    sysutils, PsionOOLexer, PsionOOParser;


var
    strFilename : String;
    CatLexer : TPsionOOLexer;
    CatParser : TPsionOOParser;
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
                else begin
                    writeln('Nope!');
                    exit();
                end;
            end;
            if length(thisParam) = 2 then begin
                inc(cur);
                thisParam := paramStr(cur);
            end else begin
                thisParam := copy(thisParam, 3);
            end;
            writeln('Found value: ', thisParam);
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

begin
    GetParams();
    
    if length(strFilename) = 0 then begin
        WriteLn('No filename given.');
        exit;
    end;

    if (length(strFilename) < 5) or ((length(strFilename) > 4) and (AnsiPos('.', UpCase(strFilename)) < 2)) then
        strfilename += '.cat';
    WriteLn('Filename: ', strFilename);
    WriteLn;

    Try
    begin
        CatLexer := TPsionOOLexer.Create;
        CatLexer.LoadFile(strFilename);

        WriteLn;
        CatLexer.PrintArray();
//        CatLexer.PrintTokenisedLines();
//        CatParser := TPsionOOParser.Create(CatLexer);
        CatLexer.Parse();

    end
    finally begin
//        FreeAndNil(CatParser);
        FreeAndNil(CatLexer);
    end;
    end;
end.

