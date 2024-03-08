{$mode objfpc}{$H+}{$J-}
program ctran;

uses
    sysutils, classes, PsionOOLexer, PsionOOCatDiagnostics, PsionSDKApp, Generics.Collections;

type
    TPsionOOMethodList = Array of String;

    TPsionOOCatClass = record
        Parent : String;
        Methods : array of TPsionOOMethodEntry;
    end;

    TDependencyList = specialize TDictionary<string, TPsionOOCatClass>;
var
    strFilename : String;
    CatLexer : TPsionOOLexer;
    ExtLexer : TPsionOOLexer;
    params : TPsionSDKAppParams;
    PathList : TStringList;
    s : String;
    ExtFileList : Array of String;
    DependencyList : TDependencyList;
    MethodList : TStringList;

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
    WriteLn('Generated by Ctran from ', lex.ModuleName, '.cat'); // TODO: Use real filename here
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

function CheckPath(s: String) : TStringList;
var
    protopaths : TStringArray;
    pathitem : String;
    pathitemexpand : String;
begin
    Result := TStringList.Create();
    protopaths := s.Split(';');

    for pathitem in protopaths do
    begin
        pathitemexpand := ExpandFileName(pathitem);
        if not DirectoryExists(pathitemexpand) then begin
            WriteLn('Path doesn''t exist. (', pathitemexpand, ' from ', pathitem, ')');
            halt;
        end;

        if RightStr(pathitemexpand, 1) <> DirectorySeparator then pathitemexpand += DirectorySeparator;
        Result.Add(pathitemexpand);
    end;
end;

function CheckExternalFile(s : String; paths : TStringList) : String;
var
    ext : String;
    path : String;
    possiblefile : String;
    extopts : Array of String;
begin
    extopts := ['', '.ext', '.EXT'];
    for path in paths do begin
        for ext in extopts do begin
            possiblefile := path + s + ext;
            if FileExists(possiblefile) then exit(possiblefile);

            {$IFNDEF GO32V2} // Don't bother checking case-sensitive options
            possiblefile := path + LowerCase(s) + ext;
            if FileExists(possiblefile) then exit(possiblefile);

            possiblefile := path + UpCase(s) + ext;
            if FileExists(possiblefile) then exit(possiblefile);
            {$ENDIF} // GO32V2
        end;
    end;
    Result := '';
end;

procedure LoadDependencies(par : TPsionOOLexer; filename : String);
var
    ext_class : TPsionOOCatClass;
    par_class : TPsionOOClass;
    method : TPsionOOMethodEntry;
begin
    for par_class in par.ClassList do
    begin
        if DependencyList.ContainsKey(par_class.Name) then begin
            WriteLn('Class ', par_class.Name, ' already defined');
            // TODO: Add source file
            // TODO: (if possible) add location in file
            halt;
        end;

        if (not DependencyList.ContainsKey(par_class.Inherits)) and (par_class.Inherits <> '') then begin
            WriteLn('Error ', filename, ': Superclass ', par_class.Inherits, ' of ', par_class.Name, ' does not exist');
            // TODO: Add source file of superclass
            // TODO: (if possible) add location in file
            halt;
        end;

        for method in par_class.Methods do
        begin
            case method.MethodType of
                methodDeclare, methodAdd: begin
                    if MethodList.IndexOf(LowerCase(method.Name)) > -1 then begin
                        WriteLn('Error ', filename, ': Method ', method.Name, ' already exists in category');
                        halt;
                    end;
                    // WriteLn('Adding method ', method.Name, ' to big list o'' methods.');
                    MethodList.Add(LowerCase(method.Name));
                end;
            end;
        end;

        // WriteLn(filename, ' : ', par_class.Name, ' ', par_class.Inherits);
        ext_class.Parent := par_class.Inherits;
        ext_class.Methods := par_class.Methods;
        DependencyList.Add(par_class.Name, ext_class);
    end;
end;

procedure LoadDependencies(filename : String);
var
    par : TPsionOOLexer;
begin
    try
        begin
            par := TPsionOOLexer.Create;
            par.LoadFile(filename);

            par.Verbose := params.InSwitch('V', 'L');
            par.Lex();

            par.Verbose := params.InSwitch('V', 'P');
            par.Parse();

            LoadDependencies(par, filename);
        end
    finally
        begin
            FreeAndNil(par);
        end;
    end;
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

        CatLexer.Verbose := params.InSwitch('V', 'L');
        CatLexer.Lex();

        if params.InSwitch('V', 'T') then PrintArray(CatLexer);

        CatLexer.Verbose := params.InSwitch('V', 'P');
        CatLexer.Parse();

        if params.InSwitch('V', 'A') then ShowTree(CatLexer);
        if params.InSwitch('V', 'R') then Reconstruct(CatLexer);

        if params.SwitchExists('X') then begin
            WriteLn;
            MakeEXT(CatLexer);
        end;

        DependencyList := TDependencyList.Create;
        MethodList := TStringList.Create;

        // TODO: Get the path for external files (from `-e`) (extra checks?)
        if params.SwitchExists('E') then PathList := CheckPath(params.SwitchVal('E'));

        // TODO: Get the list of external files from the category class (extra checks?)
        if Length(CatLexer.ExternalList) > 0 then begin
            SetLength(ExtFileList, Length(CatLexer.ExternalList));
            for s in CatLexer.ExternalList do begin
                ExtFileList := concat(ExtFileList, [CheckExternalFile(s, PathList)]);
                Write(s, ': ');
                WriteLn(ExtFileList[length(ExtFileList) - 1]);

                LoadDependencies(ExtFileList[length(ExtFileList) - 1]);
            end;
        end;

        LoadDependencies(CatLexer, strFilename);
        // for s in DependencyList.Keys do begin
        //     WriteLn(s);
        // end;

        // WriteLn(DependencyList.Count);

        // TODO: Build a dictionary of all the external classes
        // TODO: Check the parent field for a valid parent class - fail immediately if broken
        // TODO: Check the parent class (and all classes above) for duplicate methods

        // TODO: Build a dictionary of all the category file classes
        // TODO: Check the parent field for a valid parent class (in both dictionaries) - fail immediately if broken
        // TODO: Check the parent class (and all classes above) for duplicate methods
    end
    finally begin
        FreeAndNil(DependencyList);
        FreeAndNil(CatLexer);
    end;
end;

end.

