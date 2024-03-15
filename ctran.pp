{$mode objfpc}{$H+}{$J-}
program ctran;

uses
    sysutils, classes, PsionOOLexer, PsionOOCatDiagnostics, PsionSDKApp, Generics.Collections;

type
    TPsionOOMethodList = Array of String;

    TPsionOOCatClass = record
        Parent : String;
        Category : String;
        Methods : array of TPsionOOMethodEntry;
        HasProperty : Boolean;
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
    class_item : TPsionOOClass;
    cur_metaclass : TStringList;
    method_item : TPsionOOMethodEntry;
    extfile : String;

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

procedure MakeEXT(par: TPsionOOLexer);
var
    element : TPsionOOFileElement;
    method: TPsionOOMethodEntry;
    flgHasMethod: boolean;
    tfOut : TextFile;
    filepath : String;
begin
    filepath := params.SwitchVal('X');
    if (length(filepath) > 0) and (RightStr(filepath, 1) <> DirectorySeparator) then filepath += DirectorySeparator;

    AssignFile(tfOut, filepath + par.ModuleName + '.EXT');

    try
        SetTextLineEnding(tfOut, #13#10);
        rewrite(tfOut);

        WriteLn(tfOut, 'Generated by Ctran from ', ExtractFilename(strFilename));
        case par.CategoryType of
            catName:    Write(tfOut, 'NAME');
            catImage:   Write(tfOut, 'IMAGE');
            catLibrary: Write(tfOut, 'LIBRARY');
            else        Write(par.CategoryType);
        end;
        WriteLn(tfOut, ' ', LowerCase(par.ModuleName));

        for element in par.ElementList do
        begin
            case element.ElementType of
                incExternal, incInclude: begin end;

                incRequire: begin
                    WriteLn(tfOut, 'REQUIRE ', par.RequireList[element.index]);
                end;

                incClass: begin
                    flgHasMethod := false;
                    Write(tfOut, 'CLASS ', par.ClassList[element.index].Name, ' ');
                    if par.ClassList[element.index].Parent <> '' then
                        Write(tfOut, par.ClassList[element.index].Parent);
                    WriteLn(tfOut);
                    WriteLn(tfOut, '{');
                    for method in par.ClassList[element.index].Methods do
                    begin
                        case method.MethodType of
                            methodReplace: flgHasMethod := true;
                            methodDefer:   WriteLn(tfOut, 'DECLARE ', method.Name);
                            methodAdd: begin
                                WriteLn(tfOut, 'DECLARE ', method.Name);
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
                    if flgHasMethod then WriteLn(tfOut, 'HAS_METHOD');
                    if length(par.ClassList[element.index].ClassProperty) > 0 then begin
                        WriteLn(tfOut, 'HAS_PROPERTY');
                    end;
                    WriteLn(tfOut, '}');
                end;
            end;
        end;
        CloseFile(tfOut);
    except
        on E: EInOutError do
            WriteLn('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
    end;
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
    category : String;
begin
    category := par.ModuleName;

    for par_class in par.ClassList do
    begin
        if DependencyList.ContainsKey(LowerCase(par_class.Name)) then begin
            WriteLn('Class ', par_class.Name, ' already defined');
            // TODO: Add source file
            // TODO: (if possible) add location in file
            halt;
        end;

        if (not DependencyList.ContainsKey(LowerCase(par_class.Parent))) and (par_class.Parent <> '') then begin
            WriteLn('Error ', filename, ': Superclass ', par_class.Parent, ' of ', par_class.Name, ' does not exist');
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

        // WriteLn(filename, ' : ', par_class.Name, ' ', par_class.Parent);
        ext_class.Category := category;
        ext_class.Parent := LowerCase(par_class.Parent);
        ext_class.Methods := par_class.Methods;
        ext_class.HasProperty := ((par_class.HasProperty) or (length(par_class.ClassProperty) > 0));

        DependencyList.Add(LowerCase(par_class.Name), ext_class);
    end;
end;

procedure LoadDependencies(filename : String);
var
    par : TPsionOOLexer;
begin
    if filename = '' then begin
        WriteLn('LoadDependencies(): filename is empty');
        halt;
    end;

    try
        begin
            par := TPsionOOLexer.Create;
            // WriteLn('Loading ', filename);
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

function GetAncestorsWithProperty(class_item : TPsionOOClass) : TStringList;
var
    parent : String;
begin
    // TODO: Check parent classes for circular reference (parent TStringList?)
    Result := TStringList.Create;
    parent := LowerCase(class_item.Parent);
    while parent <> '' do
    begin
        if DependencyList[parent].HasProperty then Result.Add(parent);
        parent := DependencyList[parent].Parent;
    end;
end;

function MakeMetaclass(class_item : TPsionOOClass) : TStringList;
var
    cur_method : TPsionOOMethodEntry;
    parent : String;
begin
    // TODO: Check parent classes for circular reference (parent TStringList?)
    WriteLn(class_item.Name);
    Result := TStringList.Create;
    parent := LowerCase(class_item.Parent);
    while parent <> '' do
    begin
        WriteLn('Parent being processed: ', parent);
        for cur_method in DependencyList[parent].Methods do
        begin
            case cur_method.MethodType of
                methodAdd, methodDefer, methodDeclare: begin
                    if Result.IndexOf(cur_method.Name) > -1 then begin
                        WriteLn('Error ', strFilename, ': Method ', cur_method.Name, ' already exists in class ', parent);
                        halt;
                    end;
                    WriteLn('Adding method ', cur_method.Name);
                    Result.Add(cur_method.Name);
                end;
            end;
        end;
        parent := DependencyList[parent].Parent;
    end;
end;

function GetExternalAncestors(par : TPsionOOLexer) : TStringList;
var
    class_item : TPsionOOClass;
    parent : String;
    x : Integer;
begin
    // TODO: Check parent classes for circular reference (parent TStringList?)
    // TODO: Sort entries as per original CTRAN (order that they appear in External files?)
    Result := TStringList.Create;

    for class_item in par.ClassList do
    begin
        parent := LowerCase(class_item.Parent);
        while parent <> '' do
        begin
            if (Result.IndexOf(parent) = -1) and (DependencyList[parent].Category <> par.ModuleName) then Result.Add(parent);
            break;
        end;
    end;
end;

procedure MakeG(par : TPsionOOLexer);
var
    inc_file : String;
    inc_file_def : String;
    i : Integer;
    class_item : TPsionOOClass;
    constant_item : TPsionOOConstantEntry;
    s : String;
    method_id : Integer;
    ts : TStringList;
    tfOut : TextFile;
    filepath : String;
    method_list : TStringList;
begin
    filepath := params.SwitchVal('G');
    if (length(filepath) > 0) and (RightStr(filepath, 1) <> DirectorySeparator) then filepath += DirectorySeparator;

    AssignFile(tfOut, filepath + par.ModuleName + '.G');

    try
        SetTextLineEnding(tfOut, #13#10);
        rewrite(tfOut);

        WriteLn(tfOut, '/* Generated by Ctran from ', ExtractFilename(strFilename), ' */');
        WriteLn(tfOut, '#define ', UpCase(par.ModuleName), '_G');

        for inc_file in par.IncludeList do
        begin
            inc_file_def := UpCase(StringReplace(inc_file, '.', '_', [rfReplaceAll]));
            WriteLn(tfOut, '#ifndef ', inc_file_def);
            WriteLn(tfOut, '#include <', inc_file, '>');
            WriteLn(tfOut, '#endif');
        end;

        WriteLn(tfOut, '/* Category Numbers */');
        WriteLn(tfOut, '#ifndef EPOC');
        WriteLn(tfOut, 'GLREF_D P_CLASS *ct_', LowerCase(par.ModuleName), '[];');
        for i := 0 to length(par.ExternalList) - 1 do
        begin
            WriteLn(tfOut, 'GLREF_D P_CLASS *ct_', par.ExternalList[i], '[];');
        end;
        WriteLn(tfOut, '#endif /* EPOC */');
        WriteLn(tfOut, '#ifdef EPOC');
        WriteLn(tfOut, '#define CAT_', par.ModuleName, '_', par.ModuleName, ' 0');
        for i := 0 to length(par.ExternalList) - 1 do
        begin;
            WriteLn(tfOut, '#define CAT_', par.ModuleName, '_', UpCase(par.ExternalList[i]), ' ', i + 1);
        end;
        WriteLn(tfOut, '#else');
        WriteLn(tfOut, '#define CAT_', par.ModuleName, '_', par.ModuleName, ' (&ct_', LowerCase(par.ModuleName), '[0])');
        for i := 0 to length(par.ExternalList) - 1 do
        begin;
            WriteLn(tfOut, '#define CAT_', par.ModuleName, '_', UpCase(par.ExternalList[i]), ' (&ct_', LowerCase(par.ExternalList[i]), '[0])');
        end;
        WriteLn(tfOut, '#endif');

        WriteLn(tfOut, '/* Class Numbers */');
        for i := 0 to length(par.ClassList) - 1 do
        begin
            WriteLn(tfOut, '#define C_', UpCase(par.ClassList[i].Name), ' ', i);
        end;


        method_list := TStringList.Create();

        for class_item in par.ClassList do
        begin
            method_id := MakeMetaclass(class_item).Count;
            for method_item in class_item.Methods do
            begin
                case method_item.MethodType of
                    methodAdd, methodDefer: begin
                        method_list.Add(UpCase(method_item.Name) + ' ' + IntToStr(method_id));
                        Inc(method_id);
                    end;
                end;
            end;
        end;
        method_list.Sort();
        WriteLn(tfOut, '/* Method Numbers */');
        for s in method_list do
        begin
            WriteLn(tfOut, '#define O_', s);
        end;

        for class_item in par.ClassList do
        begin
            if length(class_item.ClassConstants) > 0 then begin
                WriteLn(tfOut, '/* Constants for ', class_item.Name, ' */');
                for constant_item in class_item.ClassConstants do
                begin
                    WriteLn(tfOut, '#define ', constant_item.Name, ' ', constant_item.Value);
                end;
            end;

            if length(class_item.ClassTypes) > 0 then begin
                WriteLn(tfOut, '/* Types for ', class_item.Name, ' */');
                for s in class_item.ClassTypes do
                begin
                    WriteLn(tfOut, s);
                end;
            end;

            WriteLn(tfOut, '/* Property of ', class_item.Name, ' */');
            if length(class_item.ClassProperty) > 0 then begin
                WriteLn(tfOut, 'typedef struct {');
                for s in class_item.ClassProperty do
                begin
                    WriteLn(tfOut, s);
                end;
                WriteLn(tfOut, '} PRS_', UpCase(class_item.Name), ';');
            end;
            WriteLn(tfOut, 'typedef struct pr_', class_item.Name);
            WriteLn(tfOut, '{');
            ts := GetAncestorsWithProperty(class_item);
            for i := ts.Count - 1 downto 0 do
            begin
                WriteLn(tfOut, 'PRS_', UpCase(ts[i]), ' ', ts[i], ';');
            end;
            if length(class_item.ClassProperty) > 0 then begin
                WriteLn(tfOut, 'PRS_', UpCase(class_item.Name), ' ', class_item.Name, ';');
            end;
            WriteLn(tfOut, '} PR_', UpCase(class_item.Name), ';');
        end;

        CloseFile(tfOut);
    except
        on E: EInOutError do
            WriteLn('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
    end;
end;

procedure MakeC(par : TPsionOOLexer);
var
    inc_file : String;
    inc_file_def : String;
    i : Integer;
    class_item : TPsionOOClass;
    constant_item : TPsionOOConstantEntry;
    s : String;
    method_id : Integer;
    ts : TStringList;
    tfOut : TextFile;
    filepath : String;
    method : TPsionOOMethodEntry;
    ForwardRefs : TStringList;
begin
    filepath := params.SwitchVal('C');
    if (length(filepath) > 0) and (RightStr(filepath, 1) <> DirectorySeparator) then filepath += DirectorySeparator;

    AssignFile(tfOut, filepath + par.ModuleName + '.C');

    try
        SetTextLineEnding(tfOut, #13#10);
        rewrite(tfOut);

        WriteLn(tfOut, '/* Generated by Ctran from ', ExtractFilename(strFilename), ' */');

        WriteLn(tfOut, '#include <', LowerCase(par.ModuleName), '.g>');
        WriteLn(tfOut, '/* External Superclass References */');

        ts := GetExternalAncestors(par);
        WriteLn(tfOut, '#ifdef EPOC');

        for s in ts do
        begin
            WriteLn(tfOut, '#define ERC_', UpCase(s), ' C_', UpCase(s));
        end;

        WriteLn(tfOut, '#else');

        for s in ts do
        begin
            WriteLn(tfOut, 'GLREF_D P_CLASS c_', s, ';');
            WriteLn(tfOut, '#define ERC_', UpCase(s), ' &c_', s);
        end;

        WriteLn(tfOut, '#endif');

        ForwardRefs := TStringList.Create();
        for class_item in par.ClassList do
            for method in class_item.Methods do
                if (method.ForwardRef <> '') and (ForwardRefs.IndexOf(method.ForwardRef) = -1) then
                    ForwardRefs.Add(method.ForwardRef);

        if ForwardRefs.Count > 0 then begin
            WriteLn(tfOut, '/* Method function forward references */');
            for s in ForwardRefs do WriteLn(tfOut, 'GLREF_C VOID ', s, '();');
        end;

        CloseFile(tfOut);
    except
        on E: EInOutError do
            WriteLn('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
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


        DependencyList := TDependencyList.Create;
        MethodList := TStringList.Create;

        // TODO: Get the path for external files (from `-e`) (extra checks?)
        if params.SwitchExists('E') then PathList := CheckPath(params.SwitchVal('E'));

        // TODO: Get the list of external files from the category class (extra checks?)
        if Length(CatLexer.ExternalList) > 0 then begin
            SetLength(ExtFileList, Length(CatLexer.ExternalList));
            for extfile in CatLexer.ExternalList do begin
                s := CheckExternalFile(extfile, PathList);
                if s = '' then begin
                    WriteLn('ERROR: External file "', extfile, '" not found in given path');
                    halt;
                end;
                ExtFileList := concat(ExtFileList, [s]);
                Write(extfile, ': ');
                WriteLn(ExtFileList[length(ExtFileList) - 1]);

                LoadDependencies(ExtFileList[length(ExtFileList) - 1]);
            end;
        end;

        LoadDependencies(CatLexer, strFilename);
        // for s in DependencyList.Keys do begin
        //     WriteLn(s);
        // end;
        // WriteLn(DependencyList.Count);

        MethodList.Clear;

        for class_item in CatLexer.ClassList do
        begin
            cur_metaclass := MakeMetaclass(class_item);

            for s in cur_metaclass do begin
                WriteLn(s);
            end;

            for method_item in class_item.Methods do
            begin
                case method_item.MethodType of
                    methodReplace: begin
                        if cur_metaclass.IndexOf(method_item.Name) = -1 then begin
                            WriteLn('Error: Can''t replace method ', method_item.Name, ' as it doesn''t already exist');
                            halt;
                        end;
                    end;
                    methodDefer, methodAdd: begin
                        if cur_metaclass.IndexOf(method_item.Name) > -1 then begin
                            WriteLn('Error: Method ', method_item.Name, ' already exists');
                            halt;
                        end;
                    end;
                    else begin
                        WriteLn('Unknown methodtype when trying to check if a method exists in a metaclass.');
                        halt;
                    end;
                end;
            end;
            // if class_item.Parent <> '' then begin
            //     // DependencyList[LowerCase(class_item.Parent)].Methods
            // end;
        end;

        if params.SwitchExists('X') then begin
            MakeEXT(CatLexer);
        end;
        if params.SwitchExists('G') then begin
            MakeG(CatLexer);
        end;
        if params.SwitchExists('C') then begin
            MakeC(CatLexer);
        end;

    end
    finally begin
        FreeAndNil(DependencyList);
        FreeAndNil(CatLexer);
    end;
end;

end.

