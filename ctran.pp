{$mode objfpc}{$H+}{$J-}
program ctran;

uses
    sysutils, classes, PsionOOLexer, PsionOOCatDiagnostics, PsionSDKApp, Generics.Collections;

type
    TPsionOOMethodList = Array of String;

    TMethodsForCFile = record
        Methods : array of TPsionOOMethodEntry;
        StartIndex : Integer;
    end;

    TPsionOOCatClass = record
        Parent : String;
        Category : String;
        Methods : array of TPsionOOMethodEntry;
        HasProperty : Boolean;
    end;

    TDependencyList = specialize TDictionary<string, TPsionOOCatClass>;

    TCommentType = (
        commentC,
        commentASM
    );

var
    strFilename : String;
    CatLexer : TPsionOOLexer;
    // ExtLexer : TPsionOOLexer;
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
    // AllExtClasses : TStringList;

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

function ReverseList(ts : TStringList) : TStringList;
var
    i : Integer;
begin
    Result := TStringList.Create();
    for i := ts.Count - 1 downto 0 do
    begin
        Result.Add(ts[i]);
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
    if paths.Count = 0 then begin
        WriteLn('CheckExternalFile(): Path list is empty.');
        halt;
    end;
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
    // TODO: Deal with "abstract" classes with only DEFERred methods (see SOLIPEG's TASK class as an example)
    category := par.ModuleName;

    for par_class in par.ClassList do
    begin
        if DependencyList.ContainsKey(LowerCase(par_class.Name)) then begin
            WriteLn('Error ', filename, ': Class ', par_class.Name, ' already defined');
            // TODO: Add source file of original class (i.e. 'defined in ...')
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
    // class_item : TPsionOOClass;
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
            // for class_item in par.ClassList do
            // begin
            //     AllExtClasses.Add(class_item.Name);
            // end;
        end
    finally
        begin
            FreeAndNil(par);
        end;
    end;
end;

function GetAncestorsWithProperty(class_item : TPsionOOClass) : TStringList;
var
    ancestor : String;
begin
    // TODO: Check ancestor classes for circular reference (ancestor TStringList?)
    Result := TStringList.Create;
    ancestor := LowerCase(class_item.Parent);
    while ancestor <> '' do
    begin
        if DependencyList[ancestor].HasProperty then Result.Add(ancestor);
        ancestor := DependencyList[ancestor].Parent;
    end;
end;

function GetAncestors(class_item : TPsionOOClass) : TStringList;
var
    ancestor : String;
    i : Integer;
begin
    Result := TStringList.Create();
    ancestor := LowerCase(class_item.Parent);

    while ancestor <> '' do
    begin
        if Result.indexof(ancestor) > -1 then begin
            writeln('Error: circular class dependency (', ancestor, '). current list of classes is:');
            writeln('  ', class_item.name);
            for i := Result.Count - 1 downto 0 do
            begin
                if Result[i] = ancestor then write('> ') else write('  ');
                writeln(Result[i]);
            end;
            writeln('> ', ancestor);
            halt;
        end;

        Result.insert(0, ancestor);
        ancestor := Dependencylist[ancestor].Parent;
    end;
end;

function MakeMetaclass(class_item : TPsionOOClass) : TStringList;
var
    method : TPsionOOMethodEntry;
    ancestor : String;
    ancestor_list : TStringList;
begin
    // WriteLn('MakeMetaclass() running...');
    // WriteLn(class_item.Name);
    Result := TStringList.Create();
    ancestor := LowerCase(class_item.Parent);
    ancestor_list := GetAncestors(class_item);

    for ancestor in ancestor_list do
    begin
        // WriteLn('Ancestor being processed: ', ancestor);
        for method in DependencyList[ancestor].Methods do
        begin
            case method.MethodType of
                methodAdd, methodDefer, methodDeclare: begin
                    if Result.IndexOf(method.Name) > -1 then begin
                        WriteLn('Error ', strFilename, ': Method ', method.Name, ' in class ', ancestor, 'already exists');
                        halt;
                    end;
                    // WriteLn('Adding method ', method.Name);
                    Result.Add(method.Name);
                end;
            end;
        end;
    end;
end;

function GetChildren(par : TPsionOOLexer ; parent : String) : TStringList;
var
    class_item : TPsionOOClass;
begin
    Result := TStringList.Create();
    for class_item in par.ClassList do
    begin
        if class_item.Parent = parent then Result.Add(class_item.Name);
    end;
end;

//
// Functions shared between MakeG and MakeING
//

function BuildMethodNumbers(par : TPsionOOLexer) : TStringList;
var
    class_item : TPsionOOClass;
    method_id : Integer;
    method_item : TPsionOOMethodEntry;
begin
    Result := TStringList.Create();

    for class_item in par.ClassList do
    begin
        method_id := MakeMetaclass(class_item).Count;
        for method_item in class_item.Methods do
        begin
            case method_item.MethodType of
                methodAdd, methodDefer: begin
                    Result.Add(UpCase(method_item.Name) + ' ' + IntToStr(method_id));
                    Inc(method_id);
                end;
            end;
        end;
    end;
    Result.Sort();
end;

//
// Functions shared between MakeC and MakeASM
//

// Returns a list of the first external ancestor of every class in the provided category file
function GetExternalAncestors(par : TPsionOOLexer) : TStringList;
var
    class_item : TPsionOOClass;
    ancestor : String;
    // ancestor_list : TStringList;
    // s : String;
begin
    // TODO: Check ancestor classes for circular reference (ancestor TStringList?)
    // TODO: Sort methods as per original CTRAN (the order that they appear in External files)
    Result := TStringList.Create();
    // ancestor_list := TStringList.Create();

    for class_item in par.ClassList do
    begin
        ancestor := LowerCase(class_item.Parent);
        while ancestor <> '' do
        begin
            if DependencyList[ancestor].Category <> par.ModuleName then begin
                if Result.IndexOf(ancestor) = -1 then begin
                    Result.Add(ancestor);
                end;
                break; // If it's already in the list, we don't need to check any more classes
            end else begin
                ancestor := DependencyList[ancestor].Parent;
            end;
        end;
    end;

    // for s in AllExtClasses do
    // begin
    //     if ancestor_list.IndexOf(s) > -1 then Result.Add(s);
    // end;
    // Result := ancestor_list;
end;

function MakeMethodsForOutput(class_item : TPsionOOClass) : TMethodsForCFile;
var
    method : TPsionOOMethodEntry;
    methodAdd_list : array of TPsionOOMethodEntry;
    methodReplace_list : array of TPsionOOMethodEntry;
    metaclass : TStringList;
    metaclass_method_id : Integer;
    flgFoundFirst : Boolean;
    flgExists : Boolean;
    i : Integer;
    count_possible_nulls : Integer;
begin
    SetLength(Result.Methods, 0);
    for method in class_item.Methods do
    begin
        case method.MethodType of
            methodReplace:  methodReplace_list := concat(methodReplace_list, [method]);
            methodAdd:      methodAdd_list := concat(methodAdd_list, [method]);
        end;
    end;

    count_possible_nulls := 0;
    flgFoundFirst := false;
    metaclass := MakeMetaclass(class_item);

    Result.StartIndex := metaclass.Count;
    if length(methodReplace_list) > 0 then begin
        for metaclass_method_id := 0 to metaclass.Count - 1 do
        begin
            s := metaclass[metaclass_method_id];
            flgExists := false;
            for i := 0 to length(methodReplace_list) - 1 do
            begin
                if s = methodReplace_list[i].Name then begin
                    if flgFoundFirst then begin
                        if count_possible_nulls > 0 then begin
                            SetLength(Result.Methods, length(Result.Methods) + count_possible_nulls); // Adds blank entries
                            count_possible_nulls := 0;
                        end;
                    end else begin
                        // WriteLn('*** ', class_item.Name, ': Earliest replaced method is ', s, ' (', metaclass_method_id, ')');
                        Result.StartIndex := metaclass_method_id;
                        flgFoundFirst := true;
                    end;
                    Result.Methods := concat(Result.Methods, [methodReplace_list[i]]);
                    flgExists := true;
                    break;
                end;
            end;
            if (not flgExists) and (flgFoundFirst) then begin
                inc(count_possible_nulls);
            end;
        end;
    end;

    if length(methodAdd_list) > 0 then begin
        // WriteLn('Adding trailing nulls (', count_possible_nulls, ')');
        SetLength(Result.Methods, length(Result.Methods) + count_possible_nulls);
        Result.Methods := concat(Result.Methods, methodAdd_list);
    end;
end;

function GetMethodForwardRefs(par : TPsionOOLexer) : TStringList;
var
    class_item : TPsionOOClass;
    method : TPsionOOMethodEntry;
begin
    Result := TStringList.Create();

    for class_item in par.ClassList do
        for method in class_item.Methods do
            if (method.ForwardRef <> '') and (Result.IndexOf(method.ForwardRef) = -1) then
                Result.Add(method.ForwardRef);
end;

//
// MAKE FILES
//

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

procedure MakeG(par : TPsionOOLexer);
var
    inc_file : String;
    inc_file_def : String;
    i : Integer;
    class_item : TPsionOOClass;
    constant_item : TPsionOOConstantEntry;
    s : String;
    // method_id : Integer;
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
        if not params.SwitchExists('S') then begin
            WriteLn(tfOut, '#ifndef EPOC');
            WriteLn(tfOut, 'GLREF_D P_CLASS *ct_', LowerCase(par.ModuleName), '[];');
            for s in par.ExternalList do
            begin
                WriteLn(tfOut, 'GLREF_D P_CLASS *ct_', s, '[];');
            end;
            WriteLn(tfOut, '#endif /* EPOC */');
            WriteLn(tfOut, '#ifdef EPOC');
        end;
        WriteLn(tfOut, '#define CAT_', par.ModuleName, '_', par.ModuleName, ' 0');
        for i := 0 to length(par.ExternalList) - 1 do
        begin;
            WriteLn(tfOut, format('#define CAT_%s_%s %d', [par.ModuleName, UpCase(par.ExternalList[i]),  i + 1]));
        end;
        if not params.SwitchExists('S') then begin
            WriteLn(tfOut, '#else');
            WriteLn(tfOut, format('#define CAT_%s_%s (&ct_%s[0])', [par.ModuleName, par.ModuleName, LowerCase(par.ModuleName)]));
            for s in par.ExternalList do
            begin
                WriteLn(tfOut, format('#define CAT_%s_%s (&ct_%s[0])', [par.ModuleName, UpCase(s), LowerCase(s)]));
            end;
            WriteLn(tfOut, '#endif');
        end;

        WriteLn(tfOut, '/* Class Numbers */');
        for i := 0 to length(par.ClassList) - 1 do
        begin
            WriteLn(tfOut, format('#define C_%s %d', [UpCase(par.ClassList[i].Name), i]));
        end;

        method_list := BuildMethodNumbers(par);
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
                    WriteLn(tfOut, format('#define %s %s', [constant_item.Name, constant_item.Value]));
                end;
            end;

            // TODO: Rework this for .ING files
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
                WriteLn(tfOut, format('PRS_%s %s;', [UpCase(ts[i]), ts[i]]));
            end;
            if length(class_item.ClassProperty) > 0 then begin
                WriteLn(tfOut, format('PRS_%s %s;', [UpCase(class_item.Name), class_item.Name]));
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
    i : Integer;
    class_item : TPsionOOClass;
    s : String;
    ts : TStringList;
    tfOut : TextFile;
    filepath : String;
    method : TPsionOOMethodEntry;
    ForwardRefs : TStringList;
    flg : Boolean;
    total_methods : Integer;
    c_methods : TMethodsForCFile;
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

        if not params.SwitchExists('S') then WriteLn(tfOut, '#ifdef EPOC');

        ts := GetExternalAncestors(par);
        for s in ts do
        begin
            WriteLn(tfOut, format('#define ERC_%s C_%s', [UpCase(s), UpCase(s)]));
        end;

        if not params.SwitchExists('S') then begin
            WriteLn(tfOut, '#else');

            for s in ts do
            begin
                WriteLn(tfOut, 'GLREF_D P_CLASS c_', s, ';');
                WriteLn(tfOut, format('#define ERC_%s &c_%s', [UpCase(s), s]));
            end;

            WriteLn(tfOut, '#endif');
        end;

        // Method function forward references
        ForwardRefs := GetMethodForwardRefs(par);
        if ForwardRefs.Count > 0 then begin
            WriteLn(tfOut, '/* Method function forward references */');
            for s in ForwardRefs do WriteLn(tfOut, 'GLREF_C VOID ', s, '();');
        end;
        FreeAndNil(ForwardRefs);

        for class_item in par.ClassList do
        begin
            WriteLn(tfOut);
            WriteLn(tfOut);
            WriteLn(tfOut, '/* Class ', class_item.Name, ' */');
            WriteLn(tfOut, '/* ------------------------------------------------------ */');

            c_methods := MakeMethodsForOutput(class_item);
            for method in c_methods.Methods do begin
                if (method.Name <> '') and (method.ForwardRef = '') then begin
                    WriteLn(tfOut, format('GLREF_C VOID %s_%s();', [class_item.Name, method.Name]));
                end;
            end;

            WriteLn(tfOut, 'GLDEF_D struct');
            WriteLn(tfOut, '{');
            WriteLn(tfOut, 'P_CLASS c;');

            total_methods := length(c_methods.Methods);
            if total_methods > 0 then begin
                WriteLn(tfOut, 'VOID (*v[', total_methods, '])();');
            end;

            WriteLn(tfOut, '} c_', class_item.Name, ' =');
            WriteLn(tfOut, '{');

            Write(tfOut, '{');

            flg := false;
            s := UpCase(DependencyList[DependencyList[class_item.Name].Parent].Category);
            if s = UpCase(par.ModuleName) then begin
                Write(tfOut, '0');
                flg := true;
            end else begin
                for i := 0 to length(par.ExternalList) - 1 do begin
                    if s = UpCase(par.ExternalList[i]) then begin
                        Write(tfOut, i + 1);
                        flg := true;
                        break;
                    end;
                end;
            end;

            if not flg then begin
                WriteLn('MakeC: Couldn''t find class ', s, ' anywhere - bad logic here? (Should it be ', par.ModuleName, '?)');
                halt;
            end;

            Write(tfOut, ',(P_CLASS *)');

            if DependencyList[class_item.Parent].Category = par.ModuleName then
            begin
                Write(tfOut, '&c_', class_item.Parent)
            end else begin
                Write(tfOut, 'ERC_', UpCase(class_item.Parent));
            end;

            Write(tfOut, ',sizeof(PR_', UpCase(class_item.Name), '),');
            if length(c_methods.Methods) = 0 then Write(tfOut, '0') else Write(tfOut, c_methods.StartIndex);
            WriteLn(tfOut, format(',0x6b,%d,%d},', [length(c_methods.Methods), class_item.PropertyAutodestroyCount]));

            flg := false;
            for method in c_methods.Methods do
            begin
                if flg then begin
                    WriteLn(tfOut, ',')
                end else begin
                    WriteLn(tfOut, '{');
                    flg := true;
                end;
                if (method.Name = '') then begin
                    Write(tfOut, 'NULL');
                end else if method.ForwardRef = '' then begin
                    Write(tfOut, class_item.Name, '_', method.Name);
                end else begin
                    Write(tfOut, method.ForwardRef);
                end;
            end;

            if flg then begin
                WriteLn(tfOut);
                WriteLn(tfOut, '}');
                flg := false;
            end;

            WriteLn(tfOut, '};');

        end;

        WriteLn(tfOut, '/* Class Lookup Table */');
        if not params.SwitchExists('S') then WriteLn(tfOut, '#ifdef EPOC');
        WriteLn(tfOut, 'GLDEF_D P_CLASS *ClassTable[]=');
        if not params.SwitchExists('S') then begin
            WriteLn(tfOut, '#else');
            WriteLn(tfOut, 'GLDEF_D P_CLASS *ct_', LowerCase(par.ModuleName), '[]=');
            WriteLn(tfOut, '#endif');
        end;

        WriteLn(tfOut, '{');

        flg := false;
        for class_item in par.ClassList do
        begin
            if flg then begin
                WriteLn(tfOut, ',');
            end else begin
                flg := true;
            end;
            Write(tfOut, '(P_CLASS *)&c_', class_item.Name);
        end;
        if flg then WriteLn(tfOut);

        WriteLn(tfOut, '};');

        if length(par.ExternalList) > 0 then begin
            WriteLn(tfOut, '/* External Category Name Table */');
            if not params.SwitchExists('S') then WriteLn(tfOut, '#ifdef EPOC');
            WriteLn(tfOut, 'GLDEF_D struct');
            WriteLn(tfOut, '    {');
            WriteLn(tfOut, '    UWORD number;');
            WriteLn(tfOut, '    UBYTE names[', length(par.ExternalList), '][14];');
            WriteLn(tfOut, '    } ExtCatTable =');
            WriteLn(tfOut, '    {', length(par.ExternalList), ',');
            WriteLn(tfOut, '    {');

            flg := false;
            for s in par.ExternalList do
            begin
                if flg then begin
                    WriteLn(tfOut, ',');
                end else begin
                    flg := true;
                end;
                Write(tfOut, '    {');
                for i := 1 to length(s) do
                begin
                    Write(tfOut, '''', LowerCase(copy(s, i, 1)), ''',');
                end;
                Write(tfOut, '''.'',''D'',''Y'',''L''');
                for i := length(s) + 4 to 13 do begin
                    Write(tfOut, ',0');
                end;
                Write(tfOut, '}');
            end;
            WriteLn(tfOut);
            WriteLn(tfOut, '    }');
            WriteLn(tfOut, '    };');
            if not params.SwitchExists('S') then WriteLn(tfOut, '#endif');
        end;

        CloseFile(tfOut);
    except
        on E: EInOutError do
            WriteLn('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
    end;
end;

procedure MakeLIS(par : TPsionOOLexer);
var
    filepath : String;
    tfOut : TextFile;
    class_item : TPsionOOClass;
    s : String;
    ts : TStringList;
    flg : Boolean;
    // i : Integer;
    // sa : TStringArray;
begin
    filepath := params.SwitchVal('L');
    if (length(filepath) > 0) and (RightStr(filepath, 1) <> DirectorySeparator) then filepath += DirectorySeparator;

    AssignFile(tfOut, filepath + par.ModuleName + '.LIS');

    try
        SetTextLineEnding(tfOut, #13#10);
        rewrite(tfOut);

        WriteLn(tfOut, 'Generated by Ctran from ', ExtractFilename(strFilename));
        WriteLn(tfOut);

        case par.CategoryType of
            catImage:   Write(tfOut, 'IMAGE ');
            catLibrary: Write(tfOut, 'LIBRARY ');
            catName:    Write(tfOut, 'NAME ');
            else        Write(tfOut, par.CategoryType, ' ');
        end;

        WriteLn(tfOut, LowerCase(par.ModuleName));
        WriteLn(tfOut);
        WriteLn(tfOut, '********** ', LowerCase(par.ModuleName), ' **********');

        for class_item in par.ClassList do
        begin
            WriteLn(tfOut, class_item.Name);
            ts := ReverseList(GetAncestors(class_item));
            if ts.Count > 0 then begin
                Write(tfOut, '        Derived from ');
                flg := false;
                for s in ts do
                begin
                    if flg then Write(tfOut, ',') else flg := true;
                    Write(tfOut, s);
                end;
                WriteLn(tfOut);
            end;

            // TODO: Make the order of children match classic CTRAN's .LIS files
            ts := GetChildren(par, class_item.Name);
            if ts.Count > 0 then begin
                flg := false;
                Write(tfOut, '        Subclassed by ');
                for s in ts do
                begin
                    if flg then Write(tfOut, ',') else flg := true;
                    Write(tfOut, s);
                end;
                WriteLn(tfOut);
            end;
        end;

        CloseFile(tfOut);
    except
        on E: EInOutError do
            WriteLn('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
    end;
end;

procedure MakeASM(par : TPsionOOLexer);
var
    filepath : String;
    tfOut : TextFile;
    class_item : TPsionOOClass;
    s : String;
    ts : TStringList;
    flg : Boolean;
    i : Integer;
    ForwardRefs : TStringList;
    method : TPsionOOMethodEntry;
    // total_methods : Integer;
    c_methods : TMethodsForCFile;
    method_id : Integer;
begin
    filepath := params.SwitchVal('A');
    if (length(filepath) > 0) and (RightStr(filepath, 1) <> DirectorySeparator) then filepath += DirectorySeparator;

    AssignFile(tfOut, filepath + par.ModuleName + '.ASM');

    try
        SetTextLineEnding(tfOut, #13#10);
        rewrite(tfOut);

        WriteLn(tfOut, '; Generated by Ctran from ', ExtractFilename(strFilename));
        WriteLn(tfOut, ' include ..\inc\', LowerCase(par.ModuleName), '.ing');
        WriteLn(tfOut, '; External Superclass References');

        ts := GetExternalAncestors(par);
        for s in ts do
        begin
            WriteLn(tfOut, format('ERC_%s equ C_%s', [UpCase(s), UpCase(s)]));
        end;

        ForwardRefs := GetMethodForwardRefs(par);
        if ForwardRefs.Count > 0 then begin
            WriteLn(tfOut, '; Method function forward references');
            WriteLn(tfOut, ' _TEXT segment byte public ''CODE''');
            WriteLn(tfOut, ' ASSUME CS:_TEXT');
            for s in ForwardRefs do WriteLn(tfOut, 'GLREF_C ', s);
            WriteLn(tfOut, ' _TEXT ends');
        end;
        FreeAndNil(ForwardRefs);

        for class_item in par.ClassList do
        begin
            WriteLn(tfOut);
            WriteLn(tfOut);
            WriteLn(tfOut, '; Class ', class_item.Name);
            WriteLn(tfOut, '; ------------------------------------------------------');

            c_methods := MakeMethodsForOutput(class_item);

            WriteLn(tfOut, ' _TEXT segment byte public ''CODE''');
            WriteLn(tfOut, ' ASSUME CS:_TEXT');
            for method in c_methods.Methods do begin
                if (method.Name <> '') and (method.ForwardRef = '') then begin
                    WriteLn(tfOut, format('GLREF_C %s_%s', [class_item.Name, method.Name]));
                end;
            end;
            WriteLn(tfOut, ' _TEXT ends');
            WriteLn(tfOut, ' _TEXT segment byte public ''CODE''');

            WriteLn(tfOut, 'GLDEF_C c_', class_item.Name);
            flg := false;
            s := UpCase(DependencyList[DependencyList[class_item.Name].Parent].Category);
            if s = UpCase(par.ModuleName) then begin
                Write(tfOut, '0');
                flg := true;
            end else begin
                for i := 0 to length(par.ExternalList) - 1 do begin
                    if s = UpCase(par.ExternalList[i]) then begin
                        WriteLn(tfOut, ' dw   ', i + 1);
                        flg := true;
                        break;
                    end;
                end;
            end;

            if not flg then begin
                WriteLn('MakeASM: Couldn''t find class ', s, ' anywhere - bad logic here? (Should it be ', par.ModuleName, '?)');
                halt;
            end;

            Write(tfOut, ' dw   ');
            if DependencyList[class_item.Parent].Category = par.ModuleName then
            begin
                WriteLn(tfOut, 'c_', class_item.Parent);
            end else begin
                WriteLn(tfOut, 'ERC_', UpCase(class_item.Parent));
            end;

            WriteLn(tfOut, ' dw   (SIZE PR_', UpCase(class_item.Name), ')');
            Write(tfOut, ' db   ');
            if length(c_methods.Methods) = 0 then WriteLn(tfOut, '0') else WriteLn(tfOut, c_methods.StartIndex);
            WriteLn(tfOut, ' db   06bh');
            WriteLn(tfOut, ' db   ', length(c_methods.Methods));
            WriteLn(tfOut, ' db   ', class_item.PropertyAutodestroyCount);

            flg := false;
            method_id := -1;
            for method in c_methods.Methods do
            begin
                // if flg then begin
                //     WriteLn(tfOut, ',')
                // end else begin
                //     WriteLn(tfOut, '{');
                //     flg := true;
                // end;
                if (method.Name = '') then begin
                    // WriteLn(tfOut, ' dw   0');
                    // inc(method_id);
                end else begin
                    for i := 1 to method_id do
                    begin
                        WriteLn(tfOut, ' dw   0');
                    end;
                    if method.ForwardRef = '' then begin
                        WriteLn(tfOut, ' dw   ', class_item.Name, '_', method.Name);
                    end else begin
                        WriteLn(tfOut, ' dw   ', method.ForwardRef);
                    end;
                end;
                inc(method_id);
            end;

            if flg then begin
                WriteLn(tfOut);
                flg := false;
            end;

            WriteLn(tfOut, ' EEND');
            WriteLn(tfOut, ' _TEXT ends');
        end;


        WriteLn(tfOut, '; Class Lookup Table');
        WriteLn(tfOut, ' _TEXT segment byte public ''CODE''');
        WriteLn(tfOut, 'GLDEF_C ClassTable');
        for class_item in par.ClassList do
        begin
            WriteLn(tfOut, ' dw   c_', class_item.Name);
        end;
        WriteLn(tfOut, ' EEND');
        WriteLn(tfOut, ' _TEXT ends');

        if length(par.ExternalList) > 0 then begin
            WriteLn(tfOut, '; External Category Name Table');
            WriteLn(tfOut, ' _TEXT segment byte public ''CODE''');
            WriteLn(tfOut, 'GLDEF_C ExtCatTable');

            WriteLn(tfOut, ' dw   ', length(par.ExternalList));
            for s in par.ExternalList do
            begin
                Write(tfOut, ' db "', s, '.DYL"');
                for i := length(s) + 4 to 13 do begin
                    Write(tfOut, ',0');
                end;
                WriteLn(tfOut);
            end;


            WriteLn(tfOut, ' EEND');
            WriteLn(tfOut, ' _TEXT ends');
        end;

        WriteLn(tfOut, ' end');

        CloseFile(tfOut);
    except
        on E: EInOutError do
            WriteLn('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
    end;

end;

procedure MakeING(par : TPsionOOLexer);
var
    inc_file : String;
    inc_file_def : String;
    i : Integer;
    class_item : TPsionOOClass;
    constant_item : TPsionOOConstantEntry;
    s : String;
    ts : TStringList;
    tfOut : TextFile;
    filepath : String;
    method_list : TStringList;
begin
    filepath := params.SwitchVal('I');
    if (length(filepath) > 0) and (RightStr(filepath, 1) <> DirectorySeparator) then filepath += DirectorySeparator;

    AssignFile(tfOut, filepath + par.ModuleName + '.ING');

    try
        SetTextLineEnding(tfOut, #13#10);
        rewrite(tfOut);

        WriteLn(tfOut, '; Generated by Ctran from ', ExtractFilename(strFilename));
        WriteLn(tfOut, UpCase(par.ModuleName), '_ING equ 1');

        for inc_file in par.IncludeList do
        begin
            inc_file_def := UpCase(StringReplace(inc_file, '.', '_', [rfReplaceAll]));
            WriteLn(tfOut, 'IFNDEF ', inc_file_def, '_ING');
            WriteLn(tfOut, ' INCLUDE ', inc_file, '_ING');
            WriteLn(tfOut, 'ENDIF');
        end;

        // WriteLn(tfOut, '; Category Numbers');
        WriteLn(tfOut, format('CAT_%s_%s equ 0', [par.ModuleName, par.ModuleName]));
        for i := 0 to length(par.ExternalList) - 1 do
        begin;
            WriteLn(tfOut, format('CAT_%s_%s equ %d', [par.ModuleName, UpCase(par.ExternalList[i]), i + 1]));
        end;

        // WriteLn(tfOut, '; Class Numbers');
        for i := 0 to length(par.ClassList) - 1 do
        begin
            WriteLn(tfOut, format('C_%s equ %d', [UpCase(par.ClassList[i].Name), i]));
        end;


        // FIX: This is generating the wrong numbers, but MakeG() is correct!
        method_list := BuildMethodNumbers(par);
        // WriteLn(tfOut, '; Method Numbers');
        for s in method_list do
        begin
            WriteLn(tfOut, 'O_', s);
        end;

        for class_item in par.ClassList do
        begin
            WriteLn(tfOut);
            WriteLn(tfOut);
            if length(class_item.ClassConstants) > 0 then begin
                WriteLn(tfOut, '; Constants for ', class_item.Name);
                for constant_item in class_item.ClassConstants do
                begin
                    WriteLn(tfOut, format('%s equ (%s)', [constant_item.Name, constant_item.Value]));
                end;
            end;

            if length(class_item.ClassTypes) > 0 then begin
                WriteLn(tfOut, '; Types for ', class_item.Name);
                for s in class_item.ClassTypes do
                begin
                    WriteLn(tfOut, s);
                end;
            end;

            WriteLn(tfOut, '; Property of ', class_item.Name);
            if length(class_item.ClassProperty) > 0 then begin
                WriteLn(tfOut, 'PRS_', UpCase(class_item.Name), ' struc');
                for s in class_item.ClassProperty do
                begin
                    WriteLn(tfOut, s);
                end;
                WriteLn(tfOut, 'PRS_', UpCase(class_item.Name), ' ends');
            end;
            WriteLn(tfOut, 'PR_', UpCase(class_item.Name), ' struc');
            ts := GetAncestorsWithProperty(class_item);
            for i := ts.Count - 1 downto 0 do
            begin
                WriteLn(tfOut, UpCase(class_item.Name[1]), copy(class_item.Name, 2), UpCase(ts[i][1]), copy(ts[i], 2), ' PRS_', UpCase(ts[i]), ' <>');
            end;
            if length(class_item.ClassProperty) > 0 then begin
                WriteLn(tfOut, UpCase(class_item.Name[1]), copy(class_item.Name, 2), UpCase(class_item.Name[1]), copy(class_item.Name, 2), ' PRS_', UpCase(class_item.Name), ' <>');
            end;
            WriteLn(tfOut, 'PR_', UpCase(class_item.Name), ' ends');
        end;

        CloseFile(tfOut);
    except
        on E: EInOutError do
            WriteLn('MakeING: File handling error occurred. Details: ', E.ClassName, '/', E.Message);
    end;
end;

//
// MAIN
//

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
        if params.SwitchExists('E') then begin
            PathList := CheckPath(params.SwitchVal('E'));
        end else begin
            PathList := CheckPath('.');
        end;

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

        MethodList.Clear;

        for class_item in CatLexer.ClassList do
        begin
            cur_metaclass := MakeMetaclass(class_item);

            // WriteLn;
            // WriteLn('Ancestor metaclass for ', class_item.Name, ':');
            // for s in cur_metaclass do begin
            //     WriteLn(s);
            // end;

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
        end;

        // WriteLn('List of All Classes Across All External Files:');
        // for s in AllExtClasses do begin
        //     WriteLn('  ', s);
        // end;

        if params.SwitchExists('X') then begin
            MakeEXT(CatLexer);
        end;
        if params.SwitchExists('G') then begin
            MakeG(CatLexer);
        end;
        if params.SwitchExists('L') then begin
            MakeLIS(CatLexer);
        end;
        if params.SwitchExists('C') then begin
            MakeC(CatLexer);
        end;
        if params.SwitchExists('I') then begin
            MakeING(CatLexer);
        end;
        if params.SwitchExists('A') then begin
            MakeASM(CatLexer);
        end;

    end
    finally begin
        FreeAndNil(DependencyList);
        FreeAndNil(CatLexer);
    end;
end;

end.

