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

    TDependencyList = specialize TDictionary<String, TPsionOOCatClass>;

    // TCommentType = (
    //     commentC,
    //     commentASM
    // );

    TParserDictionary = specialize TObjectDictionary<String, TPsionOOLexer>;

    TStringListHelper = Class Helper for TStringList
        Public
            Function Reverse : TStringList;
    end;

var
    strFilename : String;
    CatParser : TPsionOOLexer;
    params : TPsionSDKAppParams;
    PathList : TStringList;
    s : String;
    DependencyList : TDependencyList;
    MethodList : TStringList;
    class_item : TPsionOOClass;
    InternalClassList : TStringList;
    InternalModuleList : TStringList;
    ExternalModuleList : TStringList;
    cur_metaclass : TStringList;
    method_item : TPsionOOMethodEntry;
    extfile : String;
    parsers : TParserDictionary;
    par : TPsionOOLexer; // temporary parser storage

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

// Takes a TStringList and formats every element using format_main. If
// format_final exists, the last element in the TStringList will be formatted
// with it instead.
// Useful for generating output for a file where the last line needs to be
// different, such as a comma separated list.
function FormatStringList(sl : TStringList ; format_main : String ; format_final : String = '') : TStringList;
var
    i : Integer;
    arr_strings : TStringArray;
begin
    Result := TStringList.Create();

    // TODO: For both format_main and format_final, if there is more than one %s, repeat sl[i] in the array

    if AnsiPos('%s', format_main) = 0 then
    begin
        WriteLn('FormatStringList: Missing %s from format_main.');
        halt(-1);
    end;
    if (format_final <> '') and (AnsiPos('%s', format_main) = 0) then
    begin
        WriteLn('FormatStringList: Missing %s from format_final.');
        halt(-1);
    end;


    for i := 0 to sl.Count - 2 do
    begin
        Result.Add(format(format_main, [sl[i]]));
    end;
    if format_final = '' then
        Result.Add(format(format_main, [sl[sl.Count - 1]]))
    else
        Result.Add(format(format_final, [sl[sl.Count - 1]]));
end;

// Creates a string where s is repeated c times.
function RepeatStr(s: String; c: integer) : String;
var
    i : Integer;
    l : Integer;
begin
    Result := '';
    if c > 0 then begin
        l := Length(s);
        if l > 0 then begin
            SetLength(Result, l * c);
            for i := 0 to c - 1 do
            begin
                move(s[1], Result[l * i + 1], l);
            end;
        end;
    end;
end;

// Gets the stem of a filename (i.e. without the extension). If it has no
// extension, it just returns the filename.
function ExtractFileStem(s : String) : String;
begin
    if ExtractFileExt(s) = '' then
        Result := ExtractFileName(s)
    else
        Result := copy(ExtractFileName(s), 1, length(ExtractFileName(s)) - AnsiPos(ExtractFileExt(s), ExtractFileExt(s)) - 1);
end;

// Checks a provided path string, separated by semicolons.
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

// Reverses the order of elements in a TStringList
function TStringListHelper.Reverse() : TStringList;
var
    i : Integer;
begin
    Result := TStringList.Create();
    for i := Self.Count - 1 downto 0 do
    begin
        Result.Add(Self[i]);
    end;
end;

// Makes sure that an external category file exists. If so, return the absolute
// path.
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

// Recursively build a list of classes and their parents, so that dependencies
// can easily be found.
procedure LoadDependencies(par : TPsionOOLexer);
// FIX: Deal with "shadow" classes that only contain DEFERred methods (see SOLIPEG's TASK class as an example)
var
    ext_class : TPsionOOCatClass;
    par_class : TPsionOOClass;
    method : TPsionOOMethodEntry;
    category : String;
    element : TPsionOOFileElement;
    required : String;
begin
    category := par.ModuleName;

    for element in par.ElementList do
    begin
        case element.ElementType of
            incClass: begin
                par_class := par.ClassList[element.index];

                if DependencyList.ContainsKey(LowerCase(par_class.Name)) then begin
                    WriteLn('Error ', ExtractFilename(par.FileLocation), ': Class ', par_class.Name, ' already defined in ', DependencyList[LowerCase(par_class.Name)].Category);
                    // TODO: (if possible) add location in file
                    halt;
                end;

                if (not DependencyList.ContainsKey(LowerCase(par_class.Parent))) and (par_class.Parent <> '') then begin
                    WriteLn('Error ', ExtractFilename(par.FileLocation), ': Superclass ', par_class.Parent, ' of ', par_class.Name, ' does not exist');
                    // TODO: (if possible) add location in file
                    halt;
                end;

                for method in par_class.Methods do
                begin
                    case method.MethodType of
                        methodDeclare, methodAdd: begin
                            if MethodList.IndexOf(LowerCase(method.Name)) > -1 then begin
                                WriteLn('Error ', ExtractFilename(par.FileLocation), ': Method ', method.Name, ' already exists in category');
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
                if par.FileType <> ooExternal then InternalClassList.Add(par_class.Name);
            end;
            incRequire: begin
                required := UpCase(par.RequireList[element.index]);
                if not parsers.ContainsKey(required) then begin
                    WriteLn('LoadDependencies: Can''t find ', required, ' in dictionary!');
                    halt(-1);
                end;

                WriteLn('>>> Loading dependencies for ', required);
                LoadDependencies(parsers[required]);
            end;
        end;
    end;
end;

// Calls the main LoadDependencies() procedure after creating a temporary
// parser instance. Only used for loading external category files.
procedure LoadDependencies(filename : String);
var
    par : TPsionOOLexer;
    // class_item : TPsionOOClass;
begin
    if filename = '' then begin
        WriteLn('LoadDependencies: filename is empty');
        halt(-1);
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

            LoadDependencies(par);
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

// Return all the ancestors of a class that have a HAS_PROPERTY flag.
function GetAncestorsWithProperty(class_item : TPsionOOClass) : TStringList;
// TODO: Check if this should be modified for internal classes
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

// Return all the ancestors of a class, "eldest" first
function GetAncestors(class_item : TPsionOOClass) : TStringList;
var
    ancestor : String;
    i : Integer;
begin
    Result := TStringList.Create();
    ancestor := LowerCase(class_item.Parent);

    // TODO: Could this be tidied using TStringList.Reverse() and a separate TStringList?
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

// Builds a list of methods by superimposing all of a class's ancestors'
// methods. This does not include the current class's methods.
function MakeMetaclass(class_item : TPsionOOClass) : TStringList;
// TODO: Is it right to ignore REPLACEd methods? Do they matter if they have already been declared?
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

// Return all the first-generation children of a class
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

// Gives every method a number, based on the order it is mentioned in the
// current category (or sub-category) file.
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

// Creates a unique list of classes. It looks at the ancestry of every class in
// a category file and finds each class's first external ancestor. If an
// ancestor class is already in the list, don't add it again.
function GetExternalAncestors(par : TPsionOOLexer) : TStringList;
// TODO: Check ancestor classes for circular reference (ancestor TStringList?)
// (Might not need to do this here if it's checked elsewhere.)
// TODO: Sort methods as per original CTRAN (the order that they appear in External files)
var
    ancestor : String;
    s : String;
begin
    Result := TStringList.Create();

    for s in InternalClassList do
    begin
        ancestor := DependencyList[s].Parent;
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
// TODO: Review this!
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

// Finds all the class method forward references across all internal category
// and sub-cat files. Used to generate the "Class Method Forward References"
// section at the start of .C and .ASM files.
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

function GetClassFromParsers(class_name : String) : TPsionOOClass;
var
    class_item : TPsionOOClass;
begin
    if not DependencyList.ContainsKey(class_name) then begin
        WriteLn('GetClassFromParsers: Can''t find ', class_name, ' in DependencyList');
        halt(-1);
    end;

    for class_item in parsers[DependencyList[class_name].Category].ClassList do
    begin
        if class_item.Name = class_name then begin
            exit(class_item);
        end;
    end;

    WriteLn('GetClassFromParsers: Can''t find ', class_name, ' in parser ', DependencyList[class_name].Category);
    halt(-1);
end;

// If a class's parent is in an external module, return the ID (order in which
// they are listed in the main category file, starting at 1). If it's in an
// internal one, return 0. Otherwise halt, because there's a bug in CTRAN.
function GetParentModuleID(class_name : String) : Integer;
var
    parent : String;
    parent_module : String;
begin
    // TODO: Why did I use the DependencyList on the next line?
    // Isn't it better to just send the entire class or just the parent's name from class_item.Parent?
    parent := DependencyList[class_name].Parent;
    parent_module := UpCase(DependencyList[parent].Category);
    // WriteLn('Finding ', s, ' for ', par.ModuleName);
    if InternalModuleList.IndexOf(parent_module) > -1 then begin
        // WriteLn('Internal');
        exit(0);
    end else if ExternalModuleList.IndexOf(parent_module) > -1 then begin
        // WriteLn('External: ', ExternalModuleList.IndexOf(s) + 1, ' ', s);
        exit(ExternalModuleList.IndexOf(parent_module) + 1);
    end;

    WriteLn('GetParentModuleID: Couldn''t find parent module ', parent_module, ' of parent class ', parent, ' for class ', class_name, ' - bad logic here? (Should it be ', InternalModuleList[0], '?)');
    halt(-1);
end;

//
// MAKE FILES
//

procedure MakeEXT(par: TPsionOOLexer);
var
    // element : TPsionOOFileElement; // NOTE: Might need this again for OVAL support
    method: TPsionOOMethodEntry;
    flgHasMethod: boolean;
    tfOut : TextFile;
    filepath : String;
    class_item : TPsionOOClass;
    class_name : String;
begin
    filepath := params.SwitchVal('X');
    if (length(filepath) > 0) and (RightStr(filepath, 1) <> DirectorySeparator) then filepath += DirectorySeparator;

    AssignFile(tfOut, filepath + par.ModuleName + '.EXT');

    try
        SetTextLineEnding(tfOut, #13#10);
        rewrite(tfOut);

        WriteLn(tfOut, 'Generated by Ctran from ', ExtractFileName(par.FileLocation));
        case par.CategoryType of
            catName:    Write(tfOut, 'NAME');
            catImage:   Write(tfOut, 'IMAGE');
            catLibrary: Write(tfOut, 'LIBRARY');
            else        Write(par.CategoryType);
        end;
        WriteLn(tfOut, ' ', LowerCase(par.ModuleName));

        for class_name in InternalClassList do
        begin
            class_item := GetClassFromParsers(class_name);

            flgHasMethod := false;
            Write(tfOut, 'CLASS ', class_item.Name, ' ');
            if class_item.Parent <> '' then
                Write(tfOut, class_item.Parent);
            WriteLn(tfOut);
            WriteLn(tfOut, '{');
            for method in class_item.Methods do
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
            if length(class_item.ClassProperty) > 0 then begin
                WriteLn(tfOut, 'HAS_PROPERTY');
            end;
            WriteLn(tfOut, '}');
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
    sl : TStringList;
    tfOut : TextFile;
    filepath : String;
    method_list : TStringList;
    flgNotSDK : Boolean;
begin
    flgNotSDK := ((not params.SwitchExists('S')) and (par.FileType = ooCategory));

    filepath := params.SwitchVal('G');
    if (length(filepath) > 0) and (RightStr(filepath, 1) <> DirectorySeparator) then filepath += DirectorySeparator;

    AssignFile(tfOut, filepath + par.ModuleName + '.G');

    try
        SetTextLineEnding(tfOut, #13#10);
        rewrite(tfOut);

        WriteLn(tfOut, '/* Generated by Ctran from ', ExtractFilename(par.FileLocation), ' */');
        WriteLn(tfOut, '#define ', UpCase(par.ModuleName), '_G');

        for inc_file in par.IncludeList do
        begin
            inc_file_def := UpCase(StringReplace(inc_file, '.', '_', [rfReplaceAll]));
            WriteLn(tfOut, '#ifndef ', inc_file_def);
            WriteLn(tfOut, '#include <', inc_file, '>');
            WriteLn(tfOut, '#endif');
        end;

        if par.FileType = ooCategory then
        begin
            WriteLn(tfOut, '/* Category Numbers */');
            if flgNotSDK then begin
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
            begin
                WriteLn(tfOut, format('#define CAT_%s_%s %d', [par.ModuleName, UpCase(par.ExternalList[i]),  i + 1]));
            end;
            if flgNotSDK then begin
                WriteLn(tfOut, '#else');
                WriteLn(tfOut, format('#define CAT_%s_%s (&ct_%s[0])', [par.ModuleName, par.ModuleName, LowerCase(par.ModuleName)]));
                for s in par.ExternalList do
                begin
                    WriteLn(tfOut, format('#define CAT_%s_%s (&ct_%s[0])', [par.ModuleName, UpCase(s), LowerCase(s)]));
                end;
                WriteLn(tfOut, '#endif');
            end;
        end;

        if length(par.ClassList) > 0 then begin 
            WriteLn(tfOut, '/* Class Numbers */');
            for class_item in par.ClassList do
            begin
                if InternalClassList.IndexOf(class_item.Name) < 0 then begin
                    WriteLn('MakeG: Can''t find class ', class_item.Name, ' in InternalClassList');
                    halt(-1);
                end;
                WriteLn(tfOut, format('#define C_%s %d', [UpCase(class_item.Name), InternalClassList.IndexOf(class_item.Name)]));
            end;

            method_list := BuildMethodNumbers(par);
            if method_list.Count > 0 then begin
                WriteLn(tfOut, '/* Method Numbers */');
                for s in method_list do
                begin
                    WriteLn(tfOut, '#define O_', s);
                end;
            end;

            for class_item in par.ClassList do
            begin
                WriteLn(tfOut);
                WriteLn(tfOut);
                WriteLn(tfOut, '/* Class ', class_item.Name, ' */');
                WriteLn(tfOut, '/* ------------------------------------------------------ */');

                if length(class_item.ClassConstants) > 0 then begin
                    WriteLn(tfOut, '/* Constants for ', class_item.Name, ' */');
                    for constant_item in class_item.ClassConstants do
                    begin
                        WriteLn(tfOut, format('#define %s %s', [constant_item.Name, constant_item.Value]));
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

                sl := GetAncestorsWithProperty(class_item).Reverse;
                for s in sl do
                begin
                    WriteLn(tfOut, format('PRS_%s %s;', [UpCase(s), s]));
                end;
                FreeAndNil(sl);

                if length(class_item.ClassProperty) > 0 then begin
                    WriteLn(tfOut, format('PRS_%s %s;', [UpCase(class_item.Name), class_item.Name]));
                end;
                WriteLn(tfOut, '} PR_', UpCase(class_item.Name), ';');
            end;
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
    s : String;
    sl : TStringList;
    flg : Boolean;
    tfOut : TextFile;
    class_name : String;
    class_item : TPsionOOClass;
    filepath : String;
    method : TPsionOOMethodEntry;
    ForwardRefs : TStringList;
    total_methods : Integer;
    c_methods : TMethodsForCFile;
    parent_extcat_id : Integer;
    flgNotSDK : Boolean;
begin
    flgNotSDK := (not params.SwitchExists('S'));

    filepath := params.SwitchVal('C');
    if (length(filepath) > 0) and (RightStr(filepath, 1) <> DirectorySeparator) then filepath += DirectorySeparator;

    AssignFile(tfOut, filepath + par.ModuleName + '.C');

    try
        SetTextLineEnding(tfOut, #13#10);
        rewrite(tfOut);

        WriteLn(tfOut, '/* Generated by Ctran from ', ExtractFilename(par.FileLocation), ' */');

        for s in InternalModuleList do
        begin
            WriteLn(tfOut, '#include <', LowerCase(s), '.g>');
        end;

        WriteLn(tfOut, '/* External Superclass References */');

        if flgNotSDK then WriteLn(tfOut, '#ifdef EPOC');

        sl := GetExternalAncestors(par);
        for s in sl do
        begin
            WriteLn(tfOut, format('#define ERC_%s C_%s', [UpCase(s), UpCase(s)]));
        end;
        if flgNotSDK then begin
            WriteLn(tfOut, '#else');

            for s in sl do
            begin
                WriteLn(tfOut, 'GLREF_D P_CLASS c_', s, ';');
                WriteLn(tfOut, format('#define ERC_%s &c_%s', [UpCase(s), s]));
            end;

            WriteLn(tfOut, '#endif');
        end;
        FreeAndNil(sl);

        // Method function forward references
        ForwardRefs := GetMethodForwardRefs(par);
        if ForwardRefs.Count > 0 then begin
            WriteLn(tfOut, '/* Method function forward references */');
            for s in ForwardRefs do WriteLn(tfOut, 'GLREF_C VOID ', s, '();');
        end;
        FreeAndNil(ForwardRefs);

        // Generate Classes and Methods
        for class_name in InternalClassList do
        begin
            class_item := GetClassFromParsers(class_name);
            parent_extcat_id := GetParentModuleID(class_item.Name);

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

            // Define the class

            Write(tfOut, '{');
            Write(tfOut, parent_extcat_id);
            Write(tfOut, ',(P_CLASS *)');

            if parent_extcat_id = 0 then
            begin // It's internal
                Write(tfOut, '&c_', class_item.Parent)
            end else begin
                Write(tfOut, 'ERC_', UpCase(class_item.Parent));
            end;

            Write(tfOut, ',sizeof(PR_', UpCase(class_item.Name), '),');
            if length(c_methods.Methods) = 0 then
            begin
                Write(tfOut, '0');
            end else begin
                Write(tfOut, c_methods.StartIndex);
            end;
            WriteLn(tfOut, format(',0x6b,%d,%d},', [length(c_methods.Methods), class_item.PropertyAutodestroyCount]));

            // Define the methods

            sl := TStringList.Create();
            for method in c_methods.Methods do
            begin
                if (method.Name = '') then begin
                    sl.Add('NULL');
                end else if method.ForwardRef = '' then begin
                    sl.Add(class_item.Name + '_' + method.Name);
                end else begin
                    sl.Add(method.ForwardRef);
                end;
            end;

            if sl.Count > 0 then
            begin
                WriteLn(tfOut, '{');
                sl := FormatStringList(sl, '%s,', '%s');
                for s in sl do
                begin
                    WriteLn(tfOut, s);
                end;
                WriteLn(tfOut, '}');
            end;
            FreeAndNil(sl);

            WriteLn(tfOut, '};');
        end;

        WriteLn(tfOut, '/* Class Lookup Table */');
        if flgNotSDK then WriteLn(tfOut, '#ifdef EPOC');
        WriteLn(tfOut, 'GLDEF_D P_CLASS *ClassTable[]=');
        if flgNotSDK then begin
            WriteLn(tfOut, '#else');
            WriteLn(tfOut, 'GLDEF_D P_CLASS *ct_', LowerCase(par.ModuleName), '[]=');
            WriteLn(tfOut, '#endif');
        end;

        WriteLn(tfOut, '{');

        sl := FormatStringList(InternalClassList, '(P_CLASS *)&c_%s,', '(P_CLASS *)&c_%s');
        for s in sl do WriteLn(tfOut, s);
        FreeAndNil(sl);

        WriteLn(tfOut, '};');

        if length(par.ExternalList) > 0 then begin
            WriteLn(tfOut, '/* External Category Name Table */');
            if flgNotSDK then WriteLn(tfOut, '#ifdef EPOC');
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
                    Write(tfOut, '''', LowerCase(s[i]), ''',');
                end;
                Write(tfOut, '''.'',''D'',''Y'',''L''');
                Write(tfOut, RepeatStr(',0', 10 - length(s)));
                Write(tfOut, '}');
            end;
            WriteLn(tfOut);
            WriteLn(tfOut, '    }');
            WriteLn(tfOut, '    };');
            if flgNotSDK then WriteLn(tfOut, '#endif');
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
    sl : TStringList;
    module_name : String;
begin
    filepath := params.SwitchVal('L');
    if (length(filepath) > 0) and (RightStr(filepath, 1) <> DirectorySeparator) then filepath += DirectorySeparator;

    AssignFile(tfOut, filepath + par.ModuleName + '.LIS');

    try
        SetTextLineEnding(tfOut, #13#10);
        rewrite(tfOut);

        WriteLn(tfOut, 'Generated by Ctran from ', ExtractFilename(par.FileLocation));
        WriteLn(tfOut);

        case par.CategoryType of
            catImage:   Write(tfOut, 'IMAGE ');
            catLibrary: Write(tfOut, 'LIBRARY ');
            catName:    Write(tfOut, 'NAME ');
            else        Write(tfOut, par.CategoryType, ' ');
        end;

        WriteLn(tfOut, LowerCase(par.ModuleName));

        for module_name in InternalModuleList do
        begin
            WriteLn(tfOut);
            WriteLn(tfOut, '********** ', LowerCase(module_name), ' **********');

            for class_item in parsers[module_name].ClassList do
            begin
                WriteLn(tfOut, class_item.Name);
                sl := GetAncestors(class_item);
                if sl.Count > 0 then begin
                    WriteLn(tfOut, '        Derived from ', sl.Reverse.CommaText);
                end;

                // TODO: Make the order of children match classic CTRAN's .LIS files
                sl := GetChildren(par, class_item.Name);
                if sl.Count > 0 then begin
                    WriteLn(tfOut, '        Subclassed by ', sl.CommaText);
                end;
            end;
        end;

        CloseFile(tfOut);
    except
        on E: EInOutError do
            WriteLn('MakeLIS: File handling error occurred. Details: ', E.ClassName, '/', E.Message);
    end;
end;

procedure MakeASM(par : TPsionOOLexer);
var
    i : Integer;
    s : String;
    sl : TStringList;
    flg : Boolean;
    tfOut : TextFile;
    class_item : TPsionOOClass;
    filepath : String;
    method : TPsionOOMethodEntry;
    ForwardRefs : TStringList;
    c_methods : TMethodsForCFile;
    method_id : Integer;
    class_name : String;
    parent_extcat_id : Integer;
begin
    filepath := params.SwitchVal('A');
    if (length(filepath) > 0) and (RightStr(filepath, 1) <> DirectorySeparator) then filepath += DirectorySeparator;

    AssignFile(tfOut, filepath + par.ModuleName + '.ASM');

    try
        SetTextLineEnding(tfOut, #13#10);
        rewrite(tfOut);

        WriteLn(tfOut, '; Generated by Ctran from ', ExtractFilename(par.FileLocation));

        for s in InternalModuleList do
        begin
            WriteLn(tfOut, ' include ..\inc\', LowerCase(s), '.ing');
        end;

        WriteLn(tfOut, '; External Superclass References');

        sl := GetExternalAncestors(par);
        for s in sl do
        begin
            WriteLn(tfOut, format('ERC_%s equ C_%s', [UpCase(s), UpCase(s)]));
        end;
        FreeAndNil(sl);

        ForwardRefs := GetMethodForwardRefs(par);
        if ForwardRefs.Count > 0 then begin
            WriteLn(tfOut, '; Method function forward references');
            WriteLn(tfOut, ' _TEXT segment byte public ''CODE''');
            WriteLn(tfOut, ' ASSUME CS:_TEXT');
            for s in ForwardRefs do WriteLn(tfOut, 'GLREF_C ', s);
            WriteLn(tfOut, ' _TEXT ends');
        end;
        FreeAndNil(ForwardRefs);

        for class_name in InternalClassList do
        begin
            class_item := GetClassFromParsers(class_name);
            parent_extcat_id := GetParentModuleID(class_item.Name);

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
            WriteLn(tfOut, ' dw   ', parent_extcat_id);

            Write(tfOut, ' dw   ');

            if parent_extcat_id = 0 then
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

            method_id := -1; // INFO: I don't know why this works, but it matches the output from classic CTRAN
            for method in c_methods.Methods do
            begin
                if (method.Name <> '') then begin
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

            WriteLn(tfOut, ' EEND');
            WriteLn(tfOut, ' _TEXT ends');
        end;

        WriteLn(tfOut, '; Class Lookup Table');
        WriteLn(tfOut, ' _TEXT segment byte public ''CODE''');
        WriteLn(tfOut, 'GLDEF_C ClassTable');

        sl := FormatStringList(InternalClassList, ' dw   c_%s');
        for s in sl do WriteLn(tfOut, s);
        FreeAndNil(sl);

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
    inc_stem : String;
    i : Integer;
    class_item : TPsionOOClass;
    constant_item : TPsionOOConstantEntry;
    s : String;
    sl : TStringList;
    tfOut : TextFile;
    filepath : String;
    method_list : TStringList;
    inc_ext : String;
begin
    filepath := params.SwitchVal('I');
    if (length(filepath) > 0) and (RightStr(filepath, 1) <> DirectorySeparator) then filepath += DirectorySeparator;

    AssignFile(tfOut, filepath + par.ModuleName + '.ING');

    try
        SetTextLineEnding(tfOut, #13#10);
        rewrite(tfOut);

        WriteLn(tfOut, '; Generated by Ctran from ', ExtractFileName(par.FileLocation));
        WriteLn(tfOut, UpCase(par.ModuleName), '_ING equ 1');

        for inc_file in par.IncludeList do
        begin
            // inc_file_def := UpCase(StringReplace(inc_file, '.', '_', [rfReplaceAll]));
            inc_stem := UpCase(ExtractFileStem(inc_file));
            if UpCase(ExtractFileExt(inc_file)) = '.H' then inc_ext := 'INC' else inc_ext := 'ING';

            WriteLn(tfOut, format('IFNDEF %s_%s', [inc_stem, inc_ext]));
            WriteLn(tfOut, format(' INCLUDE ..\inc\%s.%s', [inc_stem, inc_ext]));
            WriteLn(tfOut, 'ENDIF');
        end;

        if ((par.FileType = ooCategory) and (length(par.ExternalList) > 1)) or (length(par.ExternalList) > 0) then begin
            WriteLn(tfOut, '; Category Numbers');
            if par.FileType = ooCategory then WriteLn(tfOut, format('CAT_%s_%s equ 0', [par.ModuleName, par.ModuleName]));
            for i := 0 to length(par.ExternalList) - 1 do
            begin;
                WriteLn(tfOut, format('CAT_%s_%s equ %d', [par.ModuleName, UpCase(par.ExternalList[i]), i + 1]));
            end;
        end;

        if length(par.ClassList) > 0 then begin 
            WriteLn(tfOut, '; Class Numbers');
            // NOTE: This generates different numbers to CTRAN 5.02, but I think it's correct as it matches MakeG()
            for class_item in par.ClassList do
            begin
                if InternalClassList.IndexOf(class_item.Name) < 0 then begin
                    WriteLn('MakeING: Can''t find class ', class_item.Name, ' in InternalClassList');
                    halt(-1);
                end;
                WriteLn(tfOut, format('C_%s equ %d', [UpCase(class_item.Name), InternalClassList.IndexOf(class_item.Name)]));
            end;

            method_list := BuildMethodNumbers(par);
            if method_list.Count > 0 then begin
                WriteLn(tfOut, '; Method Numbers');
                for s in method_list do
                begin
                    WriteLn(tfOut, format('O_%s equ %s', [s.Split(' ')[0], s.Split(' ')[1]]));
                end;
            end;

            for class_item in par.ClassList do
            begin
                WriteLn(tfOut);
                WriteLn(tfOut);
                if length(class_item.ClassConstants) > 0 then begin
                    WriteLn(tfOut, '; Constants for ', class_item.Name);
                    for constant_item in class_item.ClassConstants do
                    begin
                        case copy(constant_item.Value, 1, 2) of
                            '0x': s := '0' + copy(constant_item.Value, 3) + 'h';
                            else s := constant_item.Value;
                        end;
                        WriteLn(tfOut, format('%s equ (%s)', [constant_item.Name, s]));
                    end;
                end;

                // FIX: Translate C to ASM
                if length(class_item.ClassTypes) > 0 then begin
                    WriteLn(tfOut, '; Types for ', class_item.Name);
                    for s in class_item.ClassTypes do
                    begin
                        WriteLn(tfOut, s);
                    end;
                end;

                // FIX: Translate C to ASM
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

                sl := GetAncestorsWithProperty(class_item).Reverse;
                for s in sl do
                begin
                    WriteLn(tfOut, UpCase(class_item.Name[1]), copy(class_item.Name, 2), UpCase(s[1]), copy(s, 2), ' PRS_', UpCase(s), ' <>');
                end;
                FreeAndNil(sl);

                if length(class_item.ClassProperty) > 0 then begin
                    WriteLn(tfOut, UpCase(class_item.Name[1]), copy(class_item.Name, 2), UpCase(class_item.Name[1]), copy(class_item.Name, 2), ' PRS_', UpCase(class_item.Name), ' <>');
                end;
                WriteLn(tfOut, 'PR_', UpCase(class_item.Name), ' ends');
            end;
        end;
        CloseFile(tfOut);
    except
        on E: EInOutError do
            WriteLn('MakeING: File handling error occurred. Details: ', E.ClassName, '/', E.Message);
    end;
end;

// Opens all possible internal category (and sub-category) files
// It creates a new parser, opening `filename`. Once that is done, it looks at the file's
// REQUIREd sub-category files and parses them, adding each parser as a new parser object.
//
// Each internal file will now exist as a parser object in the TObjectDictionary `parsers`.
function WalkParsers(filename: String) : String;
var
    par : TPsionOOLexer;
    s : String;
begin
    WriteLn('>>> Parsing ', filename);

    par := TPsionOOLexer.Create();
    par.LoadFile(filename);

    par.Verbose := params.InSwitch('V', 'L');
    par.Lex();

    InternalModuleList.Add(par.ModuleName);

    if params.InSwitch('V', 'T') then PrintArray(par);

    par.Verbose := params.InSwitch('V', 'P');
    par.Parse();

    if params.InSwitch('V', 'A') then ShowTree(par);
    if params.InSwitch('V', 'R') then Reconstruct(par);

    parsers.Add(par.ModuleName, par);

    WriteLn;

    if length(par.RequireList) > 0 then begin
        WriteLn(par.ModuleName, ' asks for REQUIREd sub-category files:');
        for s in par.RequireList do
        begin
            WriteLn('  ', s);
        end;
        for s in par.RequireList do
        begin
            WalkParsers(s + '.cl');
        end;
        WriteLn();
    end;

    Result := par.ModuleName;
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

    // Get the path for external files (from `-e`)
    // TODO: extra checks?
    if params.SwitchExists('E') then begin
        PathList := CheckPath(params.SwitchVal('E'));
    end else begin
        PathList := CheckPath('.');
    end;

    Try
    begin
        parsers := TParserDictionary.Create();
        DependencyList := TDependencyList.Create();
        InternalModuleList := TStringList.Create();
        ExternalModuleList := TStringList.Create();

        s := WalkParsers(strFilename);
        CatParser := parsers[s];
        // WriteLn('List of parser objects');
        // for s in parsers.Keys do WriteLn(s);
        // WriteLn('List of modules');
        // for s in InternalModuleList do WriteLn(s);

        InternalClassList := TStringList.Create();
        MethodList := TStringList.Create();


        // Get the list of external files from the category class
        // TODO: extra checks?
        if Length(CatParser.ExternalList) > 0 then begin
            for extfile in CatParser.ExternalList do begin
                s := CheckExternalFile(extfile, PathList);
                if s = '' then begin
                    WriteLn('ERROR: External file "', extfile, '" not found in given path');
                    halt;
                end;
                ExternalModuleList.Add(UpCase(extfile));
                WriteLn(extfile, ': ', s);

                LoadDependencies(s);
            end;
        end;
        WriteLn('List of external modules');
        for s in ExternalModuleList do WriteLn(s);

        LoadDependencies(CatParser);

        MethodList.Clear;

        for class_item in CatParser.ClassList do
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
            MakeEXT(CatParser);
        end;
        if params.SwitchExists('G') then begin
            for par in parsers.Values do begin
                MakeG(par);
            end;
        end;
        if params.SwitchExists('L') then begin
            MakeLIS(CatParser);
        end;
        if params.SwitchExists('C') then begin
            MakeC(CatParser);
        end;
        if params.SwitchExists('I') then begin
            for par in parsers.Values do begin
                MakeING(par);
            end;
        end;
        if params.SwitchExists('A') then begin
            MakeASM(CatParser);
        end;

    end
    finally begin
        FreeAndNil(DependencyList);
        FreeAndNil(CatParser); // TODO: Check if needed
        FreeAndNil(parsers);
    end;
end;

end.

