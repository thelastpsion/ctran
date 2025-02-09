{$mode objfpc}{$H+}{$J-}
program ctran;

uses
  sysutils,
  classes,
  PsionOOParser,
  PsionOOCatDiagnostics,
  PsionSDKApp,
  Generics.Collections,
  StringThings;

type
  TMethodsForOutput = record
    Methods: TPsionOOMethodList;
    StartIndex: Integer;
  end;

  TPsionOOCatClass = record
    Parent: String;
    Category: String;
    Methods: TPsionOOMethodList;
    HasProperty: Boolean;
  end;

  TDependencyList = specialize TDictionary<String, TPsionOOCatClass>;

  // TCommentType = (
  //   commentC,
  //   commentASM
  // );

  TParserDictionary = specialize TObjectDictionary<String, TPsionOOParser>;

var
  strFilename: String;
  CatParser: TPsionOOParser;
  params: TPsionSDKAppParams;
  PathList: TStringList; // TODO: Could this be removed?
  DependencyList: TDependencyList;
  InternalClassList: TStringList;
  InternalModuleList: TStringList;
  ExternalModuleList: TStringList;
  parsers: TParserDictionary;

procedure VerText();
begin
  WriteLn('CTRAN-ng Version 0.0.2 (C) Alex Brown');
end;

procedure HelpText();
var
  s: String;
begin
  s := 'Parameters: [<name>] [-e<dir>] [-p[<dir>] -x[<dir>] -g[<dir>] -a[<dir>] -i[<dir>] -l[<dir>] -c[<dir>] -s -k -v]' + LineEnding +
      '<name>     Category source input file' + LineEnding +
      '-e<dir>    Input externals directory (multiple separated by `;`)' + LineEnding +
      '-p<dir>    Set default path for all output files' + LineEnding +
      '-x<dir>    Output .EXT file' + LineEnding +
      '-c<dir>    Output .C code file' + LineEnding +
      '-g<dir>    Output .G include file' + LineEnding +
      '-a<dir>    Output .ASM code file (EXPERIMENTAL)' + LineEnding +
      '-i<dir>    Output .ING code file (EXPERIMENTAL)' + LineEnding +
      '-l<dir>    Output .LIS file' + LineEnding +
      '-s         SDK output' + LineEnding +
      '-k         Output skeleton source files' + LineEnding +
      '-v         Verbose output, using a compination of:' + LineEnding +
      '    l        show lexer debug output' + LineEnding +
      '    t        show table of tokens' + LineEnding +
      '    p        show parser debug output' + LineEnding +
      '    a        show abstract syntax tables' + LineEnding +
      '    c        extra information when constructing new files' + LineEnding +
      '    r        reconstruct all input files from abstract syntax';
  WriteLn(s);
end;

// Checks a provided path string, separated by semicolons.
function CheckPath(s: String): TStringList;
var
  protopaths: TStringArray;
  pathitem: String;
  pathitemexpand: String;
begin
  Result := TStringList.Create();
  protopaths := s.Split(';');

  for pathitem in protopaths do
  begin
    pathitemexpand := ExpandFileName(pathitem);
    if not DirectoryExists(pathitemexpand) then begin
      WriteLn('Path doesn''t exist. (', pathitemexpand, ' from ', pathitem, ')');
      halt(-1);
    end;

    if RightStr(pathitemexpand, 1) <> DirectorySeparator then pathitemexpand += DirectorySeparator;
    Result.Add(pathitemexpand);
  end;
end;

// Returns the output path for a file, based on the "default output path" switch and the provided switch
function GetPathFromParams(sw: String): String;
begin
  if params.SwitchExists('P') and (params.SwitchVal('P') <> '') and (params.SwitchVal(LeftStr(sw, 1)) = '') then begin
    Result := CheckPath(params.SwitchVal('P'))[0];
  end else begin
    Result := CheckPath(params.SwitchVal(LeftStr(sw, 1)))[0];
  end;
end;

// Makes sure that an external category file exists. If so, return the absolute
// path.
function CheckExternalFile(s: String; paths: TStringList): String;
var
  ext: String;
  path: String;
  possiblefile: String;
  extopts: Array of String;
begin
  extopts := ['', '.ext', '.EXT'];
  if paths.Count = 0 then begin
    raise Exception.Create('CheckExternalFile(): Path list is empty.');
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
procedure LoadDependencies(par: TPsionOOParser);
// FIX: Deal with "shadow" classes that only contain DEFERred methods (see SOLIPEG's TASK class as an example)
var
  ext_class: TPsionOOCatClass;
  par_class: TPsionOOClass;
  method: TPsionOOMethodEntry;
  method_list: TStringList;
  category: String;
  element: TPsionOOFileElement;
  required: String;
begin
  category := par.ModuleName;
  method_list := TSTringList.Create();

  for element in par.ElementList do
  begin
    case element.ElementType of
      incClass: begin
        par_class := par.ClassList[element.index];

        if DependencyList.ContainsKey(LowerCase(par_class.Name)) then begin
          WriteLn('ERROR ', ExtractFilename(par.FileLocation), ': Class ', par_class.Name, ' already defined in ', DependencyList[LowerCase(par_class.Name)].Category);
          // TODO: (if possible) add location in file
          halt(-1);
        end;

        if (not DependencyList.ContainsKey(LowerCase(par_class.Parent))) and (par_class.Parent <> '') then begin
          WriteLn('ERROR ', ExtractFilename(par.FileLocation), ': Superclass ', par_class.Parent, ' of ', par_class.Name, ' does not exist');
          // TODO: (if possible) add location in file
          halt(-1);
        end;

        for method in par_class.Methods do
        begin
          case method.MethodType of
            methodDeclare, methodAdd: begin
              if method_list.IndexOf(LowerCase(method.Name)) > -1 then begin
                WriteLn('ERROR ', ExtractFilename(par.FileLocation), ': Method ', method.Name, ' already exists in category');
                halt(-1);
              end;
              // WriteLn('Adding method ', method.Name, ' to big list o'' methods.');
              method_list.Add(LowerCase(method.Name));
            end;
          end;
        end;

        // WriteLn(filename, ' : ', par_class.Name, ' ', par_class.Parent);
        ext_class.Category := category;
        ext_class.Parent := LowerCase(par_class.Parent);
        ext_class.Methods := par_class.Methods;
        ext_class.HasProperty := ((par_class.HasProperty) or (par_class.ClassProperty.Count > 0));

        DependencyList.Add(LowerCase(par_class.Name), ext_class);
        if par.FileType <> ooExternal then InternalClassList.Add(par_class.Name);
      end;
      incRequire: begin
        required := UpCase(par.RequireList[element.index]);
        if not parsers.ContainsKey(required) then begin
          raise Exception.CreateFmt('LoadDependencies: Can''t find %s in dictionary!', [required]);
        end;

        WriteLn('>>> Loading dependencies for ', required);
        LoadDependencies(parsers[required]);
      end;
    end;
  end;
end;

// Calls the main LoadDependencies() procedure after creating a temporary
// parser instance. Only used for loading external category files.
procedure LoadDependencies(filename: String);
var
  par: TPsionOOParser;
  // class_item: TPsionOOClass;
begin
  if filename = '' then begin
    raise Exception.Create('LoadDependencies: filename is empty');
  end;

  try
    begin
      par := TPsionOOParser.Create;
      // WriteLn('Loading ', filename);
      par.LoadFile(filename);

      par.Verbose := params.InSwitch('V', 'L');
      par.Lex();

      par.Verbose := params.InSwitch('V', 'P');
      par.Parse();

      LoadDependencies(par);
      // for class_item in par.ClassList do
      // begin
      //   AllExtClasses.Add(class_item.Name);
      // end;
    end
  finally
    begin
      FreeAndNil(par);
    end;
  end;
end;

// Return all the ancestors of a class that have a HAS_PROPERTY flag.
function GetAncestorsWithProperty(class_item: TPsionOOClass): TStringList;
// TODO: Check if this should be modified for internal classes
var
  ancestor: String;
begin
  // TODO: Check ancestor classes for circular reference (ancestor TStringList?)
  if params.InSwitch('V', 'C') then WriteLn('>>>   Checking class ', class_item.Name, 'ancestors with property. (do I need to check circular refs?)');
  Result := TStringList.Create;
  ancestor := LowerCase(class_item.Parent);
  while ancestor <> '' do
  begin
    if DependencyList[ancestor].HasProperty then Result.Add(ancestor);
    ancestor := DependencyList[ancestor].Parent;
  end;
end;

// Return all the ancestors of a class, "eldest" first
function GetAncestors(class_item: TPsionOOClass): TStringList;
var
  ancestor: String;
  i: Integer;
begin
  Result := TStringList.Create();
  ancestor := LowerCase(class_item.Parent);

  if params.InSwitch('V', 'C') then WriteLn('>>> Checking class ', class_item.Name, ' ancestors (and checking for circular refrerences.)');

  // TODO: Could this be tidied using TStringList.Reverse() and a separate TStringList?
  while ancestor <> '' do
  begin
    if Result.indexof(ancestor) > -1 then
    begin
      writeln('ERROR: circular class dependency (', ancestor, '). current list of classes is:');
      writeln('  ', class_item.name);
      for i := Result.Count - 1 downto 0 do
      begin
        if Result[i] = ancestor then write('> ') else write('  ');
        writeln(Result[i]);
      end;
      writeln('> ', ancestor);
      halt(-1);
    end;

    Result.insert(0, ancestor);
    ancestor := Dependencylist[ancestor].Parent;
  end;
end;

// Builds a list of methods by superimposing all of a class's ancestors'
// methods. This does not include the current class's methods.
function MakeMetaclass(class_item: TPsionOOClass): TStringList;
// TODO: Is it right to ignore REPLACEd methods? Do they matter if they have already been declared?
var
  method: TPsionOOMethodEntry;
  ancestor: String;
  ancestor_list: TStringList;
begin
  if params.InSwitch('V', 'C') then WriteLn('>>> MakeMetaclass(', class_item.Name, ') running...');
  Result := TStringList.Create();
  ancestor := LowerCase(class_item.Parent);
  ancestor_list := GetAncestors(class_item);

  for ancestor in ancestor_list do
  begin
    if params.InSwitch('V', 'C') then WriteLn('>>>   Ancestor being processed: ', ancestor);
    for method in DependencyList[ancestor].Methods do
    begin
      case method.MethodType of
        methodAdd, methodDefer, methodDeclare: begin
          if Result.IndexOf(method.Name) > -1 then
          begin
            // TODO: Look at replacing strFilename
            WriteLn('ERROR ', strFilename, ': Method ', method.Name, ' in class ', ancestor, 'already exists');
            halt(-1);
          end;
          // WriteLn('Adding method ', method.Name);
          Result.Add(method.Name);
        end;
      end;
    end;
  end;
end;

// Return all the first-generation children of a class
function GetChildren(par: TPsionOOParser; parent: String): TStringList;
// TODO: Make the order of children match classic CTRAN's .LIS files
var
  class_item: TPsionOOClass;
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
function BuildMethodNumbers(par: TPsionOOParser): TStringList;
var
  class_item: TPsionOOClass;
  method_id: Integer;
  method_item: TPsionOOMethodEntry;
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
function GetExternalAncestors(par: TPsionOOParser): TStringList;
// TODO: Check ancestor classes for circular reference (ancestor TStringList?)
// (Might not need to do this here if it's checked elsewhere.)
// TODO: Sort methods as per classic CTRAN (the order that they appear in External files)
var
  ancestor: String;
  s: String;
begin
  Result := TStringList.Create();

  for s in InternalClassList do
  begin
    ancestor := DependencyList[s].Parent;
    while ancestor <> '' do
    begin
      if DependencyList[ancestor].Category <> par.ModuleName then
      begin
        if Result.IndexOf(ancestor) = -1 then
        begin
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

function MakeMethodsForOutput(class_item: TPsionOOClass): TMethodsForOutput;
// TODO: Review this!
var
  method: TPsionOOMethodEntry;
  methodAdd_list: TPsionOOMethodList;
  methodReplace_list: TPsionOOMethodList;
  metaclass: TStringList;
  metaclass_method_id: Integer;
  flgFoundFirst: Boolean;
  flgExists: Boolean;
  i: Integer;
  count_possible_nulls: Integer;
  s: String;
begin
  // if assigned(Result.Methods) then FreeAndNil(Result.Methods); // Causes AccessViolation - is it detecting that it's assigned when it's not?
  Result.Methods := TPsionOOMethodList.Create();
  Result.StartIndex := 0;

  methodAdd_list := TPsionOOMethodList.Create();
  methodReplace_list := TPsionOOMethodList.Create();

  for method in class_item.Methods do
  begin
    case method.MethodType of
      methodReplace:  methodReplace_list.Add(method);
      methodAdd:      methodAdd_list.Add(method);
    end;
  end;
  count_possible_nulls := 0;
  flgFoundFirst := false;
  metaclass := MakeMetaclass(class_item);

  // Creates an empty method for null entries
  // TODO: This feels dirty - can I create a const or something similar?
  method.MethodType := methodNull;
  method.Name := '';
  method.ForwardRef := '';

  Result.StartIndex := metaclass.Count;
  if methodReplace_list.Count > 0 then begin
    for metaclass_method_id := 0 to metaclass.Count - 1 do
    begin
      s := metaclass[metaclass_method_id];
      flgExists := false;
      for i := 0 to methodReplace_list.Count - 1 do
      begin
        if s = methodReplace_list[i].Name then begin
          if flgFoundFirst then begin
            // TODO: Is there a better way of doing this?
            while count_possible_nulls > 0 do
            begin
              Result.Methods.Add(method);
              dec(count_possible_nulls);
            end;
          end else begin
            if params.InSwitch('V', 'C') then WriteLn('*** ', class_item.Name, ': Earliest replaced method is ', s, ' (', metaclass_method_id, ')');
            Result.StartIndex := metaclass_method_id;
            flgFoundFirst := true;
          end;
          Result.Methods.Add(methodReplace_list[i]);
          flgExists := true;
          break;
        end;
      end;
      if (not flgExists) and (flgFoundFirst) then begin
        inc(count_possible_nulls);
      end;
    end;
  end;

  if methodAdd_list.Count > 0 then begin
    // TODO: Is there a better way of doing this? (See above)
    while count_possible_nulls > 0 do
    begin
      Result.Methods.Add(method);
      dec(count_possible_nulls);
    end;
    Result.Methods.AddRange(methodAdd_list);
  end;

  FreeAndNil(methodAdd_list);
  FreeAndNil(methodReplace_list);
end;

// Finds all the class method forward references across all internal category
// and sub-cat files. Used to generate the "Class Method Forward References"
// section at the start of .C and .ASM files.
function GetMethodForwardRefs(par: TPsionOOParser): TStringList;
var
  class_item: TPsionOOClass;
  method: TPsionOOMethodEntry;
begin
  Result := TStringList.Create();

  for class_item in par.ClassList do
    for method in class_item.Methods do
      if (method.ForwardRef <> '') and (Result.IndexOf(method.ForwardRef) = -1) then
        Result.Add(method.ForwardRef);
end;

function GetClassFromParsers(class_name: String): TPsionOOClass;
var
  class_item: TPsionOOClass;
begin
  if not DependencyList.ContainsKey(class_name) then begin
    raise Exception.CreateFmt('GetClassFromParsers: Can''t find %s in DependencyList', [class_name]);
  end;

  for class_item in parsers[DependencyList[class_name].Category].ClassList do
  begin
    if class_item.Name = class_name then
    begin
      exit(class_item);
    end;
  end;

  raise Exception.CreateFmt('GetClassFromParsers: Can''t find %s in parser %s', [class_name, DependencyList[class_name].Category]);
end;

// If a class's parent is in an external module, return the ID (order in which
// they are listed in the main category file, starting at 1). If it's in an
// internal one, return 0. Otherwise halt, because there's a bug in CTRAN.
function GetParentModuleID(class_name: String): Integer;
var
  parent: String;
  parent_module: String;
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

  raise Exception.CreateFmt('GetParentModuleID: Couldn''t find parent module %s of parent class %s for class %s - bad logic here? (Should it be %s?)', [parent_module, parent, class_name, InternalModuleList[0]]);
end;


// Get the list of external files from the category class
// TODO: extra checks?
procedure MakeExternalModuleList();
var
  s: String;
  extfile: String;
begin
  if CatParser.ExternalList.Count > 0 then begin
    for extfile in CatParser.ExternalList do begin
      s := CheckExternalFile(extfile, PathList);
      if s = '' then begin
        WriteLn('ERROR: External file "', extfile, '" not found in given path');
        halt(-1);
      end;
      ExternalModuleList.Add(UpCase(extfile));
      if params.SwitchExists('V') then WriteLn(' Reading ', s);

      LoadDependencies(s);
    end;
  end;
end;

procedure CheckMethodInheritance();
var
  class_item: TPsionOOClass;
  method_item: TPsionOOMethodEntry;
  cur_metaclass: TStringList;
begin
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
            WriteLn('ERROR: Can''t replace method ', method_item.Name, ' as it doesn''t already exist');
            halt(-1);
          end;
        end;
        methodDefer, methodAdd: begin
          if cur_metaclass.IndexOf(method_item.Name) > -1 then begin
            WriteLn('ERROR: Method ', method_item.Name, ' already exists');
            halt(-1);
          end;
        end;
        else begin
          WriteLn('Unknown methodtype when trying to check if a method exists in a metaclass.');
          halt(-1);
        end;
      end;
    end;
  end;
end;

//
// MAKE FILES
//

procedure MakeEXT(par: TPsionOOParser);
var
  // element: TPsionOOFileElement; // NOTE: Might need this again for OVAL support
  method: TPsionOOMethodEntry;
  flgHasMethod: boolean;
  filepath: String;
  class_item: TPsionOOClass;
  class_name: String;
  slFile: TStringList;

  function GetFirstKeyword(): String;
  begin
    case par.CategoryType of
      catName:    Result := 'NAME';
      catImage:   Result := 'IMAGE';
      catLibrary: Result := 'LIBRARY';
      else begin
        raise Exception.CreateFmt('MakeEXT: Unknown category type %s', [par.CategoryType]);
      end;
    end;
  end;

begin
  slFile := TStringList.Create();
  slFile.Add('Generated by Ctran from ' + ExtractFileName(par.FileLocation));

  slFile.Add(GetFirstKeyword() + ' ' + LowerCase(par.ModuleName));

  for class_name in InternalClassList do
  begin
    class_item := GetClassFromParsers(class_name);

    flgHasMethod := false;
    if class_item.Parent = '' then
    begin
      slFile.Add('CLASS ' + class_item.Name);
    end else begin
      slFile.Add('CLASS ' + class_item.Name + ' ' + class_item.Parent);
    end;
    slFile.Add('{');
    for method in class_item.Methods do
    begin
      case method.MethodType of
        methodReplace: flgHasMethod := true;
        methodDefer:   slFile.Add('DECLARE ' + method.Name);
        methodAdd: begin
          slFile.Add('DECLARE ' + method.Name);
          flgHasMethod := true;
        end;
        methodDeclare: begin
          // If we hit this, something's gone wrong in the parser
          raise Exception.Create('MakeEXT: Can''t have a DECLARE in a Category file... What''s happened?');
        end;
        else begin
          // The parser has created a token that we don't understand
          raise Exception.Create('MakeEXT: Unknown token in a Category file... What''s happened?');
        end;
      end;
    end;
    if flgHasMethod then begin
      slFile.Add('HAS_METHOD');
    end;
    if class_item.ClassProperty.Count > 0 then begin
      slFile.Add('HAS_PROPERTY');
    end;
    slFile.Add('}');
  end;

  filepath := GetPathFromParams('X');
  slFile.LineBreak := #13#10;
  try
    slFile.SaveToFile(filepath + par.ModuleName + '.EXT');
  except
    on E: EFCreateError do begin
      WriteLn('ERROR: Unable to create ', par.ModuleName, '.EXT at ', filepath);
      WriteLn('[', E.ClassName, '] ', E.Message);
      halt(-1);
    end;
  end;
  FreeAndNil(slFile);
end;

procedure MakeG(par: TPsionOOParser);
var
  inc_file: String;
  inc_file_def: String;
  i: Integer;
  class_item: TPsionOOClass;
  constant_item: TPsionOOConstantEntry;
  s: String;
  sl: TStringList;
  slFile: TStringList;
  filepath: String;
  method_list: TStringList;
  flgNotSDK: Boolean;
begin
  flgNotSDK := ((not params.SwitchExists('S')) and (par.FileType = ooCategory));

  slFile := TStringList.Create();
  slFile.Add('/* Generated by Ctran from ' + ExtractFilename(par.FileLocation) + ' */');

  slFile.Add('#define ' + UpCase(par.ModuleName) + '_G');

  for inc_file in par.IncludeList do
  begin
    inc_file_def := UpCase(StringReplace(inc_file, '.', '_', [rfReplaceAll]));
    slFile.Add('#ifndef %s', [inc_file_def]);
    slFile.Add('#include <%s>', [inc_file]);
    slFile.Add('#endif');
  end;

  if par.FileType = ooCategory then
  begin
    slFile.Add('/* Category Numbers */');
    if flgNotSDK then begin
      slFile.Add('#ifndef EPOC');
      slFile.Add('GLREF_D P_CLASS *ct_' + LowerCase(par.ModuleName) + '[];');
      slFile.AddStringsFormat(par.ExternalList, 'GLREF_D P_CLASS *ct_%s[];');
      slFile.Add('#endif /* EPOC */');
      slFile.Add('#ifdef EPOC');
    end;
    slFile.Add('#define CAT_%s_%s 0', [par.ModuleName, par.ModuleName]);
    for i := 0 to par.ExternalList.Count - 1 do
    begin
      slFile.Add('#define CAT_%s_%s %d', [par.ModuleName, UpCase(par.ExternalList[i]),  i + 1]);
    end;
    if flgNotSDK then begin
      slFile.Add('#else');
      slFile.Add('#define CAT_%s_%s (&ct_%s[0])', [par.ModuleName, par.ModuleName, LowerCase(par.ModuleName)]);
      for s in par.ExternalList do
      begin
        slFile.Add('#define CAT_%s_%s (&ct_%s[0])', [par.ModuleName, UpCase(s), LowerCase(s)]);
      end;
      slFile.Add('#endif');
    end;
  end;

  if length(par.ClassList) > 0 then begin 
    slFile.Add('/* Class Numbers */');
    for class_item in par.ClassList do
    begin
      if InternalClassList.IndexOf(class_item.Name) < 0 then begin
        raise Exception.CreateFmt('MakeG: Can''t find class %s in InternalClassList', [class_item.Name]);
      end;
      slFile.Add('#define C_%s %d', [UpCase(class_item.Name), InternalClassList.IndexOf(class_item.Name)]);
    end;

    method_list := BuildMethodNumbers(par);
    if method_list.Count > 0 then begin
      slFile.Add('/* Method Numbers */');
      slFile.AddStringsFormat(method_list, '#define O_%s');
    end;

    for class_item in par.ClassList do
    begin
      slFile.Add('');
      slFile.Add('');
      slFile.Add('/* Class ' + class_item.Name + ' */');
      slFile.Add('/* ------------------------------------------------------ */');

      if class_item.ClassConstants.Count > 0 then begin
        slFile.Add('/* Constants for ' + class_item.Name + ' */');
        for constant_item in class_item.ClassConstants do
        begin
          slFile.Add('#define %s %s', [constant_item.Name, constant_item.Value]);
        end;
      end;

      if class_item.ClassTypes.Count > 0 then begin
        slFile.Add('/* Types for ' + class_item.Name + ' */');
        slFile.AddStrings(class_item.ClassTypes);
      end;

      slFile.Add('/* Property of ' + class_item.Name + ' */');
      if class_item.ClassProperty.Count > 0 then begin
        slFile.Add('typedef struct {');
        slFile.AddStrings(class_item.ClassProperty);
        slFile.Add('} PRS_' + UpCase(class_item.Name) + ';');
      end;

      slFile.Add('typedef struct pr_' + class_item.Name);
      slFile.Add('{');

      sl := GetAncestorsWithProperty(class_item).Reverse;
      for s in sl do
      begin
        slFile.Add('PRS_%s %s;', [UpCase(s), s]);
      end;
      FreeAndNil(sl);

      if class_item.ClassProperty.Count > 0 then begin
        slFile.Add('PRS_%s %s;', [UpCase(class_item.Name), class_item.Name]);
      end;
      slFile.Add('} PR_' + UpCase(class_item.Name) + ';');
    end;
  end;

  filepath := GetPathFromParams('G');
  slFile.LineBreak := #13#10;
  try
    slFile.SaveToFile(filepath + par.ModuleName + '.G');
  except
    on E: EFCreateError do begin
      WriteLn('ERROR: Unable to create ', par.ModuleName, '.G at ', filepath);
      WriteLn('[', E.ClassName, '] ', E.Message);
      halt(-1);
    end;
  end;
  FreeAndNil(slFile);
end;

procedure MakeAllG();
var
  par: TPsionOOParser;
begin
  for par in parsers.Values do begin
    MakeG(par);
  end;
end;

procedure MakeC(par: TPsionOOParser);
var
  s: String;
  sl: TStringList;
  slFile: TStringList;
  class_name: String;
  class_item: TPsionOOClass;
  filepath: String;
  method: TPsionOOMethodEntry;
  ForwardRefs: TStringList;
  total_methods: Integer;
  c_methods: TMethodsForOutput;
  parent_extcat_id: Integer;
  flgNotSDK: Boolean;
begin
  flgNotSDK := (not params.SwitchExists('S'));

  slFile := TStringList.Create();
  slFile.Add('/* Generated by Ctran from ' + ExtractFilename(par.FileLocation) + ' */');

  for s in InternalModuleList do
  begin
    slFile.Add('#include <' + LowerCase(s) + '.g>');
  end;

  slFile.Add('/* External Superclass References */');

  if flgNotSDK then slFile.Add('#ifdef EPOC');

  sl := GetExternalAncestors(par);
  for s in sl do
  begin
    slFile.Add('#define ERC_%s C_%s', [UpCase(s), UpCase(s)]);
  end;
  if flgNotSDK then begin
    slFile.Add('#else');

    for s in sl do
    begin
      slFile.Add('GLREF_D P_CLASS c_%s;', [s]);
      slFile.Add('#define ERC_%s &c_%s', [UpCase(s), s]);
    end;

    slFile.Add('#endif');
  end;
  FreeAndNil(sl);

  // Method function forward references
  ForwardRefs := GetMethodForwardRefs(par);
  if ForwardRefs.Count > 0 then begin
    slFile.Add('/* Method function forward references */');
    // for s in ForwardRefs do slFile.Add('GLREF_C VOID %s();', [s]);
    slFile.AddStringsFormat(ForwardRefs, 'GLREF_C VOID %s();');
  end;
  FreeAndNil(ForwardRefs);

  // Generate Classes and Methods
  for class_name in InternalClassList do
  begin
    class_item := GetClassFromParsers(class_name);
    parent_extcat_id := GetParentModuleID(class_item.Name);

    slFile.Add('');
    slFile.Add('');
    slFile.Add('/* Class ' + class_item.Name + ' */');
    slFile.Add('/* ------------------------------------------------------ */');

    c_methods := MakeMethodsForOutput(class_item);
    for method in c_methods.Methods do begin
      if (method.Name <> '') and (method.ForwardRef = '') then begin
        slFile.Add('GLREF_C VOID %s_%s();', [class_item.Name, method.Name]);
      end;
    end;

    slFile.Add('GLDEF_D struct');
    slFile.Add('{');
    slFile.Add('P_CLASS c;');

    total_methods := c_methods.Methods.Count;
    if total_methods > 0 then begin
      slFile.Add('VOID (*v[%d])();', [total_methods]);
    end;

    slFile.Add('} c_' + class_item.Name + ' =');
    slFile.Add('{');

    // Define the class
    s := format('{%d,(P_CLASS *)', [parent_extcat_id]);

    if parent_extcat_id = 0 then
    begin // It's internal
      s += '&c_' + class_item.Parent;
    end else begin
      s += 'ERC_' + UpCase(class_item.Parent);
    end;

    s += ',sizeof(PR_' + UpCase(class_item.Name) + '),';
    if c_methods.Methods.Count = 0 then
    begin
      s += '0';
    end else begin
      s += IntToStr(c_methods.StartIndex);
    end;
    s += format(',0x6b,%d,%d},', [c_methods.Methods.Count, class_item.PropertyAutodestroyCount]);
    slFile.Add(s);

    // Define the methods

    sl := TStringList.Create();
    for method in c_methods.Methods do
    begin
      if (method.MethodType = methodNull) then begin
        sl.Add('NULL');
      end else if method.ForwardRef = '' then begin
        sl.Add(class_item.Name + '_' + method.Name);
      end else begin
        sl.Add(method.ForwardRef);
      end;
    end;

    if sl.Count > 0 then
    begin
      slFile.Add('{');
      // slFile.AddStrings(sl.FormatAll('%s,', '%s'));
      slFile.AddStringsFormat(sl, '%s,', '%s');
      slFile.Add('}');
    end;
    FreeAndNil(sl);

    slFile.Add('};');
  end;

  slFile.Add('/* Class Lookup Table */');
  if flgNotSDK then slFile.Add('#ifdef EPOC');
  slFile.Add('GLDEF_D P_CLASS *ClassTable[]=');
  if flgNotSDK then begin
    slFile.Add('#else');
    slFile.Add('GLDEF_D P_CLASS *ct_' + LowerCase(par.ModuleName) + '[]=');
    slFile.Add('#endif');
  end;

  slFile.Add('{');
  // slFile.AddStrings(InternalClassList.FormatAll('(P_CLASS *)&c_%s,', '(P_CLASS *)&c_%s'));
  slFile.AddStringsFormat(InternalClassList, '(P_CLASS *)&c_%s,', '(P_CLASS *)&c_%s');
  slFile.Add('};');

  if par.ExternalList.Count > 0 then begin
    slFile.Add('/* External Category Name Table */');
    if flgNotSDK then slFile.Add('#ifdef EPOC');
    slFile.Add('GLDEF_D struct');
    slFile.Add('    {');
    slFile.Add('    UWORD number;');
    slFile.Add('    UBYTE names[%d][14];', [par.ExternalList.Count]);
    slFile.Add('    } ExtCatTable =');
    slFile.Add('    {%d,', [par.ExternalList.Count]);
    slFile.Add('    {');

    sl := TStringList.Create();
    for s in par.ExternalList do
    begin
      sl.Add('    {''%s''%s}', [DelimitStr(s + '.DYL', ''','''), RepeatStr(',0', 10 - length(s))]);
    end;
    // slFile.AddStrings(sl.FormatAll('%s,', '%s'));
    slFile.AddStringsFormat(sl, '%s,', '%s');
    FreeAndNil(sl);

    slFile.Add('    }');
    slFile.Add('    };');
    if flgNotSDK then slFile.Add('#endif');
  end;

  filepath := GetPathFromParams('C');
  slFile.LineBreak := #13#10;
  try
    slFile.SaveToFile(filepath + par.ModuleName + '.C');
  except
    on E: EFCreateError do begin
      WriteLn('ERROR: Unable to create ', par.ModuleName, '.C at ', filepath);
      WriteLn('[', E.ClassName, '] ', E.Message);
      halt(-1);
    end;
  end;
  FreeAndNil(slFile);
end;

procedure MakeLIS(par: TPsionOOParser);
var
  slFile: TStringList;
  sl: TStringList;
  filepath: String;
  class_item: TPsionOOClass;
  module_name: String;
  cat_type: String;
begin
  slFile := TStringList.Create();

  slFile.Add('Generated by Ctran from ' + ExtractFilename(par.FileLocation));
  slFIle.Add('');

  case par.CategoryType of
    catImage:   cat_type := 'IMAGE ';
    catLibrary: cat_type := 'LIBRARY ';
    catName:    cat_type := 'NAME ';
    else begin
      raise Exception.CreateFmt('MakeLIS: Unknown category type %s', [par.CategoryType]);
    end;
  end;

  slFile.Add(cat_type + LowerCase(par.ModuleName));

  for module_name in InternalModuleList do
  begin
    slFile.Add('');
    slFile.Add('********** ' + LowerCase(module_name) + ' **********');

    for class_item in parsers[module_name].ClassList do
    begin
      slFile.Add(class_item.Name);
      sl := GetAncestors(class_item);
      if sl.Count > 0 then begin
        slFile.Add('        Derived from ' + sl.Reverse.CommaText);
      end;
      FreeAndNil(sl);

      sl := GetChildren(par, class_item.Name);
      if sl.Count > 0 then begin
        slFile.Add('        Subclassed by ' + sl.CommaText);
      end;
      FreeAndNil(sl);
    end;
  end;

  filepath := GetPathFromParams('L');
  slFile.LineBreak := #13#10;
  try
    slFile.SaveToFile(filepath + par.ModuleName + '.LIS');
  except
    on E: EFCreateError do begin
      WriteLn('ERROR: Unable to create ', par.ModuleName, '.LIS at ', filepath);
      WriteLn('[', E.ClassName, '] ', E.Message);
      halt(-1);
    end;
  end;
  FreeAndNil(slFile);
end;

procedure MakeASM(par: TPsionOOParser);
var
  i: Integer;
  s: String;
  sl: TStringList;
  slFile: TStringList;
  class_item: TPsionOOClass;
  filepath: String;
  method: TPsionOOMethodEntry;
  c_methods: TMethodsForOutput;
  method_id: Integer;
  class_name: String;
  parent_extcat_id: Integer;
begin
  slFile := TStringList.Create();
  slFile.Add('; Generated by Ctran from ' + ExtractFilename(par.FileLocation));

  for s in InternalModuleList do
  begin
    slFile.Add(' include ..\inc\' + LowerCase(s) + '.ing');
  end;

  slFile.Add('; External Superclass References');
  sl := GetExternalAncestors(par);
  for s in sl do
  begin
    slFile.Add('ERC_%s equ C_%s', [UpCase(s), UpCase(s)]);
  end;
  FreeAndNil(sl);

  sl := GetMethodForwardRefs(par);
  if sl.Count > 0 then begin
    slFile.Add('; Method function forward references');
    slFile.Add(' _TEXT segment byte public ''CODE''');
    slFile.Add(' ASSUME CS:_TEXT');
    // slFile.AddStrings(sl.FormatAll('GLREF_C %s'));
    slFile.AddStringsFormat(sl, 'GLREF_C %s');
    slFile.Add(' _TEXT ends');
  end;
  FreeAndNil(sl);

  for class_name in InternalClassList do
  begin
    class_item := GetClassFromParsers(class_name);
    parent_extcat_id := GetParentModuleID(class_item.Name);

    slFile.Add('');
    slFile.Add('');
    slFile.Add('; Class ' + class_item.Name);
    slFile.Add('; ------------------------------------------------------');

    c_methods := MakeMethodsForOutput(class_item);

    slFile.Add(' _TEXT segment byte public ''CODE''');
    slFile.Add(' ASSUME CS:_TEXT');
    for method in c_methods.Methods do begin
      if (method.Name <> '') and (method.ForwardRef = '') then begin
        slFile.Add('GLREF_C %s_%s', [class_item.Name, method.Name]);
      end;
    end;
    slFile.Add(' _TEXT ends');

    slFile.Add(' _TEXT segment byte public ''CODE''');
    slFile.Add('GLDEF_C c_' + class_item.Name);
    slFile.Add(' dw   %d', [parent_extcat_id]);

    if parent_extcat_id = 0 then
    begin
      slFile.Add(' dw   c_' + class_item.Parent);
    end else begin
      slFIle.Add(' dw   ERC_' + UpCase(class_item.Parent));
    end;

    slFile.Add(' dw   (SIZE PR_%s)', [UpCase(class_item.Name)]);

    if c_methods.Methods.Count = 0 then
    begin
      slFile.Add(' db   0');
    end else begin
      slFile.Add(' db   %d', [c_methods.StartIndex]);
    end;

    slFile.Add(' db   06bh');
    slFile.Add(' db   %d', [c_methods.Methods.Count]);
    slFile.Add(' db   %d', [class_item.PropertyAutodestroyCount]);

    method_id := -1; // INFO: I don't know why this works, but it matches the output from classic CTRAN
    for method in c_methods.Methods do
    begin
      if (method.Name <> '') then begin
        for i := 1 to method_id do
        begin
          slFile.Add(' dw   0');
        end;
        if method.ForwardRef = '' then begin
          slFile.Add(' dw   %s_%s', [class_item.Name, method.Name]);
        end else begin
          slFile.Add(' dw   ' + method.ForwardRef);
        end;
      end;
      inc(method_id);
    end;

    slFile.Add(' EEND');
    slFile.Add(' _TEXT ends');
  end;

  slFile.Add('; Class Lookup Table');
  slFile.Add(' _TEXT segment byte public ''CODE''');
  slFile.Add('GLDEF_C ClassTable');
  // slFile.AddStrings(InternalClassList.FormatAll(' dw   c_%s'));
  slFile.AddStringsFormat(InternalClassList, ' dw   c_%s');
  slFile.Add(' EEND');

  slFile.Add(' _TEXT ends');

  if par.ExternalList.Count > 0 then begin
    slFile.Add('; External Category Name Table');
    slFile.Add(' _TEXT segment byte public ''CODE''');
    slFile.Add('GLDEF_C ExtCatTable');

    slFile.Add(' dw   %d', [par.ExternalList.Count]);
    for s in par.ExternalList do
    begin
      slFile.Add(' db "' + s + '.DYL"' + RepeatStr(',0', 10 - length(s)));
    end;

    slFile.Add(' EEND');
    slFile.Add(' _TEXT ends');
  end;

  slFile.Add(' end');

  filepath := GetPathFromParams('A');
  slFile.LineBreak := #13#10;
  try
    slFile.SaveToFile(filepath + par.ModuleName + '.ASM');
  except
    on E: EFCreateError do begin
      WriteLn('ERROR: Unable to create ', par.ModuleName, '.ASM at ', filepath);
      WriteLn('[', E.ClassName, '] ', E.Message);
      halt(-1);
    end;
  end;
  FreeAndNil(slFile);
end;

function CLineToTasm(cstr: String; prefix: String): String;
var
  c_type: String;
  asm_type: String;
  ident: String;
  space_pos: Integer;
  asm_type_size: String;
  bracket_pos: Integer;
begin
  space_pos := pos(' ', cstr);
  if space_pos < 1 then begin
    WriteLn('ERROR: No space found in this C line:');
    WriteLn(cstr);
    halt(-1);
  end;

  c_type := copy(cstr, 1, space_pos - 1);
  ident := trim(copy(cstr, space_pos + 1));
  ident := copy(ident, 1, length(ident) - 1);

  if ident[1] = '*' then ident := copy(ident, 2);

  Result := '';
  bracket_pos := pos('[', ident);
  // TODO: Check that TEXT types are definitely only ever single byte sized
  if bracket_pos > 0 then begin
    asm_type_size := copy(ident, bracket_pos + 1, pos(']', ident) - bracket_pos - 1);
    ident := copy(ident, 1, bracket_pos - 1);
    case c_type of
      'BYTE', 'UBYTE', 'char', 'TEXT': asm_type := '1';
      'WORD', 'UWORD', 'HANDLE', 'INT', 'UINT', 'int': asm_type := '2';
      'LONG', 'ULONG': asm_type := '4';
      else asm_type := 'size ' + c_type;
    end;
    asm_type := 'db (' + asm_type + ') * ' + asm_type_size + ' dup (?)';
  end else begin
    case c_type of
      'BYTE', 'UBYTE', 'char', 'TEXT': asm_type := ' db ?';
      'WORD', 'UWORD', 'HANDLE', 'INT', 'UINT', 'int': asm_type := ' dw ?';
      'LONG', 'ULONG': asm_type := ' dd ?';
      'DOUBLE': asm_type := ' db 8 dup (?)';
    end;
    if asm_type = '' then begin
      asm_type := c_type + ' <>';
    end;
  end;

  Result := prefix + UpCase(ident[1]) + copy(ident, 2) + ' ' + asm_type;
end;

function CTypesToTasm(class_item: TPsionOOClass): TStringList;
var
  cur_line: Integer;
  cur_typedef_line: Integer;
  typedef_start_line: Integer;
  typedef_end_line: Integer;
  struct_name: String;
begin
  Result := TStringList.Create();
  cur_line := 0;

  while cur_line < class_item.ClassTypes.Count - 1 do
  begin
    // if length(class_item.ClassTypes) - cur_line < 4 then begin
    //     break;
    // end;
    if copy(class_item.ClassTypes[cur_line],1, 14) <> 'typedef struct' then begin
      inc(cur_line);
      continue;
    end;
    if class_item.ClassTypes[cur_line + 1] <> '{' then begin
      WriteLn('ERROR ', class_item.Name, ': struct found, but no opening brace follows');
      WriteLn(class_item.ClassTypes[cur_line]);
      halt(-1);
    end;

    typedef_start_line := cur_line + 2;
    struct_name := '';
    for cur_typedef_line := typedef_start_line to class_item.ClassTypes.Count - 1 do
    begin
      if class_item.ClassTypes[cur_typedef_line][1] = '}' then begin
        struct_name := copy(class_item.ClassTypes[cur_typedef_line], 3);
        struct_name := copy(struct_name, 1, length(struct_name) - 1);
        typedef_end_line := cur_typedef_line - 1;
        break;
      end;
    end;
    if struct_name = '' then begin
      WriteLn('ERROR ', class_item.Name, ': can''t find a struct name');
      halt(-1);
    end;

    Result.Add(struct_name + ' struc');
    for cur_typedef_line := typedef_start_line to typedef_end_line do
    begin
      Result.Add(CLineToTasm(class_item.ClassTypes[cur_typedef_line], struct_name));
    end;
    Result.Add(struct_name + ' ends');
    cur_line := typedef_end_line + 2;
  end;
end;

procedure MakeING(par: TPsionOOParser);
var
  inc_file: String;
  inc_stem: String;
  i: Integer;
  class_item: TPsionOOClass;
  constant_item: TPsionOOConstantEntry;
  s: String;
  sl: TStringList;
  slFile: TStringList;
  filepath: String;
  method_list: TStringList;
  inc_ext: String;
begin
  slFile := TStringList.Create();

  slFile.Add('; Generated by Ctran from ' + ExtractFileName(par.FileLocation));
  slFile.Add(UpCase(par.ModuleName) + '_ING equ 1');

  for inc_file in par.IncludeList do
  begin
    // inc_file_def := UpCase(StringReplace(inc_file, '.', '_', [rfReplaceAll]));
    inc_stem := UpCase(ExtractFileStem(inc_file));
    if UpCase(ExtractFileExt(inc_file)) = '.H' then inc_ext := 'INC' else inc_ext := 'ING';

    slFile.Add('IFNDEF %s_%s', [inc_stem, inc_ext]);
    slFile.Add(' INCLUDE ..\inc\%s.%s', [inc_stem, inc_ext]);
    slFile.Add('ENDIF');
  end;

  if ((par.FileType = ooCategory) and (par.ExternalList.Count > 1)) or (par.ExternalList.Count > 0) then begin
    slFile.Add('; Category Numbers');
    if par.FileType = ooCategory then begin
      slFile.Add('CAT_%s_%s equ 0', [par.ModuleName, par.ModuleName]);
    end;
    for i := 0 to par.ExternalList.Count - 1 do
    begin;
      slFile.Add('CAT_%s_%s equ %d', [par.ModuleName, UpCase(par.ExternalList[i]), i + 1]);
    end;
  end;

  if length(par.ClassList) > 0 then begin 
    slFile.Add('; Class Numbers');
    // NOTE: This generates different numbers to CTRAN 5.02, but I think it's correct as it matches MakeG()
    for class_item in par.ClassList do
    begin
      if InternalClassList.IndexOf(class_item.Name) < 0 then begin
        raise Exception.CreateFmt('MakeING: Can''t find class %s in InternalClassList', [class_item.Name]);
      end;
      slFile.Add('C_%s equ %d', [UpCase(class_item.Name), InternalClassList.IndexOf(class_item.Name)]);
    end;

    method_list := BuildMethodNumbers(par);
    if method_list.Count > 0 then begin
      slFile.Add('; Method Numbers');
      for s in method_list do
      begin
        slFile.Add('O_%s equ %s', [s.Split(' ')[0], s.Split(' ')[1]]);
      end;
    end;

    for class_item in par.ClassList do
    begin
      slFile.Add('');
      slFile.Add('');
      if class_item.ClassConstants.Count > 0 then begin
        slFile.Add('; Constants for ' + class_item.Name);
        for constant_item in class_item.ClassConstants do
        begin
          case copy(constant_item.Value, 1, 2) of
            '0x': s := '0' + copy(constant_item.Value, 3) + 'h';
            else s := constant_item.Value;
          end;
          slFile.Add('%s equ (%s)', [constant_item.Name, s]);
        end;
      end;

      if class_item.ClassTypes.Count > 0 then begin
        slFile.Add('');
        slFile.Add('; Types for ' + class_item.Name);
        slFile.AddStrings(CTypesToTasm(class_item));
      end;

      if class_item.ClassProperty.Count > 0 then begin
        slFile.Add('');
        slFile.Add('; Property of ' + class_item.Name);
        slFile.Add( 'PRS_' + UpCase(class_item.Name) + ' struc');
        // FIX: There's some extra magic going on with detection of types in original CTRAN that I don't understand (see DEMO.CAT - PR_VAFLAT)
        for s in class_item.ClassProperty do
        begin
          slFile.Add(CLineToTasm(s, ProperCase(class_item.Name)));
        end;
        slFile.Add('PRS_' + UpCase(class_item.Name) + ' ends');
      end;

      slFile.Add('PR_' + UpCase(class_item.Name) + ' struc');

      sl := GetAncestorsWithProperty(class_item).Reverse;
      for s in sl do
      begin
        slFile.Add('%s%s PRS_%s <>', [ProperCase(class_item.Name), ProperCase(s), UpCase(s)]);
      end;
      FreeAndNil(sl);

      if class_item.ClassProperty.Count > 0 then begin
        slFile.Add('%s%s PRS_%s <>', [ProperCase(class_item.Name), ProperCase(class_item.Name), UpCase(class_item.Name)]);
      end;
      slFile.Add('PR_' + UpCase(class_item.Name) + ' ends');
    end;
  end;

  filepath := GetPathFromParams('I');
  slFile.LineBreak := #13#10;
  try
    slFile.SaveToFile(filepath + par.ModuleName + '.ING');
  except
    on E: EFCreateError do begin
      WriteLn('ERROR: Unable to create ', par.ModuleName, '.ING at ', filepath);
      WriteLn('[', E.ClassName, '] ', E.Message);
      halt(-1);
    end;
  end;
  FreeAndNil(slFile);
end;

procedure MakeAllING();
var
  par: TPsionOOParser;
begin
  for par in parsers.Values do begin
    MakeING(par);
  end;
end;

function MakeSkeletonFile(class_item: TPsionOOClass): TStringList;
var
  module_name: String;
  c_methods: TMethodsForOutput;
  method: TPsionOOMethodEntry;
begin
  if params.InSwitch('V', 'C') then WriteLn('>>> Making Skeleton File for class ', class_item.Name);
  Result := TStringList.Create();
  module_name := DependencyList[class_item.Name].Category;

  c_methods := MakeMethodsForOutput(class_item);
  if params.InSwitch('V', 'C') then WriteLn(format('>>>   length(c_methods) = %d', [c_methods.Methods.Count]));

  Result.Add('/*');
  Result.Add(LowerCase(class_item.Name) + '.c');
  Result.Add('Generated by Ctran from ' + ExtractFilename(parsers[module_name].FileLocation));
  Result.Add('*/');

  Result.Add('');
  Result.Add('#include <' + LowerCase(module_name) + '.g>');
  Result.Add('');
  Result.Add('#pragma METHOD_CALL');
  Result.Add('');

  // TODO: Check that this creates all the correct methods
  for method in c_methods.Methods do begin
    if (method.Name <> '') and (method.ForwardRef = '') then begin
      Result.Add('METHOD VOID %s_%s(PR_%s *self)', [class_item.Name, method.Name, UpCase(class_item.Name)]);
      Result.Add('    {');
      Result.Add('    }');
      Result.Add('');
    end;
  end;
end;

// Generates skeleton C files, one per class.
procedure MakeAllSkeletonFiles(par: TPsionOOParser);
// TODO: Filenames must be 8.3
// TODO: Find out what classic CTRAN does to files where the first 8 characters of a class are the same as another
var
  slFile: TStringList;
  class_name: String;
  class_item: TPsionOOClass;
  filepath: String;
begin
  filepath := GetPathFromParams('K');

  // Generate Class Files
  for class_name in InternalClassList do
  begin
    class_item := GetClassFromParsers(class_name);

    slFile := MakeSkeletonFile(class_item);
    if slFile.Count > 0 then begin
      WriteLn(filepath, class_name, '.c');
      slFile.LineBreak := #13#10;
      try
        slFile.SaveToFile(filepath + class_name + '.c');
      except
        on E: EFCreateError do begin
          WriteLn('ERROR: Unable to create skeleton file ', filepath, class_name, '.c');
          WriteLn('[', E.ClassName, '] ', E.Message);
          halt(-1);
        end;
      end;
    end;

    FreeAndNil(slFile);
  end;
end;

// Opens all possible internal category (and sub-category) files
// It creates a new parser, opening `filename`. Once that is done, it looks at the file's
// REQUIREd sub-category files and parses them, adding each parser as a new parser object.
//
// Each internal file will now exist as a parser object in the TObjectDictionary `parsers`.
function WalkParsers(filename: String): String;
var
  par: TPsionOOParser;
  // fullpath: String;
  s: String;
begin
  // TODO: Check file exists by walking through PathList
  if params.SwitchExists('V') then WriteLn(' Reading ', filename);

  par := TPsionOOParser.Create();
  par.LoadFile(filename);

  par.Verbose := params.InSwitch('V', 'L');
  par.Lex();

  InternalModuleList.Add(par.ModuleName);

  if params.InSwitch('V', 'T') then PrintArray(par);
  if params.InSwitch('V', 'K') then par.PrintTokenisedLines();

  par.Verbose := params.InSwitch('V', 'P');
  par.Parse();

  if params.InSwitch('V', 'A') then ShowTree(par);
  if params.InSwitch('V', 'R') then Reconstruct(par);

  parsers.Add(par.ModuleName, par);

  // TODO: Why are the two `for` loops below separate?
  if par.RequireList.Count > 0 then begin
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

// TODO: Add external classes (this currently only does internal ones)
function GetCountClasses(): Integer;
var
  par: TPsionOOParser;
begin
  Result := 0;
  for par in parsers.Values do
  begin
    Result := Result + Length(par.ClassList);
  end;
end;

//
// MAIN
//

begin
  params := TPsionSDKAppParams.Create;
  params.Grab;

  if params.SwitchExists('V') or (length(params.Filename) = 0) then VerText();

  WriteLn('WARNING: This is alpha quality software.');
  WriteLn('         It has had limited testing.');
  WriteLn('         Use it at your own risk.');
  WriteLn;

  if length(params.Filename) = 0 then begin
    HelpText();
    exit;
  end;

  strFilename := params.Filename;
  case ExtractFileExt(strFilename) of
    '.', '': strFilename += '.cat';
  end;

  if not(FileExists(strFilename)) then begin
    WriteLn('ERROR ', ExpandFileName(strFilename), ' 0: Failed to open ', ExpandFileName(strFilename));
    WriteLn('File does not exist');
    halt(-1);
  end;

  if params.SwitchExists('V') then WriteLn('Translating ', Upcase(ExtractFileStem(strFilename)));

  // Get the path for external files (from `-e`)
  if params.SwitchExists('E') then begin
    PathList := CheckPath(params.SwitchVal('E'));
  end else begin
    PathList := CheckPath('');
  end;

  Try
  begin
    parsers := TParserDictionary.Create();
    DependencyList := TDependencyList.Create();
    InternalModuleList := TStringList.Create();
    ExternalModuleList := TStringList.Create();

    CatParser := parsers[WalkParsers(strFilename)];
    // WriteLn('List of parser objects');
    // for s in parsers.Keys do WriteLn(s);
    // WriteLn('List of modules');
    // for s in InternalModuleList do WriteLn(s);

    InternalClassList := TStringList.Create();

    MakeExternalModuleList();

    LoadDependencies(CatParser);

    CheckMethodInheritance();

    // WriteLn('List of All Classes Across All External Files:');
    // for s in AllExtClasses do begin
    //     WriteLn('  ', s);
    // end;

    if params.SwitchExists('X') then begin
      MakeEXT(CatParser);
    end;
    if params.SwitchExists('G') then begin
      MakeAllG();
    end;
    if params.SwitchExists('L') then begin
      MakeLIS(CatParser);
    end;
    if params.SwitchExists('C') then begin
      MakeC(CatParser);
    end;
    if params.SwitchExists('I') or params.SwitchExists('A') then begin
      WriteLn;
      WriteLn('*** WARNING! TASM FILE GENERATION IS EXPERIMENTAL! ***');
      WriteLn('CTRAN-ng replicates the way that Psion''s CTRAN.EXE generates');
      WriteLn('Borland Turbo Assember (TASM) source. Conversion of the');
      WriteLn('variables and typedefs in PROPERTY and TYPES sections is very');
      WriteLn('rudimentary and barely does any error checking. Is is very');
      WriteLn('likely that generated assembly code will not work. It is');
      WriteLn('recommended that you check any generated .ASM and .ING files');
      WriteLn('before using them.');
      WriteLn;
    end;
    if params.SwitchExists('I') then begin
      MakeAllING();
    end;
    if params.SwitchExists('A') then begin
      MakeASM(CatParser);
    end;
    if params.SwitchExists('K') then begin
      MakeAllSkeletonFiles(CatParser);
    end;

    if params.SwitchExists('V') then begin
      WriteLn(Format('Classes (internal): %d', [GetCountClasses()]));
    end;

  end
  finally
  begin
    FreeAndNil(DependencyList);
    FreeAndNil(CatParser); // TODO: Check if needed
    FreeAndNil(parsers);
  end;
end;

end.

