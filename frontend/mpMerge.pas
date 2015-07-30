unit mpMerge;

interface

uses
  Windows, SysUtils, Classes, ShellAPI,
  mpBase, mpLogger, mpTracker,
  wbBSA,
  wbHelpers,
  wbInterface,
  wbImplementation,
  wbDefinitionsFNV,
  wbDefinitionsFO3,
  wbDefinitionsTES3,
  wbDefinitionsTES4,
  wbDefinitionsTES5;

  procedure BuildMerge(var merge: TMerge);
  procedure DeleteOldMergeFiles(var merge: TMerge);
  procedure RebuildMerge(var merge: TMerge);
  procedure AddCopyOperation(src, dst: string);

implementation

var
  pexPath, pscPath, compiledPath: string;
  mergeFormIndex: integer;

{******************************************************************************}
{ Renumbering Methods
  Methods for renumbering formIDs.

  Includes:
  - FindHighestFormID
  - RenumberRecord
  - RenumberRecords
  - RenumberNewRecords
}
{******************************************************************************}

var
  UsedFormIDs: array [0..$FFFFFF] of byte;

function FindHighestFormID(var pluginsToMerge: TList; var merge: TMerge): Cardinal;
var
  i, j: Integer;
  plugin: TPlugin;
  aFile: IwbFile;
  aRecord: IwbMainRecord;
  formID: cardinal;
begin
  Result := $100;

  // loop through plugins to merge
  for i := 0 to Pred(pluginsToMerge.Count) do begin
    plugin := pluginsToMerge[i];
    aFile := plugin._File;
    // loop through records
    for j := 0 to Pred(aFile.RecordCount) do begin
      aRecord := aFile.Records[j];
      // skip override records
      if IsOverride(aRecord) then continue;
      formID := LocalFormID(aRecord);
      if formID > Result then Result := formID;
    end;
  end;

  // loop through mergePlugin
  plugin := merge.plugin;
  aFile := plugin._File;
  // loop through records
  for j := 0 to Pred(aFile.RecordCount) do begin
    aRecord := aFile.Records[j];
    // skip override records
    if IsOverride(aRecord) then continue;
    formID := LocalFormID(aRecord);
    if formID > Result then Result := formID;
  end;
end;

procedure RenumberRecord(aRecord: IwbMainRecord; NewFormID: cardinal);
var
  OldFormID: cardinal;
  prc, i: integer;
begin
  OldFormID := aRecord.LoadOrderFormID;
  // change references, then change form
  prc := 0;
  while aRecord.ReferencedByCount > 0 do begin
    if prc = aRecord.ReferencedByCount then break;
    prc := aRecord.ReferencedByCount;
    if settings.debugRenumbering then
      Tracker.Write('      Changing reference on '+aRecord.ReferencedBy[0].Name);
    aRecord.ReferencedBy[0].CompareExchangeFormID(OldFormID, NewFormID);
  end;
  if (aRecord.ReferencedByCount > 0) and settings.debugRenumbering then
     Tracker.Write('      Couldn''t change reference: '+aRecord.ReferencedBy[0].Name);
  for i := Pred(aRecord.OverrideCount) downto 0 do begin
    if settings.debugRenumbering then
      Tracker.Write('      Renumbering override in file: '+aRecord.Overrides[i]._File.Name);
    aRecord.Overrides[i].LoadOrderFormID := NewFormID;
  end;
  aRecord.LoadOrderFormID := NewFormID;
end;

procedure RenumberBefore(var pluginsToMerge: TList; var merge: TMerge);
const
  debugSkips = false;
var
  i, j, rc, OldFormID, total, fileTotal: integer;
  plugin: TPlugin;
  aFile: IwbFile;
  aRecord: IwbMainRecord;
  Records: TList;
  renumberAll: boolean;
  BaseFormID, NewFormID: cardinal;
  header: IwbContainer;
begin
  if bProgressCancel then exit;
  // inital messages
  renumberAll := merge.renumbering = 'All';
  Tracker.Write(' ');
  if renumberAll then Tracker.Write('Renumbering All FormIDs')
  else Tracker.Write('Renumbering Conflicting FormIDs');

  // initialize variables
  total := 0;
  Records := TList.Create;
  BaseFormID := FindHighestFormID(pluginsToMerge, merge) + 128;
  for i := 0 to High(UsedFormIDs) do
    UsedFormIDs[i] := 0;
  if settings.debugRenumbering then
    Tracker.Write('  BaseFormID: '+IntToHex(BaseFormID, 8));

  // renumber records in all pluginsToMerge
  for i := 0 to Pred(pluginsToMerge.Count) do begin
    if bProgressCancel then exit;
    plugin := pluginsToMerge[i];
    aFile := plugin._File;
    fileTotal := 0;
    merge.map.Add(StringReplace(plugin.filename, '=', '-', [rfReplaceAll])+'=0');
    Tracker.Write('  Renumbering FormIDs in ' + plugin.filename);

    // build records array because indexed order will change
    Records.Clear;
    rc := aFile.RecordCount;
    for j := 0 to Pred(rc) do begin
      aRecord := aFile.Records[j];
      Records.Add(Pointer(aRecord));
    end;

    // renumber records in file
    for j := 0 to Pred(rc) do begin
      if bProgressCancel then exit;
      aRecord := IwbMainRecord(Records[j]);
      // skip record headers and overrides
      if aRecord.Signature = 'TES4' then continue;
      if IsOverride(aRecord) then continue;
      OldFormID := LocalFormID(aRecord);
      // skip records that aren't conflicting if not renumberAll
      if (not renumberAll) and (UsedFormIDs[OldFormID] = 0) then begin
        UsedFormIDs[OldFormID] := 1;
        Tracker.Update(1);
        if settings.debugRenumbering and debugSkips then
          Tracker.Write('    Skipping FormID '+IntToHex(OldFormID, 8));
        continue;
      end;

      // renumber record
      NewFormID := LoadOrderPrefix(aRecord) + BaseFormID;
      if settings.debugRenumbering then
        Tracker.Write('    Changing FormID to ['+IntToHex(NewFormID, 8)+'] on '+aRecord.Name);
      merge.map.Add(IntToHex(OldFormID, 8)+'='+IntToHex(BaseFormID, 8));
      RenumberRecord(aRecord, NewFormID);

      // increment BaseFormID, totals, tracker position
      Inc(BaseFormID);
      Inc(total);
      Inc(fileTotal);
      Tracker.Update(1);
    end;

    // update map with fileTotal
    merge.map.Values[plugin.filename] := IntToStr(fileTotal);
  end;

  if settings.debugRenumbering then
    Tracker.Write('  Renumbered '+IntToStr(total)+' FormIDs');

  // set next object id
  header := merge.plugin._File.Elements[0] as IwbContainer;
  header.ElementByPath['HEDR\Next Object ID'].NativeValue :=  BaseFormID;

  // free memory
  Records.Free;
end;

procedure RenumberAfter(merge: TMerge);
begin
  // soon
end;


{******************************************************************************}
{ Script Fragment Methods
  Methods for handling script fragments.
}
{******************************************************************************}

{ Copies all files from @srcPath to @dstPath, tracking them in @merge }
procedure CopyFilesForMerge(var merge: TMerge; srcPath, dstPath: string);
var
  info: TSearchRec;
  srcFile, dstFile: string;
begin
  if bProgressCancel then exit;

  // exit if the srcPath doesn't exist
  if not DirectoryExists(srcPath) then begin
    Tracker.Write('  Directory: '+srcPath+' doesn''t exist');
    exit;
  end;

  // if no files in source path, exit
  if FindFirst(srcPath + '*', faAnyFile, info) <> 0 then begin
    Tracker.Write('  No files found in '+srcPath);
    exit;
  end;
  // search source path for files
  repeat
    if (Length(info.Name) < 8) then
      continue;  // skip . and ..
    srcFile := srcPath + info.Name;
    dstFile := dstPath + info.Name;
    if settings.batCopy then AddCopyOperation(srcFile, dstFile)
    else CopyFile(PChar(srcFile), PChar(dstFile), false);
    merge.files.Add(dstFile);
  until FindNext(info) <> 0;
  FindClose(info);
end;

procedure CompileScripts(srcPath, dstPath: string);
var
  info: TSearchRec;
  total: integer;
  importPath, compileCommand: string;
begin
  if bProgressCancel then exit;

  // exit if no decompiler is available
  if not FileExists(settings.compilerPath) then begin
    Tracker.Write('  Could not compile scripts in '+srcPath+', no compiler available.');
    exit;
  end;

  // exit if the srcPath doesn't exist
  if not DirectoryExists(srcPath) then begin
    Tracker.Write('  Directory: '+srcPath+' doesn''t exist');
    exit;
  end;

  // if no script files in source path, exit
  if FindFirst(srcPath + '*.psc', faAnyFile, info) <> 0 then begin
    Tracker.Write('  No files found matching '+srcPath + '*.psc');
    exit;
  end;

  // search source path for script source files
  total := 0;
  if settings.debugScriptFragments then
    Tracker.Write('  Compiling: ');
  repeat
    if (Length(info.Name) < 8) then
      continue;  // skip . and ..
    Inc(total);
    if settings.debugScriptFragments then
      Tracker.Write('    '+info.Name);
  until FindNext(info) <> 0;
  FindClose(info);
  Tracker.Write('    Compiled '+IntToStr(total)+' scripts');

  // prepare to compile
  srcPath := RemoveFromEnd(srcPath, '\');
  dstPath := RemoveFromEnd(dstPath, '\');
  importPath := RemoveFromEnd(pscPath, '\') + ';' + wbDataPath + 'scripts\source';
  compileCommand := Format('"%s" "%s" -o="%s" -f="%s" -i="%s" -a',
    [settings.compilerPath, srcPath, dstPath, settings.flagsPath, importPath]);
  if settings.debugScriptFragments then
    Tracker.Write('  Compile command: '+compileCommand);
  // execute compiler synchronously
  ExecNewProcess(PChar(compileCommand), true);
end;

procedure RenumberScripts(merge: TMerge; srcPath: string);
var
  info: TSearchRec;
  srcFile, oldFormID, oldFileFormID, newFileFormID: string;
  index, total: Integer;
  sl: TStringList;
begin
  if bProgressCancel then exit;

  // exit if the srcPath doesn't exist
  if not DirectoryExists(srcPath) then begin
    Tracker.Write('  Directory: '+srcPath+' doesn''t exist');
    exit;
  end;

  // if no script files in source path, exit
  if FindFirst(srcPath + '*.psc', faAnyFile, info) <> 0 then begin
    Tracker.Write('  No files found matching '+srcPath + '*.psc');
    exit;
  end;
  // search source path for script source files
  sl := TStringList.Create;
  total := 0;
  repeat
    if (Length(info.Name) < 8) then
      continue;  // skip . and ..
    srcFile := info.Name;
    oldFormID := Copy(info.Name, Length(srcFile) - 11, 8);
    oldFileFormID := '00' + Copy(info.Name, Length(srcFile) - 9, 6);

    // exit if we can't find a remapped formID for the source script
    index := merge.map.IndexOfName(oldFileFormID);
    if (index = -1) then begin
      if settings.debugScriptFragments then
        Tracker.Write(Format('    FormID [%s] was not renumbered in merge %s', [oldFileFormID, merge.name]));
      continue;
    end;

    // remap formID in file name and file contents
    Inc(total);
    newFileFormID := IntToHex(mergeFormIndex, 2) + Copy(merge.map.Values[oldFileFormID], 3, 6);
    if settings.debugScriptFragments then
      Tracker.Write(Format('    Remapping [%s] to [%s] on %s', [oldFormID, newFileFormID, srcFile]));
    srcFile := StringReplace(srcFile, oldFormID, newFileFormID, []);
    sl.LoadFromFile(srcPath + info.Name);
    sl.Text := StringReplace(sl.Text, oldFormID, newFileFormID, [rfReplaceAll]);
    sl.SaveToFile(srcPath + srcFile);
    DeleteFile(srcPath + Info.Name);
  until FindNext(info) <> 0;
  Tracker.Write('    Renumbered '+IntToStr(total)+' scripts');

  // clean up
  FindClose(info);
  sl.Free;
end;

procedure DecompileScripts(srcPath, dstPath: string);
var
  info: TSearchRec;
  total: Integer;
  decompileCommand: string;
begin
  if bProgressCancel then exit;

  // exit if no decompiler is available
  if not FileExists(settings.decompilerPath) then begin
    Tracker.Write('  Could not decompile scripts in '+srcPath+', no decompiler available.');
    exit;
  end;

  // exit if the srcPath doesn't exist
  if not DirectoryExists(srcPath) then begin
    Tracker.Write('  Directory: '+srcPath+' doesn''t exist');
    exit;
  end;

  // if no script files in source path, exit
  if FindFirst(srcPath + '*.pex', faAnyFile, info) <> 0 then begin
    Tracker.Write('  No files found matching '+srcPath + '*.pex');
    exit;
  end;
  // search source path for script files
  total := 0;
  if settings.debugScriptFragments then
    Tracker.Write('  Decompiling: ');
  repeat
    if (Length(info.Name) < 8) then
      continue;  // skip . and ..
    Inc(total);
    if settings.debugScriptFragments then
      Tracker.Write('    '+info.Name);
  until FindNext(info) <> 0;
  FindClose(info);
  Tracker.Write('    Decompiled '+IntToStr(total)+' scripts');

  // prepare to decompile
  srcPath := RemoveFromEnd(srcPath, '\');
  dstPath := RemoveFromEnd(dstPath, '\');
  decompileCommand := Format('"%s" "%s" -p "%s"',
    [settings.decompilerPath, srcPath, dstPath]);
  if settings.debugScriptFragments then
    Tracker.Write('  Decompile command: '+decompileCommand);
  // execute decompiler synchronously
  ExecNewProcess(PChar(decompileCommand), true);
end;

{ Copies the script source matching @sfn from @srcpath to @@pscPath }
function CopySource(sfn, srcpath: string): boolean;
var
  srcFile, dstFile: string;
begin
  Result := false;
  srcFile := srcPath + 'source\' + ChangeFileExt(sfn, '.psc');
  if not FileExists(srcFile) then begin
    if settings.debugScriptFragments then
      Tracker.Write('      Couldn''t find script source at '+srcFile);
    exit;
  end;
  dstFile := pscPath + ChangeFileExt(sfn, '.psc');
  Result := CopyFile(PChar(srcFile), PChar(dstFile), false);
end;

{ Copies the script matching @fn from @srcpath to @@pexPath }
function CopyScript(fn, srcpath: string): boolean;
var
  srcFile, dstFile: string;
begin
  Result := false;
  srcFile := srcPath + ChangeFileExt(fn, '.pex');
  if not FileExists(srcFile) then begin
    if settings.debugScriptFragments then
      Tracker.Write('      Couldn''t find script at '+srcFile);
    exit;
  end;
  dstFile := pexPath + ChangeFileExt(fn, '.pex');
  Result := CopyFile(PChar(srcFile), PChar(dstFile), false);
end;

{ Traverses the DIAL\INFO group in @plugin for script fragments.  When found,
  script fragments are copied from @srcPath to @dstPath if they correspond
  to a record that has been renumbered in @merge }
procedure CopyTopicInfoFragments(var plugin: TPlugin; var merge: TMerge; srcPath: string);
const
  infoFragmentsPath = 'VMAD - Virtual Machine Adapter\Data\Info VMAD\Script Fragments Info';
var
  f: IwbFile;
  group: IwbGroupRecord;
  rec, subgroup, container: IwbContainer;
  element, fragments: IwbElement;
  i, j, index: Integer;
  fn, oldFormID, oldFileFormID, newFileFormID: string;
begin
  f := plugin._File;
  // exit if no DIAL records in file
  if not f.HasGroup('DIAL') then begin
    if settings.debugScriptFragments then
      Tracker.Write('      '+plugin.filename+' has no DIAL record ground, skipping.');
    exit;
  end;

  // find all DIAL records
  group := f.GroupBySignature['DIAL'];
  for i := 0 to Pred(group.ElementCount) do begin
    element := group.Elements[i];
    // find all INFO records
    if not Supports(element, IwbContainer, subgroup) then
      continue;
    for j := 0 to Pred(subgroup.ElementCount) do begin
      rec := subgroup.Elements[j] as IwbContainer;
      fragments := rec.ElementByPath[infoFragmentsPath];
      if not Assigned(fragments) then
        continue;
      if not Supports(fragments, IwbContainer, container) then
        continue;
      fn := container.ElementValues['fileName'];
      if (Pos('TIF_', fn) > 0) then begin
        if settings.debugScriptFragments then
          Tracker.Write('      Found script fragment '+fn);
        oldFormID := Copy(fn, Length(fn) - 7, 8);
        oldFileFormID := '00' + Copy(fn, Length(fn) - 5, 6);
        index := merge.map.IndexOfName(oldFileFormID);
        if (index = -1) then begin
          if settings.debugScriptFragments then
            Tracker.Write(Format('      Skipping [%s], FormID not renumbered in merge', [oldFileFormID]));
          continue;
        end
        else begin
        newFileFormID := IntToHex(mergeFormIndex, 2) + Copy(merge.map.Values[oldFileFormID], 3, 6);
          if settings.debugScriptFragments then
            Tracker.Write(Format('      Script fragment renumbered from [%s] to [%s]', [oldFormID, newFileFormID]));
          if not CopySource(fn, srcPath) then
            if not CopyScript(fn, srcPath) then
              Tracker.Write('      Failed to copy '+srcPath+ChangeFileExt(fn, '.pex'));
          container.ElementEditValues['fileName'] := StringReplace(fn, oldFormID, newFileFormID, []);
        end;
      end;
    end;
  end;
end;

{ Traverses the QUST group in @plugin for script fragments.  When found,
  script fragments are copied from @srcPath to @dstPath if they correspond
  to a record that has been renumbered in @merge }
procedure CopyQuestFragments(var plugin: TPlugin; var merge: TMerge; srcPath: string);
const
  questFragmentsPath = 'VMAD - Virtual Machine Adapter\Data\Quest VMAD\Script Fragments Quest';
var
  f: IwbFile;
  group: IwbGroupRecord;
  rec, container: IwbContainer;
  fragments: IwbElement;
  i, index: Integer;
  fn, oldFormID, oldFileFormID, newFileFormID: string;
begin
  f := plugin._File;
  // exit if no QUST records in file
  if not f.HasGroup('QUST') then begin
    if settings.debugScriptFragments then
      Tracker.Write('      '+plugin.filename+' has no QUST record ground, skipping.');
    exit;
  end;

  // find all QUST records
  group := f.GroupBySignature['QUST'];
  for i := 0 to Pred(group.ElementCount) do begin
    rec := group.Elements[i] as IwbContainer;
    fragments := rec.ElementByPath[questFragmentsPath];
    if not Assigned(fragments) then
      continue;
    if not Supports(fragments, IwbContainer, container) then
      continue;
    fn := container.ElementValues['fileName'];
    if (Pos('QF_', fn) > 0) then begin
      if settings.debugScriptFragments then
        Tracker.Write('      Found script fragment '+fn);
      oldFormID := Copy(fn, Length(fn) - 7, 8);
      oldFileFormID := '00' + Copy(fn, Length(fn) - 5, 6);
      index := merge.map.IndexOfName(oldFileFormID);
      if (index = -1) then begin
        if settings.debugScriptFragments then
          Tracker.Write(Format('      Skipping [%s], FormID not renumbered in merge', [oldFileFormID]));
        continue;
      end
      else begin
        newFileFormID := IntToHex(mergeFormIndex, 2) + Copy(merge.map.Values[oldFileFormID], 3, 6);
        if settings.debugScriptFragments then
          Tracker.Write(Format('      Script fragment renumbered from [%s] to [%s]', [oldFormID, newFileFormID]));
        if not CopySource(fn, srcPath) then
          if not CopyScript(fn, srcPath) then
            Tracker.Write('      Failed to copy '+srcPath+ChangeFileExt(fn, '.pex'));
        container.ElementEditValues['fileName'] := StringReplace(fn, oldFormID, newFileFormID, []);
      end;
    end;
  end;
end;

{ Traverses the SCEN group in @plugin for script fragments.  When found,
  script fragments are copied from @srcPath to @dstPath if they correspond
  to a record that has been renumbered in @merge }
procedure CopySceneFragments(var plugin: TPlugin; var merge: TMerge; srcPath: string);
const
  sceneFragmentsPath = 'VMAD - Virtual Machine Adapter\Data\Quest VMAD\Script Fragments Quest';
var
  f: IwbFile;
  group: IwbGroupRecord;
  rec, container: IwbContainer;
  fragments: IwbElement;
  i, index: Integer;
  fn, oldFormID, oldFileFormID, newFileFormID: string;
begin
  f := plugin._File;
  // exit if no SCEN records in file
  if not f.HasGroup('SCEN') then begin
    if settings.debugScriptFragments then
      Tracker.Write('      '+plugin.filename+' has no SCEN record ground, skipping.');
    exit;
  end;

  // find all SCEN records
  group := f.GroupBySignature['SCEN'];
  for i := 0 to Pred(group.ElementCount) do begin
    rec := group.Elements[i] as IwbContainer;
    fragments := rec.ElementByPath[sceneFragmentsPath];
    if not Assigned(fragments) then
      continue;
    if not Supports(fragments, IwbContainer, container) then
      continue;
    fn := container.ElementValues['fileName'];
    if (Pos('SF_', fn) > 0) then begin
      if settings.debugScriptFragments then
        Tracker.Write('      Found script fragment '+fn);
      oldFormID := Copy(fn, Length(fn) - 7, 8);
      oldFileFormID := '00' + Copy(fn, Length(fn) - 5, 6);
      index := merge.map.IndexOfName(oldFileFormID);
      if (index = -1) then begin
        if settings.debugScriptFragments then
          Tracker.Write(Format('      Skipping [%s], FormID not renumbered in merge', [oldFileFormID]));
        continue;
      end
      else begin
        newFileFormID := IntToHex(mergeFormIndex, 2) + Copy(merge.map.Values[oldFileFormID], 3, 6);
        if settings.debugScriptFragments then
          Tracker.Write(Format('      Script fragment renumbered from [%s] to [%s]', [oldFormID, newFileFormID]));
        if not CopySource(fn, srcPath) then
          if not CopyScript(fn, srcPath) then
            Tracker.Write('      Failed to copy '+srcPath+ChangeFileExt(fn, '.pex'));
        container.ElementEditValues['fileName'] := StringReplace(fn, oldFormID, newFileFormID, []);
      end;
    end;
  end;
end;

{******************************************************************************}
{ Copying Methods
  Methods for copying records.

  Includes:
  - CopyRecord
  - CopyRecords
}
{******************************************************************************}

procedure CopyRecord(aRecord: IwbMainRecord; var merge: TMerge; asNew: boolean);
var
  aFile: IwbFile;
begin
  try
    aFile := merge.plugin._File;
    wbCopyElementToFile(aRecord, aFile, asNew, True, '', '', '');
  except on x : Exception do begin
      Tracker.Write('    Exception copying '+aRecord.Name+': '+x.Message);
      merge.fails.Add(aRecord.Name+': '+x.Message);
    end;
  end;
end;

procedure CopyRecords(var pluginsToMerge: TList; var merge: TMerge);
var
  i, j: integer;
  aFile: IwbFile;
  aRecord: IwbMainRecord;
  plugin: TPlugin;
  asNew: boolean;
begin
  if bProgressCancel then exit;

  Tracker.Write(' ');
  Tracker.Write('Copying records');
  //masters := TStringList.Create;
  asNew := merge.method = 'New Records';
  // copy records from all plugins to be merged
  for i := Pred(pluginsToMerge.Count) downto 0 do begin
    if bProgressCancel then exit;
    plugin := TPlugin(pluginsToMerge[i]);
    aFile := plugin._File;
    // copy records from file
    Tracker.Write('  Copying records from '+plugin.filename);
    for j := 0 to Pred(aFile.RecordCount) do begin
      if bProgressCancel then exit;
      aRecord := aFile.Records[j];
      if aRecord.Signature = 'TES4' then Continue;
      // copy record
      if settings.debugRecordCopying then
        Tracker.Write('    Copying record '+aRecord.Name);
      CopyRecord(aRecord, merge, asNew);
      Tracker.Update(1);
    end;
  end;
end;


{******************************************************************************}
{ Copy Assets methods
  Methods for copying file-specific assets.

  Includes:
  - CopyFaceGen
  - CopyVoice
  - CopyTranslations
  - SaveTranslations
  - CopyScriptFragments
  - CopyAssets
}
{******************************************************************************}

const
  faceTintPath = 'textures\actors\character\facegendata\facetint\';
  faceGeomPath = 'meshes\actors\character\facegendata\facegeom\';
  voicePath = 'sound\voice\';
  translationPath = 'interface\translations\';
  scriptsPath = 'scripts\';
var
  languages, CopiedFrom, MergeIni: TStringList;
  translations: array[0..31] of TStringList; // 32 languages maximum

function GetMapIndex(var merge: TMerge; fn: string; oldForm: string): integer;
var
  i, max: integer;
begin
  // start one entry after the plugin's filename
  Result := merge.map.IndexOfName(fn) + 1;

  // get maximum index to search to - index of next plugin in the map
  i := merge.plugins.IndexOf(fn) + 1;
  if i = merge.plugins.Count then
    max := merge.map.Count
  else
    max := merge.map.IndexOfName(merge.plugins[i]);

  // loop until we reach max
  while (Result < max) do begin
    // look for oldForm
    if SameText(merge.map.Names[Result], oldForm) then
      exit;
    Inc(Result);
  end;

  // return -1 if not found
  Result := -1;
end;

procedure CopyFaceGen(var plugin: TPlugin; var merge: TMerge; srcPath, dstPath: string);
var
  info: TSearchRec;
  oldForm, newForm, dstFile, srcFile: string;
  index: integer;
begin
  if bProgressCancel then exit;
  srcPath := srcPath + plugin.filename + '\';
  dstPath := dstPath + merge.filename + '\';
  ForceDirectories(dstPath);
  // if no files in source path, exit
  if FindFirst(srcPath + '*', faAnyFile, info) <> 0 then
    exit;
  // search srcPath for asset files
  repeat
    if (Length(info.Name) < 8) then
      continue;  // skip . and ..
    // use merge.map to map to new filename if necessary
    srcFile := info.Name;
    dstFile := srcFile;
    oldForm := Copy(srcFile, 1, 8);
    index := GetMapIndex(merge, plugin.filename, oldForm);
    if (index > -1) then begin
      newForm := merge.map.ValueFromIndex[index];
      dstFile := StringReplace(srcFile, oldForm, newForm, []);
    end;

    // copy file
    if settings.debugAssetCopying then
      Tracker.Write('    Copying asset "'+srcFile+'" to "'+dstFile+'"');
    if settings.batCopy then AddCopyOperation(srcPath + srcFile, dstPath + dstFile)
    else CopyFile(PChar(srcPath + srcFile), PChar(dstPath + dstFile), false);
    merge.files.Add(dstPath + dstFile);
  until FindNext(info) <> 0;
  FindClose(info);
end;

procedure CopyVoice(var plugin: TPlugin; var merge: TMerge; srcPath, dstPath: string);
var
  info, folder: TSearchRec;
  oldForm, newForm, dstFile, srcFile: string;
  index: integer;
begin
  if bProgressCancel then exit;
  srcPath := srcPath + plugin.filename + '\';
  dstPath := dstPath + merge.filename + '\';
  // if no folders in srcPath, exit
  if FindFirst(srcPath + '*', faDirectory, folder) <> 0 then
    exit;
  // search source path for asset folders
  repeat
    if (Pos('.', folder.Name) = 1) then
      continue; // skip . and ..
    ForceDirectories(dstPath + folder.Name); // make folder
    if FindFirst(srcPath + folder.Name + '\*', faAnyFile, info) <> 0 then
      continue; // if folder is empty, skip to next folder
    // search folder for files
    repeat
      if (Length(info.Name) < 8) then
        continue; // skip . and ..
      // use merge.map to map to new filename if necessary
      srcFile := info.Name;
      dstFile := srcFile;
      oldForm := Copy(srcFile, 1, 8);
      index := GetMapIndex(merge, plugin.filename, oldForm);
      if (index > -1) then begin
        newForm := merge.map.ValueFromIndex[index];
        dstFile := StringReplace(srcFile, oldForm, newForm, []);
      end;

      // copy file
      if settings.debugAssetCopying then
        Tracker.Write('    Copying asset "'+srcFile+'" to "'+dstFile+'"');
      srcFile := srcPath + folder.Name + '\' + srcFile;
      dstFile := dstPath + folder.Name + '\' + dstFile;
      if settings.batCopy then AddCopyOperation(srcFile, dstFile)
      else CopyFile(PChar(srcFile), PChar(dstFile), false);
      merge.files.Add(dstFile);
    until FindNext(info) <> 0;
    FindClose(info);
  until FindNext(folder) <> 0;
  FindClose(folder);
end;

procedure CopyTranslations(var plugin: TPlugin; var merge: TMerge; srcPath: string);
var
  info: TSearchRec;
  fn, language: string;
  index: integer;
  sl: TStringList;
begin
  if bProgressCancel then exit;
  fn := Lowercase(ChangeFileExt(plugin.filename, ''));
  if FindFirst(srcPath+'*.txt', faAnyFile, info) <> 0 then
    exit;
  repeat
    if (Pos(fn, Lowercase(info.Name)) <> 1) then
      continue;
    Tracker.Write('    Copying MCM translation "'+info.Name+'"');
    language := StringReplace(Lowercase(info.Name), fn, '', [rfReplaceAll]);
    index := languages.IndexOf(language);
    if index > -1 then begin
      sl := TStringList.Create;
      sl.LoadFromFile(srcPath + info.Name);
      translations[index].Text := translations[index].Text + #13#10#13#10 + sl.Text;
      sl.Free;
    end
    else begin
      translations[languages.Count] := TStringList.Create;
      translations[languages.Count].LoadFromFile(srcPath + info.Name);
      languages.Add(language);
    end;
  until FindNext(info) <> 0;
  FindClose(info);
end;

procedure SaveTranslations(var merge: TMerge);
var
  i: integer;
  output, path: string;
begin
  if bProgressCancel then exit;
  // exit if we have no translation files to save
  if languages.Count = 0 then exit;

  // set destination path
  path := merge.dataPath + translationPath;
  ForceDirectories(path);

  // save all new translation files
  for i := Pred(languages.Count) downto 0 do begin
    output := path + ChangeFileExt(merge.filename, '') + languages[i];
    translations[i].SaveToFile(output);
    merge.files.Add(output);
    translations[i].Free;
  end;
end;

procedure CopyIni(var plugin: TPlugin; var merge: TMerge; srcPath: string);
var
  fn: string;
  PluginIni: TStringList;
begin
  if bProgressCancel then exit;
  // exit if ini doesn't exist
  fn := plugin.dataPath + ChangeFileExt(plugin.filename, '.ini');
  if not FileExists(fn) then exit;

  // copy PluginIni file contents to MergeIni
  Tracker.Write('    Copying INI '+ChangeFileExt(plugin.filename, '.ini'));
  PluginIni := TStringList.Create;
  PluginIni.LoadFromFile(fn);
  MergeIni.Add(PluginIni.Text);
  PluginIni.Free;
end;

procedure SaveIni(var merge: TMerge);
begin
  if bProgressCancel then exit;
  // exit if we have no ini to save
  if MergeIni.Count = 0 then exit;
  merge.files.Add(merge.name+'\'+ChangeFileExt(merge.filename, '.ini'));
  MergeIni.SaveToFile(merge.dataPath + ChangeFileExt(merge.filename, '.ini'));
end;

procedure CopyScriptFragments(var plugin: TPlugin; var merge: TMerge; srcPath, dstPath: string);
begin
  CopySceneFragments(plugin, merge, srcPath);
  CopyQuestFragments(plugin, merge, srcPath);
  CopyTopicInfoFragments(plugin, merge, srcPath);
end;

procedure CopyGeneralAssets(var plugin: TPlugin; var merge: TMerge);
var
  srcPath, dstPath: string;
  fileIgnore, dirIgnore, filesList: TStringList;
begin
  if bProgressCancel then exit;
  // remove path delim for robocopy to work correctly
  srcPath := RemoveFromEnd(plugin.dataPath, PathDelim);
  dstPath := RemoveFromEnd(merge.dataPath, PathDelim);
  // exit if we've already copyied files from the source path
  if CopiedFrom.IndexOf(srcPath) > -1 then
    exit;

  // set up files to ignore
  fileIgnore := TStringList.Create;
  fileIgnore.Delimiter := ' ';
  fileIgnore.Add('meta.ini');
  fileIgnore.Add('*.esp');
  fileIgnore.Add('*.esm');
  fileIgnore.Add(ChangeFileExt(plugin.filename, '.seq'));
  fileIgnore.Add(ChangeFileExt(plugin.filename, '.ini'));
  if settings.extractBSAs then begin
    fileIgnore.Add('*.bsa');
    fileIgnore.Add('*.bsl');
  end;

  // set up directories to ignore
  dirIgnore := TStringList.Create;
  dirIgnore.Delimiter := ' ';
  dirIgnore.Add(plugin.filename);
  dirIgnore.Add('translations');
  dirIgnore.Add('TES5Edit Backups');

  // get list of files in directory
  filesList := TStringList.Create;
  GetFilesList(srcPath, fileIgnore, dirIgnore, filesList);
  merge.files.Text := merge.files.Text +
    StringReplace(filesList.Text, srcPath, dstPath, [rfReplaceAll]);

  // copy files
  CopiedFrom.Add(srcPath);
  Tracker.Write('    Copying general assets from '+srcPath);
  if settings.batCopy then
    batch.Add('robocopy "'+srcPath+'" "'+dstPath+'" /e /xf '+fileIgnore.DelimitedText+' /xd '+dirIgnore.DelimitedText)
  else
    CopyFiles(srcPath, dstPath, filesList);
end;

procedure AddCopyOperation(src, dst: string);
begin
  batch.Add('copy /Y "'+src+'" "'+dst+'"');
end;

procedure CopyAssets(var plugin: TPlugin; var merge: TMerge);
var
  bsaFilename: string;
begin
  if bProgressCancel then exit;
  // get plugin data path
  plugin.GetDataPath;
  //Tracker.Write('  dataPath: '+plugin.dataPath);

  // handleFaceGenData
  if settings.handleFaceGenData and (HAS_FACEDATA in plugin.flags) then begin
    // if BSA exists, extract FaceGenData from it to temp path and copy
    if HAS_BSA in plugin.flags then begin
      bsaFilename := wbDataPath + ChangeFileExt(plugin.filename, '.bsa');
      Tracker.Write('    Extracting '+bsaFilename+'\'+faceTintPath+plugin.filename);
      ExtractBSA(bsaFilename, faceTintPath+plugin.filename, TempPath);
      Tracker.Write('    Extracting '+bsaFilename+'\'+faceGeomPath+plugin.filename);
      ExtractBSA(bsaFilename, faceGeomPath+plugin.filename, TempPath);

      // copy assets from tempPath
      CopyFaceGen(plugin, merge, TempPath + faceTintPath, merge.dataPath + faceTintPath);
      CopyFaceGen(plugin, merge, TempPath + faceGeomPath, merge.dataPath + faceGeomPath);
    end;

    // copy assets from plugin.dataPath
    CopyFaceGen(plugin, merge, plugin.dataPath + faceTintPath, merge.dataPath + faceTintPath);
    CopyFaceGen(plugin, merge, plugin.dataPath + faceGeomPath, merge.dataPath + faceGeomPath);
  end;

  // handleVoiceAssets
  if bProgressCancel then exit;
  if settings.handleVoiceAssets and (HAS_VOICEDATA in plugin.flags) then begin
    // if BSA exists, extract voice assets from it to temp path and copy
    if HAS_BSA in plugin.flags then begin
      bsaFilename := wbDataPath + ChangeFileExt(plugin.filename, '.bsa');
      Tracker.Write('    Extracting '+bsaFilename+'\'+voicePath+plugin.filename);
      ExtractBSA(bsaFilename, voicePath+plugin.filename, TempPath);
      CopyVoice(plugin, merge, TempPath + voicePath, merge.dataPath + voicePath);
    end;
    CopyVoice(plugin, merge, plugin.dataPath + voicePath, merge.dataPath + voicePath);
  end;

  // handleMCMTranslations
  if settings.handleMCMTranslations and (HAS_TRANSLATION in plugin.flags) then begin
    // if BSA exists, extract MCM translations from it to temp path and copy
    if HAS_BSA in plugin.flags then begin
      bsaFilename := wbDataPath + ChangeFileExt(plugin.filename, '.bsa');
      Tracker.Write('    Extracting '+bsaFilename+'\'+translationPath);
      ExtractBSA(bsaFilename, translationPath, TempPath);
      CopyTranslations(plugin, merge, TempPath + translationPath);
    end;
    CopyTranslations(plugin, merge, plugin.dataPath + translationPath);
  end;

  // handleINI
  if bProgressCancel then exit;
  if settings.handleINIs and (HAS_INI in plugin.flags) then
    CopyIni(plugin, merge, plugin.dataPath);

  // handleScriptFragments
  if bProgressCancel then exit;
  if settings.handleScriptFragments and (HAS_FRAGMENTS in plugin.flags) then begin
    // if BSA exists, extract scripts from it to temp path and copy
    Tracker.Write('    Copying script fragments for '+plugin.filename);
    if HAS_BSA in plugin.flags then begin
      bsaFilename := wbDataPath + ChangeFileExt(plugin.filename, '.bsa');
      Tracker.Write('    Extracting '+bsaFilename+'\'+scriptsPath);
      ExtractBSA(bsaFilename, scriptsPath, TempPath);
      CopyScriptFragments(plugin, merge, TempPath + scriptsPath, merge.dataPath + scriptsPath);
    end;
    CopyScriptFragments(plugin, merge, plugin.dataPath + scriptsPath, merge.dataPath + scriptsPath);
  end;

  // copyGeneralAssets
  if bProgressCancel then exit;
  if settings.copyGeneralAssets then
    CopyGeneralAssets(plugin, merge);
end;

{******************************************************************************}
{ Merge Handling methods
  Methods for building, rebuilding, and deleting merges.

  Includes:
  - BuildMerge
  - DeleteOldMergeFiles
  - RebuildMerge
}
{******************************************************************************}

procedure BuildMerge(var merge: TMerge);
const
  bPauseBatch = false;
var
  plugin: TPlugin;
  mergeFile, aFile: IwbFile;
  e, masters: IwbContainer;
  failed, masterName, mergeDesc, desc, bfn, mergeFilePrefix: string;
  pluginsToMerge: TList;
  i, LoadOrder: Integer;
  usedExistingFile: boolean;
  slMasters: TStringList;
  FileStream: TFileStream;
  time: TDateTime;
begin
  // initialize
  Tracker.Write('Building merge: '+merge.name);
  batch := TStringList.Create;
  CopiedFrom := TStringList.Create;
  time := Now;
  failed := 'Failed to merge '+merge.name;
  merge.fails.Clear;

  // set up directories
  pexPath := TempPath + 'pex\';
  pscPath := TempPath + 'psc\';
  compiledPath := TempPath + 'compiled\';
  mergeFilePrefix := merge.dataPath + 'merge\'+ChangeFileExt(merge.filename, '');
  // delete papyrus directories, they should be empty before we begin
  DeleteDirectory(pexPath);
  DeleteDirectory(pscPath);
  DeleteDirectory(compiledPath);
  // force directories to exist so we can put files in them
  ForceDirectories(pexPath);
  ForceDirectories(pscPath);
  ForceDirectories(compiledPath);
  ForceDirectories(ExtractFilePath(mergeFilePrefix));

  // don't merge if merge has plugins not found in current load order
  pluginsToMerge := TList.Create;
  for i := 0 to Pred(merge.plugins.Count) do begin
    plugin := PluginByFileName(merge.plugins[i]);

    if not Assigned(plugin) then begin
      Tracker.Write(failed + ', couldn''t find plugin '+merge.plugins[i]);
      pluginsToMerge.Free;
      merge.status := 11;
      exit;
    end;
    pluginsToMerge.Add(plugin);
  end;

  // identify destination file or create new one
  plugin := PluginByFilename(merge.filename);
  merge.plugin := nil;
  merge.map.Clear;
  if Assigned(plugin) then begin
    usedExistingFile := true;
    merge.plugin := plugin;
  end
  else begin
    usedExistingFile := false;
    merge.plugin := CreateNewPlugin(merge.filename);
  end;
  mergeFile := merge.plugin._File;
  Tracker.Write(' ');
  Tracker.Write('Merge is using plugin: '+merge.plugin.filename);

  // don't merge if mergeFile not assigned
  if not Assigned(merge.plugin) then begin
    Tracker.Write(failed + ', couldn''t assign merge file.');
    merge.status := 11;
    exit;
  end;

  // don't merge if mergeFile is at an invalid load order position relative
  // don't the plugins being merged
  if usedExistingFile then begin
    for i := 0 to Pred(pluginsToMerge.Count) do begin
      plugin := pluginsToMerge[i];

      if PluginsList.IndexOf(plugin) > PluginsList.IndexOf(merge.plugin) then begin
        Tracker.Write(failed + ', '+plugin.filename +
          ' is at a lower load order position than '+merge.filename);
        pluginsToMerge.Free;
        merge.status := 11;
        exit;
      end;
    end;
  end;

  // force merge directories to exist
  merge.dataPath := settings.mergeDirectory + merge.name + '\';
  Tracker.Write('Merge data path: '+merge.dataPath);
  ForceDirectories(merge.dataPath);

  // add required masters
  slMasters := TStringList.Create;
  Tracker.Write('Adding masters...');
  for i := 0 to Pred(pluginsToMerge.Count) do begin
    plugin := TPlugin(pluginsToMerge[i]);
    slMasters.AddObject(plugin.filename, merge.plugins.Objects[i]);
    GetMasters(plugin._File, slMasters);
  end;
  try
    mergeFormIndex := slMasters.Count - merge.plugins.Count;
    slMasters.CustomSort(MergePluginsCompare);
    AddMasters(merge.plugin._File, slMasters);
    if settings.debugMasters then begin
      Tracker.Write('Masters added:');
      Tracker.Write(slMasters.Text);
      slMasters.Clear;
      GetMasters(merge.plugin._File, slMasters);
      Tracker.Write('Actual masters:');
      Tracker.Write(slMasters.Text);
    end;
  except on Exception do
    // nothing
  end;
  slMasters.Free;
  if bProgressCancel then exit;
  Tracker.Write('Done adding masters');

  try
    // overrides merging method
    if merge.method = 'Overrides' then begin
      RenumberBefore(pluginsToMerge, merge);
      CopyRecords(pluginsToMerge, merge);
    end;
  except
    // exception, discard changes to source plugin, free merge file, and exit
    on x : Exception do begin
      Tracker.Write('Exception: '+x.Message);
      Tracker.Write(' ');
      Tracker.Write('Discarding changes to source plugins');
      for i := 0 to Pred(pluginsToMerge.Count) do begin
        plugin := pluginsToMerge[i];
        Tracker.Write('  Reloading '+plugin.filename+' from disk');
        LoadOrder := PluginsList.IndexOf(plugin);
        plugin._File := wbFile(wbDataPath + plugin.filename, LoadOrder);
      end;

      // release merge file
      mergeFile._Release;
      mergeFile := nil;

      // exit
      Tracker.Write(' ');
      Tracker.Write('Merge failed.');
      merge.status := 11;
      exit;
    end;
  end;

  // new records merging method
  if merge.method = 'New records' then begin
     CopyRecords(pluginsToMerge, merge);
     RenumberAfter(merge);
  end;

  // copy assets
  Tracker.Write(' ');
  Tracker.Write('Copying assets');
  languages := TStringList.Create;
  MergeIni := TStringList.Create;
  for i := Pred(pluginsToMerge.Count) downto 0 do begin
    plugin := pluginsToMerge[i];
    Tracker.Write('  Copying assets for '+plugin.filename);
    CopyAssets(plugin, merge);
  end;
  // save combined assets
  SaveTranslations(merge);
  SaveINI(merge);
  // clean up
  languages.Free;
  MergeIni.Free;

  // handle script fragments
  if settings.handleScriptFragments then begin
    Tracker.Write(' ');
    Tracker.Write('Decompiling scripts');
    DecompileScripts(pexPath, pscPath);
    Tracker.Write(' ');
    Tracker.Write('Remapping FormIDs in scripts');
    RenumberScripts(merge, pscPath);
    Tracker.Write(' ');
    Tracker.Write('Compiling scripts');
    CompileScripts(pscPath, compiledPath);
    Tracker.Write(' ');
    Tracker.Write('Copying modified scripts');
    CopyFilesForMerge(merge, compiledPath, merge.dataPath + 'scripts\');
    CopyFilesForMerge(merge, pscPath, merge.dataPath + 'scripts\source\');
  end;

  // batch copy assets
  if settings.batCopy and (batch.Count > 0) and (not bProgressCancel) then begin
    bfn := mergeFilePrefix + '.bat';
    if bPauseBatch then batch.Add('pause');
    batch.SaveToFile(bfn);
    batch.Clear;
    ShellExecute(0, 'open', PChar(bfn), '', PChar(wbProgramPath), SW_SHOWMINNOACTIVE);
  end;

  // create SEQ file
  CreateSEQFile(merge);

  // set description
  desc := 'Merged Plugin: ';
  for i := 0 to Pred(pluginsToMerge.Count) do begin
    plugin := pluginsToMerge[i];
    aFile := plugin._File;
    mergeDesc := (aFile.Elements[0] as IwbContainer).ElementEditValues['SNAM'];
    if Pos('Merged Plugin', mergeDesc) > 0 then
      desc := desc+StringReplace(mergeDesc, 'Merged Plugin:', '', [rfReplaceAll])
    else
      desc := desc+#13#10+'  '+merge.plugins[i];
  end;
  (mergeFile.Elements[0] as IwbContainer).ElementEditValues['SNAM'] := desc;

  // clean masters
  mergeFile.CleanMasters;

  // if overrides method, remove masters to force clamping
  if merge.method = 'Overrides' then begin
    Tracker.Write(' ');
    Tracker.Write('Removing unncessary masters');
    masters := mergeFile.Elements[0] as IwbContainer;
    masters := masters.ElementByPath['Master Files'] as IwbContainer;
    for i := Pred(masters.ElementCount) downto 0 do begin
      e := masters.Elements[i] as IwbContainer;
      masterName := e.ElementEditValues['MAST'];
      if (masterName = '') then Continue;
      if merge.plugins.IndexOf(masterName) > -1 then begin
        Tracker.Write('  Removing master '+masterName);
        masters.RemoveElement(i);
      end;
    end;
  end;

  // reload plugins to be merged to discard changes
  if merge.method = 'Overrides' then begin
    Tracker.Write(' ');
    Tracker.Write('Discarding changes to source plugins');
    for i := 0 to Pred(pluginsToMerge.Count) do begin
      plugin := pluginsToMerge[i];
      Tracker.Write('  Reloading '+plugin.filename+' from disk');
      LoadOrder := PluginsList.IndexOf(plugin);
      plugin._File._Release;
      plugin._File := wbFile(wbDataPath + plugin.filename, LoadOrder);
      plugin._File._AddRef;
      plugin._File.BuildRef;
    end;
  end;
  if bProgressCancel then exit;

  // update merge plugin hashes
  merge.UpdateHashes;

  // save merged plugin
  FileStream := TFileStream.Create(merge.dataPath + merge.filename, fmCreate);
  try
    Tracker.Write(' ');
    Tracker.Write('Saving: ' + merge.dataPath + merge.filename);
    mergeFile.WriteToStream(FileStream, False);
    merge.files.Add(merge.dataPath + merge.filename);
  finally
    FileStream.Free;
  end;

  // save merge map, files, fails
  merge.map.SaveToFile(mergeFilePrefix+'_map.txt');
  merge.files.SaveToFile(mergeFilePrefix+'_files.txt');
  merge.fails.SaveToFile(mergeFilePrefix+'_fails.txt');
  merge.plugins.SaveToFile(mergeFilePrefix+'_plugins.txt');

  // update statistics
  if merge.status = 7 then
    Inc(sessionStatistics.pluginsMerged, merge.plugins.Count);
  Inc(sessionStatistics.mergesBuilt);

  // done merging
  time := (Now - time) * 86400;
  merge.dateBuilt := Now;
  merge.status := 5;
  Tracker.Write('Done merging '+merge.name+' ('+FormatFloat('0.###', time) + 's)');
end;

procedure DeleteOldMergeFiles(var merge: TMerge);
var
  i: integer;
  path: string;
begin
  // delete files
  for i := Pred(merge.files.Count) downto 0 do begin
    path := merge.files[i];
    if FileExists(path) then
      DeleteFile(path);
    merge.files.Delete(i);
  end;
end;

procedure RebuildMerge(var merge: TMerge);
begin
  DeleteOldMergeFiles(merge);
  BuildMerge(merge);
end;

end.
