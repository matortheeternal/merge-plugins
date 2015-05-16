unit mpMerge;

interface

uses
  Windows, SysUtils, Classes,
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

implementation

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

const
  debugRenumbering = false;

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
    aRecord.ReferencedBy[0].CompareExchangeFormID(OldFormID, NewFormID);
  end;
  for i := Pred(aRecord.OverrideCount) downto 0 do
    aRecord.Overrides[i].LoadOrderFormID := NewFormID;
  aRecord.LoadOrderFormID := NewFormID;
end;

procedure RenumberBefore(var pluginsToMerge: TList; var merge: TMerge);
var
  i, j, rc, Index: integer;
  plugin: TPlugin;
  aFile: IwbFile;
  aRecord: IwbMainRecord;
  Records: TList;
  renumberAll: boolean;
  BaseFormID, NewFormID, OldFormID: cardinal;
begin
  // inital messages
  renumberAll := merge.renumbering = 'All';
  if renumberAll then Tracker.Write('Renumbering All Records')
  else Tracker.Write('Renumbering Conflicting Records');

  // initialize variables
  Records := TList.Create;
  BaseFormID := FindHighestFormID(pluginsToMerge, merge) + 128;
  if debugRenumbering then
    Tracker.Write('  BaseFormID: '+IntToHex(BaseFormID, 8));

  // renumber records in all pluginsToMerge
  for i := 0 to Pred(pluginsToMerge.Count) do begin
    plugin := pluginsToMerge[i];
    aFile := plugin._File;
    Tracker.Write('  Renumbering records in ' + plugin.filename);
    aFile.BuildRef; // build reference table so we can renumber references

    // build records array because indexed order will change
    Records.Clear;
    rc := aFile.RecordCount;
    for j := 0 to Pred(rc) do begin
      aRecord := aFile.Records[j];
      Records.Add(Pointer(aRecord));
    end;

    // renumber records in file
    for j := 0 to Pred(rc) do begin
      aRecord := IwbMainRecord(Records[j]);
      // skip record headers and overrides
      if aRecord.Signature = 'TES4' then continue;
      if IsOverride(aRecord) then continue;

      OldFormID := aRecord.LoadOrderFormID;
      Index := LocalFormID(aRecord);
      //Tracker.Write('    '+IntToHex(Index, 8));
      // skip records that aren't conflicting if not renumberAll
      if (not renumberAll) and (not UsedFormIDs[Index] = 1) then begin
        UsedFormIDs[Index] := 1;
        continue;
      end;

      // renumber record
      NewFormID := LoadOrderPrefix(aRecord) + BaseFormID;
      if debugRenumbering then
        Tracker.Write('    Changing FormID to ['+IntToHex(NewFormID, 8)+'] on '+aRecord.Name);
      merge.map.Add(IntToHex(OldFormID, 8)+'='+IntToHex(NewFormID, 8));
      RenumberRecord(aRecord, NewFormID);

      // increment BaseFormID, tracker position
      Inc(BaseFormID);
      Tracker.Update(1);
    end;
  end;
end;

procedure RenumberAfter(merge: TMerge);
begin
  // soon
end;

{******************************************************************************}
{ Copying Methods
  Methods for copying records.

  Includes:
  - CopyRecord
  - CopyRecords
}
{******************************************************************************}

procedure CopyRecord(aRecord: IwbMainRecord; merge: TMerge; asNew: boolean);
var
  aFile: IwbFile;
begin
  try
    aFile := merge.plugin._File;
    wbCopyElementToFile(aRecord, aFile, asNew, True, '', '', '');
  except on x : Exception do begin
      Tracker.Write('    Exception copying '+aRecord.Name+': '+x.Message);
      merge.fails.Add(aRecord.FullPath+': '+x.Message);
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
  Tracker.Write('Copying records');
  asNew := merge.method = 'New Records';
  // copy records from all plugins to be merged
  for i := Pred(pluginsToMerge.Count) downto 0 do begin
    plugin := TPlugin(pluginsToMerge[i]);
    aFile := plugin._File;
    // copy records from file
    Tracker.Write('  Copying records from '+plugin.filename);
    for j := 0 to Pred(aFile.RecordCount) do begin
      aRecord := aFile.Records[j];
      if aRecord.Signature = 'TES4' then Continue;
      CopyRecord(aRecord, merge, asNew);
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
  voicePath = 'sound\voice';
  translationPath = 'interface\translations\';
  scriptsPath = 'scripts\';
var
  languages: TStringList;
  translations: array[0..31] of TStringList; // 32 languages maximum

procedure CopyFaceGen(var plugin: TPlugin; var merge: TMerge; srcPath, dstPath: string);
var
  info: TSearchRec;
  oldForm, newForm, dstFile, srcFile: string;
  index: integer;
begin
  srcPath := srcPath + plugin.filename + '\';
  dstPath := dstPath + plugin.filename + '\';
  // if no files in source path, exit
  if FindFirst(srcPath + '*', faAnyFile, info) <> 0 then
    exit;
  // search srcPath for asset files
  repeat
    if (Length(info.Name) < 8) then
      continue;  // skip . and ..
    // use merge.map to map to new filename if necessary
    srcFile := info.Name;
    dstFile := dstPath + srcFile;
    oldForm := Copy(srcFile, 1, 8);
    index := merge.map.IndexOfName(oldForm);
    if (index > -1) then begin
      newForm := '00' + Copy(merge.map.Values[oldForm], 3, 6);
      dstFile := dstPath + StringReplace(srcFile, oldForm, newForm, []);
    end;

    // copy file
    Tracker.Write('    Copying asset "'+srcFile+'" to "'+dstFile+'"');
    CopyFile(PChar(srcPath + srcFile), PChar(dstPath + dstFile), true);
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
  srcPath := srcPath + plugin.filename + '\';
  dstPath := dstPath + plugin.filename + '\';
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
      dstFile := dstPath + srcFile;
      oldForm := Copy(srcFile, 1, 8);
      index := merge.map.IndexOfName(oldForm);
      if (index > -1) then begin
        newForm := '00' + Copy(merge.map.Values[oldForm], 3, 6);
        dstFile := dstPath + StringReplace(srcFile, oldForm, newForm, []);
      end;

      // copy file
      Tracker.Write('    Copying asset "'+srcFile+'" to "'+dstFile+'"');
      CopyFile(PChar(srcPath + srcFile), PChar(dstPath + dstFile), true);
      merge.files.Add(dstPath + dstFile);
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
  // terminate if we have no translation files to save
  if languages.Count = 0 then
    exit;

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

procedure CopyScriptFragments(var plugin: TPlugin; var merge: TMerge; srcPath, dstPath: string);
begin
  // soon
end;

procedure CopyGeneralAssets(var plugin: TPlugin; var merge: TMerge);
begin
  // soon
end;

procedure CopyAssets(var plugin: TPlugin; var merge: TMerge);
var
  bsaFilename: string;
begin
  // handleFaceGenData
  if settings.handleFaceGenData and (HAS_FACEDATA in plugin.flags) then begin
    // if BSA exists, extract FaceGenData from it to temp path and copy
    if HAS_BSA in plugin.flags then begin
      bsaFilename := wbDataPath + ChangeFileExt(plugin.filename, '.bsa');
      Tracker.Write('    Extracting '+bsaFilename+'\'+faceTintPath+plugin.filename);
      ExtractBSA(bsaFilename, faceTintPath+plugin.filename, tempPath);
      Tracker.Write('    Extracting '+bsaFilename+'\'+faceGeomPath+plugin.filename);
      ExtractBSA(bsaFilename, faceGeomPath+plugin.filename, tempPath);

      // copy assets from tempPath
      CopyFaceGen(plugin, merge, tempPath + faceTintPath, merge.dataPath + faceTintPath);
      CopyFaceGen(plugin, merge, tempPath + faceGeomPath, merge.dataPath + faceGeomPath);
    end;

    // copy assets from wbDataPath
    CopyFaceGen(plugin, merge, wbDataPath + faceTintPath, merge.dataPath + faceTintPath);
    CopyFaceGen(plugin, merge, wbDataPath + faceGeomPath, merge.dataPath + faceGeomPath);
  end;

  // handleVoiceAssets
  if settings.handleVoiceAssets and (HAS_VOICEDATA in plugin.flags) then begin
    // if BSA exists, extract voice assets from it to temp path and copy
    if HAS_BSA in plugin.flags then begin
      bsaFilename := wbDataPath + ChangeFileExt(plugin.filename, '.bsa');
      Tracker.Write('    Extracting '+bsaFilename+'\'+voicePath+plugin.filename);
      ExtractBSA(bsaFilename, voicePath+plugin.filename, tempPath);
      CopyVoice(plugin, merge, tempPath + voicePath, merge.dataPath + voicePath);
    end;
    CopyVoice(plugin, merge, wbDataPath + voicePath, merge.dataPath + voicePath);
  end;

  // handleMCMTranslations
  if settings.handleMCMTranslations and (HAS_TRANSLATION in plugin.flags) then begin
    // if BSA exists, extract MCM translations from it to temp path and copy
    if HAS_BSA in plugin.flags then begin
      bsaFilename := wbDataPath + ChangeFileExt(plugin.filename, '.bsa');
      Tracker.Write('    Extracting '+bsaFilename+'\'+translationPath);
      ExtractBSA(bsaFilename, translationPath, tempPath);
      CopyTranslations(plugin, merge, tempPath + translationPath);
    end;
    CopyTranslations(plugin, merge, wbDataPath + translationPath);
  end;

  // handleScriptFragments
  if settings.handleScriptFragments and (HAS_FRAGMENTS in plugin.flags) then begin
    // if BSA exists, extract scripts from it to temp path and copy
    if HAS_BSA in plugin.flags then begin
      bsaFilename := wbDataPath + ChangeFileExt(plugin.filename, '.bsa');
      Tracker.Write('    Extracting '+bsaFilename+'\'+scriptsPath);
      ExtractBSA(bsaFilename, scriptsPath, tempPath);
      CopyScriptFragments(plugin, merge, tempPath + scriptsPath, merge.dataPath + scriptsPath);
    end;
    CopyScriptFragments(plugin, merge, wbDataPath + scriptsPath, merge.dataPath + scriptsPath);
  end;

  // copyGeneralAssets
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
var
  plugin: TPlugin;
  mergeFile, aFile: IwbFile;
  e, masters: IwbContainer;
  failed, masterName, mergeDesc, desc: string;
  pluginsToMerge: TList;
  i, LoadOrder: Integer;
  usedExistingFile: boolean;
  slMasters: TStringList;
  FileStream: TFileStream;
  time: TDateTime;
begin
  // initialize
  Tracker.Write('Building merge: '+merge.name);
  time := Now;
  failed := 'Failed to merge '+merge.name;
  merge.fails.Clear;

  // don't merge if merge has plugins not found in current load order
  pluginsToMerge := TList.Create;
  for i := 0 to Pred(merge.plugins.Count) do begin
    plugin := PluginByFileName(merge.plugins[i]);

    if not Assigned(plugin) then begin
      Tracker.Write(failed + ', couldn''t find plugin '+merge.plugins[i]);
      pluginsToMerge.Free;
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
        exit;
      end;
    end;
  end;

  // force merge directories to exist
  merge.dataPath := settings.mergeDirectory + merge.name + '\';
  ForceDirectories(merge.dataPath);

  // add required masters
  slMasters := TStringList.Create;
  slMasters.Sorted := true;
  slMasters.Duplicates := dupIgnore;
  Tracker.Write('Adding masters...');
  for i := 0 to Pred(pluginsToMerge.Count) do begin
    plugin := TPlugin(pluginsToMerge[i]);
    slMasters.Add(plugin.filename);
    GetMasters(plugin._File, slMasters);
  end;
  try
    AddMasters(merge.plugin._File, slMasters);
    mergeFile.SortMasters;
  except on Exception do
    // nothing
  end;
  Tracker.Write('Done adding masters');

  // overrides merging method
  if merge.method = 'Overrides' then begin
    Tracker.Write(' ');
    RenumberBefore(pluginsToMerge, merge);
    Tracker.Write(' ');
    CopyRecords(pluginsToMerge, merge);
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
  for i := 0 to Pred(pluginsToMerge.Count) do begin
    plugin := pluginsToMerge[i];
    Tracker.Write('  Copying assets for '+plugin.filename);
    CopyAssets(plugin, merge);
  end;
  SaveTranslations(merge);
  languages.Free;

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
      plugin._File := wbFile(wbDataPath + plugin.filename, LoadOrder);
    end;
  end;

  // update merge pluginSizes and pluginDates
  merge.hashes.Clear;
  for i := 0 to Pred(pluginsToMerge.Count) do begin
    plugin := TPlugin(pluginsToMerge[i]);
    merge.hashes.Add(plugin.hash);
  end;

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

  // done merging
  time := (Now - Time) * 86400;
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
