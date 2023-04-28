{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Boban Spasic

 Unit description:
 Methods to manipulate banks and voices as objects

}

unit untDXObjInterface;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, untDX7Voice, untDX7Bank, untDXUtils, untUtils,
  untSysExUtils, HlpHashFactory;

procedure GetVoices(aStream: TMemoryStream; var aList: TStringList);
procedure SplitVMEM2VCED(aStream: TMemoryStream; aOutDir: string);
procedure RipMidiQuest(aStream: TMemoryStream; aOutDir: string);
procedure XSplitVMEM2VCED(aStream: TMemoryStream; aOutDir: string);
procedure JoinVCED2VMEM(aInputDir: string; aOutFile: string);
function Hash2Name(aFile: string): boolean;
function Test_VCEDHash(aFile: string): string;
function Test_VMEMHash(aFile: string; aVoiceNr: integer = 1): string;
function Voice2Name(aFile: string): boolean;
function CheckVMEMIntegrity(aStream: TMemoryStream; aPos: integer;
  var aNullVoice: boolean): integer;
function CheckVCEDIntegrity(aStream: TMemoryStream; aPos: integer;
  var aNullVoice: boolean): integer;
procedure NormalizeVMEM(aStream: TMemoryStream; aPos: integer; aFile: string);
procedure NormalizeVCED(aStream: TMemoryStream; aPos: integer; aFile: string);

implementation

procedure GetVoices(aStream: TMemoryStream; var aList: TStringList);
var
  fBank: TDX7BankContainer;
  i: integer;
  iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
  tmpByte: byte;
begin
  fBank := TDX7BankContainer.Create;
  aList.Clear;
  iPos := -1;
  if PosBytes(abSysExID, aStream) >= 0 then
  begin
    aStream.Position := 3;
    tmpByte := aStream.ReadByte;
    if tmpByte = 9 then
    begin
      iPos := 6;
      aList.Add('    VMEM header found');
    end;
  end
  else
  begin
    if aStream.Size = 4096 then
    begin
      iPos := 0;
      aList.Add('    Headerless file');
    end;
  end;
  if iPos <> -1 then
  begin
    fBank.LoadBankFromStream(aStream, iPos);
    for i := 1 to 32 do
      aList.Add(Format('%.2d', [i]) + ': ' + fBank.GetVoiceName(i));
  end
  else
    aList.Add('    Not a DX7 VMEM file');

  fBank.Free;
end;

function Hash2Name(aFile: string): boolean;
var
  fVoice: TDX7VoiceContainer;
  iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
  tmpByte: byte;
  fStream: TMemoryStream;
  fDirectory: string;
  sPart: string;
  sNPart: string;
  i: integer;
begin
  Result := False;
  fVoice := TDX7VoiceContainer.Create;
  fStream := TMemoryStream.Create;
  fStream.LoadFromFile(aFile);
  iPos := -1;
  if PosBytes(abSysExID, fStream) >= 0 then
  begin
    fStream.Position := 3;
    tmpByte := fStream.ReadByte;
    if tmpByte = 0 then
    begin
      iPos := 6;
      WriteLn('    VCED header found');
    end;
  end
  else
  begin
    if fStream.Size = 155 then
    begin
      iPos := 0;
      WriteLn('    Headerless file');
    end;
  end;
  try
    if iPos <> -1 then
    begin
      fVoice.Load_VCED_FromStream(fStream, iPos);
      fDirectory := IncludeTrailingPathDelimiter(ExtractFileDir(aFile));
      if fDirectory = PathDelim then fDirectory := '';
      sPart := fDirectory + fVoice.CalculateHash;
      sNPart := sPart;
      i := 0;
      if aFile <> sPart + '.syx' then
      begin
        while FileExists(sNPart + '.syx') do
        begin
          Inc(i);
          sNPart := sPart + '_' + Format('%.2d', [i]);
        end;
        Result := RenameFile(aFile, sNPart + '.syx');
        if Result then
          WriteLn('File ' + aFile + ' renamed to ' + sNPart + '.syx')
        else
          WriteLn('File ' + aFile + ' failed to rename');
      end
      else
        WriteLn('File ' + aFile + ' will not be renamed');
    end
    else
      WriteLn('    Not a DX7 VCED file');
  finally
    fStream.Free;
    fVoice.Free;
  end;
end;

function Voice2Name(aFile: string): boolean;
var
  fVoice: TDX7VoiceContainer;
  i: integer;
  iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
  tmpByte: byte;
  fStream: TMemoryStream;
  fDirectory: string;
begin
  Result := False;
  fVoice := TDX7VoiceContainer.Create;
  fStream := TMemoryStream.Create;
  fStream.LoadFromFile(aFile);
  iPos := -1;
  if PosBytes(abSysExID, fStream) >= 0 then
  begin
    fStream.Position := 3;
    tmpByte := fStream.ReadByte;
    if tmpByte = 0 then
    begin
      iPos := 6;
      WriteLn('    VCED header found');
    end;
  end
  else
  begin
    if fStream.Size = 155 then
    begin
      iPos := 0;
      WriteLn('    Headerless file');
    end;
  end;
  try
    if iPos <> -1 then
    begin
      fVoice.Load_VCED_FromStream(fStream, iPos);
      fDirectory := IncludeTrailingPathDelimiter(ExtractFileDir(aFile));
      if fDirectory = PathDelim then fDirectory := '';
      //do not overwrite the file with same name, add a number to the file name
      i := 0;
      if not FileExists(fDirectory + GetValidFileName(fVoice.GetVoiceName) + '.syx') then
        RenameFile(aFile, fDirectory + GetValidFileName(fVoice.GetVoiceName) + '.syx')
      else
        while Result = False do
        begin
          Inc(i);
          if not FileExists(fDirectory + GetValidFileName(fVoice.GetVoiceName) +
            '_' + IntToStr(i) + '.syx') then
          begin
            RenameFile(aFile, fDirectory + GetValidFileName(fVoice.GetVoiceName) +
              '_' + IntToStr(i) + '.syx');
            Result := True;
          end;
        end;
      Result := True;
    end
    else
      WriteLn('    Not a DX7 VCED file');
  finally
    fStream.Free;
    fVoice.Free;
  end;
end;

procedure SplitVMEM2VCED(aStream: TMemoryStream; aOutDir: string);
var
  fBank: TDX7BankContainer;
  fVoice: TDX7VoiceContainer;
  msVoice: TMemoryStream;
  i: integer;
  sVoiceName: string;
  slVoices: TStringList;
  iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
  tmpByte: byte;
begin
  fBank := TDX7BankContainer.Create;
  iPos := -1;
  if PosBytes(abSysExID, aStream) >= 0 then
  begin
    aStream.Position := 3;
    tmpByte := aStream.ReadByte;
    if tmpByte = 9 then
    begin
      iPos := 6;
      WriteLn('    VMEM header found');
    end;
  end
  else
  begin
    if aStream.Size = 4096 then
    begin
      iPos := 0;
      WriteLn('    Headerless file');
    end;
  end;
  if iPos <> -1 then
  begin
    fBank.LoadBankFromStream(aStream, iPos);
    slVoices := TStringList.Create;
    for i := 1 to 32 do
    begin
      sVoiceName := IncludeTrailingPathDelimiter(aOutDir) +
        GetValidFileName(fBank.GetVoiceName(i)) + '.dx7vced.syx';
      WriteLn('Create :' + sVoiceName);
      fVoice := TDX7VoiceContainer.Create;
      fBank.GetVoice(i, fVoice);
      slVoices.AddPair(GetValidFileName(fBank.GetVoiceName(i)), fBank.GetVoiceName(i));
      msVoice := TMemoryStream.Create;
      fVoice.SysExVoiceToStream(1, msVoice);
      msVoice.SaveToFile(sVoiceName);
      msVoice.Free;
      fVoice.Free;
    end;
    slVoices.SaveToFile(IncludeTrailingPathDelimiter(aOutDir) + 'voices.lst');
    slVoices.Free;
  end
  else
    WriteLn('    Not a DX7 VMEM file');

  fBank.Free;
end;

procedure RipMidiQuest(aStream: TMemoryStream; aOutDir: string);
var
  abBODY: array[0..3] of byte = ($42, $4F, $44, $59);
  msVCED: TMemoryStream;
  msHeadless: TMemoryStream;
  iCounter: Integer;
  iPos: Integer;
  checksum: Integer;
  i: Integer;
begin
  msVCED := TMemoryStream.Create;
  msHeadless := TMemoryStream.Create;
  iCounter := 1;
  iPos := -1;
  WriteLn('Stream size ' + IntToStr(aStream.Size));
  while iPos < aStream.Size do
  begin
    iPos := PosBytes(abBODY, aStream, iPos + 1);
    WriteLn('iPos: ' + IntToStr(iPos));
    if iPos > -1 then
    begin
      aStream.Position := iPos + 14;
      if aStream.Position + 155 < aStream.Size then
      begin
        msVCED.WriteByte($F0);
        msVCED.WriteByte($43);
        msVCED.WriteByte($00);
        msVCED.WriteByte($00);
        msVCED.WriteByte($01);
        msVCED.WriteByte($1B);

        msHeadless.CopyFrom(aStream, 155);
        msHeadless.Position:=0;

        msVCED.CopyFrom(msHeadless, 155);

        msHeadless.Position:=0;
        checksum :=0;
        for i := 1 to 155 do
          checksum := checksum + msHeadless.ReadByte;
        checksum := ((not (checksum and 255)) and 127) + 1;

        msVCED.WriteByte(checksum);
        msVCED.WriteByte($F7);
        //msHeadless.SaveToFile(aOutDir + Format('%.6d', [iCounter]) + '.hsyx');
        msVCED.SaveToFile(aOutDir + Format('%.6d', [iCounter]) + '.syx');
        msHeadless.Clear;
        msVCED.Clear;
        Inc(iCounter);
      end;
    end else break;
  end;
  msVCED.Free;
  msHeadless.Free;
end;

procedure XSplitVMEM2VCED(aStream: TMemoryStream; aOutDir: string);
var
  fBank: TDX7BankContainer;
  fVoice: TDX7VoiceContainer;
  msVoice: TMemoryStream;
  i: integer;
  sVoiceName: string;
  slVoices: TStringList;
  iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
  tmpByte: byte;
begin
  fBank := TDX7BankContainer.Create;
  iPos := -1;
  if PosBytes(abSysExID, aStream) >= 0 then
  begin
    aStream.Position := 3;
    tmpByte := aStream.ReadByte;
    if tmpByte = 9 then
    begin
      iPos := 6;
      WriteLn('    VMEM header found');
    end;
  end
  else
  begin
    if aStream.Size = 4096 then
    begin
      iPos := 0;
      WriteLn('    Headerless file');
    end;
  end;
  if iPos <> -1 then
  begin
    fBank.LoadBankFromStream(aStream, iPos);
    slVoices := TStringList.Create;
    for i := 1 to 32 do
    begin
      {WriteLn('Voice name :"'+fBank.GetVoiceName(i)+'"');
      sVoiceName := IncludeTrailingPathDelimiter(aOutDir) +
        fBank.CalculateHash(i) + '.dx7vced.syx';
      WriteLn('Create :' + sVoiceName);     }

      fVoice := TDX7VoiceContainer.Create;
      fBank.GetVoice(i, fVoice);
      WriteLn('Voice name :"' + fVoice.GetVoiceName + '"');
      sVoiceName := IncludeTrailingPathDelimiter(aOutDir) +
        fVoice.CalculateHash + '.dx7vced.syx';
      WriteLn('Create :' + sVoiceName);
      slVoices.AddPair(fVoice.CalculateHash, fVoice.GetVoiceName);

      msVoice := TMemoryStream.Create;
      fVoice.SysExVoiceToStream(1, msVoice);
      msVoice.SaveToFile(sVoiceName);
      msVoice.Free;
      fVoice.Free;
    end;
    slVoices.SaveToFile(IncludeTrailingPathDelimiter(aOutDir) + 'voices.lst');
    slVoices.Free;
  end
  else
    WriteLn('    Not a DX7 VMEM file');
  fBank.Free;
end;

procedure JoinVCED2VMEM(aInputDir: string; aOutFile: string);
var
  slVoices: TStringList;
  i, j: integer;
  fBank: TDX7BankContainer;
  fVoice: TDX7VoiceContainer;
  msVoice: TMemoryStream;
  iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
  iCount: integer;
  iBankCount: integer;
  slList: TStringList;
  tmpByte: byte;
  sOutFile: string;
begin
  slVoices := TStringList.Create;
  FindSYX(aInputDir, slVoices);
  fBank := TDX7BankContainer.Create;
  fVoice := TDX7VoiceContainer.Create;
  if FileExists(IncludeTrailingPathDelimiter(aInputDir) + 'voices.lst') then
  begin
    slList := TStringList.Create;
    slList.LoadFromFile(IncludeTrailingPathDelimiter(aInputDir) + 'voices.lst');
    WriteLn('Joining bank according to voices.lst');
    for i := 0 to slList.Count - 1 do
    begin
      iCount := -1;
      for j := 0 to slVoices.Count - 1 do
      begin
        if pos(slList.Names[i], slVoices[j]) > 0 then
          iCount := j;
      end;
      if iCount <> -1 then
      begin
        msVoice := TMemoryStream.Create;
        msVoice.LoadFromFile(IncludeTrailingPathDelimiter(aInputDir) + slVoices[iCount]);

        iPos := -1;
        if PosBytes(abSysExID, msVoice) >= 0 then
        begin
          msVoice.Position := 3;
          tmpByte := msVoice.ReadByte;
          if tmpByte = 0 then
          begin
            iPos := 6;
            WriteLn(slVoices[iCount] + ' - VCED header found');
          end;
        end
        else
        begin
          if msVoice.Size = 155 then
          begin
            iPos := 0;
            WriteLn(slVoices[iCount] + ' - Headerless file');
          end;
        end;
        if iPos <> -1 then
        begin
          fVoice.Load_VCED_FromStream(msVoice, iPos);
          fBank.SetVoice(i + 1, fVoice);
        end
        else
          WriteLn(slVoices[iCount] + ' - Not a DX7 VCED file');
        msVoice.Free;
      end;
    end;
    fBank.SaveBankToSysExFile(aOutFile);
  end
  else
  begin
    if slVoices.Count < 32 then iBankCount := 0
    else
      iBankCount := 1;
    i := 0;
    while i < slVoices.Count do
    begin
      iCount := (i + 1) mod 32;
      msVoice := TMemoryStream.Create;
      msVoice.LoadFromFile(IncludeTrailingPathDelimiter(aInputDir) + slVoices[i]);
      iPos := -1;
      if PosBytes(abSysExID, msVoice) >= 0 then
      begin
        msVoice.Position := 3;
        tmpByte := msVoice.ReadByte;
        if tmpByte = 0 then
        begin
          iPos := 6;
          WriteLn(slVoices[i] + ' - VCED header found');
        end;
      end
      else
      begin
        if msVoice.Size = 155 then
        begin
          iPos := 0;
          WriteLn(slVoices[i] + ' - Headerless file');
        end;
      end;
      if iPos <> -1 then
      begin
        fVoice.Load_VCED_FromStream(msVoice, iPos);
        if iCount = 0 then iCount := 32;
        fBank.SetVoice(iCount, fVoice);
      end
      else
        WriteLn(slVoices[iCount] + ' - Not a DX7 VCED file');
      msVoice.Free;
      Inc(i);
      if (iCount = 32) or (i = slVoices.Count) then
      begin
        if iBankCount = 0 then
          fBank.SaveBankToSysExFile(aOutFile)
        else
        begin
          sOutFile := ExtractFileNameWithoutExt(aOutFile) + '_' +
            Format('%.3d', [iBankCount]) + '.syx';
          fBank.SaveBankToSysExFile(sOutFile);
          Inc(iBankCount);
          fBank.InitBank;
        end;
      end;
    end;
  end;
  fBank.Free;
  fVoice.Free;
  slVoices.Free;
end;

function CheckVMEMIntegrity(aStream: TMemoryStream; aPos: integer;
  var aNullVoice: boolean): integer;
var
  fBank: TDX7BankContainer;
  fVoice: TDX7VoiceContainer;
  i: integer;
  msRebuilt: TMemoryStream;
  bLoadSaveCheck: boolean;
  bMinMaxCheck: boolean;
  slCorruptedVoices: TStringList;
  slReport: TStringList;
begin
  Result := 0;
  bLoadSaveCheck := True;
  bMinMaxCheck := True;
  aNullVoice := False;

  msRebuilt := TMemoryStream.Create;
  slCorruptedVoices := TStringList.Create;
  slReport := TStringList.Create;
  fBank := TDX7BankContainer.Create;
  fVoice := TDX7VoiceContainer.Create;

  fBank.LoadBankFromStream(aStream, aPos);

  //check if the same after loading and saving again
  //because the VMEM to VCED conversion does some sanity checks
  for i := 1 to 32 do
  begin
    fBank.GetVoice(i, fVoice);
    fVoice.Save_VMEM_ToStream(msRebuilt);
  end;
  aStream.Position := aPos;
  msRebuilt.Position := 0;
  for i := 1 to 4096 do
  begin
    if aStream.ReadByte <> msRebuilt.ReadByte then bLoadSaveCheck := False;
  end;

  //check if the parameters are between min and max values
  for i := 1 to 32 do
  begin
    fBank.GetVoice(i, fVoice);
    slReport.Clear;
    if not fVoice.CheckMinMax(slReport) then
    begin
      bMinMaxCheck := False;
      Inc(Result, slReport.Count);
      slCorruptedVoices.Add(Format('%.2d', [i]) + ': ' + fVoice.GetVoiceName);
      slCorruptedVoices.AddStrings(slReport);
      //check for Nulls in voice name
      if fVoice.HasNullInName then
      begin
        aNullVoice := True;
        slCorruptedVoices.Add('*The voice name contains Null bytes*');
      end;
      slCorruptedVoices.Add('');
    end;
  end;

  if bLoadSaveCheck = False then
  begin
    WriteLn('File corruption:');
    WriteLn('  Bank contains data outside the required bits');
    Result := Result + 10000;
  end;

  if bMinMaxCheck = False then
  begin
    WriteLn('File corruption:');
    WriteLn('  The following voices have data outside the minimum/maximum parameter limits:');
    for i := 0 to slCorruptedVoices.Count - 1 do
      WriteLn('    ' + slCorruptedVoices[i]);
  end;

  msRebuilt.Free;
  fBank.Free;
  fVoice.Free;
  slReport.Free;
  slCorruptedVoices.Free;
end;

function CheckVCEDIntegrity(aStream: TMemoryStream; aPos: integer;
  var aNullVoice: boolean): integer;
var
  fVoice: TDX7VoiceContainer;
  i: integer;
  msRebuilt: TMemoryStream;
  bLoadSaveCheck: boolean;
  slReport: TStringList;
begin
  Result := 0;
  bLoadSaveCheck := True;
  aNullVoice := False;

  msRebuilt := TMemoryStream.Create;
  slReport := TStringList.Create;
  fVoice := TDX7VoiceContainer.Create;

  fVoice.Load_VCED_FromStream(aStream, aPos);

  //check if the same after loading and saving again
  //because the VMEM to VCED conversion does some sanity checks
  fVoice.Save_VMEM_ToStream(msRebuilt);
  fVoice.Load_VMEM_FromStream(msRebuilt, 0);
  msRebuilt.Clear;
  fVoice.Save_VCED_ToStream(msRebuilt);

  aStream.Position := aPos;
  msRebuilt.Position := 0;
  for i := 1 to 155 do
  begin
    if aStream.ReadByte <> msRebuilt.ReadByte then bLoadSaveCheck := False;
  end;
  if bLoadSaveCheck = False then
  begin
    WriteLn('File corruption:');
    WriteLn('  Voice contains data outside the required bits');
    WriteLn('');
    Result := Result + 10000;
  end;

  //check if the parameters are between min and max values
  slReport.Clear;
  if not fVoice.CheckMinMax(slReport) then
  begin
    WriteLn('File corruption:');
    WriteLn('  The voice has parameters outside the min/max values');
    for i := 0 to slReport.Count - 1 do
    begin
      WriteLn('    ' + slReport[i]);
      Inc(Result);
    end;
    WriteLn('');
  end;

  //check for Nulls in voice name
  if fVoice.HasNullInName then
  begin
    aNullVoice := True;
    WriteLn('File corruption:');
    WriteLn('  The voice name contains Null bytes');
  end;

  msRebuilt.Free;
  fVoice.Free;
  slReport.Free;
end;

procedure NormalizeVMEM(aStream: TMemoryStream; aPos: integer; aFile: string);
var
  fBank: TDX7BankContainer;
  fVoice: TDX7VoiceContainer;
  sOutName: string;
  msOutFile: TMemoryStream;
  i: integer;
begin
  sOutName := ExtractFileName(aFile);
  sOutName := ExtractFileNameWithoutExt(sOutName);
  sOutName := IncludeTrailingPathDelimiter(ExtractFileDir(aFile)) +
    sOutName + '.normalized.syx';

  msOutFile := TMemoryStream.Create;

  fBank := TDX7BankContainer.Create;
  fVoice := TDX7VoiceContainer.Create;

  fBank.LoadBankFromStream(aStream, aPos);

  for i := 1 to 32 do
  begin
    fBank.GetVoice(i, fVoice);
    fVoice.Normalize;
    fBank.SetVoice(i, fVoice);
  end;
  fBank.SysExBankToStream(1, msOutFile);
  msOutFile.SaveToFile(sOutName);

  msOutFile.Free;
  fBank.Free;
  fVoice.Free;
end;

procedure NormalizeVCED(aStream: TMemoryStream; aPos: integer; aFile: string);
var
  fVoice: TDX7VoiceContainer;
  sOutName: string;
  msOutFile: TMemoryStream;
begin
  sOutName := ExtractFileName(aFile);
  sOutName := ExtractFileNameWithoutExt(sOutName);
  sOutName := IncludeTrailingPathDelimiter(ExtractFileDir(aFile)) +
    sOutName + '.normalized.syx';

  msOutFile := TMemoryStream.Create;

  fVoice := TDX7VoiceContainer.Create;

  fVoice.Load_VCED_FromStream(aStream, aPos);
  fVoice.Normalize;

  fVoice.SysExVoiceToStream(1, msOutFile);
  msOutFile.SaveToFile(sOutName);

  msOutFile.Free;
  fVoice.Free;
end;

function Test_VCEDHash(aFile: string): string;
var
  fVoice: TDX7VoiceContainer;
  iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
  tmpByte: byte;
  fStream: TMemoryStream;
  fTmpStream: TMemoryStream;
  i: integer;
  bStreamCheck: boolean;
begin
  Result := '0';
  fVoice := TDX7VoiceContainer.Create;
  fStream := TMemoryStream.Create;
  fStream.LoadFromFile(aFile);
  iPos := -1;
  if PosBytes(abSysExID, fStream) >= 0 then
  begin
    fStream.Position := 3;
    tmpByte := fStream.ReadByte;
    if tmpByte = 0 then
    begin
      iPos := 6;
      WriteLn('    VCED header found');
    end;
  end
  else
  begin
    if fStream.Size = 155 then
    begin
      iPos := 0;
      WriteLn('    Headerless file');
    end;
  end;
  try
    if iPos <> -1 then
    begin
      Writeln('Stream data before loading:');
      WriteLn(SysExStreamToStr(fStream));
      fVoice.Load_VCED_FromStream(fStream, iPos);
      fTmpStream := TMemoryStream.Create;
      fVoice.Add_VCED_ToStream(fTmpStream);
      Writeln('Stream data after saving:');
      WriteLn(SysExStreamToStr(fTmpStream));
      if (fStream.Size - 8) = fTmpStream.Size then
      begin
        bStreamCheck := True;
        fTmpStream.Position := 0;
        //cut off the headers in a dirty way
        fStream.Position := 6;
        for i := 0 to fStream.Size - 9 do
          if fStream.ReadByte <> fTmpStream.ReadByte then bStreamCheck := False;
        if bStreamCheck then WriteLn('Stream check PASS')
        else
          WriteLn('Stream check FAIL');
      end
      else
        WriteLn('Load and save streams not the same size');
      Result := fVoice.CalculateHash;
      WriteLn('Hash summ from record: ' + Result);
      fTmpStream.SetSize(144);
      fTmpStream.Position := 0;
      WriteLn('Hash summ from stream: ' +
        THashFactory.TCrypto.CreateSHA2_256().ComputeStream(fTmpStream).ToString());
      fTmpStream.Free;
    end
    else
      WriteLn('    Not a DX7 VCED file');
  finally
    fStream.Free;
    fVoice.Free;
  end;
end;

function Test_VMEMHash(aFile: string; aVoiceNr: integer = 1): string;
var
  fBank: TDX7BankContainer;
  fVoice: TDX7VoiceContainer;
  iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
  tmpByte: byte;
  fStream: TMemoryStream;
  fTmpStream: TMemoryStream;
begin
  Result := '0';

  fStream := TMemoryStream.Create;
  fStream.LoadFromFile(aFile);

  fBank := TDX7BankContainer.Create;
  iPos := -1;
  if PosBytes(abSysExID, fStream) >= 0 then
  begin
    fStream.Position := 3;
    tmpByte := fStream.ReadByte;
    if tmpByte = 9 then
    begin
      iPos := 6;
      WriteLn('    VMEM header found');
    end;
  end
  else
  begin
    if fStream.Size = 4096 then
    begin
      iPos := 0;
      WriteLn('    Headerless file');
    end;
  end;
  if iPos <> -1 then
  begin
    fBank.LoadBankFromStream(fStream, iPos);
    fTmpStream := TMemoryStream.Create;
    fVoice := TDX7VoiceContainer.Create;
    fBank.GetVoice(aVoiceNr, fVoice);
    Writeln('Loading voice ' + IntToStr(aVoiceNr) + ': ' + fVoice.GetVoiceName);
    fVoice.Add_VCED_ToStream(fTmpStream);
    Writeln('Stream data after loading from VMEM to VCED:');
    WriteLn(SysExStreamToStr(fTmpStream));
    Result := fVoice.CalculateHash;
    WriteLn('Hash summ from record: ' + Result);
    fTmpStream.SetSize(144);
    fTmpStream.Position := 0;
    WriteLn('Hash summ from stream: ' +
      THashFactory.TCrypto.CreateSHA2_256().ComputeStream(fTmpStream).ToString());
    fTmpStream.Free;
    fVoice.Free;
  end
  else
    WriteLn('    Not a DX7 VMEM file');
  fBank.Free;
  fStream.Free;
end;

end.
