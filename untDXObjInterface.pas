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
  Classes, SysUtils, untDX7Voice, untDX7Bank, untDXUtils, untUtils;

procedure GetVoices(aStream: TMemoryStream; var aList: TStringList);
procedure SplitVMEM2VCED(aStream: TMemoryStream; aOutDir: string);
procedure XSplitVMEM2VCED(aStream: TMemoryStream; aOutDir: string);
procedure JoinVCED2VMEM(aInputDir: string; aOutFile: string);


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
      sVoiceName := IncludeTrailingPathDelimiter(aOutDir) +
        fBank.CalculateHash(i) + '.dx7vced.syx';
      WriteLn('Create :' + sVoiceName);
      fVoice := TDX7VoiceContainer.Create;
      fBank.GetVoice(i, fVoice);
      slVoices.AddPair(fBank.CalculateHash(i), fBank.GetVoiceName(i));
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
  slList: TStringList;
  tmpByte: byte;
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
  end
  else
  begin
    //do not load more than 32 files
    if slVoices.Count > 32 then
    begin
      iCount := 32;
      WriteLn('Directory contains more than 32 voices');
      WriteLn('Just the first 32 voices will be joined');
    end
    else
      iCount := slVoices.Count;
    for i := 0 to iCount - 1 do
    begin
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
  fBank.Free;
  fVoice.Free;
  slVoices.Free;
end;

end.
