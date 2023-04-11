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
procedure JoinVCED2VMEM(aInputDir: string; aOutFile: string);


implementation

procedure GetVoices(aStream: TMemoryStream; var aList: TStringList);
var
  fBank: TDX7BankContainer;
  i: integer;
  iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
begin
  fBank := TDX7BankContainer.Create;
  aList.Clear;
  if PosBytes(abSysExID, aStream) >= 0 then
  begin
    iPos := 6;
    aList.Add('    DX header found');
  end
  else
  begin
    iPos := 0;
    aList.Add('    Headerless file');
  end;
  fBank.LoadBankFromStream(aStream, iPos);
  for i := 1 to 32 do
    aList.Add(Format('%.2d', [i]) + ': ' + fBank.GetVoiceName(i));
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
begin
  fBank := TDX7BankContainer.Create;
  if PosBytes(abSysExID, aStream) >= 0 then
  begin
    iPos := 6;
    WriteLn('    DX header found');
  end
  else
  begin
    iPos := 0;
    WriteLn('    Headerless file');
  end;
  fBank.LoadBankFromStream(aStream, iPos);
  slVoices := TStringList.Create;
  for i := 1 to 32 do
  begin
    sVoiceName := IncludeTrailingPathDelimiter(aOutDir) + Format('%.2d', [i]) +
      '_' + GetValidFileName(fBank.GetVoiceName(i)) + '.syx';
    WriteLn('Create :' + sVoiceName);
    fVoice := TDX7VoiceContainer.Create;
    fBank.GetVoice(i, fVoice);
    slVoices.Add(Format('%.2d', [i]) + ': ' + fBank.GetVoiceName(i) +
      #9 + fBank.CalculateHash(i));
    msVoice := TMemoryStream.Create;
    fVoice.SysExVoiceToStream(1, msVoice);
    msVoice.SaveToFile(sVoiceName);
    msVoice.Free;
    fVoice.Free;
  end;
  slVoices.SaveToFile(IncludeTrailingPathDelimiter(aOutDir) + 'voices.lst');
  slVoices.Free;
  fBank.Free;
end;

procedure JoinVCED2VMEM(aInputDir: string; aOutFile: string);
var
  slVoices: TStringList;
  i: integer;
  fBank: TDX7BankContainer;
  fVoice: TDX7VoiceContainer;
  msVoice: TMemoryStream;
  iPos: integer;
  abSysExID: array[0..1] of byte = ($F0, $43);
  iCount: integer;
begin
  slVoices := TStringList.Create;
  FindSYX(aInputDir, slVoices);
  fBank := TDX7BankContainer.Create;
  fVoice := TDX7VoiceContainer.Create;
  //do not load more than 32 files
  if slVoices.Count > 32 then iCount := 32
  else
    iCount := slVoices.Count;
  for i := 0 to iCount - 1 do
  begin
    msVoice := TMemoryStream.Create;
    msVoice.LoadFromFile(IncludeTrailingPathDelimiter(aInputDir) + slVoices[i]);
    if PosBytes(abSysExID, msVoice) >= 0 then
    begin
      iPos := 6;
      WriteLn(slVoices[i] + '    DX header found');
    end
    else
    begin
      iPos := 0;
      WriteLn(slVoices[i] + '    Headerless file');
    end;
    fVoice.Load_VCED_FromStream(msVoice, iPos);
    fBank.SetVoice(i + 1, fVoice);
    msVoice.Free;
  end;
  fBank.SaveBankToSysExFile(aOutFile);
  fBank.Free;
  fVoice.Free;
  slVoices.Free;
end;

end.
