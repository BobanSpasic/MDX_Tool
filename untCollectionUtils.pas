{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Boban Spasic

 Unit description:
 Methods to manage collection of DX7 banks and voices

}

unit untCollectionUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, untDX7Voice, untDX7Bank, untDXUtils, untUtils;

procedure Analyze(aDir, aReportDir: string);
procedure Compare(aMaster, aIncoming, aReport: string);

implementation

procedure Analyze(aDir, aReportDir: string);
var
  slFiles: TStringList;
  slHashes: TStringList;
  fOutput: string;
  i, j: integer;
  fDXBank: TDX7BankContainer;
  fDXVoice: TDX7VoiceContainer;
  msFileStream: TMemoryStream;
  iStart, iDmp: integer;
begin
  //calculate the path and name for the output hash list file
  fOutput := ExcludeTrailingPathDelimiter(aDir);
  fOutput := Copy(fOutput, RPos(PathDelim, fOutput), Length(fOutput) -
    RPos(PathDelim, fOutput) + 1);
  fOutput := IncludeTrailingPathDelimiter(aReportDir) + fOutput + '.hsl';

  slFiles := TStringList.Create;
  slHashes := TStringList.Create;
  FindSYXRecursive(aDir, slFiles);
  for i := 0 to slFiles.Count - 1 do
  begin
    WriteLn('Processing ' + slFiles[i]);
    if not FileExists(slFiles[i]) then Break;
    ;
    msFileStream := TMemoryStream.Create;
    msFileStream.LoadFromFile(slFiles[i]);
    if msFileStream.Size = 163 then
    begin
      iStart := 0;
      iDmp := 0;
      if ContainsDX7VoiceDump(msFileStream, iStart, iDmp) then
      begin
        fDXVoice := TDX7VoiceContainer.Create;
        fDXVoice.Load_VCED_FromStream(msFileStream, iDmp);
        slHashes.AddPair(fDXVoice.CalculateHash, slFiles[i] + #9 +
          '01: ' + fDXVoice.GetVoiceName);
        fDXVoice.Free;
      end;
    end;
    if msFileStream.Size = 4104 then
    begin
      iStart := 0;
      iDmp := 0;
      if ContainsDX7BankDump(msFileStream, iStart, iDmp) then
      begin
        fDXBank := TDX7BankContainer.Create;
        fDXBank.LoadBankFromStream(msFileStream, iDmp);
        fDXVoice := TDX7VoiceContainer.Create;
        for j := 1 to 32 do
        begin
          fDXBank.GetVoice(j, fDXVoice);
          slHashes.AddPair(fDXVoice.CalculateHash, slFiles[i] + #9#9 +
            Format('%.2d', [j]) + ' ' + fDXVoice.GetVoiceName);
        end;
        fDXVoice.Free;
        fDXBank.Free;
      end;
    end;
    msFileStream.Free;
  end;
  if slHashes.Count > 0 then
    slHashes.SaveToFile(fOutput);
  WriteLn('Processed ' + IntToStr(slFiles.Count) + ' files');
  slFiles.Free;
  slHashes.Free;
end;

procedure Compare(aMaster, aIncoming, aReport: string);
var
  slMaster: TStringList;
  slMasterHash: TStringList;
  slIncomingHash: TStringList;
  slIncoming: TStringList;
  slMasterHasMore: TStringList;
  slIncomingHasMore: TStringList;
  slMasterDuplicates: TStringList;
  slIncomingDuplicates: TStringList;
  i: integer;
begin
  slMaster := TStringList.Create;
  slIncoming := TStringList.Create;
  slMasterHasMore := TStringList.Create;
  slIncomingHasMore := TStringList.Create;
  slMasterHash := TStringList.Create;
  slIncomingHash := TStringList.Create;
  slMasterDuplicates := TStringList.Create;
  slIncomingDuplicates := TStringList.Create;

  slMasterHash.Sorted := True;
  slMasterHash.Duplicates := dupError;

  slIncomingHash.Sorted := True;
  slIncomingHash.Duplicates := dupIgnore;

  slMaster.LoadFromFile(aMaster);
  slIncoming.LoadFromFile(aIncoming);

  //fill hash lists
  for i := 0 to slMaster.Count - 1 do
    try
      slMasterHash.Add(slMaster.Names[i]);
    except
      on e: Exception do
      begin
        WriteLn('Entry ' + slMaster[i] + ' is already a duplicate in master list');
        slMasterDuplicates.Add(slMaster[i]);
      end;
    end;
  for i := 0 to slIncoming.Count - 1 do
    try
      slIncomingHash.Add(slIncoming.Names[i]);
    except
      on e: Exception do
      begin
        WriteLn('Entry ' + slIncoming[i] + ' is already a duplicate in incoming list');
        slIncomingDuplicates.Add(slIncoming[i]);
      end;
    end;

  WriteLn('Processing stage 1 with ' + IntToStr(slMaster.Count) + ' entries');
  for i := 0 to slMaster.Count - 1 do
  begin
    if (i mod 1000) = 0 then WriteLn('Processed ' + IntToStr(i) + ' entries');
    if slIncomingHash.IndexOf(slMaster.Names[i]) = -1 then
      slMasterHasMore.Add(slMaster[i]);
  end;
  WriteLn('Processing stage 2 with ' + IntToStr(slIncoming.Count) + ' entries');
  for i := 0 to slIncoming.Count - 1 do
  begin
    if (i mod 1000) = 0 then WriteLn('Processed ' + IntToStr(i) + ' entries');
    if slMasterHash.IndexOf(slIncoming.Names[i]) = -1 then
      slIncomingHasMore.Add(slIncoming[i]);
  end;

  slMasterHasMore.SaveToFile(IncludeTrailingPathDelimiter(aReport) +
    'MasterHasMore.dif');
  slIncomingHasMore.SaveToFile(IncludeTrailingPathDelimiter(aReport) +
    'IncomingHasMore.dif');
  slMasterDuplicates.SaveToFile(IncludeTrailingPathDelimiter(aReport) +
    'MasterInternal.duplicates');
  slIncomingDuplicates.SaveToFile(IncludeTrailingPathDelimiter(aReport) +
    'IncomingInternal.duplicates');

  slMaster.Free;
  slIncoming.Free;
  slMasterHasMore.Free;
  slIncomingHasMore.Free;
  slMasterHash.Free;
  slIncomingHash.Free;
  slMasterDuplicates.Free;
  slIncomingDuplicates.Free;
end;

end.
