{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Boban Spasic

 Unit description:
 This unit implements the detection/recognition of DX-Series SysEx Messages.
 Sequencer and some other (for me less important) headers are not implemented.
 Not all the MSB/LSB Data could be found in Yamaha's documentation. I've got some
 of them by inspecting various SysEx dumps.
}

unit untDXUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, untUtils;

type
  TDXSysExHeader = record
    f0: byte;
    id: byte;  // $43 or d67 = Yamaha
    sc: byte;  // s = sub-status, c = channel  0sssnnnn
    {
    s=0 - voice/suppl./perf. dump
    s=1 - direct single parameter change
    s=2 - dump request
    }
    f: byte;
    {
    f=0 - single_voice DX7
    f=1 - single_function TX7
    f=2 - bulk_function TX7
    f=3 - single_voice DX11/TX81z/V50/DX21
    f=4 - bulk_voice DX11/TX81z/V50/DX21
    f=5 - single_supplement DX7II
    f=6 - bulk_supplement DX7II
    f=7 -
    f=8 -
    f=9 - bulk_voice DX7
    f=7E - see TDXSysExUniversalDump
    }
    msb: byte; // MSB packet size
    lsb: byte; // LSB packet size
    {
    MSB/LSB=155 - single_voice DX7        expanded VCED
    MSB/LSB=93 - single_voice V50/DX21    expanded VCED
    MSB/LSB=49 - single_supplement DX7II  expanded ACED
    MSB/LSB=1120 - bulk_supplement DX7II  packed AMEM
    MSB/LSB=4096 - bulk_voice DX7         packed VMEM
    MSB/LSB=4096 - bulk_voice V50/DX21    packed VMEM
    }
    chk: byte;
    f7: byte;
  end;

  TDXSysExUniversalDump = record
    f0: byte;
    id: byte;  // $43 / #67 = Yamaha
    sc: byte;  // s = 0, c = channel  0sssnnnn
    f: byte;   // f=7E
    classification: array[0..3] of char; // LM _ _
    data_format: array [0..5] of char;
    // classification and data_format can repeat more than once in the dump
    { underscores are spaces
    DX7II
    LM__8973PE    61 byte   DX7II Performance Edit Buffer                     1x
    LM__8973PM  1642 byte   DX7II Packed 32 Performance                       1x
    LM__8973S_   112 byte   DX7II System Set-up                               1x
    LM__MCRYE_   266 byte   Micro Tuning Edit Buffer                          1x
    LM__MCRYMx   266 byte   Micro Tuning with Memory #x=(0,1)                 2x
    LM__MCRYC_   266 byte   Micro Tuning Cartridge                           64x
    LM__FKSYE_   502 byte   Fractional Scaling Edit Buffer                    1x
    LM__FKSYC_   502 byte   Fractional Scaling in Cartridge with Memory #    32x
    V50/DX11/TX81z
    LM__8976AE    33 byte   ACED    TX81Z
    LM__8023AE    20 byte   ACED2   DX11
    LM__8073AE    30 byte   ACED3   V50
    LM__8976PE   120 byte   PCED    DX11
    LM__8073PE    43 byte   PCED2   V50
    LM__8976PM  2442 byte   PMEM    DX11
    LM__8073PM   810 byte   PMEM2   V50
    LM__8976Sx    xx byte   System
    LM__MCRTE0    34 byte   Micro Tuning Edit Buffer OCT
    LM__MCRTE1   274 byte   Micro Tuning Edit Buffer FULL
    LM__8023S0    26 byte   System
    LM__8073S0    42 byte   System
    }
  end;

function ContainsDX_SixOP_Data_New(dmp: TMemoryStream; var StartPos: integer;
  const Report: TStrings): boolean;
function ContainsDX7VoiceDump(dmp: TMemoryStream;
  var StartPos, StartDmp: integer): boolean;
function ContainsDX7BankDump(dmp: TMemoryStream;
  var StartPos, StartDmp: integer): boolean;
function RepairDX7SysEx(aFileName: string; const Report: TStrings): boolean;
function CropHeaders(aFileName: string): boolean;
function CheckSum(dmp: TMemoryStream; StartPos, DumpLen: integer): boolean;
function PosBytes(aBytes: array of byte; aStream: TStream;
  aFromPos: integer = 0): integer;
function Printable(c: char): char;

implementation

operator in (const AByte: byte; const AArray: array of byte): boolean; inline;
var
  Item: byte;
begin
  for Item in AArray do
    if Item = AByte then
      Exit(True);

  Result := False;
end;

function PosBytes(aBytes: array of byte; aStream: TStream;
  aFromPos: integer = 0): integer;
var
  i, j: integer;
  arrLen: integer;
begin
  Result := -1;
  arrLen := Length(aBytes);
  if arrLen = 0 then Exit;
  if arrLen >= aStream.Size then Exit;
  if (aFromPos + arrLen) > aStream.Size then Exit;
  aStream.Position := aFromPos;
  while aStream.Position <= (aStream.Size - arrLen) do
  begin
    i := aStream.Position;
    if aStream.ReadByte = aBytes[0] then
    begin
      Result := i;
      if arrLen = 1 then Exit;
      for j := 1 to High(aBytes) do
      begin
        if Result <> -1 then
          if aStream.ReadByte = aBytes[j] then
          begin
            Result := i;
          end
          else
          begin
            Result := -1;
            //Exit;
          end;
        //Exit;
      end;
      if Result <> -1 then Exit;
    end;
  end;
end;

function ContainsDX_SixOP_Data_New(dmp: TMemoryStream; var StartPos: integer;
  const Report: TStrings): boolean;
var
  abSysExID: array[0..1] of byte = ($F0, $43);
  abSysExType: array [0..6] of byte = ($00, $01, $02, $05, $06, $09, $7E);
  rHeader: TDXSysExHeader;
  iDumpStart: integer; // position of $F0
  iDataSize: integer;  // calculated from msb and lsb bytes
  iDumpEnd: integer;   // position of $F7
  iCalcChk: integer;
  iRep: integer;
  i: integer;
begin
  iDumpStart := -1;
  iDataSize := -1;
  iDumpEnd := -1;
  Result := False;
  iRep := StartPos;
  while iRep < dmp.Size do
  begin
    iDumpStart := PosBytes(abSysExID, dmp, iRep);
    if iDumpStart > -1 then
    begin
      //Report.Add('Header found at: ' + IntToStr(iDumpStart));
      //do not read behind the end of the stream
      if (iDumpStart + 8) <= dmp.Size then
      begin
        dmp.Position := iDumpStart;
        rHeader.f0 := dmp.ReadByte;
        rHeader.id := dmp.ReadByte;
        rHeader.sc := dmp.ReadByte;
        rHeader.f := dmp.ReadByte;
        rHeader.msb := dmp.ReadByte;
        rHeader.lsb := dmp.ReadByte;
        if rHeader.f in abSysExType then
        begin
          StartPos := dmp.Position;
          if rHeader.f = $00 then
            Report.Add('DX7/DX9 Voice - VCED at position ' +
              IntToStr(StartPos));
          if rHeader.f = $01 then
            Report.Add('TX7/TX816 Performance - PCED at position ' +
              IntToStr(StartPos));
          if rHeader.f = $02 then
            Report.Add('TX7/TX816 Performance Bank - PMEM at position ' +
              IntToStr(StartPos));
          if rHeader.f = $05 then
            Report.Add('DX7II Voice Supplement - ACED at position ' +
              IntToStr(StartPos));
          if rHeader.f = $06 then
            Report.Add('DX7II Voice Bank Supplement - AMEM at position ' +
              IntToStr(StartPos));
          if rHeader.f = $09 then
            Report.Add('DX7/DX9 Voice Bank - VMEM at position ' +
              IntToStr(StartPos));
        end
        else
        begin
          Report.Add('Unknown Yamaha DX dump type: ' + IntToStr(rHeader.f));
        end;
        iDataSize := (rHeader.msb shl 7) + rHeader.lsb;
        Report.Add('Calculated data size: ' + IntToStr(iDataSize));
        if (iDumpStart + iDataSize + 8) <= dmp.Size then
        begin
          iDumpEnd := PosBytes($F7, dmp, iDumpStart + 1);
          if iDumpEnd = -1 then iDumpEnd := dmp.Size;
          Report.Add('Real data size: ' + IntToStr(iDumpEnd - iDumpStart - 7));
          if iDumpEnd = (iDumpStart + iDataSize + 7) then
          begin
            dmp.Position := iDumpEnd - 1;
            rHeader.chk := dmp.ReadByte;
            iCalcChk := 0;
            dmp.Position := iDumpStart + 6;
            for i := 1 to iDataSize do
              iCalcChk := iCalcChk + dmp.ReadByte;
            iCalcChk := ((not (iCalcChk and 255)) and 127) + 1;
            if (rHeader.chk = iCalcChk) or (rHeader.chk = 0) then
            begin
              Report.Add('Checksum match');
              Result := True;
            end
            else
            begin
              Report.Add('Checksum mismatch');
              Result := False;
            end;
          end
          else
          begin
            Report.Add('Data size mismatch');
          end;
          iRep := iDumpEnd + 1;
        end
        else
        begin
          Report.Add('File too short');
          Exit;
        end;
      end
      else
      begin
        Report.Add('File too short');
        Exit;
      end;
    end
    else
    begin
      Report.Add('DX header not found');
      Exit;
    end;
  end;
end;


function ContainsDX7VoiceDump(dmp: TMemoryStream;
  var StartPos, StartDmp: integer): boolean;
var
  dummy: byte;
begin
  if StartPos <= dmp.Size then
  begin
    dmp.Position := StartPos;
    StartPos := -1;
    //ToDo while loop
    while (StartPos = -1) and (dmp.Position < dmp.Size) do
      if dmp.ReadByte = $F0 then                  // $F0 - SysEx
        StartPos := dmp.Position - 1;
    if StartPos <> -1 then
    begin
      dummy := dmp.ReadByte;
      if not (dummy = $43) then StartPos := -1;     // $43 - Yamaha
      dummy := dmp.ReadByte;                        // sub-status + channel number
      dummy := dmp.ReadByte;
      if not (dummy = $00) then StartPos := -1;     // $00 - 1 Voice dump
      dummy := dmp.ReadByte;
      if not (dummy = $01) then StartPos := -1;     // byte count MS
      dummy := dmp.ReadByte;
      if not (dummy = $1B) then StartPos := -1;     // byte count LS
    end;
    if StartPos <> -1 then
    begin
      Result := True;
      StartDmp := StartPos + 6;
    end
    else
    begin
      Result := False;
      StartDmp := -1;
    end;
  end
  else
  begin
    StartPos := -1;
    Result := False;
  end;
end;

function ContainsDX7BankDump(dmp: TMemoryStream;
  var StartPos, StartDmp: integer): boolean;
var
  dummy: byte;
begin
  if StartPos <= dmp.Size then
  begin
    dmp.Position := StartPos;
    StartPos := -1;
    while (StartPos = -1) and (dmp.Position < dmp.Size - 6) do
    begin
      dummy := dmp.ReadByte;
      if dummy = $F0 then                           // $F0 - SysEx
      begin
        StartPos := dmp.Position - 1;
        dummy := dmp.ReadByte;
        if not (dummy = $43) then StartPos := -1;     // $43 - Yamaha
        dummy := dmp.ReadByte;                        // sub-status + channel number
        dummy := dmp.ReadByte;
        if not (dummy = $09) then StartPos := -1;     // $09 - 32 Voice dump
        dummy := dmp.ReadByte;
        if not (dummy = $20) then StartPos := -1;     // byte count MS
        dummy := dmp.ReadByte;
        if not (dummy = $00) then StartPos := -1;     // byte count LS
      end;
    end;
    if StartPos <> -1 then
      if (dmp.Size - StartPos) < 4104 then
        StartPos := -1;  //file too short

    if StartPos <> -1 then
    begin
      Result := True;
      StartDmp := StartPos + 6;
    end
    else
    begin
      Result := False;
      StartDmp := -1;
    end;
  end
  else
  begin
    StartPos := -1;
    Result := False;
  end;
end;

function RepairDX7SysEx(aFileName: string; const Report: TStrings): boolean;
var
  sDirRepaired: string;
  sNameRepaired: string;
  msToRepair: TMemoryStream;
  msRepaired: TMemoryStream;
  i: integer;

  abSysExID: array[0..1] of byte = ($F0, $43);
  rHeader: TDXSysExHeader;
  iDumpStart: integer; // position of $F0
  //iDataSize: integer;  // calculated from msb and lsb bytes
  iDumpEnd: integer;   // position of $F7
  iCalcChk: integer;
  iRep: integer;
  needRewriteHeader: boolean;
  needExtractDump: boolean;
  needFillDump: boolean;
begin
  sNameRepaired := ExtractFileName(aFileName);
  sNameRepaired := ExtractFileNameWithoutExt(sNameRepaired);
  sDirRepaired := IncludeTrailingPathDelimiter(ExtractFileDir(aFileName));
  //sNameRepaired := sDirRepaired + sNameRepaired + '_DX7_repaired.syx';
  Result := False;
  msToRepair := TMemoryStream.Create;
  msRepaired := TMemoryStream.Create;
  msToRepair.LoadFromFile(aFileName);

  iDumpStart := -1;
  //iDataSize := -1;
  iDumpEnd := -1;
  Result := False;
  iRep := 0;
  while iRep < msToRepair.Size do
  begin
    needRewriteHeader := False;
    needExtractDump := False;
    needFillDump := False;
    iDumpStart := PosBytes(abSysExID, msToRepair, iRep);
    if iDumpStart > -1 then
    begin
      if (iDumpStart + 8) <= msToRepair.Size then
      begin
        msToRepair.Position := iDumpStart;
        rHeader.f0 := msToRepair.ReadByte;
        rHeader.id := msToRepair.ReadByte;
        rHeader.sc := msToRepair.ReadByte;
        rHeader.f := msToRepair.ReadByte;
        rHeader.msb := msToRepair.ReadByte;
        rHeader.lsb := msToRepair.ReadByte;

        if (rHeader.f = $09) and (rHeader.f0 = $F0) and (rHeader.id = $43) then
        begin
          //contains data before header
          if (iDumpStart > 0) and (msToRepair.Size >= 4104 + iDumpStart) then
            needExtractDump := True;
          //wrong msb/lsb data in header
          if ((rHeader.msb shl 7) + rHeader.lsb) <> 4096 then needRewriteHeader := True;
          //search for end of the dump $F7
          iDumpEnd := PosBytes($F7, msToRepair, iDumpStart + 1);
          //no $F7 found - take the end of file as the end of the dump
          if iDumpEnd = -1 then
          begin
            iDumpEnd := msToRepair.Size;
            needFillDump := True;
          end;
          //end of the dump comes to early
          if (iDumpEnd - iDumpStart - 7) < 4096 then needFillDump := True;
          if (msToRepair.Size - 1) > iDumpEnd then needExtractDump := True;
          //is checksum OK?
          if iDumpEnd = (iDumpStart + 4103) then
          begin
            msToRepair.Position := iDumpEnd - 1;
            rHeader.chk := msToRepair.ReadByte;
            iCalcChk := 0;
            msToRepair.Position := iDumpStart + 6;
            for i := 1 to 4096 do
              iCalcChk := iCalcChk + msToRepair.ReadByte;
            iCalcChk := ((not (iCalcChk and 255)) and 127) + 1;
            if rHeader.chk <> iCalcChk then
              needRewriteHeader := True;
          end
          else
            needFillDump := True;
          iRep := iDumpEnd + 1;
        end
        else
        begin
          if PosBytes(abSysExID, msToRepair, iRep + 1) = -1 then iRep := msToRepair.Size;
        end;
      end
      else
      begin
        Report.Add('File too short. It can''t be repaired');
        Result := True;
        Exit;
      end;
    end
    else
    begin
      iRep := msToRepair.Size;
      if msToRepair.Size = 4096 then
      begin
        msRepaired.WriteByte($F0);
        msRepaired.WriteByte($43);
        msRepaired.WriteByte($00);
        msRepaired.WriteByte($09);
        msRepaired.WriteByte($20);
        msRepaired.WriteByte($00);
        msToRepair.Position := 0;
        msRepaired.CopyFrom(msToRepair, msToRepair.Size);
        iCalcChk := 0;
        msToRepair.Position := 0;
        for i := 1 to 4096 do
          iCalcChk := iCalcChk + msToRepair.ReadByte;
        iCalcChk := ((not (iCalcChk and 255)) and 127) + 1;
        msRepaired.WriteByte(iCalcChk);
        msRepaired.WriteByte($F7);
        sNameRepaired := sDirRepaired + sNameRepaired + '_DX7_repaired.syx';
        msRepaired.SaveToFile(sNameRepaired);
        Report.Add('File is just 4096 bytes long. Writting headers.');
        Result := True;
      end;
    end;
    if needExtractDump or needFillDump or needRewriteHeader then
    begin
      //ToDo do recursive search
      msRepaired.WriteByte($F0);
      msRepaired.WriteByte($43);
      msRepaired.WriteByte($00);
      msRepaired.WriteByte($09);
      msRepaired.WriteByte($20);
      msRepaired.WriteByte($00);
      msToRepair.Position := iDumpStart + 6;
      msRepaired.CopyFrom(msToRepair, iDumpEnd - iDumpStart - 6);
      while msRepaired.Size < 4102 do
        msRepaired.WriteByte($00);
      iCalcChk := 0;
      msRepaired.Position := 6;
      for i := 1 to 4096 do
        iCalcChk := iCalcChk + msRepaired.ReadByte;
      iCalcChk := ((not (iCalcChk and 255)) and 127) + 1;
      msRepaired.WriteByte(iCalcChk);
      msRepaired.WriteByte($F7);
      sNameRepaired := sDirRepaired + sNameRepaired + '_' +
        IntToStr(iDumpStart) + '_DX7_repaired.syx';
      msRepaired.SaveToFile(sNameRepaired);
      Result := True;
    end;
    if needExtractDump then
      Report.Add('Extracting VMEM dump from a file that also contains other data.');
    if needFillDump then Report.Add(
        'Fill dump with zeros in the place of missing bytes. Please check this file by hand.');
    if needRewriteHeader then Report.Add('Rewritting headers.');
  end;
  msToRepair.Free;
  msRepaired.Free;
end;

function CropHeaders(aFileName: string): boolean;
var
  msToCrop: TMemoryStream;
  msCropped: TMemoryStream;
  abSysExID: array[0..1] of byte = ($F0, $43);
  sNameCropped: string;
  sDirCropped: string;
  tmpByte: byte;
begin
  Result := False;
  sNameCropped := ExtractFileName(aFileName);
  sNameCropped := ExtractFileNameWithoutExt(sNameCropped);
  sDirCropped := IncludeTrailingPathDelimiter(ExtractFileDir(aFileName));
  msToCrop := TMemoryStream.Create;
  msCropped := TMemoryStream.Create;
  msToCrop.LoadFromFile(aFileName);
  if PosBytes(abSysExID, msToCrop) = 0 then  //check for DX7 header
  begin
    if msToCrop.Size = 4104 then
    begin
      msToCrop.Position := 3;
      tmpByte := msToCrop.ReadByte;
      if tmpByte = 9 then     //VMEM
      begin
        msToCrop.Position := 6;
        msCropped.CopyFrom(msToCrop, 4096);
        msCropped.SaveToFile(sDirCropped + sNameCropped + '.dx7hdlsvmem.syx');
        Result := True;
      end;
    end;

    if msToCrop.Size = 163 then
    begin
      msToCrop.Position := 3;
      tmpByte := msToCrop.ReadByte;
      if tmpByte = 0 then     //VCED
      begin
        msToCrop.Position := 6;
        msCropped.CopyFrom(msToCrop, 155);
        msCropped.SaveToFile(sDirCropped + sNameCropped + '.dx7hdlsvced.syx');
        Result := True;
      end;
    end;
  end;

  msToCrop.Free;
  msCropped.Free;
end;

//function RepairDX7SysEx(aFileName, aOutFileName: string;
//  const Report: TStrings): boolean;
//var
//  msToRepair: TMemoryStream;
//  msRepaired: TMemoryStream;
//  checksum: integer;
//  bChk: byte;
//  i, j: integer;
//begin
//  Result := False;
//  msToRepair := TMemoryStream.Create;
//  msRepaired := TMemoryStream.Create;
//  msToRepair.LoadFromFile(aFileName);
//  Report.Add('Repairing ' + aFileName);

//  //32 VCEDs without header
//  if msToRepair.Size = 4960 then
//  begin
//    Report.Add('File size = 4960. Saving separate VCED SysEx dumps.');
//    msToRepair.Position := 0;
//    for j := 1 to 32 do
//    begin
//      try
//        msRepaired.Clear;
//        msRepaired.Size := 0;
//        //write DX7 VCED header
//        msRepaired.WriteByte($F0);
//        msRepaired.WriteByte($43);
//        msRepaired.WriteByte($00);
//        msRepaired.WriteByte($00);
//        msRepaired.WriteByte($01);
//        msRepaired.WriteByte($1B);

//        //copy data
//        msRepaired.CopyFrom(msToRepair, 155);

//        //get checksum
//        checksum := 0;
//        msRepaired.Position := 6;
//        while msRepaired.Position < (msRepaired.Size) do
//          checksum := checksum + msRepaired.ReadByte;
//        bChk := byte(((not (checksum and 255)) and 127) + 1);

//        msRepaired.WriteByte(bChk);
//        msRepaired.WriteByte($F7);
//        Result := True;
//      except
//        on E: Exception do Result := False;
//      end;
//      if Result then msRepaired.SaveToFile(aOutFileName + 'DX7_R' +
//          IntToHex(j, 2) + '.syx');

//    end;
//  end;

//  msToRepair.Free;
//  msRepaired.Free;
//end;

function CheckSum(dmp: TMemoryStream; StartPos, DumpLen: integer): boolean;
var
  CalcCheckSum: integer;
  tmpPos: int64;
  i: integer;
  ChkFromFile: byte;
begin
  tmpPos := dmp.Position;
  CalcCheckSum := 0;
  dmp.Position := StartPos + 6;
  for i := 0 to (DumpLen - 9) do
  begin
    CalcCheckSum := CalcCheckSum + dmp.ReadByte;
  end;
  CalcCheckSum := ((not (CalcCheckSum and 255)) and 127) + 1;
  ChkFromFile := dmp.ReadByte;
  if ChkFromFile <> 0 then
    Result := (CalcCheckSum = ChkFromFile)
  else
    Result := True;
  dmp.Position := tmpPos;
end;

function Printable(c: char): char;
begin
  if (Ord(c) > 31) and (Ord(c) < 127) then Result := c
  else
    Result := #32;
end;

end.
