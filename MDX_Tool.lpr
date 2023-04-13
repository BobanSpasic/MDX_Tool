{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Boban Spasic

 Program description:
 This program can get some info from Yamaha DX7 SysEx files. The info is more about the integrity/corruption of the files.
 Second aspect of the program is to repair some of the common corrupted files found on the internet.
 Third aspect (not yet implemented) will be the conversion from VMEM to VCED and vice-versa.
 
 ToDo:
 - get voice list
 - conversion VMEM <> VCED
 - duplicate finder along a collection of files
}

program MDX_Tool;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
             {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  untUtils, untDXUtils,
  untDXObjInterface;

type

  { TMDX_Tool }

  TMDX_Tool = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TMDX_Tool }

  procedure TMDX_Tool.DoRun;
  var
    ErrorMsg: string;
    fInput: string;
    fOutputDir: string;
    slReport: TStringList;
    msInputFile: TMemoryStream;
    i: integer;
    iStartPos: integer;
  begin
    fInput := '';
    // quick check parameters
    ErrorMsg := CheckOptions('hirvcsxjf:d:', 'help info repair voices crop split xsplit join file: dir:');

    if ErrorMsg <> '' then
    begin
      WriteHelp;
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    if (ParamCount = 0) or HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if HasOption('f', 'file') then
      fInput := GetOptionValue('f', 'file');

    if HasOption('r', 'repair') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end;

      slReport := TStringList.Create;

      WriteLn('Repairing file ' + ExtractFileName(fInput));
      if trim(ExtractFileDir(fInput)) = '' then
        fInput := IncludeTrailingPathDelimiter(GetCurrentDir) + fInput;
      if RepairDX7SysEx(fInput, slReport) then
      begin
        for i := 0 to slReport.Count - 1 do
          WriteLn(slReport[i]);
      end
      else
        WriteLn('No defects recognized');

      slReport.Free;
    end;

    if HasOption('i', 'info') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        slReport := TStringList.Create;
        msInputFile := TMemoryStream.Create;
        msInputFile.LoadFromFile(fInput);
        iStartPos := 0;

        if ContainsDX_SixOP_Data_New(msInputFile, iStartPos, slReport) then
        begin

        end;

        for i := 0 to slReport.Count - 1 do
          WriteLn(slReport[i]);

        msInputFile.Free;
        slReport.Free;
      end;
    end;

    if HasOption('v', 'voices') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        slReport := TStringList.Create;
        msInputFile := TMemoryStream.Create;
        msInputFile.LoadFromFile(fInput);

        GetVoices(msInputFile, slReport);

        for i := 0 to slReport.Count - 1 do
          WriteLn(slReport[i]);

        msInputFile.Free;
        slReport.Free;
      end;
    end;

    if HasOption('c', 'crop') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end;

      if trim(ExtractFileDir(fInput)) = '' then
        fInput := IncludeTrailingPathDelimiter(GetCurrentDir) + fInput;
      if CropHeaders(fInput) then
      begin
        WriteLn('Cropping file ' + ExtractFileName(fInput) + ': OK');
      end
      else
        WriteLn('Cropping file ' + ExtractFileName(fInput) + ': FAIL');
    end;

    if HasOption('s', 'split') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        if not HasOption('d', 'dir') then
        begin
          WriteLn('Parameter -d {directory} is missing');
          Terminate;
          Exit;
        end
        else
        begin
          fOutputDir :=IncludeTrailingPathDelimiter(GetOptionValue('d', 'outdir'));
          if pos(':\', fOutputDir) = 0 then fOutputDir := IncludeTrailingPathDelimiter(GetCurrentDir) + fOutputDir;
          if not DirectoryExists(fOutputDir) then
          ForceDirectories(fOutputDir);
          msInputFile := TMemoryStream.Create;
          msInputFile.LoadFromFile(fInput);

          SplitVMEM2VCED(msInputFile, fOutputDir);

          msInputFile.Free;
          WriteLn('Done!');
        end;
      end;
    end;

    if HasOption('x', 'xsplit') then
    begin
      if not FileExists(fInput) then
      begin
        WriteLn('Parameter -f {filename} is missing or the file {filename} could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        if not HasOption('d', 'dir') then
        begin
          WriteLn('Parameter -d {directory} is missing');
          Terminate;
          Exit;
        end
        else
        begin
          fOutputDir :=IncludeTrailingPathDelimiter(GetOptionValue('d', 'outdir'));
          if pos(':\', fOutputDir) = 0 then fOutputDir := IncludeTrailingPathDelimiter(GetCurrentDir) + fOutputDir;
          if not DirectoryExists(fOutputDir) then
          ForceDirectories(fOutputDir);
          msInputFile := TMemoryStream.Create;
          msInputFile.LoadFromFile(fInput);

          XSplitVMEM2VCED(msInputFile, fOutputDir);

          msInputFile.Free;
          WriteLn('Done!');
        end;
      end;
    end;

     if HasOption('j', 'join') then
    begin
      fOutputDir :=IncludeTrailingPathDelimiter(GetOptionValue('d', 'dir'));
      if pos(':\', fOutputDir) = 0 then fOutputDir := IncludeTrailingPathDelimiter(GetCurrentDir) + fOutputDir;
      if not DirectoryExists(fOutputDir) then
      begin
        WriteLn('Parameter -d {directory} is missing or the directory could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        if not HasOption('f', 'file') then
        begin
          WriteLn('Parameter -f {filename} is missing');
          Terminate;
          Exit;
        end
        else
        begin
          JoinVCED2VMEM(fOutputDir, fInput);
          WriteLn('Done!');
        end;
      end;
    end;

    Terminate;
  end;

  constructor TMDX_Tool.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TMDX_Tool.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TMDX_Tool.WriteHelp;
  begin
    writeln('');
    writeln('');
    writeln('MDX_Tool 1.2 - tool for various manipulation of DX7 VMEM and VCED SysEx files');
    writeln('Author: Boban Spasic');
    writeln('https://github.com/BobanSpasic/MDX_Tool');
    writeln('');
    writeln('Usage: ', ExtractFileName(ExeName), ' -parameters');
    writeln('  Parameters (short and long form):');
    writeln('       -h                 --help                   This help message');
    writeln('       -i                 --info                   Information');
    writeln('       -r                 --repair                 Repair/extract DX7 VMEM data from files');
    writeln('       -c                 --crop                   Crop headers from the VMEM/VCED files');
    writeln('       -s                 --split                  Split bank (VMEM) into single voices (VCED)');
    writeln('       -x                 --xsplit                 Split bank (VMEM) into single voices (VCED)');
    writeln('                                                   and take the SHA2-256 hash as a file name.');
    writeln('                                                   Voice name (10xASCII) is not a part of the hash');
    writeln('       -j                 --join                   Join single voices (VCED) into a bank (VMEM)');
    writeln('                                                   If the file voices.lst exists inside the input directory');
    writeln('                                                   - the voices inside the bank will be sorted according to the list');
    writeln('');
    writeln('       -f {filename}      --file={filename}        Input file (or output file for -j parameter)');
    writeln('       -d {directory}     --dir={directory}        Output directory for -s and -x parameters');
    writeln('                                                   Input directory for -j parameter');
    writeln('                                                   If it does not contain a drive letter, a sub-directory in');
    writeln('                                                   the current directory will be created.');
    writeLn('');
    writeln('  Example usage:');
    writeln('       MDX_Tool -i -f my_dx_file.syx');
    writeln('       MDX_Tool -r -f my_dx_file.syx');
    writeln('       MDX_Tool -s -f my_dx_file.syx -d new_directory');
    writeln('       MDX_Tool -j -f my_new_bank.syx -d directory_with_VCEDs');
    writeln('       MDX_Tool --info --file=my_dx_file.syx');
    writeLn('');
    writeLn('');
    writeLn('Split and Join parameters expect non-corrupted files as input (headerless files are accepted).');
  end;

var
  Application: TMDX_Tool;
begin
  Application := TMDX_Tool.Create(nil);
  Application.Title := 'MDX_Tool';
  Application.Run;
  Application.Free;
end.
