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
 - conversion VMEM <> VCED
 - duplicate finder along a collection of files
}

program MDX_Collect;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
                 {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  //untUtils,
  //untDXUtils,
  //untDXObjInterface,
  untCollectionUtils;

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
    fMaster: string;
    fIncoming: string;
    fReportDir: string;
    fInputDir: string;
  begin
    fMaster := '';
    fIncoming := '';
    fReportDir := '';
    fInputDir := '';
    // quick check parameters
    ErrorMsg := CheckOptions('hacm:i:r:d:',
      'help analyze compare master: incoming: report: dir:');

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

    if HasOption('m', 'master') then
      fMaster := GetOptionValue('m', 'master');
    if HasOption('i', 'incoming') then
      fIncoming := GetOptionValue('i', 'incoming');
    if HasOption('r', 'report') then
      fReportDir := IncludeTrailingPathDelimiter(GetOptionValue('r', 'report'));
    if HasOption('d', 'dir') then
      fInputDir := IncludeTrailingPathDelimiter(GetOptionValue('d', 'dir'));

    if HasOption('a', 'analyze') then
    begin
      if (fInputDir = '') or (fReportDir = '') then
      begin
        WriteLn('Parameter -d {directory} or parameter -r {directory} is missing or the directory could not be found');
        Terminate;
        Exit;
      end;
      if pos(':\', fInputDir) = 0 then
        fInputDir := IncludeTrailingPathDelimiter(GetCurrentDir) + fInputDir;
      if not DirectoryExists(fInputDir) then
      begin
        WriteLn('Parameter -d {directory} is missing or the directory could not be found');
        Terminate;
        Exit;
      end
      else
      begin
        if not DirectoryExists(fReportDir) then CreateDir(fReportDir);
        WriteLn('Input directory: ' + fInputDir);
        WriteLn('Report directory: ' + fReportDir);
        Analyze(fInputDir, fReportDir);
        WriteLn('Done!');
      end;
    end;

    if HasOption('c', 'compare') then
    begin
      if (fMaster = '') or (fIncoming = '') or (fReportDir = '') then
      begin
        WriteLn('Some of the Parameters -m, -i or -r is missing');
        Terminate;
        Exit;
      end
      else
      begin
        if pos(':\', fReportDir) = 0 then
        fReportDir := IncludeTrailingPathDelimiter(GetCurrentDir) + fReportDir;
        WriteLn('Master list: ' + fMaster);
        WriteLn('Incoming list: ' + fIncoming);
        WriteLn('Reports directory: ' + fReportDir);
        Compare(fMaster, fIncoming, fReportDir);
        WriteLn('Done!');
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
    writeln('MDX_Collect 1.6 - tool for hashing bank collections');
    writeln('Author: Boban Spasic');
    writeln('https://github.com/BobanSpasic/MDX_Tool');
    writeln('');
    writeln('Usage: ', ExtractFileName(ExeName), ' -parameters');
    writeln('  Parameters (short and long form):');
    writeln('   -h                 --help                    This help message');
    writeLn('');
    writeln('   -a                 --analyze                 Make a hash list of VMEM/VCED files in a directory');
    writeln('       -d {directory}     --dir={directory}     Input directory for -a parameter');
    writeln('       -r {directory}     --report={directory}  Output directory for the reports');
    writeLn('');
    writeln('   -c                 --compare                 Compare two hash lists');
    writeln('       -m {filename}      --master={filename}   Hash list of your collection');
    writeln('       -i {filename}      --incoming={filename} Hash list of incoming collection');
    writeln('       -r {directory}     --report={directory}  Output directory for the reports');
    writeLn('');
    writeln('  Example usage:');
    writeln('       MDX_Tool -a -d MyCollection -r MyReports');
    writeln('       MDX_Tool -a -d NewFiles -r NewReports');
    writeln('       MDX_Tool -c -m MyCollection.hsl -i NewFiles.hsl');
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
