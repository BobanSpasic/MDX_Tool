{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Boban Spasic

 Program description:
 This program helps organizing your Yamaha DX7 SysEx collection
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

  { TMDX_Collect }

  TMDX_Collect = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TMDX_Collect }

  procedure TMDX_Collect.DoRun;
  var
    ErrorMsg: string;
    fMaster: string;
    fIncoming: string;
    fReportDir: string;
    fInputDir: string;
    fOutput: string;
  begin
    fMaster := '';
    fIncoming := '';
    fReportDir := '';
    fInputDir := '';
    fOutput := '';

    // quick check parameters
    ErrorMsg := CheckOptions('hactm:i:r:d:o:',
      'help analyze compare movetree master: incoming: report: dir: output:');

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
    if HasOption('o', 'output') then
      fOutput := IncludeTrailingPathDelimiter(GetOptionValue('o', 'output'));

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

     if HasOption('t', 'movetree') then
    begin
      if (fOutput = '') or (fIncoming = '') or (fInputDir = '') then
      begin
        WriteLn('Some of the Parameters -o, -i or -d is missing');
        Terminate;
        Exit;
      end
      else
      begin
        WriteLn('Incoming list: ' + fIncoming);
        WriteLn('Tree root directory of the incoming list: ' + fInputDir);
        WriteLn('Output directory: ' + fOutput);
        MoveTree(fIncoming, fInputDir, fOutput);
        WriteLn('Done!');
      end;
    end;

    Terminate;
  end;

  constructor TMDX_Collect.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TMDX_Collect.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TMDX_Collect.WriteHelp;
  begin
    writeln('');
    writeln('');
    writeln('MDX_Collect 1.8 - tool for managing DX7 SysEx collections');
    writeln('Author: Boban Spasic');
    writeln('https://github.com/BobanSpasic/MDX_Tool');
    writeln('');
    writeln('Usage: ', ExtractFileName(ExeName), ' -parameters');
    writeln('  Parameters (short and long form):');
    writeln('   -h                 --help                    This help message');
    writeLn('');
    writeln('   -a                 --analyze                 Make a hash list of VMEM/VCED files in a directory');
    writeln('       -d {directory}     --dir={directory}     Input directory');
    writeln('       -r {directory}     --report={directory}  Output directory for the reports');
    writeLn('');
    writeln('   -c                 --compare                 Compare two hash lists');
    writeln('       -m {filename}      --master={filename}   Hash list of your collection');
    writeln('       -i {filename}      --incoming={filename} Hash list of incoming collection');
    writeln('       -r {directory}     --report={directory}  Output directory for the reports');
    writeLn('');
    writeln('   -t                 --movetree                Copy directory tree to the new location and move the diff. files');
    writeln('       -i {filename}      --incoming={filename} IncomingHasMore.dif list of the incoming collection');
    writeln('       -d {directory}     --dir={directory}     Root directory of the incoming file collection');
    writeln('       -o {directory}     --output={directory}  Output directory for the moved files');
    writeLn('');
    writeln('  Example usage:');
    writeln('       MDX_Collect -a -d MyCollection -r MyReports');
    writeln('       MDX_Collect -a -d NewFiles -r NewReports');
    writeln('       MDX_Collect -c -m MyCollection.hsl -i NewFiles.hsl -r MyReports');
  end;

var
  Application: TMDX_Collect;
begin
  Application := TMDX_Collect.Create(nil);
  Application.Title := 'MDX_Collect';
  Application.Run;
  Application.Free;
end.
