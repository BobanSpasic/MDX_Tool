program MDX_Tool;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
           {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  untDXUtils;

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
    slReport: TStringList;
    msInputFile: TMemoryStream;
    i: integer;
    iStartPos: integer;
  begin
    fInput := '';
    // quick check parameters
    ErrorMsg := CheckOptions('hirf:', 'help info repair file:');

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
        WriteLn('Parameter -f {filename} is missing or the file {file_name} could not be found');
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
        WriteLn('Parameter -f {filename} is missing or the file {file_name} could not be found');
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
    writeln('MDX_Tool 0.9 (beta) - tool for getting info from Yamaha DX SysEx files');
    writeln('Author: Boban Spasic');
    writeln('');
    writeln('Usage: ', ExtractFileName(ExeName), ' -parameters');
    writeln('  Parameters (short and long form):');
    writeln('       -h                 --help                   This help message');
    writeln('       -i                 --info                   Information');
    writeln('       -r                 --repair                 Repair/extract DX7 VMEM data from files');
    //    writeln('       -d                 --dx7                    output DX7 mark I files');
    //    writeln('                                               (DX7II and TX data will be removed)');
    writeln('       -f {filename}      --file={filename}        Input file');
    WriteLn('');
    writeln('  Example usage:');
    writeln('       MDX_Tool -i -f my_dx_file.syx');
    writeln('       MDX_Tool -r -f my_dx_file.syx');
    writeln('       MDX_Tool --info --file=my_dx_file.syx');
  end;

var
  Application: TMDX_Tool;
begin
  Application := TMDX_Tool.Create(nil);
  Application.Title := 'MDX_Tool';
  Application.Run;
  Application.Free;
end.
