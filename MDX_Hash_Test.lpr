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

program MDX_Hash_Test;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
                   {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  untUtils,
  untDXUtils,
  untDXObjInterface;

type

  { TMDX_Hash_Test }

  TMDX_Hash_Test = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TMDX_Hash_Test }

  procedure TMDX_Hash_Test.DoRun;
  var
    ErrorMsg: string;
    fVCED: string;
    fVMEM: string;
    fNr: string;
    iNr: integer;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('hc:m:n:', 'help vced: vmem: nr:');

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

    fVCED := '';
    fVMEM := '';
    fNr := '';

    if HasOption('c', 'vced') then
    begin
      fVCED := GetOptionValue('c', 'vced');
      WriteLn ('Call -c with the value *' + fVCED +'*');
    end;
    if HasOption('m', 'vmem') then
    begin
      fVMEM := GetOptionValue('m', 'vmem');
      WriteLn ('Call -m with the value *' + fVMEM +'*');
    end;
    if HasOption('n', 'nr') then
    begin
      fNr := GetOptionValue('n', 'nr');
      WriteLn ('Call -n with the value *' + fNr +'*');
    end;

    if (HasOption('c', 'vced')) and (fVCED <> '') then
    begin
      if FileExists(fVCED) then
        Test_VCEDHash(fVCED)
      else
        WriteLn('File ' + fVCED + ' could not be found');
    end;

    if (HasOption('m', 'vmem')) and (fVMEM <> '') and (fNr <> '') then
    begin
      iNr := StrToIntDef(fNr, -1);
      if iNr <> -1 then
      begin
        if (iNr > 0) and (iNr < 33) then
        begin
          if FileExists(fVMEM) then
            Test_VMEMHash(fVMEM, iNr)
          else
            WriteLn('File ' + fVMEM + ' could not be found');
        end
        else
          WriteLn('-n parameter is outside the 1 - 32 range');
      end
      else
        WriteLn('-n parameter is missing');
    end;

    Terminate;
  end;

  constructor TMDX_Hash_Test.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TMDX_Hash_Test.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TMDX_Hash_Test.WriteHelp;
  begin
    writeln('');
    writeln('');
    writeln('MDX_Hash_Test - test program for the generated hash summs');
    writeln('Author: Boban Spasic');
    writeln('https://github.com/BobanSpasic/MDX_Tool');
    writeln('');
    writeln('Usage: ', ExtractFileName(ExeName), ' -parameters');
    writeln('  Parameters (short and long form):');
    writeln('       -h               --help               This help message');
    writeln('       -c               --vced               The single voice for test');
    writeln('       -m               --vmem               The bank for test');
    writeln('       -n               --nr                 The voice number inside the bank');
    writeLn('');
    writeln('  Example usage:');
    writeln('       MDX_Hash_Test -i -f my_dx_file.syx');
    writeLn('');
    writeLn('');
  end;

var
  Application: TMDX_Hash_Test;
begin
  Application := TMDX_Hash_Test.Create(nil);
  Application.Title := 'MDX_Hash_Test';
  Application.Run;
  Application.Free;
end.
