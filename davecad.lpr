program davecad;

{$mode objfpc}{$H+}


uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, davecad_main, davecad_file, davecad_error, davecad_file_parser,
davecad_renderer, davecad_sheet_properties_form
  { you can add units after this };

{$R newFile.rc}
{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSheetProps, frmSheetProps);
  Application.Run;
end.

