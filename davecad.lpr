program davecad;

{$mode objfpc}{$H+}


uses
  heaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, davecad_main, davecad_file, davecad_error, davecad_file_parser,
davecad_renderer, davecad_sheet_properties_form, davecad_about, davecad_enum
  { you can add units after this };

{$R newFile.rc}
{$R *.res}

begin
  {$IFDEF LazVersionGT1}
  RequireDerivedFormResource := True;
  {$ENDIF}
  {$IFDEF HeapTraceToFile}
  SetHeapTraceOutput('heap.trc');
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSheetProps, frmSheetProps);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;
end.

