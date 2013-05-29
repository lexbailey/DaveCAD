unit davecad_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ComCtrls, ExtCtrls, StdCtrls, StdActns, laz2_XMLRead, laz2_DOM,
  davecad_file, davecad_error, lclintf, davecad_file_parser, davecad_renderer;

type

  TFileState = (fsNoFile, fsLoadedInvalid, fsLoadedValidNoSheet, fsLoadedValid);

  { TfrmMain }

  TfrmMain = class(TForm)
    actAbout: TAction;
    miEditSheet: TMenuItem;
    miDeleteSheet: TMenuItem;
    miNewSheet: TMenuItem;
    miSheet: TMenuItem;
    SheetEdit: TAction;
    SheetDelete: TAction;
    SheetNew: TAction;
    FileNew: TAction;
    EditRedo: TAction;
    FileClose: TAction;
    FileSave: TAction;
    MainActions: TActionList;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditDelete1: TEditDelete;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    FileExit1: TFileExit;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    smallIcons: TImageList;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    miNew: TMenuItem;
    miDelete: TMenuItem;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    miSelectAll: TMenuItem;
    miUndo: TMenuItem;
    miRedo: TMenuItem;
    miExit: TMenuItem;
    miAbout: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    miClose: TMenuItem;
    miHelp: TMenuItem;
    miEdit: TMenuItem;
    miFile: TMenuItem;
    pbDrawing: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    tcSheets: TTabControl;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    tbNew: TToolButton;
    tbOpen: TToolButton;
    tbSave: TToolButton;
    ToolButton1: TToolButton;
    tbCut: TToolButton;
    tbCopy: TToolButton;
    tbPaste: TToolButton;
    ToolButton2: TToolButton;
    tbDelete: TToolButton;
    tbUndo: TToolButton;
    tbRedo: TToolButton;
    tbNewSheet: TToolButton;
    tbDeleteSheet: TToolButton;
    tbEditSheet: TToolButton;
    procedure FileCloseExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpen1Accept(Sender: TObject);
    procedure FileOpen1BeforeExecute(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure pbDrawingPaint(Sender: TObject);
    procedure tcSheetsChange(Sender: TObject);
    procedure TOpenDialogShow(Sender: TObject);
  private
    { private declarations }
    procedure pbDrawText(pbtext: string);
    procedure pbDrawTextTwoLine(pbtext: string; pbtext2: string);
    procedure rescan();
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;
  loadedFile: TDaveCadFile;
  errors: TDaveCadMessageList;
  fileState: TFileState;
  fileIsModified: boolean;
  fileWasSaved: boolean;

implementation

{$R *.lfm}

{ TfrmMain }

procedure FileError (E: EXMLReadError);
begin
  errors.addMessage(E.Message);
end;

procedure TfrmMain.FileOpen1Accept(Sender: TObject);
begin
  //clear errors list
  errors.clear;
  //load a file (this might add things to the error list)
  try
  loadedFile.loadFile(FileOpen1.Dialog.FileName);
  finally
  end;
  //If we had errors
  if not errors.isEmpty then
    //show the errors
    errors.showMessages(mtError);

  //if the loaded file is valid...
  if loadedFile.isValid then begin
    //set the state to valid no sheet, then check for sheets and update if needed
    fileState:=fsLoadedValidNoSheet;
    if loadedFile.hasSheets then
      fileState:=fsLoadedValid;
    rescan;
  end else
  begin
    //file is not valid
    fileState:=fsLoadedInvalid;
  end;

  pbDrawing.Invalidate; //redraw now that a new file is loaded.
end;

procedure TfrmMain.FileOpen1BeforeExecute(Sender: TObject);
begin
  FileClose.Execute;
end;

procedure TfrmMain.FileSaveAs1Accept(Sender: TObject);
begin
  loadedFile.newFileName(FileSaveAs1.Dialog.FileName);
  loadedFile.save;
  fileWasSaved := true;
end;

procedure TfrmMain.FileSaveExecute(Sender: TObject);
begin
  if (fileState = fsLoadedValidNoSheet) or (fileState = fsLoadedValid) then begin
    if loadedFile.save then begin
      FileSaveAs1.Execute;
    end else
    begin
      fileWasSaved := true;
    end;
  end else
  begin
    showError(DCAD_ERROR_FILE_NOT_SAVEABLE);
    fileWasSaved := false;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FileClose.Execute;
  if not fileWasSaved then
    CloseAction := caNone;
end;

procedure TfrmMain.FileCloseExecute(Sender: TObject);
var answer: TModalResult;
begin
  if ((fileState = fsLoadedValidNoSheet) or (fileState = fsLoadedValid)) and fileIsModified then begin
    answer := askQuestion(DCAD_QUES_UNSAVED_CHANGES);
    case answer of
      mrYes: begin
        fileWasSaved := false;
        FileSave.Execute; //Save file before loading new one.
        if not fileWasSaved then
          exit;
      end;
      mrNo:{Nothing to do, allow file to be discarded};
      mrCancel: begin
        fileWasSaved:=false;
        exit;
      end;
    end;
  end;
  loadedFile.Free;
  loadedFile := TDaveCadFile.create(@FileError);
  fileState:=fsNoFile;
  pbDrawing.Invalidate;
  tcSheets.Tabs.Clear;
  fileWasSaved := true;
end;

procedure TfrmMain.FileNewExecute(Sender: TObject);
begin
  FileClose.Execute;
  loadedFile.new;
//  fileState:=fsLoadedValidNoSheet;
  fileState:=fsLoadedValid;
  pbDrawing.Invalidate;
  rescan;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  loadedFile := TDaveCadFile.create(@FileError);
  errors := TDaveCadMessageList.create;
  errors.setHead('DaveCAD Error');
  fileState:=fsNoFile;
  fileIsModified := true;//for now.
  fileWasSaved := true;
end;

procedure TfrmMain.pbDrawText(pbtext: string);
var
  pbtop, pbleft: integer;
begin
  pbtop := round((pbDrawing.Height/2) - (pbDrawing.Canvas.TextHeight(pbtext)/2));
  pbleft := round((pbDrawing.Width/2) - (pbDrawing.Canvas.TextWidth(pbtext)/2));
  pbDrawing.Canvas.TextOut(pbleft,pbtop,pbtext);
end;

procedure TfrmMain.pbDrawTextTwoLine(pbtext: string; pbtext2: string);
var
  pbtop, pbleft: integer;
begin
  pbtop := round((pbDrawing.Height/2) - (pbDrawing.Canvas.TextHeight(pbtext)));
  pbleft := round((pbDrawing.Width/2) - (pbDrawing.Canvas.TextWidth(pbtext)/2));
  pbDrawing.Canvas.TextOut(pbleft,pbtop,pbtext);

  pbtop := round((pbDrawing.Height/2));
  pbleft := round((pbDrawing.Width/2) - (pbDrawing.Canvas.TextWidth(pbtext2)/2));
  pbDrawing.Canvas.TextOut(pbleft,pbtop,pbtext2);
end;

procedure TfrmMain.pbDrawingPaint(Sender: TObject);
var sheets: TDaveCADSheetList;
  sheet: TDaveCADSheet;
begin
  //Decide what to draw!
  case fileState of
    //If no file is loaded the show a message to say so.
    fsNoFile: pbDrawTextTwoLine(getErrorMessage(DCAD_WARN_NO_FILE),getErrorMessage(DCAD_WARN_NO_FILE_HINT));
    //If the file is corrupt, show a warning
    fsLoadedInvalid: pbDrawText(getErrorMessage(DCAD_ERROR_FILE_CORRUPT));
    //If the file has no sheets, tell the user and recomment that they add one
    fsLoadedValidNoSheet: pbDrawTextTwoLine(getErrorMessage(DCAD_WARN_NO_SHEETS), getErrorMessage(DCAD_WARN_NO_SHEETS_HINT));
    //Otherwise, the file can be displayed using the current sheet.
    fsLoadedValid:begin
      //Get all the sheets in the file
      sheets := loadedFile.getSheets;
      //fund the selected sheet
      sheet := sheets.sheet[loadedFile.Session.SelectedSheet];
      //if the selected sheet doesn't exist then show a warning, this is nearly impossible
      if sheet = nil then begin
        pbDrawText(getErrorMessage(DCAD_WARN_INVALID_SHEET_SELECTED));
      end else
      begin
        //here we can actually draw the sheet
        renderSheet(sheet, pbDrawing.Canvas, pbDrawing.Width, pbDrawing.Height);
      end;
    end;
  end;

end;

procedure TfrmMain.tcSheetsChange(Sender: TObject);
begin
  //User has selected a new sheet!
  loadedFile.Session.SelectedSheet := tcSheets.Tabs.Strings[tcSheets.TabIndex];
  //Force redraw with new sheet.
  pbDrawing.Invalidate;
end;

procedure TfrmMain.TOpenDialogShow(Sender: TObject);
begin
  //If the user was about to save the current file and then canceled, dont open anything new.
  if not fileWasSaved then
    FileOpen1.Dialog.Close;
end;

procedure TfrmMain.rescan; //Scans the loaded file for sheets and adds them to the tabcontrol
var sheets: TDaveCadSheetList;
  i: integer;
begin
  //If a valid file is loaded
  if fileState = fsLoadedValid then begin
    //Get all sheets from that file
    sheets := loadedFile.getSheets;
    //clear all old sheets
    tcSheets.Tabs.Clear;
    //Loop through all sheets
    for i := 0 to sheets.count-1 do begin
      //add them to the tabs
      tcSheets.Tabs.Add(sheets.Item[i].Name);
    end;
  end;
end;

end.

