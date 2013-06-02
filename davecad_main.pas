unit davecad_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ComCtrls, ExtCtrls, StdCtrls, StdActns, laz2_XMLRead, laz2_DOM,
  davecad_file, davecad_error, lclintf, davecad_file_parser, davecad_renderer,
  davecad_sheet_properties_form, davecad_about, math;

type

  TFileState = (fsNoFile, fsLoadedInvalid, fsLoadedValidNoSheet, fsLoadedValid);

  { TfrmMain }

  TfrmMain = class(TForm)
    actAbout: TAction;
    ilDrawTools: TImageList;
    ilEditTools: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    miEditSheet: TMenuItem;
    miDeleteSheet: TMenuItem;
    miNewSheet: TMenuItem;
    miSheet: TMenuItem;
    pnlToolbox: TPanel;
    sbToolbox: TScrollBox;
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
    Panel2: TPanel;
    Splitter1: TSplitter;
    Status: TStatusBar;
    tbDrawFree: TToolButton;
    tbDrawingTool: TToolBar;
    tbFelt: TToolButton;
    tbMove: TToolButton;
    tbPencil: TToolButton;
    tbText: TToolButton;
    tcSheets: TTabControl;
    ToolBar1: TToolBar;
    tbEditTool: TToolBar;
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
    tpBallPoint: TToolButton;
    procedure actAboutExecute(Sender: TObject);
    procedure FileCloseExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpen1Accept(Sender: TObject);
    procedure FileOpen1BeforeExecute(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure pbDrawingPaint(Sender: TObject);
    procedure sbToolboxResize(Sender: TObject);
    procedure SheetDeleteExecute(Sender: TObject);
    procedure SheetEditExecute(Sender: TObject);
    procedure SheetNewExecute(Sender: TObject);
    procedure tbDrawingToolClick(Sender: TObject);
    procedure tbEditingToolClick(Sender: TObject);
    procedure hintMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure tcSheetsChange(Sender: TObject);
    procedure TOpenDialogShow(Sender: TObject);
  private
    { private declarations }
    procedure pbDrawText(pbtext: string);
    procedure pbDrawTextTwoLine(pbtext: string; pbtext2: string);
    procedure rescan();
  public
    { public declarations }
    procedure changeSheetProps(oldSheetName, newSheetName, newSheetAuthor, newSheetDate, newSheetMedia: string);
  end;

var
  frmMain: TfrmMain;
  loadedFile: TDaveCadFile;
  errors: TDaveCadMessageList;
  fileState: TFileState;
  fileWasSaved: boolean;
  drawingTool : integer;
  edittingTool : integer;

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
  if ((fileState = fsLoadedValidNoSheet) or (fileState = fsLoadedValid)) and loadedFile.IsModified then begin
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

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  frmAbout.Show;
end;

procedure TfrmMain.FileNewExecute(Sender: TObject);
begin
  FileClose.Execute;
  loadedFile.new;
//  fileState:=fsLoadedValidNoSheet;
  fileState:=fsLoadedValid;
  rescan;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  loadedFile := TDaveCadFile.create(@FileError);
  errors := TDaveCadMessageList.create;
  errors.setHead('DaveCAD Error');
  fileState:=fsNoFile;
  fileWasSaved := true;

  drawingTool := 0;
  edittingTool := 0;
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
      //find the selected sheet
      sheet := sheets.sheet[loadedFile.Session.SelectedSheet];
      //we don't need sheets any more
      sheets.Free;
      //if the selected sheet doesn't exist then show a warning, this is nearly impossible
      if sheet = nil then begin
        pbDrawText(getErrorMessage(DCAD_WARN_INVALID_SHEET_SELECTED));
      end else
      begin
        //here we can actually draw the sheet
        renderSheet(sheet, pbDrawing.Canvas, pbDrawing.Width, pbDrawing.Height);
        sheet.Free;
      end;

    end;
  end;

end;

procedure TfrmMain.sbToolboxResize(Sender: TObject);
begin
  pnlToolbox.Width:=max(sbToolbox.Width-4, 90);
end;

procedure TfrmMain.SheetDeleteExecute(Sender: TObject);
begin
  showError(loadedFile.deleteSheet(loadedFile.Session.SelectedSheet));
  rescan;
end;

procedure TfrmMain.SheetEditExecute(Sender: TObject);
var sheet: TDaveCADSheet;
begin
  sheet := loadedFile.getSheets.Sheet[loadedFile.Session.SelectedSheet];
  with frmSheetProps do begin
    gbEdit.Caption:= 'Editing sheet in file ' + loadedFile.fileName;
    sheetEdit:=sheet.Name;
    eName.Text:=sheet.Name;
    eAuthor.Text:=sheet.Author;
    eDate.Text:=sheet.Date;
    cbMedia.ItemIndex:=cbMedia.Items.IndexOf(sheet.Media);
    callback:=@frmMain.changeSheetProps;
    Show;
  end;
  sheet.Free;
end;

procedure TfrmMain.changeSheetProps(oldSheetName, newSheetName, newSheetAuthor, newSheetDate, newSheetMedia: string);
var oldSheet: TDaveCADSheet;
  allSheets: TDOMNodeList;
  i: integer;
begin
  oldSheet := TDaveCADSheet.create;
  allSheets :=loadedFile.getDOM.DocumentElement.GetElementsByTagName('sheet');
  for i := 0 to allSheets.Count-1 do begin;
    oldSheet.loadFrom(TDOMElement(allSheets.Item[i]));
    if oldSheet.Name = oldSheetName then //we have our guy
    begin
      loadedFile.updateSheetProps(TDomElement(allSheets.Item[i]), newSheetName, newSheetAuthor, newSheetDate, newSheetMedia);
    end;
  end;
  oldSheet.Free;
  allSheets.Free;
  rescan;
end;

procedure TfrmMain.SheetNewExecute(Sender: TObject);
var sheetName: string;
begin
  sheetName := 'sheet '+inttostr(loadedFile.getSheets.count+1);
  loadedFile.addSheet( sheetName, 'post-it', 'User', '');
  loadedFile.Session.SelectedSheet:=sheetName;
  rescan;
end;

//Single handler for each group of toolbox items
procedure TfrmMain.tbEditingToolClick(Sender: TObject);
var i :integer;
begin
  //loop through all buttons in the group and deselect each one
  for i := 0 to tbEditTool.ButtonCount-1 do begin
    tbEditTool.Buttons[i].Down:=false;
  end;
  //select the one that was clicked
  TToolButton(sender).Down:=true;
  //Save the tool name
  EdittingTool := TToolButton(sender).Tag;
end;

procedure TfrmMain.hintMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Status.Panels[0].Text := TControl(sender).Hint;
end;

//Similar to avove
procedure TfrmMain.tbDrawingToolClick(Sender: TObject);
var i :integer;
begin
  for i := 0 to tbDrawingTool.ButtonCount-1 do begin
    tbDrawingTool.Buttons[i].Down:=false;
  end;
  TToolButton(sender).Down:=true;
  DrawingTool := TToolButton(sender).Tag;
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
    if assigned(sheets) then begin
      for i := 0 to sheets.count-1 do begin
        //add them to the tabs
        tcSheets.Tabs.Add(sheets.Item[i].Name);
      end;
      sheets.Free;
    end;

  end;

  if tcSheets.Tabs.IndexOf(loadedFile.Session.SelectedSheet) = -1 then begin
    if tcSheets.Tabs.Count >= 1 then begin
      loadedFile.Session.SelectedSheet:=tcSheets.Tabs.Strings[0];
    end else
    begin
      loadedFile.Session.SelectedSheet:='';
    end;
  end;
  pbDrawing.Invalidate;

end;

end.

