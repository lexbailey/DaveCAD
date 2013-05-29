unit davecad_error;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Dialogs, Forms, Controls;

type
  TDaveCadMessageList = class(TObject)
    fList : Classes.TStrings;
    fIsEmpty: boolean;
    fHead: string;
    public
      constructor create;
      function isEmpty: boolean;
      procedure setHead(head: string);
      procedure addMessage(m: string);
      procedure showMessages;
      procedure showMessages(dType: TMsgDlgType);
      procedure clear;
  end;

  procedure showError(errorNum: integer);
  procedure showWarning(errorNum: integer);
  function askQuestion(errorNum: integer): TModalResult;
  function getErrorMessage(errorNum: integer):string;


const
  //This is a list of self explanatory error codes, they all start "DCAD_ERROR"
  DCAD_ERROR_FILE_NOT_FOUND=1;
  DCAD_ERROR_FILE_CORRUPT=2;
  DCAD_ERROR_FILE_NOT_WRITABLE=3;
  DCAD_ERROR_FILE_NOT_SAVEABLE=4;

  //This is a list of self explanatory warning codes, they all start "DCAD_WARN"
  DCAD_WARN_UNSAVED_CHANGES=1000;
  DCAD_WARN_NO_FILE=1001;
  DCAD_WARN_NO_FILE_HINT=1002;
  DCAD_WARN_NO_SHEETS=1003;
  DCAD_WARN_NO_SHEETS_HINT=1004;
  DCAD_WARN_INVALID_SHEET_SELECTED=1005;

  //This is a list of self explanatory question codes, they all start "DCAD_QUES"
  DCAD_QUES_UNSAVED_CHANGES=2000;

implementation

  constructor TDaveCadMessageList.create;
  begin
    inherited create;
    fIsEmpty := true;
    fList := TStringList.Create;
  end;

  procedure TDaveCadMessageList.addMessage(m: string);
  begin
    fList.Add(m);
    fIsEmpty:=false;
  end;

  function TDaveCadMessageList.isEmpty: boolean;
  begin
    result := fIsEmpty;
  end;

  procedure TDaveCadMessageList.showMessages;
  begin
    showMessage(fHead + fList.Text);
  end;

  procedure TDaveCadMessageList.showMessages(dType: TMsgDlgType);
  begin
    Dialogs.MessageDlg('DaveCAD Error', fHead + ' ' + fList.Text, dType,[mbOK], 0);
  end;

  procedure TDaveCadMessageList.setHead(head: string);
  begin
    fHead := head;
  end;

  procedure TDaveCadMessageList.clear;
  begin
    fList.Clear;
  end;

  //This is a function to get a nice error message from the error code
  function getErrorMessage(errorNum: integer):string;
  begin
    //If you can, find the propper error message
    case errorNum of
      DCAD_ERROR_FILE_NOT_FOUND: result:= 'Specified file cannot be found.';
      DCAD_ERROR_FILE_CORRUPT: result:= 'Loaded file is not a valid DaveCAD file.';
      DCAD_ERROR_FILE_NOT_WRITABLE: result:= 'File cannot be written, check your permission.';
      DCAD_ERROR_FILE_NOT_SAVEABLE: result:= 'Cannot save, no file loaded or file is corrupt.';

      DCAD_WARN_UNSAVED_CHANGES: result:= 'You have made changes that have not been saved.';
      DCAD_WARN_NO_FILE:result:= 'No file loaded.';
      DCAD_WARN_NO_FILE_HINT:result:= 'Click ''New Drawing'' or ''Open''';
      DCAD_WARN_NO_SHEETS:result:= 'Loaded file contains no sheets.';
      DCAD_WARN_NO_SHEETS_HINT:result:= 'Click ''Add sheet'' to add the fist sheet.';
      DCAD_WARN_INVALID_SHEET_SELECTED: result := 'An invalid sheet is selected.';

      DCAD_QUES_UNSAVED_CHANGES: result:= 'You have made changes that have not been saved. Would you like to save them now?';

      else
        //if no other message is found, use this one.
        result := 'You have an error error, yes that''s right, its a double error.'+
        ' There was an error, this error produced an error code, the error code'+
        ' didn''t actually match an error message and so you have been shown this'+
        ' really anoying, really long rather pointless error message. If you know'+
        ' how to use github, submit a bug report!';
    end;
  end;

  procedure showError(errorNum: integer);
  begin
    Dialogs.MessageDlg('DaveCAD Error', getErrorMessage(errorNum), mtError,[mbOK], 0);
  end;

  procedure showWarning(errorNum: integer);
  begin
    Dialogs.MessageDlg('DaveCAD Error', getErrorMessage(errorNum), mtWarning,[mbOK], 0);
  end;

  function askQuestion(errorNum: integer): TModalResult;
  begin
    result := MessageDlg('DaveCAD Error', getErrorMessage(errorNum), mtConfirmation, [mbYes, mbNo, mbCancel], 0);
  end;

end.

