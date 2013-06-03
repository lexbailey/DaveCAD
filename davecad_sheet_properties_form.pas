unit davecad_sheet_properties_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, lclType;

type

  TSheetPropsEditCallback = procedure(oldSheetName, newSheetName, newSheetAuthor, newSheetDate, newSheetMedia: string) of object;

  { TfrmSheetProps }

  TfrmSheetProps = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    cbMedia: TComboBox;
    eAuthor: TLabeledEdit;
    eDate: TLabeledEdit;
    gbEdit: TGroupBox;
    eName: TLabeledEdit;
    Label1: TLabel;

    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: char);

  private
    fsheetEdit: string;
    epCallback: TSheetPropsEditCallback;
    { private declarations }
  public
    { public declarations }
    property sheetEdit: string read fsheetEdit write fsheetEdit;
    property callback: TSheetPropsEditCallback write epCallback;
  end;

var
  frmSheetProps: TfrmSheetProps;

implementation

{$R *.lfm}

{ TfrmSheetProps }

procedure TfrmSheetProps.btnCancelClick(Sender: TObject);
begin
  frmSheetProps.Close;
end;

procedure TfrmSheetProps.btnOKClick(Sender: TObject);
begin
  epCallback(fsheetEdit, eName.Text, eAuthor.Text, eDate.Text, cbMedia.Text);
  frmSheetProps.Close;
end;

procedure TfrmSheetProps.eNameKeyPress(Sender: TObject; var Key: char);
begin
  if key = char(VK_RETURN) then
    btnOK.Click;
end;

end.

