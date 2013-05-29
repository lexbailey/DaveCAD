unit davecad_file;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_XMLRead, laz2_DOM, davecad_error, laz2_XMLWrite, davecad_file_parser, dialogs;

  type

    TDaveCadFileErrorCallback = procedure (E: EXMLReadError);

    TDaveCADFileSession = record
      SelectedSheet: string;
    end;

    TDaveCadFile = class(TObject)

      private
        fFileName: string;
        fFileValid: boolean;
        fFile: TXMLDocument;

        fECallback: TDaveCadFileErrorCallback;

        procedure XMLErrorHandler(E: EXMLReadError);

      public

        Session: TDaveCADFileSession;

        //function for loading DaveCAD files
        function loadFile(fileName: string): integer;

        //function for closing the currently loaded DaveCAD file
        procedure closeFile;

        //This function will save the file if it can and return false, otherwise it will return true
        function save:boolean;

        //This turns this file into a new file
        procedure new;

        //This function gives a new file a name, it will fail if the file name is already set
        procedure newFileName(name: string);

        //function for getting validity
        function isValid: boolean;

        //functions for adding things to the file
        procedure addSheet(name_s: string; media_s: string; author_s: string; date_s: string);

        //functions for getting info from the file
        function getDOM:TXMLDocument; //Gets everything
        function hasSheets:boolean;
        function getSheets:TDaveCADSheetList;

        //some stuff, the usual.
        constructor create;
        constructor create(E: TDaveCadFileErrorCallback); //adds callback too
        destructor destroy; override;

    end;

const
  FileSaveVersionNumber: string = '0.1';

implementation

constructor TDaveCadFile.create;
begin
  inherited create;
  //just some inits...
  fFileName := '';
  fFileValid := false;
end;

constructor TDaveCadFile.create (E: TDaveCadFileErrorCallback);
begin
  //another init...
  create;
  fEcallback := E;
end;

destructor TDaveCadFile.destroy;
begin
  //make sure everything is free
  try
    closeFile;
  finally
  end;
  inherited destroy;
end;

procedure TDaveCadFile.XMLErrorHandler(E: EXMLReadError);
begin
  if (E.Severity = esError) or (E.Severity = esFatal) then
    fFileValid := false; //There was an error, file is no longer valid
  if not(fECallback = nil) then //if there is another handler that wants to know
     fECallback(E); //let them!
end;

function TDaveCadFile.loadFile(fileName: string): integer;
var
  Parser: TDOMParser;
  Src: TXMLInputSource;
  Input: TFileStream;
  sheetList: TDaveCadSheetList;
begin
  //store the file name
  fFileName := fileName;
  if fileExists(fFileName) then
  begin
    try
      fFileValid := true;
      // create a parser object
      Parser := TDOMParser.Create;
      // get the actual file from disk
      Input := TFileStream.Create(fileName, fmOpenRead);
      // create the input source
      Src := TXMLInputSource.Create(Input);
      // we want validation
      Parser.Options.Validate := True;
      // assign a error handler which will receive notifications
      Parser.OnError := @XMLErrorHandler;
      // now do the job
      Parser.Parse(Src, fFile);
      // ...and cleanup
    except
    end;

    Input.free;
    Parser.Free;
    Src.Free;
  end else begin
    result := DCAD_ERROR_FILE_NOT_FOUND;
    fFileValid := false;
  end;
  sheetList := getSheets;
  if sheetList.count >= 1 then begin
    session.SelectedSheet:=sheetList.Item[0].Name;
  end;
end;

function TDaveCadFile.isValid: boolean;
begin
  result := fFileValid;
end;

procedure TDaveCadFile.closeFile;
begin
  //reset to new state
  fFileValid := false;
  fFileName := '';
  try
    fFile.Free;
  except
  end;
end;

function TDaveCadFile.save:boolean;
begin
  result:=true;
  if (not (ffileName = ''))and fFileValid then begin
    writeXMLFile(fFile, fFileName);
    result:= false;
  end;
end;

procedure TDaveCadFile.new;
var RootNode: TDOMNode;
begin
  closeFile;
  fFile := TXMLDocument.Create;

  RootNode := fFile.CreateElement('DaveCADDrawing');
  TDOMElement(RootNode).SetAttribute('file-version', FileSaveVersionNumber);
  fFile.AppendChild(RootNode);

  addSheet('sheet1', 'post-it', 'user', '00-00-00');
  session.SelectedSheet:='sheet1';

  fFileValid:=true;
end;

procedure TDaveCadFile.newFileName(name: string);
begin
  if fFileName = '' then
    fFileName:=name;
end;

//This function adds a sheet to the currently loaded file.
procedure TDaveCadFile.addSheet(name_s: string; media_s: string; author_s: string; date_s: string);
var root, sheet, properties, objects, name, media, author, date, name_data, media_data, author_data, date_data: TDOMNode;
begin
  //make a new sheet
  sheet := fFile.CreateElement('sheet');
    //give the sheet some properties
    properties := fFile.CreateElement('properties');
    sheet.AppendChild(properties);

      name := fFile.CreateElement('property'); TDOMElement(name).SetAttribute('name', 'sheet-name');
      media := fFile.CreateElement('property'); TDOMElement(media).SetAttribute('name', 'media');
      author := fFile.CreateElement('property'); TDOMElement(author).SetAttribute('name', 'author');
      date := fFile.CreateElement('property'); TDOMElement(date).SetAttribute('name', 'date');

      properties.AppendChild(name);
      properties.AppendChild(media);
      properties.AppendChild(author);
      properties.AppendChild(date);

      name_data := fFile.CreateTextNode(name_s);
      media_data := fFile.CreateTextNode(media_s);
      author_data := fFile.CreateTextNode(author_s);
      date_data := fFile.CreateTextNode(date_s);

      name.AppendChild(name_data);
      media.AppendChild(media_data);
      author.AppendChild(author_data);
      date.AppendChild(date_data);

      //Add the objects section
      objects := fFile.CreateElement('objects');
      sheet.AppendChild(objects);

    //Put it all in the root node
    root := fFile.FindNode('DaveCADDrawing') ;
    root.AppendChild(sheet);

end;

function TDaveCadFile.getDOM:TXMLDocument;
begin
   result := fFile;
end;

function TDaveCadFile.hasSheets:boolean;
var sheets: TDOMNodeList;
begin
  sheets := fFile.DocumentElement.GetElementsByTagName('sheet');
  result := sheets.Count >= 1;
end;

function TDaveCadFile.getSheets:TDaveCADSheetList;
var sheets: TDOMNodeList;
  sheetList: TDaveCadSheetList;
  sheet: TDaveCADSheet;
  i: integer;
begin
  sheets := fFile.DocumentElement.GetElementsByTagName('sheet');
  sheetList := TDaveCADSheetList.Create;
  for i := 0 to sheets.Count-1 do begin
      sheet := TDaveCADSheet.create;
      sheet.loadFrom(TDOMElement(sheets.Item[i]));
      sheetList.add(sheet);
  end;
  result := sheetList;
end;

end.

