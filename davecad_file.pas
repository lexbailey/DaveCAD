unit davecad_file;

{$mode objfpc}{$H+}

//{$R newFile.rc}

interface

uses
  Classes, SysUtils, laz2_XMLRead, laz2_DOM, davecad_error, laz2_XMLWrite, davecad_file_parser, dialogs, LResources;

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
        fModified: boolean;

        fECallback: TDaveCadFileErrorCallback;

        procedure XMLErrorHandler(E: EXMLReadError);
        procedure modify;

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

        //functions for adding things to the file, editting or removing them
        procedure addSheet(name_s: string; media_s: string; author_s: string; date_s: string);
        function deleteSheet(name_s: string): integer;
        procedure updateSheetProps(sheet: TDOMElement; newname, newAuthor, newDate, newMedia: string);

        //functions for getting info from the file
        function getDOM:TXMLDocument; //Gets everything
        function hasSheets:boolean;
        function getSheets:TDaveCADSheetList;

        //some stuff, the usual.
        constructor create;
        constructor create(E: TDaveCadFileErrorCallback); //adds callback too
        destructor destroy; override;

        property IsModified: boolean read fModified;
        property fileName: string read fFileNAme;

    end;

const
  FileSaveVersionNumber: string = '0.1';

implementation

procedure TDaveCadFile.modify;
begin
  fModified:=true;
end;

constructor TDaveCadFile.create;
begin
  inherited create;
  //just some inits...
  fFileName := '';
  fFileValid := false;
  fModified:=false;
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
  sheetList.Free;
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
    fModified:=false;
    result:= false;
  end;
end;

procedure TDaveCadFile.new;
var
  Parser: TDOMParser;
  Src: TXMLInputSource;
  InputS: TResourceStream;
begin
  closeFile;
  try
        fFileValid := true;
        // create a parser object
        Parser := TDOMParser.Create;
        // get the data for a blank file
        InputS := TResourceStream.Create(HInstance, 'newFile', PChar(10));//is PChar(10) dodgy????
        // create the input source
        Src := TXMLInputSource.Create(InputS);
        // we want validation
        Parser.Options.Validate := True;
        // assign a error handler which will receive notifications
        Parser.OnError := @XMLErrorHandler;
        // now do the job
        Parser.Parse(Src, fFile);
  except
  end;
  InputS.free;
  Parser.Free;
  Src.Free;

  session.SelectedSheet:='sheet1';
  fFileValid:=true;
  modify;
end;

procedure TDaveCadFile.newFileName(name: string);
begin
    fFileName:=name;
    //file modified
end;

//This function adds a sheet to the currently loaded file.
procedure TDaveCadFile.addSheet(name_s: string; media_s: string; author_s: string; date_s: string);
var sheet, properties, objects, name, media, author, date: TDOMElement;
  name_data, media_data, author_data, date_data: TDOMText;
begin
  //make a new sheet
  sheet := fFile.CreateElement('sheet');
    //give the sheet some properties
    properties := fFile.CreateElement('properties');
    sheet.AppendChild(properties);

      name := fFile.CreateElement('property'); name.SetAttribute('name', 'sheet-name');
      media := fFile.CreateElement('property'); media.SetAttribute('name', 'media');
      author := fFile.CreateElement('property'); author.SetAttribute('name', 'author');
      date := fFile.CreateElement('property'); date.SetAttribute('name', 'date');

      properties.AppendChild(name);
      properties.AppendChild(media);
      properties.AppendChild(author);
      properties.AppendChild(date);

      name_data := (fFile.CreateTextNode(name_s));
      media_data := (fFile.CreateTextNode(media_s));
      author_data := (fFile.CreateTextNode(author_s));
      date_data := (fFile.CreateTextNode(date_s));

      name.AppendChild(name_data);
      media.AppendChild(media_data);
      author.AppendChild(author_data);
      date.AppendChild(date_data);

      //Add the objects section
      objects := fFile.CreateElement('objects');
      sheet.AppendChild(objects);

    fFile.DocumentElement.AppendChild(sheet);
    modify;

    //<sillyjoke>
    //Storm the bastille! (Pascal was french you know)
    sheet.Free;
    properties.Free;
    objects.Free;
    name.Free;
    media.Free;
    author.Free;
    date.Free;
    //I don't know where these prisoners came from
    name_data.Free;
    media_data.Free;
    author_data.Free;
    date_data.Free;
    //</sillyjoke>
end;

function TDaveCadFile.deleteSheet(name_s:string): integer;
var sheets: TDOMNodeList;
  sheet: TDaveCADSheet;
  i : integer;
begin
  sheets := fFile.DocumentElement.GetElementsByTagName('sheet');
  for i := 0 to sheets.Count-1 do begin
      sheet := TDaveCADSheet.create;
      sheet.loadFrom(TDOMElement(sheets.Item[i]));
      if sheet.Name = name_s then //we have out guy
      begin
        fFile.DocumentElement.RemoveChild(sheets.Item[i]);
        result:= 0;
        sheet.Free;
        sheets.Free;
        exit;
      end;
  end;
  sheet.Free;
  sheets.Free;
  result := DCAD_WARN_INVALID_SHEET_SELECTED;
end;

procedure TDaveCadFile.updateSheetProps(sheet: TDOMElement; newname, newAuthor, newDate, newMedia: string);
var properties: TDOMNodeList;
    i: integer;
    ThisProp: TDOMElement;
begin
  properties := TDOMElement(sheet.FindNode('properties')).GetElementsByTagName('property');
    for i := 0 to properties.Count-1 do begin
      ThisProp := TDOMElement(properties.Item[i]);
      if ThisProp.GetAttribute('name') = 'sheet-name' then
        ThisProp.ReplaceChild(fFile.CreateTextNode(newName), ThisProp.FirstChild);
      if ThisProp.GetAttribute('name') = 'author' then
        ThisProp.ReplaceChild(fFile.CreateTextNode(newAuthor), ThisProp.FirstChild);
      if ThisProp.GetAttribute('name') = 'media' then
        ThisProp.ReplaceChild(fFile.CreateTextNode(newMedia), ThisProp.FirstChild);
      if ThisProp.GetAttribute('name') = 'date' then
        ThisProp.ReplaceChild(fFile.CreateTextNode(newDate), ThisProp.FirstChild);
    end;
  modify;
  properties.Free;
  ThisProp.Free;
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
  sheets.Free;
end;

function TDaveCadFile.getSheets:TDaveCADSheetList;
var sheets: TDOMNodeList;
  sheet: TDaveCADSheet;
  i: integer;
begin
  sheets := fFile.DocumentElement.GetElementsByTagName('sheet');
  result := TDaveCADSheetList.Create;
  for i := 0 to sheets.Count-1 do begin
      sheet := TDaveCADSheet.create;
      sheet.loadFrom(TDOMElement(sheets.Item[i]));
      result.add(sheet);
  end;
  sheet.Free;
end;

end.

