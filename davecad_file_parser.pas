unit davecad_file_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_DOM;

type
  TDaveCADSheet = class(TObject)
    private
      fName: string;
      fAuthor: string;
      fDate: string;
      fMedia: string;
    public
      procedure loadFrom(sheet: TDOMElement);
      function getElement: TDOMElement;

      property Name: string read fName;
      property Author: string read fAuthor;
      property Date: string read fDate;
      property Media: string read fMedia;

      constructor create;
      destructor destroy;

  end;

  TDaveCADSheetList = class(TObject)
    private
      fItems: array of TDaveCADSheet;
      function GetItem(index: LongWord): TDaveCADSheet;
      function GetCount: LongWord;
      function GetSheet(name: string): TDaveCADSheet;
    public
      procedure add(sheet: TDaveCADSheet);
      property Item[index: longword]: TDaveCADSheet read GetItem; default;
      property count: LongWord read GetCount;

      property Sheet[name: string]: TDaveCADSheet read GetSheet;
  end;

implementation

  constructor TDaveCADSheet.create;
  begin
    inherited create;
    fName := '';
    fAuthor := '';
    fDate := '';
    fMedia := '';
  end;

  destructor TDaveCADSheet.destroy;
  begin
    //nothing yet...
    inherited destroy;
  end;

  //Load a sheet from the DOM element
  procedure TDaveCADSheet.loadFrom(sheet: TDOMElement);
  var properties: TDOMNodeList;
    i: integer;
    ThisProp: TDOMElement;
  begin
    properties := TDOMElement(sheet.FindNode('properties')).GetElementsByTagName('property');
    for i := 0 to properties.Count-1 do begin
      ThisProp := TDOMElement(properties.Item[i]);
      if ThisProp.GetAttribute('name') = 'sheet-name' then
        fName := ThisProp.TextContent;
      if ThisProp.GetAttribute('name') = 'author' then
        fAuthor := ThisProp.TextContent;
      if ThisProp.GetAttribute('name') = 'media' then
        fMedia := ThisProp.TextContent;
      if ThisProp.GetAttribute('name') = 'date' then
        fDate := ThisProp.TextContent;
    end;
  end;

  function TDaveCADSheet.getElement: TDOMElement;
  begin
    //
  end;

  function TDaveCADSheetList.GetItem(index: LongWord): TDaveCADSheet;
  begin
    result := fItems[index];
  end;

  function TDaveCADSheetList.GetCount: LongWord;
  begin
    result := Length(fItems);
  end;

  procedure TDaveCADSheetList.add(sheet: TDaveCADSheet);
  begin
    setLength(fItems, Length(fItems)+1); //increase array size
    fItems[Length(fItems)-1] := sheet; //add in this sheet
  end;

  function TDaveCADSheetList.GetSheet(name: string): TDaveCADSheet;
  var i : integer;
  begin
    for i := 0 to count-1 do begin
      if getItem(i).fName = name then begin
        result := getItem(i);
        exit;
      end;
    end;
    result := nil;
  end;

end.

