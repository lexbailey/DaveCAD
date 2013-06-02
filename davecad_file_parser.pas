unit davecad_file_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_DOM, davecad_enum;

type

  //Class for loading information about an object from a TDaveCADFile.getDOM
  TDaveCADObject = class (TObject)
    private
      fObjName: string; //retrieved from the the XML text node, this identifies the object
      fP1, fP2: TPoint; //these are the points related to the object, usually on the origin (fP1) is used, fP2 is used in (for example) lines.
      fTool: integer;
      fColour: integer;
    public

      procedure loadFrom(dcObject: TDOMElement);

      property Name: string read fObjName;
      property Origin: TPoint read fP1;
      property point1: TPoint read fP1; //alias for origin for using where it makes more sense
      property point2: TPoint read fP2;

      //etc...
      property originX: longint read fP1.x;
      property originY: longint read fP1.y;
      property point1X: longint read fP1.x;
      property point1Y: longint read fP1.y;
      property point2X: longint read fP2.x;
      property point2Y: longint read fP2.y;

      property colour: integer read fColour;
  end;

  //Class for loading information about a sheet from a TDaveCADFile.getDOM
  TDaveCADSheet = class(TObject)
    private
      fName: string;
      fAuthor: string;
      fDate: string;
      fMedia: string;
      fObjects: array of TDaveCADObject;

      function getObject(id: integer): TDaveCADObject;
      function getObjCount: integer;

    public
      //This function loads the sheet from the TDOMElemnt named 'sheet'
      procedure loadFrom(sheet: TDOMElement);
      procedure loadWithObjectFrom(sheet: TDOMElement);

      //properties for all loaded attributes
      property Name: string read fName;
      property Author: string read fAuthor;
      property Date: string read fDate;
      property Media: string read fMedia;

      //objects
      property objects[id: integer]: TDaveCADObject read getObject;
      property objectCount: integer read getObjCount;

      //stuff, mostly legal.
      constructor create;

  end;

  //A list object for used for containing all sheets in a file
  TDaveCADSheetList = class(TObject)
    private
      fItems: array of TDaveCADSheet;
      fLastSheet: integer;
      function GetItem(index: LongWord): TDaveCADSheet;
      function GetCount: LongWord;
      function GetSheet(name: string): TDaveCADSheet;
    public
      procedure add(sheet: TDaveCADSheet);
      property Item[index: longword]: TDaveCADSheet read GetItem; default;
      property count: LongWord read GetCount;
      property lastSheet: integer read fLastSheet;
      property Sheet[name: string]: TDaveCADSheet read GetSheet;
  end;

implementation

  //----------------------------------------------------------------------------
  // TDaveCADSheet
  //----------------------------------------------------------------------------
  constructor TDaveCADSheet.create;
  begin
    inherited create;
    fName := '';
    fAuthor := '';
    fDate := '';
    fMedia := '';
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
    ThisProp.Free;
  end;

  procedure TDaveCADSheet.loadWithObjectFrom(sheet: TDOMElement);
  var mobjects: TDOMNodeList;
    i: integer;
    ThisObj: TDOMElement;
    ThisdcObj: TDaveCADObject;
  begin
    loadFrom(sheet);
    mobjects := TDOMElement(sheet.FindNode('objects')).GetElementsByTagName('object');
    for i := 0 to mobjects.Count-1 do begin
      ThisObj := TDOMElement(mobjects.Item[i]);
      ThisdcObj := TDaveCADObject.Create;
      ThisdcObj.loadFrom(ThisObj);
      setLength(fObjects, length(fObjects) +1); //make room!
      fObjects[length(fObjects)-1] := ThisdcObj;
    end;
    ThisdcObj.Free;
    ThisObj.Free;
  end;

  function TDaveCADSheet.getObject(id: integer):TDaveCADObject;
  begin
    result := fObjects[id];
  end;

  function TDaveCADSheet.getObjCount: integer;
  begin
    result := length(fObjects);
  end;

  //----------------------------------------------------------------------------
  // TDaveCADSheetList
  //----------------------------------------------------------------------------

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
    fLastSheet := -1;
    for i := 0 to count-1 do begin
      if getItem(i).fName = name then begin
        result := getItem(i);
        fLastSheet := i;
        exit;
      end;
    end;
    result := nil;
  end;

  //----------------------------------------------------------------------------
  // TDaveCADObject
  //----------------------------------------------------------------------------
  procedure TDaveCADObject.loadFrom(dcObject: TDOMElement);
  begin
    fObjName := dcObject.TextContent;
    fP1:=point(strtoint(dcObject.GetAttribute('top')), strtoint(dcObject.GetAttribute('left')));
    if dcObject.hasAttribute('top1') and dcObject.hasAttribute('top1') then
      fP2:=point(strtoint(dcObject.GetAttribute('top1')), strtoint(dcObject.GetAttribute('left1')));

    fTool:=getDrawTool(dcObject.GetAttribute('tool'));
    fColour:=getColour(dcObject.GetAttribute('colour'));
  end;

end.


