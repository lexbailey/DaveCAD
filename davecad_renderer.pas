unit davecad_renderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, davecad_file_parser, graphics, LResources, davecad_enum;

procedure initRenderer;
procedure freeRenderer;
procedure renderSheet(sheet: TDaveCADSheet; canvas: TCanvas; cwidth, cheight: integer; scale: double; translation : TPoint);
procedure renderObject(obj: TDaveCADObject; canvas: TCanvas; startX, startY: integer; scale: double; translation : TPoint);
function getOrigin(sheet: TDaveCADSheet; cwidth, cheight: integer; scale: double): TPoint;

var
  postitPic, notepadA4Pic : TPicture;

implementation

procedure initRenderer;
begin
  postitPic := TPicture.Create;
  postitPic.LoadFromLazarusResource(RENDER_MEDIA_POST_IT);

  notepadA4Pic := TPicture.Create;
  notepadA4Pic.LoadFromLazarusResource(RENDER_MEDIA_NOTEBOOK_A4);
end;

procedure freeRenderer;
begin
  postitPic.Free;
  notepadA4Pic.Free;
end;

procedure renderSheet(sheet: TDaveCADSheet; canvas: TCanvas; cwidth, cheight: integer; scale: double; translation : TPoint);
var picture: TPicture;
  brush: TBrush;

  centreX, centreY: integer;
  startX, startY: integer;
  i: integer;
  picWidth, picHeight: integer;
begin
  //get some stuff
  centreX := round(cwidth/2);
  centreY := round(cheight/2);

  //Whitewash
  brush := TBrush.Create;
  brush.Color:=clWhite;
  canvas.Brush:=brush;
  canvas.FillRect(0,0,cWidth, cHeight);

  //draw the background for the sheet we are viewing
  if (sheet.Media = RENDER_MEDIA_POST_IT) or (sheet.Media = RENDER_MEDIA_NOTEBOOK_A4) then begin
    if sheet.Media = RENDER_MEDIA_POST_IT then picture := postitPic;
    if sheet.Media = RENDER_MEDIA_NOTEBOOK_A4 then picture := notepadA4Pic;

    picWidth := round(picture.Width*scale);
    picHeight := round(picture.Height*scale);
    startX := centreX-round(picWidth/2);
    startY := centreY-round(picHeight/2);
    canvas.StretchDraw(rect(startX,startY, startX+picWidth, startY+ picHeight),picture.Bitmap);

    for i:= 0 to sheet.objectCount-1 do begin
      renderObject(sheet.objects[i], canvas, startX, startY, scale, translation);
    end;

  end else
  begin
    //show message.
  end;
  brush.Free;

end;

procedure renderObject(obj: TDaveCADObject; canvas: TCanvas; startX, startY: integer; scale: double; translation: TPoint);
var pen: TPen;
begin
  //tool dependant pen options
  pen := TPen.Create;
  case obj.tool of
    DRAW_TOOL_PENCIL: pen.Width:=round(2*scale);
    DRAW_TOOL_BALLPOINT: pen.Width:=round(3*scale);
    DRAW_TOOL_FELT: pen.Width:=round(5*scale);
  end;

  if obj.tool <> DRAW_TOOL_PENCIL then begin //pencil doesn't have a colour, it is always grey
    pen.Color:=getTColor(obj.colour);
  end else
  begin
    pen.Color:=clGray;
  end;

  //init pen
  canvas.Pen := pen;

  if obj.Name = 'line' then begin
    canvas.MoveTo(round(obj.point1X*scale)+startX, round(obj.point1Y*scale)+startY);
    canvas.LineTo(round(obj.point2X*scale)+startX, round(obj.point2Y*scale)+startY);
  end;

  pen.Free;
end;

function getOrigin(sheet: TDaveCADSheet; cwidth, cheight: integer; scale: double): TPoint;
var picture: TPicture;
  brush: TBrush;

  centreX, centreY: integer;
  startX, startY: integer;
  i: integer;
  picWidth, picHeight: integer;
begin
  //get some stuff
  centreX := round(cwidth/2);
  centreY := round(cheight/2);

  startX := 0;
  startY := 0;

  //blah blah etc...
  if (sheet.Media = RENDER_MEDIA_POST_IT) or (sheet.Media = RENDER_MEDIA_NOTEBOOK_A4) then begin
    picture := TPicture.Create;
    picture.LoadFromLazarusResource(sheet.Media);
    picWidth := round(picture.Width*scale);
    picHeight := round(picture.Height*scale);
    startX := centreX-round(picWidth/2);
    startY := centreY-round(picHeight/2);
    picture.Free;
  end;
  result := point(startX, startY);
end;

initialization

  {$I post-it.lrs}
  {$I notepad-A4.lrs}

end.


