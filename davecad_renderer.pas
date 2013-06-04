unit davecad_renderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, davecad_file_parser, graphics, LResources, davecad_enum;

procedure renderSheet(sheet: TDaveCADSheet; canvas: TCanvas; cwidth, cheight: integer);
procedure renderObject(obj: TDaveCADObject; canvas: TCanvas; startX, startY: integer);

implementation

procedure renderSheet(sheet: TDaveCADSheet; canvas: TCanvas; cwidth, cheight: integer);
var picture: TPicture;
  brush: TBrush;

  centreX, centreY: integer;
  startX, startY: integer;
  i: integer;

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
    //TODO, move picture section away from draw to speed things up
    picture := TPicture.Create;
    picture.LoadFromLazarusResource(sheet.Media);
    startX := centreX-round(picture.Width/2);
    startY := centreY-round(picture.Height/2);
    canvas.Draw(startX,startY,picture.Bitmap);
    picture.Free;

    for i:= 0 to sheet.objectCount-1 do begin
      renderObject(sheet.objects[i], canvas, startX, startY);
    end;

  end else
  begin
    //show message.
  end;
  brush.Free;

end;

procedure renderObject(obj: TDaveCADObject; canvas: TCanvas; startX, startY: integer);
var pen: TPen;
begin
  //tool dependant pen options
  pen := TPen.Create;
  case obj.tool of
    DRAW_TOOL_PENCIL: pen.Width:=2;
    DRAW_TOOL_BALLPOINT: pen.Width:=3;
    DRAW_TOOL_FELT: pen.Width:=5;
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
    canvas.MoveTo(obj.point1X+startX, obj.point1Y+startY);
    canvas.LineTo(obj.point2X+startX, obj.point2Y+startY);
  end;

  pen.Free;
end;

initialization

  {$I post-it.lrs}
  {$I notepad-A4.lrs}

end.


