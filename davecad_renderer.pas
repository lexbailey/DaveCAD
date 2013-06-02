unit davecad_renderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, davecad_file_parser, graphics, LResources, davecad_enum;

procedure renderSheet(sheet: TDaveCADSheet; canvas: TCanvas; cwidth, cheight: integer);

implementation

procedure renderSheet(sheet: TDaveCADSheet; canvas: TCanvas; cwidth, cheight: integer);
var picture: TPicture;
  brush: TBrush;

  centreX, centreY: integer;

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
     picture := TPicture.Create;
     picture.LoadFromLazarusResource(sheet.Media);
     canvas.Draw(centreX-round(picture.Width/2),centreY-round(picture.Height/2),picture.Bitmap);
     picture.Free;
  end;
  brush.Free;

end;

initialization

  {$I post-it.lrs}
  {$I notepad-A4.lrs}

end.

