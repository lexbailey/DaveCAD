unit davecad_renderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, davecad_file_parser, graphics, LResources;

procedure renderSheet(sheet: TDaveCADSheet; canvas: TCanvas; cwidth, cheight: integer);

const
  RENDER_MEDIA_POST_IT = 'post-it';
  RENDER_MEDIA_NOTEBOOK_A4 = 'notebook-A4';

  RENDER_TOOL_BALL_POINT = 'ball-point';
  RENDER_TOOL_FELT_TIP = 'felt-tip';
  RENDER_TOOL_PENCIL = 'pencil';

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
  if sheet.Media = RENDER_MEDIA_POST_IT then begin
     picture := TPicture.Create;
     picture.LoadFromLazarusResource('post-it');
     canvas.Draw(centreX-round(picture.Width/2),centreY-round(picture.Height/2),picture.Bitmap);
  end;
end;

initialization

  {$I post-it.lrs}

end.

