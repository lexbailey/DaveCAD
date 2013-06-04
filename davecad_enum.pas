unit davecad_enum;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;


function getDrawTool(tool: string):integer;
function drawToolName(tool: integer):string;
function getEditTool(tool: string):integer;
function getColour(colour: string):integer;
function colourName(colour: integer):string;
function getTColor(colour: integer):TColor;

const
  DRAW_TOOL_PENCIL = 0;
  DRAW_TOOL_BALLPOINT = 1;
  DRAW_TOOL_FELT = 2;

  EDIT_TOOL_DRAWFREE = 0;
  EDIT_TOOL_MOVE = 1;
  EDIT_TOOL_TEXT = 2;
  EDIT_TOOL_LINE = 3;

  COLOUR_RED = 0;
  COLOUR_BLUE = 1;
  COLOUR_GREEN = 2;
  COLOUR_BLACK = 3;

  RENDER_MEDIA_POST_IT = 'post-it';
  RENDER_MEDIA_NOTEBOOK_A4 = 'notepad-A4';

  RENDER_TOOL_BALL_POINT = 'ball-point';
  RENDER_TOOL_FELT_TIP = 'felt-tip';
  RENDER_TOOL_PENCIL = 'pencil';

implementation

function getDrawTool(tool: string):integer;
begin
  if tool = 'pencil' then result := DRAW_TOOL_PENCIL;
  if tool = 'ball-point' then result := DRAW_TOOL_BALLPOINT;
  if tool = 'felt-tip' then result := DRAW_TOOL_FELT;
end;

function drawToolName(tool: integer):string;
begin
  if tool = DRAW_TOOL_PENCIL then result := 'pencil';
  if tool = DRAW_TOOL_BALLPOINT then result := 'ball-point';
  if tool = DRAW_TOOL_FELT then result := 'felt-tip';
end;

function getEditTool(tool: string):integer;
begin
  if tool = 'freehand' then result := EDIT_TOOL_DRAWFREE;
  if tool = 'move' then result := EDIT_TOOL_MOVE;
  if tool = 'text' then result := EDIT_TOOL_TEXT;
end;

function getColour(colour: string):integer;
begin
  if colour = 'red' then result := COLOUR_RED;
  if colour = 'blue' then result := COLOUR_BLUE;
  if colour = 'green' then result := COLOUR_GREEN;
  if colour = 'black' then result := COLOUR_BLACK;
end;

function colourName(colour: integer):string;
begin
  if colour = COLOUR_RED then result := 'red';
  if colour = COLOUR_BLUE then result := 'blue';
  if colour = COLOUR_GREEN then result := 'green';
  if colour = COLOUR_BLACK then result := 'black';
end;

function getTColor(colour: integer):TColor;
begin
  if colour = COLOUR_RED then result := clRed;
  if colour = COLOUR_BLUE then result := clBlue;
  if colour = COLOUR_GREEN then result := clGreen;
  if colour = COLOUR_BLACK then result := clBlack;
end;

end.

