program WhiteBoard;

{%TogetherDiagram 'ModelSupport_WhiteBoard\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_WhiteBoard\WhiteBoard\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_WhiteBoard\frmMain\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_WhiteBoard\default.txvpck'}
{%TogetherDiagram 'ModelSupport_WhiteBoard\frmMain\default.txvpck'}
{%TogetherDiagram 'ModelSupport_WhiteBoard\WhiteBoard\default.txvpck'}
{%TogetherDiagram 'ModelSupport_WhiteBoard\DrawShape\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_WhiteBoard\DrawShape\default.txvpck'}

uses
  Forms,
  frmMain in 'frmMain.pas' {Form1},
  DrawShape in 'DrawShape.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
