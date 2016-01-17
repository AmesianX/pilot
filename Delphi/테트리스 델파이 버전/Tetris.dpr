program Tetris;

{%TogetherDiagram 'ModelSupport_Tetris\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tetris\UI_Interface\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tetris\frmMain\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tetris\Tetris\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tetris\Logic_Interface\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tetris\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Tetris\Tetris\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Tetris\Logic_Interface\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Tetris\UI_Interface\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Tetris\frmMain\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Tetris\BlockCell\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tetris\BlockShape\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tetris\GameTimer\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tetris\BlockCell\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Tetris\BlockShape\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Tetris\GameTimer\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Tetris\Unit1\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tetris\CanvasPanel\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tetris\Unit1\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Tetris\_frmMain\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tetris\_frmMain\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Tetris\CanvasPanel\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Tetris\Block\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tetris\Block\default.txvpck'}
{%TogetherDiagram 'ModelSupport_Tetris\RotateBlocks\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_Tetris\RotateBlocks\default.txvpck'}

uses
  Forms,
  UI_Interface in 'UI_Interface.pas',
  Logic_Interface in 'Logic_Interface.pas',
  GameTimer in 'GameTimer.pas',
  BlockCell in 'BlockCell.pas',
  BlockShape in 'BlockShape.pas',
  CanvasPanel in 'CanvasPanel.pas',
  frmMain in 'frmMain.pas' {fmMain},
  _frmMain in '_frmMain.pas',
  Block in 'Block.pas',
  RotateBlocks in 'RotateBlocks.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
