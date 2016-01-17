object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 293
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object TreeView1: TTreeView
    Left = 24
    Top = 20
    Width = 121
    Height = 97
    DragMode = dmAutomatic
    Indent = 19
    TabOrder = 0
    OnDragDrop = TreeView1DragDrop
    OnDragOver = TreeView1DragOver
    OnStartDrag = TreeView1StartDrag
    Items.NodeData = {
      0103000000230000000000000000000000FFFFFFFFFFFFFFFF00000000000000
      000578007800780078007800230000000000000000000000FFFFFFFFFFFFFFFF
      00000000000000000579007900790079007900250000000000000000000000FF
      FFFFFFFFFFFFFF0000000000000000067A007A007A007A007A007A00}
  end
  object Memo1: TMemo
    Left = 233
    Top = 80
    Width = 185
    Height = 89
    ImeName = 'Microsoft IME 2003'
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
end
