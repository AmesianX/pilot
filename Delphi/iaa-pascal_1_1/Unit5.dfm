object Form5: TForm5
  Left = 158
  Top = 93
  BorderStyle = bsDialog
  Caption = 'Licencja'
  ClientHeight = 383
  ClientWidth = 510
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 510
    Height = 312
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier'
    Font.Style = []
    Lines.Strings = (
      'IAA-pascal - interpreter jêzyka Pascal'
      'Copyright (C) 2005 Artur Moœcicki, Igor Kruk, Adam Miler'
      ''
      'Niniejszy program jest wolnym oprogramowaniem; mo¿esz go '
      'rozprowadzaæ dalej i/lub modyfikowaæ na warunkach Powszechnej'
      'Licencji Publicznej GNU, wydanej przez Fundacjê Wolnego'
      'Oprogramowania - wed³ug wersji 2-giej tej Licencji lub którejœ'
      'z póŸniejszych wersji. '
      ''
      
        'Niniejszy program rozpowszechniany jest z nadziej¹, i¿ bêdzie on' +
        ' '
      'u¿yteczny - jednak BEZ JAKIEJKOLWIEK GWARANCJI, nawet domyœlnej '
      
        'gwarancji PRZYDATNOŒCI HANDLOWEJ albo PRZYDATNOŒCI DO OKREŒLONYC' +
        'H '
      'ZASTOSOWAÑ. W celu uzyskania bli¿szych informacji - Powszechna '
      'Licencja Publiczna GNU. '
      ''
      
        'Z pewnoœci¹ wraz z niniejszym programem otrzyma³eœ te¿ egzemplar' +
        'z '
      
        'Powszechnej Licencji Publicznej GNU (GNU General Public License)' +
        ';'
      
        'jeœli nie - napisz do Free Software Foundation, Inc., 675 Mass A' +
        've,'
      'Cambridge, MA 02139, USA.'
      ''
      
        '----------------------------------------------------------------' +
        '---'
      ''
      'IAA-pascal - the interpreter of Pascal language'
      'Copyright (C) 2005 Artur Moœcicki, Igor Kruk, Adam Miler'
      ''
      'This program is free software; you can redistribute it and/or'
      'modify it under the terms of the GNU General Public License'
      'as published by the Free Software Foundation; either version 2'
      'of the License, or (at your option) any later version.'
      ''
      'This program is distributed in the hope that it will be useful,'
      'but WITHOUT ANY WARRANTY; without even the implied warranty of'
      'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the'
      'GNU General Public License for more details.'
      ''
      
        'You should have received a copy of the GNU General Public Licens' +
        'e'
      'along with this program; if not, write to the Free Software'
      'Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  '
      '02110-1301, USA')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 312
    Width = 510
    Height = 71
    Align = alBottom
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 200
      Height = 25
      Caption = 'Powszechna Licencja Publiczna GNU'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 8
      Top = 32
      Width = 200
      Height = 25
      Caption = 'GNU General Public License '
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 248
      Top = 8
      Width = 233
      Height = 25
      Caption = 'Licencja GNU GPL programu'
      TabOrder = 2
      OnClick = Button3Click
    end
    object BitBtn1: TBitBtn
      Left = 248
      Top = 32
      Width = 233
      Height = 25
      Caption = '&Zamknij'
      TabOrder = 3
      Kind = bkClose
    end
  end
end
