unit _frMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrMain = class(TFrame)
    Button1: TButton;
  private
  public
    procedure MakeHole;
  end;

implementation

{$R *.dfm}

{ TfrMain }

procedure TfrMain.MakeHole;
var
  hRect, hDelete, hResult: THandle;
begin
  hResult := CreateRectRgn(0, 0, Width, Height);
  hRect := CreateRectRgn(0, 0, Width, Height);

  hDelete := CreateRectRgn( 0, 0, Width div 2, Height );

  if CombineRgn(hResult, hRect, hDelete, RGN_XOR) <> ERROR then begin
    SetWindowRgn(Handle, hResult, True);
  end;
end;

end.
