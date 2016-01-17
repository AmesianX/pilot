unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    btOpen: TButton;
    Button1: TButton;
    Timer: TTimer;
    procedure btOpenClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FHandle : pointer;
  public
  end;

var
  fmMain: TfmMain;

implementation

function createBitmapOutput(parentHandle:THandle; w,h:integer):pointer; cdecl;
         external 'libBitmapOutput.dll';

function getWindowHandle(pHandle:pointer):THandle; cdecl;
         external 'libBitmapOutput.dll';

function setWindowSize(pHandle:pointer; w,h:integer):THandle; cdecl;
         external 'libBitmapOutput.dll';

{$R *.dfm}

procedure TfmMain.btOpenClick(Sender: TObject);
begin
  FHandle := createBitmapOutput( Self.Handle, Width, Height );
end;

procedure TfmMain.Button1Click(Sender: TObject);
begin
  setWindowSize( FHandle, Width div 2, Height div 2 );
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FHandle := nil;
//  FHandle := createBitmapOutput( Self.Handle, Width, Height );

  Timer.Enabled := true;
end;

procedure TfmMain.FormResize(Sender: TObject);
begin
  if FHandle <> nil then
    setWindowSize( FHandle, Width div 2, Height div 2 );
end;

end.
