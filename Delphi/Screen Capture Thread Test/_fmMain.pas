unit _fmMain;

interface

uses
  DebugTools, SimpleThread, ScreenCapture, RyuGraphics, SyncObjs,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    ScrollBox: TScrollBox;
    Image: TImage;
    procedure FormCreate(Sender: TObject);
  private
    procedure do_WM_USER(var AMsg:TMessage); message WM_USER;
  private
    FCS : TCriticalSection;
    FDeskCanvas : TCanvas;
    FSimpleThread : TSimpleThread;
    procedure on_Repeat(Sender:TObject);
  public
    procedure ScreenCapture;
  end;

var
  fmMain: TfmMain;

implementation

type
  TRepeater = class (TThread)
  private
    FMainForm : TfmMain;
  protected
    procedure Execute; override;
  public
    constructor Create(AMainForm:TfmMain); reintroduce;
  end;

{ TRepeater }

constructor TRepeater.Create(AMainForm:TfmMain);
begin
  FMainForm := AMainForm;

  inherited Create(false);
end;

procedure TRepeater.Execute;
begin
  while not Terminated do begin
    Synchronize(FMainForm.ScreenCapture);
    PostMessage(FMainForm.Handle, WM_USER, 0, 0);
    Sleep(5);
  end;
end;

{$R *.dfm}

{ TfmMain }

procedure TfmMain.do_WM_USER(var AMsg: TMessage);
begin
  FCS.Enter;
  try
    Image.Repaint;
  finally
    FCS.Leave;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Image.Picture.Bitmap.PixelFormat := pf32bit;

  FCS := TCriticalSection.Create;
  FDeskCanvas := TCanvas.Create;

  TRepeater.Create(Self);
//  FSimpleThread := TSimpleThread.Create(on_Repeat);
end;

procedure TfmMain.on_Repeat(Sender: TObject);
begin
  while not FSimpleThread.Terminated do begin
    ScreenCapture;
    PostMessage(Handle, WM_USER, 0, 0);
    Sleep(5);
  end;
end;

procedure TfmMain.ScreenCapture;
var
  hWin : Cardinal;
  DC : HDC;
  iWidth, iHeight : integer;
  SRect, DRect : TRect;
  Src, Dst : TBitmap;
begin
  hWin := GetDesktopWindow;
  DC := GetWindowDC(hWin) ;
  if DC = 0 then Exit;

  try
    FDeskCanvas.Handle := DC;

    iWidth := GetDeviceCaps(DC, HORZRES);
    iHeight := GetDeviceCaps(DC, VERTRES);

    SRect := Rect(0, 0, iWidth, iHeight);
    DRect := Rect(0, 0, iWidth, iHeight);

    FCS.Enter;
    try
//      Src := TBitmap.Create;
      try
//        Src.PixelFormat := pf32bit;
//        Src.Width := iWidth;
//        Src.Height := iHeight;
//        Src.Canvas.CopyRect(DRect, FDeskCanvas, SRect);

        Dst := Image.Picture.Bitmap;
        Dst.PixelFormat := pf32bit;
        Dst.Width := iWidth;
        Dst.Height := iHeight;
        Dst.Canvas.CopyRect(DRect, FDeskCanvas, SRect);

//        Move(Src.ScanLine[iHeight-1]^, Dst.ScanLine[iHeight-1]^, iWidth * iHeight * 4);
      finally
//        FreeAndNil(Src);
      end;
    finally
      FCS.Leave;
    end;
  finally
    DeleteDC(DC);
  end;
end;

end.


procedure ScreenCapture(Dst:TBitmap);
var
  DC : HDC;
  hWin : Cardinal;
  iWidth, iHeight : integer;
  Bitmap : TBitmap;
begin
  hWin := GetDesktopWindow;
  DC := GetDC(hWin) ;
  if DC = 0 then Exit;

  try
    iWidth := GetDeviceCaps(DC, HORZRES);
    iHeight := GetDeviceCaps(DC, VERTRES);

    Bitmap := TBitmap.Create;
    try
      Bitmap.PixelFormat := pf32bit;
      Bitmap.Width := iWidth;
      Bitmap.Height := iHeight;
      if not BitBlt(Bitmap.Canvas.Handle, 0, 0, iWidth, iHeight, DC, 0, 0, SRCCOPY) then
       raise Exception.Create('Error ScreenCapture');

      Dst.PixelFormat := pf32bit;
      Dst.Width := iWidth;
      Dst.Height := iHeight;

      Move(Bitmap.ScanLine[iHeight-1]^, Dst.ScanLine[iHeight-1]^, iWidth * iHeight * 4);
    finally
      Bitmap.FreeImage;
      FreeAndNil(Bitmap);
    end;
  finally
    ReleaseDC(hWin, DC);
  end;
end;


uses
  Windows   // we need it since we will use some WIndows APIs imported in this unit
  , Graphics  // unit where TBitmap is defined
  ;


// this procedure will capture the screen content into the suppliet ABitmap parameter.
procedure CaptureScreen(ABitmap: TBitmap)
var
  vDesktopDC: HDC;   // variable to store the device context handle of desktop window
begin
  // get the device context handle of current desktop window
  vDesktopDC := GetWindowDC(GetDesktopWindow);
  try
      // adjust the dimension and format of the supplied bitmap to match the screen
      ABitmap.PixelFormat := pf24bit;
      ABitmap.Height := Screen.Height;
      ABitmap.Width := Screen.Width;

      // draw the content of desktop into ABitmap
      BitBlt(ABitmap.Canvas.Handle, 0, 0, ABitmap.Width, ABitmap.Height, vDesktopDC, 0, 0, SRCCOPY);
  finally
    // mark that we have done with the desktop device context
    ReleaseDC(GetDesktopWindow, vDesktopDC);
  end;
end

procedure CaptureScreen(AFileName: string);
const
  CAPTUREBLT = $40000000;
var
  Wnd : HWND;
  hdcScreen: HDC;
  hdcCompatible: HDC;
  bmp: TBitmap;
  hbmScreen: HBITMAP;
begin
  Wnd := GetDesktopWindow ;
  hdcScreen := GetWindowDC(Wnd);
  hdcCompatible := CreateCompatibleDC(hdcScreen);
  hbmScreen := CreateCompatibleBitmap(hdcScreen,
    GetDeviceCaps(hdcScreen, HORZRES),
    GetDeviceCaps(hdcScreen, VERTRES));
  SelectObject(hdcCompatible, hbmScreen);
  bmp := TBitmap.Create;
  bmp.Handle := hbmScreen;
  BitBlt(hdcCompatible,
    0, 0,
    bmp.Width, bmp.Height,
    hdcScreen,
    0, 0,
    SRCCOPY or CAPTUREBLT);
  bmp.SaveToFile(AFileName);
  bmp.Free;
  DeleteDC(hdcScreen);
  DeleteDC(hdcCompatible);
  DeleteObject(hbmScreen);
end;
