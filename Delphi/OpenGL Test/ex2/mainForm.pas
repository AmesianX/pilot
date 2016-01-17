unit mainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs,  StdCtrls, ExtCtrls, ComCtrls,
  OpenGL;

type
  TFormMain = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure Draw; //Draws an OpenGL scene on request
  public
    angle : single;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

procedure setupPixelFormat(DC:HDC);
 const
  pfd : TPIXELFORMATDESCRIPTOR =
         (nSize           : sizeof(TPIXELFORMATDESCRIPTOR); // size
          nVersion        : 1;                // version
          dwFlags         : PFD_SUPPORT_OPENGL or PFD_DRAW_TO_WINDOW or
                            PFD_DOUBLEBUFFER; // support double-buffering
          iPixelType      : PFD_TYPE_RGBA;    // color type
          cColorBits      : 24;               // preferred color depth
          cRedBits        :  0;
          cRedShift       :  0;               // color bits (ignored)
          cGreenBits      :  0;
          cGreenShift     :  0;
          cBlueBits       :  0;
          cBlueShift      :  0;
          cAlphaBits      :  0;
          cAlphaShift     :  0;               // no alpha buffer
          cAccumBits      :  0;
          cAccumRedBits   :  0;               // no accumulation buffer,
          cAccumGreenBits :  0;               // accum bits (ignored)
          cAccumBlueBits  :  0;
          cAccumAlphaBits :  0;
          cDepthBits      : 16;               // depth buffer
          cStencilBits    :  0;               // no stencil buffer
          cAuxBuffers     :  0;               // no auxiliary buffers
          iLayerType      : PFD_MAIN_PLANE;   // main layer
          bReserved       :  0;
          dwLayerMask     :  0;
          dwVisibleMask   :  0;
          dwDamageMask    :  0; );            // no layer, visible, damage masks

 Var
  pixelFormat : integer;
begin
 pixelFormat := ChoosePixelFormat(DC, @pfd);
 if (pixelFormat = 0) then exit;
 if (SetPixelFormat(DC, pixelFormat, @pfd) <> TRUE) then exit;
end;

procedure GLInit;
begin
   // set viewing projection
   glMatrixMode(GL_PROJECTION);
   glFrustum(-0.1, 0.1, -0.1, 0.1, 0.3, 25.0);
   // position viewer
   glMatrixMode(GL_MODELVIEW);
   glEnable(GL_DEPTH_TEST);


end;

Procedure TFormMain.FormCreate(Sender: TObject);
 Var
  DC : HDC;
  RC : HGLRC;
   i : integer;
 begin
  DC := GetDC(Handle);        // Actually, you can use any windowed control here
  SetupPixelFormat(DC);
  RC := wglCreateContext(DC); // makes OpenGL window out of DC
  wglMakeCurrent(DC, RC);     // makes OpenGL window active
  GLInit;                     // initialize OpenGL
 end;

procedure TFormMain.Draw;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);  // Smaže obrazovku a hloubkov?buffer
  glLoadIdentity();	                                    // Reset matice
  glTranslatef(-1.5,0.0,-6.0);                          // Posun doleva a do hloubky
  glBegin(GL_TRIANGLES);                                // Zaèátek kreslen?troj?eln??
    glColor3f(1.0,0.0,0.0);                             // ?rven?barva
    glVertex3f( 0.0, 1.0, 0.0);                         // Horn?bod
    glColor3f(0.0,1.0,0.0);                             // Zelen?barva
    glVertex3f(-1.0,-1.0, 0.0);                         // Lev?doln?bod
    glColor3f(0.0,0.0,1.0);                             // Modr?barva
    glVertex3f( 1.0,-1.0, 0.0);                         // Prav?doln?bod
  glEnd();                                              // Ukon?n?kreslen?troj?eln??
  glTranslatef(3,0.0,0.0);                              // Posun o 3 jednotky doprava
  glColor3f(0.5,0.5,1.0);                               // Sv?le modr?barva
  glBegin(GL_QUADS);                                    // Zaèátek kreslen?obd?n??
    glVertex3f(-1.0, 1.0, 0.0);                         // Lev?horn?bod
    glVertex3f( 1.0, 1.0, 0.0);                         // Prav?horn?bod
    glVertex3f( 1.0,-1.0, 0.0);                         // Prav?doln?bod
    glVertex3f(-1.0,-1.0, 0.0);                         // Lev?doln?bod
  glEnd();                                              // Konec kreslen?obd?n??
  SwapBuffers(wglGetCurrentDC);
 end;

procedure TFormMain.FormPaint(Sender: TObject);
begin
 Draw;
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
 angle :=angle+1.0;
 Draw;
end;

end.

