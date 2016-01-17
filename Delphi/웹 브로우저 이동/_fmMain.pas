unit _fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OleCtrls, SHDocVw_EWB, EwbCore, EmbeddedWB, StdCtrls, Buttons,
  ExtCtrls, SHDocVw;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    Edit1: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    WebBrowser1: TWebBrowser;
    procedure WebBrowser1CommandStateChange(ASender: TObject; Command: Integer;
      Enable: WordBool);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.BitBtn1Click(Sender: TObject);
begin
  WebBrowser1.GoBack;
end;

procedure TForm2.BitBtn2Click(Sender: TObject);
begin
  WebBrowser1.GoForward;
end;

procedure TForm2.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if key = #13 then begin
    Key:= #0;
    WebBrowser1.Navigate(Edit1.text);
  end;
end;

procedure TForm2.WebBrowser1CommandStateChange(ASender: TObject;
  Command: Integer; Enable: WordBool);
begin
 case Command of
      CSC_NAVIGATEBACK  :
       begin
         BitBtn1.Enabled    := Enable;
       end;
     CSC_NAVIGATEFORWARD:
      begin
        BitBtn2.Enabled := Enable;
      end;
 end;
end;

end.

