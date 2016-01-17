unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure Notification(AComponent:TComponent; Operation:TOperation); override;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Edit1.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.FreeNotification(Self);
end;

procedure TForm1.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (AComponent = Edit1) and (Operation = opRemove) then Caption := 'Hi';
end;

end.
