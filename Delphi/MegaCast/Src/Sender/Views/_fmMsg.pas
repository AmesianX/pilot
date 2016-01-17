unit _fmMsg;

interface

uses
  ValueList,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmMsg = class(TForm)
    moMsg: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
    procedure Add(AValue:string);
  end;

var
  fmMsg: TfmMsg;

implementation

uses
  Global, View, Option;

{$R *.dfm}

procedure TfmMsg.Add(AValue: string);
begin
  moMsg.Lines.Add(AValue);
  moMsg.Lines.Add('****************************************************');
  moMsg.Lines.Add('');
end;

procedure TfmMsg.FormCreate(Sender: TObject);
begin
  TView.Obj.Add(Self);

  {$IFDEF DEBUG}
    Show;
  {$ENDIF}
end;

procedure TfmMsg.FormDestroy(Sender: TObject);
begin
  TView.Obj.Remove(Self);
end;

end.
