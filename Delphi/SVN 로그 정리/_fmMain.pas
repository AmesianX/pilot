unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    Panel1: TPanel;
    moMsg: TMemo;
    btRun: TButton;
    moOutput: TMemo;
    Splitter1: TSplitter;
    SaveDialog: TSaveDialog;
    btSave: TButton;
    procedure btRunClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
  private
    procedure save_Log(AStringList:TStringList);
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btRunClick(Sender: TObject);
var
  Loop: Integer;
  Lines : string;
  ToSkip : boolean;
  StringList : TStringList;
begin
  Lines := '';

  StringList := TStringList.Create;

  ToSkip := false;
  for Loop := 0 to moMsg.Lines.Count-1 do begin
    Lines := moMsg.Lines[Loop];
    if (Pos('Author:', Lines) > 0) or (Pos('Message:', Lines) > 0) then Continue;

    if Pos('--', Lines) > 0 then ToSkip := true;

    if Trim(Lines) = '' then begin
      if StringList.Count > 0 then save_Log(StringList);

      StringList.Clear;
      ToSkip := false;
    end else begin
      if not ToSkip then StringList.Add(Lines);
    end;
  end;

  if StringList.Count > 0 then save_Log(StringList);

  StringList.Free;
end;

procedure TfmMain.btSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    moOutput.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TfmMain.save_Log(AStringList: TStringList);
begin
  moOutput.Lines.Add(AStringList.Text);
  moOutput.Lines.Add('');
end;

end.

Revision: 607
Author: Ryu
Date: 2012년 12월 26일 수요일 오후 4:37:38
Message:
메모리 풀을 4KB 까지 확대, 메모리 풀 교차 현상이 일어나는 것을 수정, 잔상 버그 수정
----
Modified : /Lib - Appliance/RoomList.pas
Modified : /Lib - Appliance/RoomServer.Socket.pas
Modified : /Lib - Appliance/RoomServer.pas
Modified : /Src - Appliance/Receiver/ReceiverA.dproj
Modified : /Src - Appliance/Receiver/ReceiverA.res
Modified : /Src - Appliance/Server/APPSRV.DB
