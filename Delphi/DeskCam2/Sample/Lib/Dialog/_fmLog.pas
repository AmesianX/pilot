unit _fmLog;

interface

uses
  DeskCamAddonUtils,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmLog = class(TForm)
    gb_Cache: TGroupBox;
    gb_Packet: TGroupBox;
    gb_FrameBuffer: TGroupBox;
    gb_EncodeDecode: TGroupBox;
    edt_Cache_UsedAmount_Capacity: TLabeledEdit;
    edt_CacheRemainTime: TLabeledEdit;
    edt_CacheCount: TLabeledEdit;
    edt_CacheReusedCount: TLabeledEdit;
    edt_CacheRequestCount: TLabeledEdit;
    edt_CacheSendCount: TLabeledEdit;
    edt_CacheSendAmount: TLabeledEdit;
    edt_PacketSendCount: TLabeledEdit;
    edt_PacketSendAmount: TLabeledEdit;
    edt_PacketRecvCount: TLabeledEdit;
    edt_PacketRecvAmount: TLabeledEdit;
    edt_FrameBufferCount: TLabeledEdit;
    edt_FrameBufferAmount: TLabeledEdit;
    edt_EncodeTime: TLabeledEdit;
    edt_EncodeTotalTime: TLabeledEdit;
    edt_DecodeTime: TLabeledEdit;
    edt_DecodeTotalTime: TLabeledEdit;
    btn_Close: TButton;
    Timer: TTimer;
    edt_ElapsedTime: TLabeledEdit;
    btn_Pause: TButton;
    edt_CacheReusedAmount: TLabeledEdit;
    edt_PacketBlockRecvCount: TLabeledEdit;
    edt_PacketBlockRecvAmount: TLabeledEdit;
    procedure btn_CloseClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btn_PauseClick(Sender: TObject);
  private
    FLog: TLog;
    FStartTick: Cardinal;
    procedure DisplayLog;
  public
    property Log: TLog read FLog write FLog;
  end;

implementation

{$R *.dfm}

procedure TfmLog.btn_CloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmLog.btn_PauseClick(Sender: TObject);
begin
  Timer.Enabled := not Timer.Enabled;
end;

procedure TfmLog.DisplayLog;
var
  CurrentTick: Cardinal;
begin
  CurrentTick := GetTickCount;
  edt_ElapsedTime.Text := IntToStr(CurrentTick - FStartTick);

  // 캐쉬
  edt_Cache_UsedAmount_Capacity.Text := IntToStr(FLog.CacheUsedAmount) + ' / ' + IntToStr(FLog.CacheCapacity);
  edt_CacheRemainTime.Text := IntToStr(FLog.CacheRemainTime);
  edt_CacheCount.Text := IntToStr(FLog.CacheCount);
  edt_CacheReusedCount.Text := IntToStr(FLog.CacheReusedCount);
  edt_CacheReusedAmount.Text := IntToStr(FLog.CacheReusedAmount);
  edt_CacheRequestCount.Text := IntToStr(FLog.CacheRequestCount);
  edt_CacheSendCount.Text := IntToStr(FLog.CacheSendCount);
  edt_CacheSendAmount.Text := IntToStr(FLog.CacheSendAmount);

  // 패킷
  edt_PacketSendCount.Text := IntToStr(FLog.PacketSendCount);
  edt_PacketSendAmount.Text := IntToStr(FLog.PacketSendAmount);
  edt_PacketRecvCount.Text := IntToStr(FLog.PacketRecvCount);
  edt_PacketRecvAmount.Text := IntToStr(FLog.PacketRecvAmount);

  edt_PacketBlockRecvCount.Text := IntToStr(FLog.PacketBlockRecvCount);
  edt_PacketBlockRecvAmount.Text := IntToStr(FLog.PacketBlockRecvAmount);

  // 프레임버퍼
  edt_FrameBufferCount.Text := IntToStr(FLog.FrameBufferCount);
  edt_FrameBufferAmount.Text := IntToStr(FLog.FrameBufferAmount);

  // 인코드/디코드
  edt_EncodeTime.Text := IntToStr(FLog.EncodeTime);
  edt_EncodeTotalTime.Text := IntToStr(FLog.EncodeTotalTime);
  edt_DecodeTime.Text := IntToStr(FLog.DecodeTime);
  edt_DecodeTotalTime.Text := IntToStr(FLog.DecodeTotalTime);
end;

procedure TfmLog.FormCreate(Sender: TObject);
begin
  FStartTick := GetTickCount;
end;

procedure TfmLog.TimerTimer(Sender: TObject);
begin
  DisplayLog;
end;

end.
