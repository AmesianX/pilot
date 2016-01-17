unit DeskCamAddonUtils;

interface

uses
  Messages, SysUtils,
  SystemFolder;

const
  WM_DESK_RECEIVED = WM_USER + 3;

type
  TPacketType = (
    ptNone,
    ptSendUserInfo,
    ptAlreadySenderConnected,
    ptSendConnectionID,
    ptSendFrameToServer,
    ptSendEndOfFrameToServer,
    ptSendFrameToReceiver,
    ptSendEndOfFrameToReceiver,
    ptNeedFrameToSender,
    ptNeedFrameToServer
  );
  PPacketType = ^TPacketType;

  TLog = class
  public
    // ĳ��
    CacheCapacity: Cardinal;
    CacheRemainTime: Cardinal;
    CacheUsedAmount: Cardinal;
    CacheCount: Cardinal;
    CacheReusedCount: Cardinal;
    CacheReusedAmount: Cardinal;
    CacheRequestCount: Cardinal;
    CacheSendCount: Cardinal;
    CacheSendAmount: Cardinal;

    // ��Ŷ
    PacketSendCount: Cardinal;
    PacketSendAmount: Cardinal;
    PacketRecvCount: Cardinal;
    PacketRecvAmount: Cardinal;
    PacketBlockRecvCount: Cardinal;
    PacketBlockRecvAmount: Cardinal;

    // �����ӹ���
    FrameBufferCount: Cardinal;
    FrameBufferAmount: Cardinal;

    // ���ڵ�/���ڵ�
    EncodeTime: Cardinal;
    EncodeTotalTime: Cardinal;
    DecodeTime: Cardinal;
    DecodeTotalTime: Cardinal;
  end;

  PLog = ^TLog;

function CacheFileName: string;

implementation

function CacheFileName: string;
var
  GUID: TGUID;
  GUIDString: string;
  FileName: string;
begin
  CreateGUID(GUID);
  GUIDString := GUIDToString(GUID);
  FileName := GUIDString + '.' + 'tmp';

  Result := TempFolder + '\' + FileName;
end;

end.
