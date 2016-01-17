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
    // 캐쉬
    CacheCapacity: Cardinal;
    CacheRemainTime: Cardinal;
    CacheUsedAmount: Cardinal;
    CacheCount: Cardinal;
    CacheReusedCount: Cardinal;
    CacheReusedAmount: Cardinal;
    CacheRequestCount: Cardinal;
    CacheSendCount: Cardinal;
    CacheSendAmount: Cardinal;

    // 패킷
    PacketSendCount: Cardinal;
    PacketSendAmount: Cardinal;
    PacketRecvCount: Cardinal;
    PacketRecvAmount: Cardinal;
    PacketBlockRecvCount: Cardinal;
    PacketBlockRecvAmount: Cardinal;

    // 프레임버퍼
    FrameBufferCount: Cardinal;
    FrameBufferAmount: Cardinal;

    // 인코드/디코드
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
