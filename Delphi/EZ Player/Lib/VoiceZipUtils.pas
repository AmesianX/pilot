unit VoiceZipUtils;

interface

uses
  msacm2, ThreadRepeater,
  Windows, Messages, SysUtils, Classes, MMSystem;

const
  _Channels = 1;
  _SampleRate = 8000;

  // 현재 사용 중인 Speex의 FrameSize를 기준으로 한 것임.
  _FrameSize = 320;
  _FrameTime = 20;

type
  TBufferSmallInt = packed array of SmallInt;
  TBufferSingle = packed array of single;

  TVoiceDataEvent = procedure (Sender:TObject; AData:pointer; ASize,AVolume:integer) of object;
  TDataEvent      = procedure (Sender:TObject; AData:pointer; ASize:integer) of object;
  TErrorEvent     = procedure (Sender:TObject; AErrorCode:Integer; AErrorMsg:String) of object;

implementation

end.
