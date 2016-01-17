unit VirtualSocketUtils;

interface

const
  _PacketSizeLimit = 1024 * 64;
  _IdleCountLimit = 1000 * 30;

  // Channel 정보의 128 위의 범위는 내부 프로토콜로 사용한다
  _NeedDataFromClient  = 128 + 01;
  _NeedDataFromServer  = 128 + 02;
  _EndOfDataFromClient = 128 + 03;
  _EndOfDataFromServer = 128 + 04;

implementation

end.
