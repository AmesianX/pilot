unit VirtualSocketUtils;

interface

const
  _PacketSizeLimit = 1024 * 64;
  _IdleCountLimit = 1000 * 30;

  // Channel ������ 128 ���� ������ ���� �������ݷ� ����Ѵ�
  _NeedDataFromClient  = 128 + 01;
  _NeedDataFromServer  = 128 + 02;
  _EndOfDataFromClient = 128 + 03;
  _EndOfDataFromServer = 128 + 04;

implementation

end.
