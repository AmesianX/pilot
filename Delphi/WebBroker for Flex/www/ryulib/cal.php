<?
// Title  : ��¥�� ���� ���̺귯��
// �ۼ��� : ������ (ryujt658@hanmail.net)
// �ۼ��� : 2002�� 1�� 7��
// �� ���α׷��� ���۱ǹ��� ���Ͽ� ��ȣ�� �ް� �ֽ��ϴ�.


  //// �Լ� ���� �κ�

  function DayOfYear($Year)
  {
    if (($Year % 4) == 0) {
      if (($Year % 400) == 0) {
        if (($Year % 100) == 0) return 366;
        else return 365;
      } else return 366;
    } else return 365;
  }

  function DayOfMonth($Year, $Month)
  {
    switch ($Month) {
      case 1 :
      case 3 :
      case 5 :
      case 7 :
      case 8 :
      case 10 :
      case 12 :
        return 31;
        break;

      case 4 :
      case 6 :
      case 11 :
        return 30;
        break;

      case 2 :
        if (DayOfYear($Year) == 365) return 28;
        else return 29;
        break;
    }
  }

  function Days($yFrom, $mFrom, $dFrom, $yTo, $mTo, $dTo)
  {
    $iDays = 0;
    for ($i=$yFrom; $i<$yTo; $i++) $iDays += DayOfYear($i);
    for ($i=1; $i<$mFrom; $i++) $iDays -= DayOfMonth($yFrom, $i);
    for ($i=1; $i<$mTo; $i++) $iDays += DayOfMonth($yTo, $i);
    return $iDays + $dTo - $dFrom;
  }

?>
