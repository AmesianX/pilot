<?
// Title  : 날짜에 관한 라이브러리
// 작성자 : 류종택 (ryujt658@hanmail.net)
// 작성일 : 2002년 1월 7일
// 본 프로그램은 저작권법에 의하여 보호를 받고 있습니다.


  //// 함수 정의 부분

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
