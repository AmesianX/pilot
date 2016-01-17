<?
// Title  : 문자열에 관한 라이브러리
// 작성자 : 류종택 (ryujt658@hanmail.net)
// 작성일 : 2001년 9월 28일


  //// 함수 정의 부분

  function IndexOf($SubStr, $Source)
  {
    $iPos = strpos($Source, $SubStr);
    if (empty($iPos)) return 0;
    else return $iPos + 1;
  }

  function MoneyStr($Value)
  {
    $Result = "";

    for ($i=1; $i<=strlen($Value)/3; $i++) {
      $Result = substr($Value, strlen($Value)-$i*3, 3) . "," . $Result;
    }
    if (strlen($Value)%3 != 0)
      $Result = substr($Value, 0, strlen($Value)%3) . "," . $Result;

    return substr($Result, 0, strlen($Result)-1);
  }


  //// Class 정의 부분

  // TSeparate의 내부 문자를 파싱하는데 사용
  class TSeparatePaser
  {
    var $iTop;
    var $iPos;
    var $EOF;
    var $Data;
    var $Split;

    function TSeparatePaser($Value, $Split)
    {
      $this->iTop = 0;
      $this->iPos = 0;
      $this->EOF = 0;
      $this->Data = $Value;
      $this->Split = $Split;
    }

    function Next()
    {
      // 이미 끝까지 검색하였는 지 검사
      if ($this->EOF) return "";

      while ($this->iPos < strlen($this->Data)) {
        if (substr($this->Data, $this->iPos, strlen($this->Split)) == $this->Split) {
          if ($this->iTop < $this->iPos) $Result = substr($this->Data, $this->iTop, $this->iPos-$this->iTop);
          $this->iPos += strlen($this->Split);
          $this->iTop = $this->iPos;
          return $Result;
        } else {
          $this->iPos += 1;
        }
      }

      $this->EOF = 1;
      if ($this->iTop < $this->iPos) return substr($this->Data, $this->iTop, $this->iPos-$this->iTop);
    }

    function Find($Split)
    {
      $this->Split = $Split;
      
      return $this->Next();
    }

    function Reset()
    {
      $this->iTop = 0;
      $this->iPos = 0;
      $this->EOF = 0;
    }

  }

  // 문자열을 여러 구성요소로 분할, 복잡한 HTML Templet 다루기 등에 사용
  // 구분자($Split)를 이용해서 미리 문자열을 잘라서 Items에 배열로 저장한다.
  class TSeparate
  {

    var $Items;
    var $Split;

    function Separate($Lines)
    {
      $SeparatePaser = new TSeparatePaser($Lines, $this->Split);

      while (!$SeparatePaser->EOF) {
        $this->Items[] = $SeparatePaser->Next();
      }
    }

    function TSeparate($Value, $Split)
    {
      $this->Split = $Split;
      $this->Separate($Value);
    }

  }

  // 문자열 내의 문자열을 치환하는 클래스, 반복적인 문자열 치환에 의한 퍼포먼스를 높여준다.
  class TStrParser
  {

    var $Text;
    var $Begin;
    var $End;
    var $Tokens;
    var $Target;

    function TStrParser($Text, $Begin, $End)
    {
      $this->Text =  $Text;
      $this->Begin = $Begin;
      $this->End =   $End;
    }

    function Replace($Source, $Target)
    {
      $this->Tokens[$Source] = $Target;
    }

    function ResultLine()
    {
      $Result = "";

      for ($i=0; $i<strlen($this->Text); $i++) {
        // 시작 토큰과 같지 않으면 결과 문자열로 복사
        if (substr($this->Text, $i, strlen($this->Begin)) != $this->Begin) $Result .= $this->Text[$i];
        // 시작토큰과 같으면 Replace해야할 Source를 찾아서 일치되는 Target으로 치환
        else {
          $iStart = $i+strlen($this->Begin);
          $iEnd   = $iStart;
          while ( (substr($this->Text, $iEnd, strlen($this->End)) != $this->End) and
                  ($iEnd < strlen($this->Text))
                ) $iEnd++;
          $Result .= $this->Tokens[substr($this->Text, $iStart, $iEnd-$iStart)];
          $i = $iEnd + strlen($this->End);
        }
      }

      return $Result;
    }

  }

?>
