<?
// Title      : 데이터베이스 관련 콤포넌트
// 작성자     : 류종택 (ryujt658@hanmail.net)
// 작성일     : 2001년  9월 24일
// 최종수정일 : 2009년 10월 21일

  //// Class 정의 부분

  # MySQL 데이터베이스에 접속할 때 사용되는 Class
  class TMyConnection
  {

    var $link_id;
    var $Host;
    var $UserName;
    var $Password;
    var $DatabaseName;

    var $ErrorCode;
    var $ErrorMsg;

    function TMyConnection($Host, $UserName, $Password, $DatabaseName)
    {
      $this->Host = $Host;
      $this->UserName = $UserName;
      $this->Password = $Password;
      $this->DatabaseName = $DatabaseName;
    }

    function Open()
    {
      $this->link_id = mysqli_connect($this->Host, $this->UserName, $this->Password);
      if (!$this->link_id) {
        $this->ErrorCode = 1;
        $this->ErrorMsg = "Host를 연결할 수가 없습니다.";
      } else {
        if (!mysqli_select_db($this->link_id, $this->DatabaseName)) {
          $this->ErrorCode = mysqli_errno();
          $this->ErrorMsg = mysqli_error();
        } else {
          $this->ErrorCode = 0;
          $this->ErrorMsg = "";
        }
      }
    }

    function Affected()
    {
      return mysqli_affected_rows($this->link_id);
    }

  }


  # MySQL에서 SQL을 실행하거나 그 결과값을 처리하고자할 때 사용되는 Class
  class TMyQuery
  {

    var $Database;

    var $SQL;

    var $Records;
    var $Record;

    var $RecNo;
    var $RecNoDesc;
    var $RecordCount;

    var $Fields;
    var $FieldCount;

    var $EOF;

    var $Page;
    var $PageSize;
    var $LastPage;

    var $TextFields  = "";
    var $MoneyFields = "";
    var $DateFields  = "";
    var $TimeFields  = "";
    var $MagicFields = "";
    var $MemoFields  = "";
    var $BeginMask   = "#";
    var $EndMask     = "#";
    var $SelectField = "";

    function TMyQuery($Database)
    {
      $this->Database = $Database;
    }

    private function MoneyStr($Value)
    {
      $Result = "";

      for ($i=1; $i<=strlen($Value)/3; $i++) {
        $Result = substr($Value, strlen($Value)-$i*3, 3) . "," . $Result;
      }
      if (strlen($Value)%3 != 0)
        $Result = substr($Value, 0, strlen($Value)%3) . "," . $Result;

      return substr($Result, 0, strlen($Result)-1);
    }

    private function DateStr($Value)
    {
      return
        substr($Value, 0, 4) . "-" .
        substr($Value, 4, 2) . "-" .
        substr($Value, 6, 2);
    }

    private function TimeStr($Value)
    {
      return
        substr($Value, 0, 2) . ":" .
        substr($Value, 2, 2) . ":" .
        substr($Value, 4, 2);
    }

    private function do_Init()
    {
      $this->RecordCount = 0;
      $this->FieldCount = 0;
      $this->RecNo = 0;
      $this->RecNoDesc = 0;
      $this->EOF = 1;
    }

    function Open($SQL)
    {
      $this->SQL = $SQL;

      // 레코드 카운트를 얻어온다.
      $SQL = str_replace("@SelectField", "Count(*)", $this->SQL);
      $this->Records = mysqli_query($this->Database->link_id, $SQL, MYSQLI_STORE_RESULT);
      if ($this->Records) {
        $this->Record = mysqli_fetch_row($this->Records);
        $this->RecordCount = $this->Record[0];
        mysqli_free_result($this->Records);
      } else {
        $this->do_Init();
      }

      // 레코드가 없음
      if (!$this->RecordCount) {
        $this->do_Init();
        return false;
      }

      // Select할 필드를 치환한 후 오픈한다.
      $SQL = str_replace("@SelectField", $this->SelectField, $this->SQL);
      $this->Records = mysqli_query($this->Database->link_id, $SQL, MYSQLI_STORE_RESULT);

      if ($this->Records) {
        $this->FieldCount = mysqli_num_fields($this->Records);

        unset($this->Fields);
        $this->Fields = mysqli_fetch_fields($this->Records);

        $this->Record = mysqli_fetch_row($this->Records);

        $this->RecNo = 1;
        $this->RecNoDesc = $this->RecordCount;
        $this->EOF = $this->RecordCount == 0;
      }

      return true;
    }

    function Close()
    {
      $this->RecordCount = 0;
      $this->FieldCount =  0;
      $this->RecNo = 0;
      $this->RecNoDesc = 0;
      unset($this->Fields);
      mysqli_free_result($this->Records);
    }

    function First()
    {
      // 레코드가 없음
//      if (!$this->Records) {
//        $this->RecordCount = 0;
//        $this->FieldCount =  0;
//        $this->RecNo = 0;
//        $this->RecNoDesc = 0;
//        $this->EOF = 1;
//        return;
//      }

//      mysqli_data_seek($this->Records, 0);

//      $this->Record = mysqli_fetch_row($this->Records);
//      $this->RecNo = 1;
//      $this->RecNoDesc = $this->RecordCount;

//      if ($this->Record) {
//        $this->EOF = 0;
//      } else {
//        $this->EOF = 1;
//      }
    }

    function Prev()
    {
    }

    function Next()
    {
      $this->Record = mysqli_fetch_row($this->Records);
      $this->RecNo += 1;
      $this->RecNoDesc -= 1;

      if ($this->Record) {
        $this->EOF = 0;
      } else {
        $this->EOF = 1;
      }
    }

    function Last()
    {
    }

    function ExecSQL($SQL)
    { 
      return mysqli_query($this->Database->link_id, $SQL, MYSQLI_STORE_RESULT);
    }

    function OpenPage($SQL, $PageSize, $Page)
    {
      $this->SQL = $SQL;
      $this->Page = $Page;
      $this->PageSize = $PageSize;

      // 레코드 카운트를 얻어온다.
      $SQL = str_replace("@SelectField", "Count(*)", $this->SQL);
      $this->Records = mysqli_query($this->Database->link_id, $SQL, MYSQLI_STORE_RESULT);
      if ($this->Records) {
        $this->Record = mysqli_fetch_row($this->Records);
        $this->RecordCount = $this->Record[0];
        mysqli_free_result($this->Records);
      } else {
        $this->do_Init();
      }

      // 레코드가 없음
      if (!$this->RecordCount) {
        $this->do_Init();
        return false;
      }

      $this->RecNo = $PageSize * ($Page-1) + 1;
      $this->RecNoDesc = $this->RecordCount - $PageSize * ($Page-1);
      $this->LastPage = intval(($this->RecordCount-1) / $PageSize) + 1;

      // Select할 필드를 치환한 후 오픈한다.
      $SQL = str_replace("@SelectField", $this->SelectField, $this->SQL);
      $SQL = $SQL . sprintf(" Limit %d, %d", $PageSize * ($Page-1), $PageSize);
      $this->Records = mysqli_query($this->Database->link_id, $SQL, MYSQLI_STORE_RESULT);

      if ($this->Records) {
        $this->FieldCount = mysqli_num_fields($this->Records);

        unset($this->Fields);
        $this->Fields = mysqli_fetch_fields($this->Records);

        $this->Record = mysqli_fetch_row($this->Records);

        $this->EOF = $this->RecordCount == 0;
      }
    }

    function FieldByName($FieldName)
    {
      $Result = "";
      for ($i=0; $i<$this->FieldCount; $i++) {
        if ($this->Fields[$i]->name == $FieldName) $Result = $this->Record[$i];
      }

      // 메모형식의 필드인 경우에는 CRLF를 <br>로 변환한다.
      if (strpos(" ".$this->TextFields, $FieldName)) {
        $Result = str_replace("\n", "<br>",   $Result);
        $Result = str_replace("  ",  "&nbsp;&nbsp;", $Result);
      }

      // 금액형식의 세자리 마다 콤마 찍기
      if (strpos(" ".$this->MoneyFields, $FieldName)) {
        $Result = $this->MoneyStr($Result);
      }

      // 날짜형식
      if (strpos(" ".$this->DateFields, $FieldName)) {
        $Result = $this->DateStr($Result);
      }

      // 시간형식
      if (strpos(" ".$this->TimeFields, $FieldName)) {
        $Result = $this->TimeStr($Result);
      }

      return $Result;
    }

    function Translate($Text)
    {
      $ResultLine = $Text;

      for ($i=0; $i<$this->FieldCount; $i++) {
        // 필드에 공통적으로 변환해야할 것
        $Record = stripslashes($this->Record[$i]);

        // 메모형식의 필드인 경우에는 CRLF를 <br>로 변환한다.
        if (strpos(" ".$this->TextFields, $this->Fields[$i]->name)) {
          $Record = str_replace("\n", "<br>",   $Record);
          $Record = str_replace("  ",  "&nbsp;&nbsp;", $Record);
        }

        // HTML 허용이 거부된 필드들 치환
        if (strpos(" ".$this->MagicFields, $this->Fields[$i]->name)) {
          $Record = str_replace("<",       "&lt;",   $Record);
          $Record = str_replace('"',       "&quot;", $Record);
        }

        // 메모형식의 필드에 Text만 출력
        if (strpos(" ".$this->MemoFields, $this->Fields[$i]->name)) {
          $Record = str_replace("\n",      "<br>",         $Record);
          $Record = str_replace("  ",      "&nbsp;&nbsp;", $Record);
          $Record = str_replace("<",       "&lt;",         $Record);
          $Record = str_replace('"',       "&quot;",       $Record);
          $Record = str_replace("&lt;br>", "<br>",         $Record);

        }

        // 금액형식의 세자리 마다 콤마 찍기
        if (strpos(" ".$this->MoneyFields, $this->Fields[$i]->name)) {
          $Record = $this->MoneyStr($Record);
        }

        // 날짜형식
        if (strpos(" ".$this->DateFields, $this->Fields[$i]->name)) {
          $Record = $this->DateStr($Record);
        }

        // 시간형식
        if (strpos(" ".$this->TimeFields, $this->Fields[$i]->name)) {
          $Record = $this->TimeStr($Record);
        }

        $ResultLine = str_replace($this->BeginMask.$this->Fields[$i]->name.$this->EndMask, $Record, $ResultLine);

        $ResultLine = str_replace("@RecNoDesc", $this->RecNoDesc, $ResultLine);
        $ResultLine = str_replace("@RecNo", $this->RecNo, $ResultLine);
      }

      return $ResultLine;
    }

    // 필드이름에 Tag를 붙여서, 한 줄에 여러 래코드를 처리할 때 사용한다.
    function TranslateTag($Text, $Tag)
    {
      $ResultLine = $Text;

      for ($i=0; $i<$this->FieldCount; $i++) {
        // 더 이상 레코드가 없을 때는 필드명 등을 공백으로 치환
        if ($this->EOF) {
          $ResultLine = str_replace($this->BeginMask.$this->Fields[$i]->name.$Tag.$this->EndMask, "", $ResultLine);
          $ResultLine = str_replace("@RecNoDesc".$Tag, "", $ResultLine);
          $ResultLine = str_replace("@RecNo".$Tag, "", $ResultLine);

          continue;
        }

        $Record = stripslashes($this->Record[$i]);

        // 메모형식의 필드인 경우에는 CRLF를 <br>로 변환한다.
        if (strpos(" ".$this->TextFields, $this->Fields[$i]->name)) {
          $Record = str_replace("\n", "<br>",   $Record);
          $Record = str_replace("  ",  "&nbsp;&nbsp;", $Record);
        }

        // HTML 허용이 거부된 필드들 치환
        if (strpos(" ".$this->MagicFields, $this->Fields[$i]->name)) {
          $Record = str_replace("<",  "&lt;", $Record);
          $Record = str_replace('"', "&quot;", $Record);
        }

        // 금액형식의 세자리 마다 콤마 찍기
        if (strpos(" ".$this->MoneyFields, $this->Fields[$i]->name)) {
          $Record = $this->MoneyStr($Record);
        }

        // 날짜형식
        if (strpos(" ".$this->DateFields, $this->Fields[$i]->name)) {
          $Record = $this->DateStr($Record);
        }

        // 시간형식
        if (strpos(" ".$this->TimeFields, $this->Fields[$i]->name)) {
          $Record = $this->TimeStr($Record);
        }

        $ResultLine = str_replace($this->BeginMask.$this->Fields[$i]->name.$Tag.$this->EndMask, $Record, $ResultLine);
        $ResultLine = str_replace("@RecNoDesc".$Tag, $this->RecNoDesc, $ResultLine);
        $ResultLine = str_replace("@RecNo".$Tag, $this->RecNo, $ResultLine);
      }

      return $ResultLine;
    }

    function DataSetProducer($Text)
    { 
      $ResultLine = "";

      $this->First();
      while (!$this->EOF) {
        $Record = $Text;

        // 레코드 번호 치환
        $Record = str_replace("@RecNoDesc", $this->RecNoDesc, $Record);
        $Record = str_replace("@RecNo",     $this->RecNo,     $Record);

        // #필드명 치환
        $ResultLine .= $this->Translate($Record);

        $this->Next();
      }

      return $ResultLine;
    }

    function FiledsDef()
    {
      $Result = "";
      
      foreach ($this->Fields as $Field) {
        switch ($Field->type) {
          case 3: $TypeName = "int"; break;
          case 252: $TypeName = "text"; break;
          case 253: $TypeName = "varchar"; break;
          case 254: $TypeName = "char"; break;
          default: $TypeName = "unKnown"; break;
        }
        
         $Result .= 
           "Name=" . $Field->name . ";" .
//           "TypeName=" . $TypeName . ";" .
           "Length=" . $Field->length . ";" .
           "Type=" . $Field->type . ";\n" ;
      }
      return $Result;
    }

  }

?>