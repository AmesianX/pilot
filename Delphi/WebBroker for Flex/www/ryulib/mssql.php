<?
// Title      : 데이터베이스 관련 콤포넌트 - MS SQL Version
// 작성자     : 류종택 (ryujt658@hanmail.net)
// 작성일     : 2002년 8월 19일

  //// Class 정의 부분

  # MSSQL 데이터베이스에 접속할 때 사용되는 Class
  class TMSConnection
  {

    var $link_id;
    var $Host;
    var $UserName;
    var $Password;
    var $DatabaseName;

    var $ErrorCode;
    var $ErrorMsg;

    function TMSConnection($Host, $UserName, $Password, $DatabaseName)
    {
      $this->Host = $Host;
      $this->UserName = $UserName;
      $this->Password = $Password;
      $this->DatabaseName = $DatabaseName;
    }

    function Open()
    {
      $this->link_id = mssql_connect($this->Host, $this->UserName, $this->Password);
      if (!$this->link_id) {
        $this->ErrorCode = 1;
        $this->ErrorMsg = "Host를 연결할 수가 없습니다.";
      } else {
        if (!mssql_select_db($this->DatabaseName)) {
          $this->ErrorCode = 2;
          $this->ErrorMsg = "데이터베이스를 선택할 수가 없습니다.";
        } else {
          $this->ErrorCode = 0;
          $this->ErrorMsg = "";
        }
      }
    }

    function Affected()
    {
      return mssql_rows_affected($this->link_id);
    }

  }


  # MSSQL에서 SQL을 실행하거나 그 결과값을 처리하고자할 때 사용되는 Class
  class TMSQuery
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
    var $PageCount;

    var $TextFields  = "";
    var $MoneyFields = "";
    var $MagicFields = "";
    var $MemoFields  = "";
    var $EndMask     = "";

    function TMSQuery($Database)
    {
      $this->Database = $Database;
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

    function PrepareOpen()
    {
      $this->Records = mssql_query($this->SQL, $this->Database->link_id);

      // 레코드가 없음
      if (!$this->Records) {
        $this->RecordCount = 0;
        $this->PageSize = 0;
        $this->FieldCount = 0;
        $this->RecNo = 0;
        $this->RecNoDesc = 0;
        $this->EOF = 1;
        return false;
      }

      $this->RecordCount = mssql_num_rows($this->Records);
      $this->PageSize = $this->RecordCount;
      $this->FieldCount = mssql_num_fields($this->Records);
      unset($this->Fields);
      for ($i=0; $i<$this->FieldCount; $i++) {
        $this->Fields[] = mssql_field_name($this->Records, $i);
      }
      $this->Record = mssql_fetch_row($this->Records);

      $this->RecNo = 1;
      $this->RecNoDesc = $this->RecordCount;
      $this->EOF = $this->RecordCount == 0;
      $this->PageCount = 0;

      return true;
    }

    function Open($SQL)
    {
      $this->SQL = $SQL;

      return $this->PrepareOpen();
    }

    function Close()
    {
      $this->RecordCount = 0;
      $this->FieldCount =  0;
      $this->RecNo = 0;
      $this->RecNoDesc = 0;
      unset($this->Fields);
      mssql_free_result($this->Records);
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

//      mssql_data_seek($this->Records, 0);

//      $this->Record = mssql_fetch_row($this->Records);
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
      $this->Record = mssql_fetch_row($this->Records);
      $this->RecNo += 1;
      $this->RecNoDesc -= 1;
      $this->PageCount += 1;

      if (($this->Record) && ($this->PageCount < $this->PageSize)) {
        $this->EOF = 0;
      } else {
        $this->EOF = 1;
      }

    }

    function Last()
    {
    }

    function Refresh()
    {
      $this->PrepareOpen();
    }

    function ExecSQL($SQL) 
    { 
      return mssql_query($SQL, $this->Database->link_id);
    }

    function OpenPage($SQL, $PageSize, $Page)
    {
      $this->Page = $Page;
      $this->PageSize = $PageSize;

      $this->Records = mssql_query($SQL, $this->Database->link_id);
      $this->RecordCount = mssql_num_rows($this->Records);
      $this->RecNo = $PageSize * ($Page-1) + 1;
      $this->RecNoDesc = $this->RecordCount - $PageSize * ($Page-1);
      $this->LastPage = intval(($this->RecordCount-1) / $PageSize) + 1;
      $this->PageCount = 0;

      for ($i=0; $i<$PageSize*($Page-1); $i++) $this->Record = mssql_fetch_row($this->Records);
      $this->Record = mssql_fetch_row($this->Records);

      $this->FieldCount = mssql_num_fields($this->Records);
      unset($this->Fields);
      for ($i=0; $i<$this->FieldCount; $i++) {
        $this->Fields[] = mssql_field_name($this->Records, $i);
      }
    }

    function FieldByName($FieldName)
    {
      $Result = "";
      for ($i=0; $i<$this->FieldCount; $i++) {
        if ($this->Fields[$i] == $FieldName) $Result = $this->Record[$i];
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

      return $Result;
    }

    function Para($Name, $Value)
    {
      $this->SQL = str_replace(":".$Name, addslashes($Value),  $this->SQL);
    }

    function Translate($Text)
    {
      $ResultLine = $Text;

      for ($i=0; $i<$this->FieldCount; $i++) {
        // 필드에 공통적으로 변환해야할 것
        $Record = stripslashes($this->Record[$i]);

        // 메모형식의 필드인 경우에는 CRLF를 <br>로 변환한다.
        if (strpos(" ".$this->TextFields, $this->Fields[$i])) {
          $Record = str_replace("\n", "<br>",   $Record);
          $Record = str_replace("  ",  "&nbsp;&nbsp;", $Record);
        }

        // HTML 허용이 거부된 필드들 치환
        if (strpos(" ".$this->MagicFields, $this->Fields[$i])) {
          $Record = str_replace("<",       "&lt;",   $Record);
          $Record = str_replace('"',       "&quot;", $Record);
        }

        // 메모형식의 필드에 Text만 출력
        if (strpos(" ".$this->MemoFields, $this->Fields[$i])) {
          $Record = str_replace("\n",      "<br>",         $Record);
          $Record = str_replace("  ",      "&nbsp;&nbsp;", $Record);
          $Record = str_replace("<",       "&lt;",         $Record);
          $Record = str_replace('"',       "&quot;",       $Record);
          $Record = str_replace("&lt;br>", "<br>",         $Record);

        }

        // 금액형식의 세자리 마다 콤마 찍기
        if (strpos(" ".$this->MoneyFields, $this->Fields[$i])) {
          $Record = $this->MoneyStr($Record);
        }

        $ResultLine = str_replace("#".$this->Fields[$i].$this->EndMask, $Record, $ResultLine);

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
          $ResultLine = str_replace("#".$this->Fields[$i].$Tag.$this->EndMask, "", $ResultLine);
          $ResultLine = str_replace("@RecNoDesc".$Tag, "", $ResultLine);
          $ResultLine = str_replace("@RecNo".$Tag, "", $ResultLine);

          continue;
        }

        $Record = stripslashes($this->Record[$i]);

        // 메모형식의 필드인 경우에는 CRLF를 <br>로 변환한다.
        if (strpos(" ".$this->TextFields, $this->Fields[$i])) {
          $Record = str_replace("\n", "<br>",   $Record);
          $Record = str_replace("  ",  "&nbsp;&nbsp;", $Record);
        }

        // HTML 허용이 거부된 필드들 치환
        if (strpos(" ".$this->MagicFields, $this->Fields[$i])) {
          $Record = str_replace("<",  "&lt;", $Record);
          $Record = str_replace('"', "&quot;", $Record);
        }

        // 금액형식의 세자리 마다 콤마 찍기
        if (strpos(" ".$this->MoneyFields, $this->Fields[$i])) {
          $Record = $this->MoneyStr($Record);
        }

        $ResultLine = str_replace("#".$this->Fields[$i].$Tag.$this->EndMask, $Record, $ResultLine);

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

  }

?>