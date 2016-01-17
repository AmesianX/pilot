<?
// Title      : �����ͺ��̽� ���� ������Ʈ
// �ۼ���     : ������ (ryujt658@hanmail.net)
// �ۼ���     : 2001��  9�� 24��
// ���������� : 2009�� 10�� 21��

  //// Class ���� �κ�

  # MySQL �����ͺ��̽��� ������ �� ���Ǵ� Class
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
        $this->ErrorMsg = "Host�� ������ ���� �����ϴ�.";
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


  # MySQL���� SQL�� �����ϰų� �� ������� ó���ϰ����� �� ���Ǵ� Class
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

      // ���ڵ� ī��Ʈ�� ���´�.
      $SQL = str_replace("@SelectField", "Count(*)", $this->SQL);
      $this->Records = mysqli_query($this->Database->link_id, $SQL, MYSQLI_STORE_RESULT);
      if ($this->Records) {
        $this->Record = mysqli_fetch_row($this->Records);
        $this->RecordCount = $this->Record[0];
        mysqli_free_result($this->Records);
      } else {
        $this->do_Init();
      }

      // ���ڵ尡 ����
      if (!$this->RecordCount) {
        $this->do_Init();
        return false;
      }

      // Select�� �ʵ带 ġȯ�� �� �����Ѵ�.
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
      // ���ڵ尡 ����
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

      // ���ڵ� ī��Ʈ�� ���´�.
      $SQL = str_replace("@SelectField", "Count(*)", $this->SQL);
      $this->Records = mysqli_query($this->Database->link_id, $SQL, MYSQLI_STORE_RESULT);
      if ($this->Records) {
        $this->Record = mysqli_fetch_row($this->Records);
        $this->RecordCount = $this->Record[0];
        mysqli_free_result($this->Records);
      } else {
        $this->do_Init();
      }

      // ���ڵ尡 ����
      if (!$this->RecordCount) {
        $this->do_Init();
        return false;
      }

      $this->RecNo = $PageSize * ($Page-1) + 1;
      $this->RecNoDesc = $this->RecordCount - $PageSize * ($Page-1);
      $this->LastPage = intval(($this->RecordCount-1) / $PageSize) + 1;

      // Select�� �ʵ带 ġȯ�� �� �����Ѵ�.
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

      // �޸������� �ʵ��� ��쿡�� CRLF�� <br>�� ��ȯ�Ѵ�.
      if (strpos(" ".$this->TextFields, $FieldName)) {
        $Result = str_replace("\n", "<br>",   $Result);
        $Result = str_replace("  ",  "&nbsp;&nbsp;", $Result);
      }

      // �ݾ������� ���ڸ� ���� �޸� ���
      if (strpos(" ".$this->MoneyFields, $FieldName)) {
        $Result = $this->MoneyStr($Result);
      }

      // ��¥����
      if (strpos(" ".$this->DateFields, $FieldName)) {
        $Result = $this->DateStr($Result);
      }

      // �ð�����
      if (strpos(" ".$this->TimeFields, $FieldName)) {
        $Result = $this->TimeStr($Result);
      }

      return $Result;
    }

    function Translate($Text)
    {
      $ResultLine = $Text;

      for ($i=0; $i<$this->FieldCount; $i++) {
        // �ʵ忡 ���������� ��ȯ�ؾ��� ��
        $Record = stripslashes($this->Record[$i]);

        // �޸������� �ʵ��� ��쿡�� CRLF�� <br>�� ��ȯ�Ѵ�.
        if (strpos(" ".$this->TextFields, $this->Fields[$i]->name)) {
          $Record = str_replace("\n", "<br>",   $Record);
          $Record = str_replace("  ",  "&nbsp;&nbsp;", $Record);
        }

        // HTML ����� �źε� �ʵ�� ġȯ
        if (strpos(" ".$this->MagicFields, $this->Fields[$i]->name)) {
          $Record = str_replace("<",       "&lt;",   $Record);
          $Record = str_replace('"',       "&quot;", $Record);
        }

        // �޸������� �ʵ忡 Text�� ���
        if (strpos(" ".$this->MemoFields, $this->Fields[$i]->name)) {
          $Record = str_replace("\n",      "<br>",         $Record);
          $Record = str_replace("  ",      "&nbsp;&nbsp;", $Record);
          $Record = str_replace("<",       "&lt;",         $Record);
          $Record = str_replace('"',       "&quot;",       $Record);
          $Record = str_replace("&lt;br>", "<br>",         $Record);

        }

        // �ݾ������� ���ڸ� ���� �޸� ���
        if (strpos(" ".$this->MoneyFields, $this->Fields[$i]->name)) {
          $Record = $this->MoneyStr($Record);
        }

        // ��¥����
        if (strpos(" ".$this->DateFields, $this->Fields[$i]->name)) {
          $Record = $this->DateStr($Record);
        }

        // �ð�����
        if (strpos(" ".$this->TimeFields, $this->Fields[$i]->name)) {
          $Record = $this->TimeStr($Record);
        }

        $ResultLine = str_replace($this->BeginMask.$this->Fields[$i]->name.$this->EndMask, $Record, $ResultLine);

        $ResultLine = str_replace("@RecNoDesc", $this->RecNoDesc, $ResultLine);
        $ResultLine = str_replace("@RecNo", $this->RecNo, $ResultLine);
      }

      return $ResultLine;
    }

    // �ʵ��̸��� Tag�� �ٿ���, �� �ٿ� ���� ���ڵ带 ó���� �� ����Ѵ�.
    function TranslateTag($Text, $Tag)
    {
      $ResultLine = $Text;

      for ($i=0; $i<$this->FieldCount; $i++) {
        // �� �̻� ���ڵ尡 ���� ���� �ʵ�� ���� �������� ġȯ
        if ($this->EOF) {
          $ResultLine = str_replace($this->BeginMask.$this->Fields[$i]->name.$Tag.$this->EndMask, "", $ResultLine);
          $ResultLine = str_replace("@RecNoDesc".$Tag, "", $ResultLine);
          $ResultLine = str_replace("@RecNo".$Tag, "", $ResultLine);

          continue;
        }

        $Record = stripslashes($this->Record[$i]);

        // �޸������� �ʵ��� ��쿡�� CRLF�� <br>�� ��ȯ�Ѵ�.
        if (strpos(" ".$this->TextFields, $this->Fields[$i]->name)) {
          $Record = str_replace("\n", "<br>",   $Record);
          $Record = str_replace("  ",  "&nbsp;&nbsp;", $Record);
        }

        // HTML ����� �źε� �ʵ�� ġȯ
        if (strpos(" ".$this->MagicFields, $this->Fields[$i]->name)) {
          $Record = str_replace("<",  "&lt;", $Record);
          $Record = str_replace('"', "&quot;", $Record);
        }

        // �ݾ������� ���ڸ� ���� �޸� ���
        if (strpos(" ".$this->MoneyFields, $this->Fields[$i]->name)) {
          $Record = $this->MoneyStr($Record);
        }

        // ��¥����
        if (strpos(" ".$this->DateFields, $this->Fields[$i]->name)) {
          $Record = $this->DateStr($Record);
        }

        // �ð�����
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

        // ���ڵ� ��ȣ ġȯ
        $Record = str_replace("@RecNoDesc", $this->RecNoDesc, $Record);
        $Record = str_replace("@RecNo",     $this->RecNo,     $Record);

        // #�ʵ�� ġȯ
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