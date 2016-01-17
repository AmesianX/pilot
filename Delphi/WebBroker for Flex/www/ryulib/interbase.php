<?
// Title      : �����ͺ��̽� ���� ������Ʈ - Interbase
// �ۼ���     : ������ (ryujt658@hanmail.net)
// �ۼ���     : 2002�� 9�� 2��

  //// Class ���� �κ�

  # Interbase �����ͺ��̽��� ������ �� ���Ǵ� Class
  class TIBConnection
  {

    var $link_id;
    var $Host;
    var $UserName;
    var $Password;

    var $ErrorCode;
    var $ErrorMsg;

    function TIBConnection($Host, $UserName, $Password)
    {
      $this->Host = $Host;
      $this->UserName = $UserName;
      $this->Password = $Password;
    }

    function Open()
    {
      $this->link_id = ibase_connect($this->Host, $this->UserName, $this->Password);
      if (!$this->link_id) {
        $this->ErrorCode = 1;
        $this->ErrorMsg = "Host�� ������ ���� �����ϴ�.";
      }
    }

  }


  # Interbase���� SQL�� �����ϰų� �� ������� ó���ϰ����� �� ���Ǵ� Class
  class TIBQuery
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
    var $MagicFields = "";
    var $MemoFields  = "";
    var $EndMask     = "";
    var $SelectField = "";

    function TQuery($Database)
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

    function FieldName($Index)
    {
      $FieldInfo = ibase_field_info($this->Records, $Index);
      if ($FieldInfo['alias']) {
        return $FieldInfo['alias'];
      } else {
        return $FieldInfo['name'];
      }
    }

    function Open($SQL)
    {
      $this->SQL = $SQL;

      // ���ڵ� ī��Ʈ�� ���´�.
      $SQL = str_replace("@SelectField", "Count(*)", $this->SQL);
      $this->Records = ibase_query($SQL, $this->Database->link_id);
      $this->Record = ibase_fetch_row($this->Records);
      $this->RecordCount = $this->Record[0];
      ibase_free_result($this->Records);

      // ���ڵ尡 ����
      if (!$this->RecordCount) {
        $this->RecordCount = 0;
        $this->FieldCount = 0;
        $this->RecNo = 0;
        $this->RecNoDesc = 0;
        $this->EOF = 1;

        return false;
      }

      // Select�� �ʵ带 ġȯ�� �� �����Ѵ�.
      $SQL = str_replace("@SelectField", $this->SelectField, $this->SQL);
      $this->Records = ibase_query($SQL, $this->Database->link_id);

      $this->FieldCount = ibase_num_fields($this->Records);
      unset($this->Fields);
      for ($i=0; $i<$this->FieldCount; $i++) {
        $this->Fields[] = $this->FieldName($i);
      }
      $this->Record = ibase_fetch_row($this->Records);

      $this->RecNo = 1;
      $this->RecNoDesc = $this->RecordCount;
      $this->EOF = $this->RecordCount == 0;

      return true;
    }

    function Close()
    {
      $this->RecordCount = 0;
      $this->FieldCount =  0;
      $this->RecNo = 0;
      $this->RecNoDesc = 0;

      unset($this->Fields);
      ibase_free_result($this->Records);
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

//      ibase_data_seek($this->Records, 0);

//      $this->Record = ibase_fetch_row($this->Records);
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
      $this->Record = ibase_fetch_row($this->Records);
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
      return ibase_query($SQL, $this->Database->link_id); 
    }

    function OpenPage($SQL, $PageSize, $Page)
    {
      $this->SQL = $SQL;
      $this->Page = $Page;
      $this->PageSize = $PageSize;

      // ���ڵ� ī��Ʈ�� ���´�.
      $SQL = str_replace("@First", "1", $this->SQL);
      $SQL = str_replace("@Skip",  "0", $SQL);
      $SQL = str_replace("@SelectField", "Count(*)", $SQL);
      $this->Records = ibase_query($SQL, $this->Database->link_id);
      $this->Record = ibase_fetch_row($this->Records);
      $this->RecordCount = $this->Record[0];
      ibase_free_result($this->Records);

      // ���ڵ尡 ����
      if (!$this->RecordCount) {
        $this->RecordCount = 0;
        $this->FieldCount = 0;
        $this->RecNo = 0;
        $this->RecNoDesc = 0;
        $this->EOF = 1;

        return false;
      }

      $this->RecNo = $PageSize * ($Page-1) + 1;
      $this->RecNoDesc = $this->RecordCount - $PageSize * ($Page-1);
      $this->LastPage = intval(($this->RecordCount-1) / $PageSize) + 1;

      // Select�� �ʵ带 ġȯ�� �� �����Ѵ�.
      $SQL = str_replace("@First", $PageSize,           $this->SQL);
      $SQL = str_replace("@Skip",  $PageSize*($Page-1), $SQL);
      $SQL = str_replace("@SelectField", $this->SelectField, $SQL);
      $this->Records = ibase_query($SQL, $this->Database->link_id);

      $this->FieldCount = ibase_num_fields($this->Records);
      unset($this->Fields);
      for ($i=0; $i<$this->FieldCount; $i++) {
        $this->Fields[] = $this->FieldName($i);
      }

      $this->Record = ibase_fetch_row($this->Records);
      if ($this->Record) {
        $this->EOF = 0;
      } else {
        $this->EOF = 1;
      }
    }

    function FieldByName($FieldName)
    {
      $Result = "";
      for ($i=0; $i<$this->FieldCount; $i++) {
        if ($this->Fields[$i] == $FieldName) $Result = $this->Record[$i];
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
        // �ʵ忡 ���������� ��ȯ�ؾ��� ��
        $Record = stripslashes($this->Record[$i]);

        // �޸������� �ʵ��� ��쿡�� CRLF�� <br>�� ��ȯ�Ѵ�.
        if (strpos(" ".$this->TextFields, $this->Fields[$i])) {
          $Record = str_replace("\n", "<br>",   $Record);
          $Record = str_replace("  ",  "&nbsp;&nbsp;", $Record);
        }

        // HTML ����� �źε� �ʵ�� ġȯ
        if (strpos(" ".$this->MagicFields, $this->Fields[$i])) {
          $Record = str_replace("<",       "&lt;",   $Record);
          $Record = str_replace('"',       "&quot;", $Record);
        }

        // �޸������� �ʵ忡 Text�� ���
        if (strpos(" ".$this->MemoFields, $this->Fields[$i])) {
          $Record = str_replace("\n",      "<br>",         $Record);
          $Record = str_replace("  ",      "&nbsp;&nbsp;", $Record);
          $Record = str_replace("<",       "&lt;",         $Record);
          $Record = str_replace('"',       "&quot;",       $Record);
          $Record = str_replace("&lt;br>", "<br>",         $Record);

        }

        // �ݾ������� ���ڸ� ���� �޸� ���
        if (strpos(" ".$this->MoneyFields, $this->Fields[$i])) {
          $Record = $this->MoneyStr($Record);
        }

        $ResultLine = str_replace("#".$this->Fields[$i].$this->EndMask, $Record, $ResultLine);

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
          $ResultLine = str_replace("#".$this->Fields[$i].$Tag.$this->EndMask, "", $ResultLine);
          $ResultLine = str_replace("@RecNoDesc".$Tag, "", $ResultLine);
          $ResultLine = str_replace("@RecNo".$Tag, "", $ResultLine);

          continue;
        }

        $Record = stripslashes($this->Record[$i]);

        // �޸������� �ʵ��� ��쿡�� CRLF�� <br>�� ��ȯ�Ѵ�.
        if (strpos(" ".$this->TextFields, $this->Fields[$i])) {
          $Record = str_replace("\n", "<br>",   $Record);
          $Record = str_replace("  ",  "&nbsp;&nbsp;", $Record);
        }

        // HTML ����� �źε� �ʵ�� ġȯ
        if (strpos(" ".$this->MagicFields, $this->Fields[$i])) {
          $Record = str_replace("<",  "&lt;", $Record);
          $Record = str_replace('"', "&quot;", $Record);
        }

        // �ݾ������� ���ڸ� ���� �޸� ���
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

        // ���ڵ� ��ȣ ġȯ
        $Record = str_replace("@RecNoDesc", $this->RecNoDesc, $Record);
        $Record = str_replace("@RecNo",     $this->RecNo,     $Record);

        // #�ʵ�� ġȯ
        $ResultLine .= $this->Translate($Record);

        $this->Next();
      }

      return $ResultLine;
    }

  }

?>
