<?
// Title  : ���ڿ��� ���� ���̺귯��
// �ۼ��� : ������ (ryujt658@hanmail.net)
// �ۼ��� : 2001�� 9�� 28��


  //// �Լ� ���� �κ�

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


  //// Class ���� �κ�

  // TSeparate�� ���� ���ڸ� �Ľ��ϴµ� ���
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
      // �̹� ������ �˻��Ͽ��� �� �˻�
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

  // ���ڿ��� ���� ������ҷ� ����, ������ HTML Templet �ٷ�� � ���
  // ������($Split)�� �̿��ؼ� �̸� ���ڿ��� �߶� Items�� �迭�� �����Ѵ�.
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

  // ���ڿ� ���� ���ڿ��� ġȯ�ϴ� Ŭ����, �ݺ����� ���ڿ� ġȯ�� ���� �����ս��� �����ش�.
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
        // ���� ��ū�� ���� ������ ��� ���ڿ��� ����
        if (substr($this->Text, $i, strlen($this->Begin)) != $this->Begin) $Result .= $this->Text[$i];
        // ������ū�� ������ Replace�ؾ��� Source�� ã�Ƽ� ��ġ�Ǵ� Target���� ġȯ
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
