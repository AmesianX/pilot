<?
// Title  : ���ø� ó�� ������Ʈ �� �����α׷��� ���� ��ƾ
// �ۼ��� : ������ (ryujt658@hanmail.net)
// �ۼ��� : 2001�� 9�� 24��


  //// ��� �� ���κ��� ���� �κ�

  // ù ��°�� ������ �Ķ���� ���ϱ�
  // �̴� �� ���������� �ٲ��� �ϹǷ� ���ܽ��Ѿ� �Ѵ�.
  if ($argc > 0) {
    if (substr($argv[0], 0, 5) == "Page=")
      $Option = substr($argv[0], strpos($argv[0], "&")+1, strlen($argv[0]));
    else $Option = substr($argv[0], strpos($argv[0], "?"), strlen($argv[0]));
  } else {
    $Option = "";
  }

  $Remote_Addr = getenv("REMOTE_ADDR");


  //// �Լ� ���� �κ�

  function Redirect($URL)
  {
    $Lines = sprintf('<meta http-equiv="Refresh" content="0; URL=%s">', $URL);
    die($Lines);
  }


  //// Class ���� �κ�

  // �����ͺ��̽��� ���̺�� �����Ͽ� ����Ʈ�� ����� �� ���̴� Class
  class THTMLFile
  {

    var $TextFields = "";
    var $MoneyFields = "";
    var $FileName;
    var $ResultLine;
    var $TopLine;
    var $MiddleLine;
    var $BottomLine;

    function THTMLFile($FileName)
    {
      $this->FileName = $FileName;
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

    function Separate($Index)
    {
      $Segment = 0;
      $CurrentLine = "";
      $BeginToken = "<!Begin of " . $Index . ">";
      $EndToken = "<!End of " . $Index . ">";
      $IncToken = "<!Include";

      $this->TopLine = "";
      $this->MiddleLine = "";
      $this->BottomLine = "";

      $fp = fopen($this->FileName, "r");

      while (!feof($fp)) {
        $CurrentLine = fgets($fp, 1024);

        // �ݺ��κ� ����
//        if (strncmp($BeginToken, $CurrentLine, strlen($BeginToken)) == 0) {
        if (strpos(" ".$CurrentLine, $BeginToken) > 0) {
          $Segment = 1;
        }

        // �ݺ��κ� ����
//        if (strncmp($EndToken, $CurrentLine, strlen($EndToken)) == 0) {
        if (strpos(" ".$CurrentLine, $EndToken) > 0) {
          $Segment = 2;
        }

        // ���۰� ���Ḧ �˷��ִ� ���ڿ��� �����ϰ� �������� �и��س���.
//        if ((strncmp($BeginToken, $CurrentLine, strlen($BeginToken)) != 0) &&
//            (strncmp($EndToken, $CurrentLine, strlen($EndToken)) != 0)) {
        $iPosB = strpos($CurrentLine, $BeginToken);
        $iPosE = strpos($CurrentLine, $EndToken);
        if ( empty($iPosB) && empty($iPosE) ) {

          // SSI ���
          if (strncmp($IncToken, $CurrentLine, strlen($IncToken)) == 0) {
            list($stTemp, $FileName) = explode(" ", $CurrentLine);
            $CurrentLine = "";
            $fpSSI = fopen($FileName, "r");
            while (!feof($fpSSI)) {
              $CurrentLine .= fgets($fpSSI, 1024);
            }
            fclose($fpSSI);
          }

          switch ($Segment) {
            case 0 :
              $this->TopLine .= $CurrentLine;
              break;
            case 1 :
              $this->MiddleLine .= $CurrentLine;
              break;
            case 2 :
              $this->BottomLine .= $CurrentLine;
              break;
          }
        }
      }

      fclose($fp);
    }

    function Translate($Source, $Target)
    {
      $this->TopLine = str_replace($Source, $Target, $this->TopLine);
      $this->MiddleLine = str_replace($Source, $Target, $this->MiddleLine);
      $this->BottomLine = str_replace($Source, $Target, $this->BottomLine);
    }

    function DataSetProducer($Query)
    {
      $this->ResultLine = $this->TopLine;

      $Query->First();

      while (!$Query->EOF) {
        $stTemp = $this->MiddleLine;

        for ($i=0; $i<$Query->FieldCount; $i++) {
          // �ʵ忡 ���������� ��ȯ�ؾ��� ��
          $Record = stripslashes($Query->Record[$i]);

          // �޸������� �ʵ��� ��쿡�� CRLF�� <br>�� ��ȯ�Ѵ�.
          if (strpos(" ".$this->TextFields, $Query->Fields[$i])) {
            $Record = str_replace("\n", "<br>", $Record);
            $Record = str_replace("  ", "&nbsp;&nbsp;", $Record);
          }

          // HTML ����� �źε� �ʵ�� ġȯ
          if (strpos(" ".$this->MagicFields, $Query->Fields[$i])) {
            $Record = str_replace("<",  "&lt;", $Record);
            $Record = str_replace('"', "&quot;", $Record);
          }

          // �ݾ������� ���ڸ� ���� �޸� ���
          if (strpos(" ".$this->MoneyFields, $Query->Fields[$i])) {
            $Record = $this->MoneyStr($Record);
          }

          $stTemp = str_replace("#".$Query->Fields[$i], $Record, $stTemp);
        }

        // ���ڵ� ��ȣ ġȯ
        $stTemp = str_replace("@RecNoDesc", $Query->RecNoDesc, $stTemp);
        $stTemp = str_replace("@RecNo", $Query->RecNo, $stTemp);

        $this->ResultLine .= $stTemp;

        $Query->Next();
      }

      $this->ResultLine .= $this->BottomLine;
    }

  }


  // THTMLFile�� ������� ����Ʈ�� �� �������� �ƴ� ��, �׺���̼ǿ� �ʿ��� ������ �����Ѵ�.
  class THTMLButton
  {

    var $enFirst = "";
    var $enPrev  = "";
    var $enNext  = "";
    var $enLast  = "";
    var $dsFirst = "";
    var $dsPrev  = "";
    var $dsNext  = "";
    var $dsLast  = "";
    var $NaviBar = "";

    var $ResultLine;
    var $CurrentPage;
    var $LastPage;

    function  THTMLButton($ResultLine, $CurrentPage, $LastPage)
    {
      $this->ResultLine = $ResultLine;
      $this->CurrentPage = $CurrentPage;
      $this->LastPage = $LastPage;
    }

    function MakeButton()
    {
      if ($this->CurrentPage > 1) {
        $this->ResultLine = str_replace("@First", $this->enFirst, $this->ResultLine);
        $stTemp = str_replace("@Page", $this->CurrentPage-1, $this->enPrev);
        $this->ResultLine = str_replace("@Prev",  $stTemp,  $this->ResultLine);
      }
      if ($this->CurrentPage < $this->LastPage) {
        $stTemp = str_replace("@Page", $this->CurrentPage+1, $this->enNext);
        $this->ResultLine = str_replace("@Next",  $stTemp, $this->ResultLine);
        $stTemp = str_replace("@Page", $this->LastPage, $this->enLast);
        $this->ResultLine = str_replace("@Last",  $stTemp, $this->ResultLine);
      }
      $this->ResultLine = str_replace("@First", $this->dsFirst, $this->ResultLine);
      $this->ResultLine = str_replace("@Prev",  $this->dsPrev,  $this->ResultLine);
      $this->ResultLine = str_replace("@Next",  $this->dsNext,  $this->ResultLine);
      $this->ResultLine = str_replace("@Last",  $this->dsLast,  $this->ResultLine);

      $NaviStart = $this->CurrentPage-4;
      if ($NaviStart < 1) {$NaviStart = 1;}
      $NaviEnd = $NaviStart + 9;
      if ($NaviEnd > $this->LastPage) {$NaviEnd = $this->LastPage;}
      $stTemp = "";
      for ($i=$NaviStart; $i<=$NaviEnd; $i++) {
        if ($this->enNaviBar) {
          if ($i == $this->CurrentPage) $stTemp .= str_replace("@Page", $i, $this->dsNaviBar);
          else $stTemp .= str_replace("@Page", $i, $this->enNaviBar);
        } else $stTemp .= str_replace("@Page", $i, $this->NaviBar);
      }
      $this->ResultLine = str_replace("@NaviBar",  $stTemp,  $this->ResultLine);
    }

  }

?>
