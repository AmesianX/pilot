<?
// Title  : ȭ�� �� ���丮 ������ ���� ���̺귯�� 
// �ۼ��� : ������ (ryujt658@hanmail.net)
// �ۼ��� : 2001�� 9�� 24��


  //// �Լ� ���� �κ�
  function Redirect($URL)
  {
    $Lines = sprintf('<meta http-equiv="Refresh" content="0; URL=%s">', $URL);
    die($Lines);
  }

  function SaveToFile($Str, $FileName)
  {
    $fp = fopen($FileName, "w");
    fputs($fp, $Str);
    fclose($fp);
  }

  function LoadFromFile($FileName)
  {
    $Lines = "";
    $IncToken = "<!Include";

    $fp = fopen($FileName, "r");
    while (!feof($fp)) {
      $CurrentLine = fgets($fp, 1024);

      // SSI ���
      if (strncmp($IncToken, $CurrentLine, strlen($IncToken)) == 0) {
        list($stTemp, $IncName) = explode(" ", $CurrentLine);
        $IncName = str_replace("@DOCUMENT_ROOT", $_SERVER[DOCUMENT_ROOT], $IncName);
        
        $CurrentLine = "";
        $fpInc = fopen($IncName, "r");
        while (!feof($fpInc)) {
          $CurrentLine .= fgets($fpInc, 1024);
        }
        fclose($fpInc);
      }

      $Lines .= $CurrentLine;
    }
    fclose($fp);

    return $Lines;
  }

?>