<?
// Title  : 화일 및 디렉토리 관리에 관한 라이브러리 
// 작성자 : 류종택 (ryujt658@hanmail.net)
// 작성일 : 2001년 9월 24일


  //// 함수 정의 부분
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

      // SSI 기능
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