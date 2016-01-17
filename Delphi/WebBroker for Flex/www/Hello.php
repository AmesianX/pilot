<?

  header("Content-type: text/xml;charset=utf-8");
  header("Cache-Control: no-cache, must-revalidate");
  header("Pragma: no-cache");
  
  function Send($Text)
  {
    echo iconv("euckr", "utf-8", $Text);
  }

  Send("¾È³çÇÏ¼¼¿ä? Hello?");
  
?>

