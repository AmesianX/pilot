<?

  include_once $_SERVER[DOCUMENT_ROOT]."/ryulib/disk.php";
  include_once $_SERVER[DOCUMENT_ROOT]."/ryulib/mysql.php";
  include_once $_SERVER[DOCUMENT_ROOT]."/ryulib/templet.php";

  header("Content-type: text/xml;charset=utf-8");
  header("Cache-Control: no-cache, must-revalidate");
  header("Pragma: no-cache");


  function Send($Text)
  {
    echo iconv("euckr", "utf-8", $Text);
  }

  $Host = "localhost";
  $DB_User = "root";
  $DB_Pass = "~52todrkr";
  $DB_Name = "MegaTube";

  $DB = new TMyConnection($Host, $DB_User, $DB_Pass, $DB_Name);
  $DB->Open();
  if ($DB->ErrorCode != 0) die("DB 접속에 실패하였습니다.");

  $Query = new TMyQuery($DB);
  $Query->DateFields = "Date; ";
  $Query->SelectField = " * ";
  $Query->SQL = " Select @SelectField from Notice ";
  $Query->Open($Query->SQL);
  
  echo "<Records>";
  
  Send($Query->DataSetProducer("<Record><Date>#Date#</Date><Title>#Title#</Title></Record>"));
  
  echo "</Records>";
?>  
