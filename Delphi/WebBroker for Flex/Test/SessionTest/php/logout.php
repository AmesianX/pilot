<?php
//
// 제작자 : 지용호(http://blog.jidolstar.com)
// 제작일 : 2007-12-09
// 프로그램 설명 : Session을 이용해 로그아웃 실시 
//			<logout>success</logout> 반환
//

session_start();
$_SESSION['login_id'] = "";

header("Content-type: text/xml;charset=utf-8");
header("Cache-Control: no-cache, must-revalidate");
header("Pragma: no-cache");

echo "<?xml version=\"1.0\" encoding=\"utf-8\"?>";

echo "<root><logout>success</logout></root>";

?>
