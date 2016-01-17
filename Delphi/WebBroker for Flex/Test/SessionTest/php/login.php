<?php
//
// 제작자 : 지용호(http://blog.jidolstar.com)
// 제작일 : 2007-12-09
// 프로그램 설명 : Session을 이용해 로그인 실시 
// 			로그인에 성공하면 <login>success</login>, employee정보를 반환
//			실패하면 <login>fail</login>
//
session_start();

$METHOD = $_GET;
$ADMIN_ID = "jidolstar";
$ADMIN_PW = "1111";

$id = $METHOD['id'];
$pw = $METHOD['pw'];

$login_id = $_SESSION['login_id'];
if( $login_id )
{
	$id = $login_id;
}

header("Content-type: text/xml;charset=utf-8");
header("Cache-Control: no-cache, must-revalidate");
header("Pragma: no-cache");
echo "<?xml version=\"1.0\" encoding=\"utf-8\"?>";

if( ($id == $ADMIN_ID && $pw == $ADMIN_PW )|| $login_id )
{

	$xml= "
		<root>
		<id>$id</id>
		<login>success</login>
        <employee>
            <name>Christina Coenraets</name>
            <phone>555-219-2270</phone>
            <email>ccoenraets@fictitious.com</email>
            <active>true</active>
        </employee>
        <employee>
            <name>Joanne Wall</name>
            <phone>555-219-2012</phone>
            <email>jwall@fictitious.com</email>
            <active>true</active>
        </employee>
        <employee>
            <name>Maurice Smith</name>
            <phone>555-219-2012</phone>
            <email>maurice@fictitious.com</email>
            <active>false</active>
        </employee>
        <employee>
            <name>Mary Jones</name>
            <phone>555-219-2000</phone>
            <email>mjones@fictitious.com</email>
            <active>true</active>
        </employee>
		</root>
	";

	if( !$login_id )
	{
		$_SESSION['login_id'] = $id;
	}

}
else
{
	$xml = "
		<root>
		<id/>
		<login>fail</login>
		</root>
		";
}
echo iconv("CP949", "UTF-8", $xml);

?>
