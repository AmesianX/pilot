<?
// Title  : 템플릿에 관한 라이브러리
// 작성자 : 류종택 (ryujt658@hanmail.net)
// 작성일 : 2002년 5월 16일


  //// Class 정의 부분

  // 탬플릿 파일을 조각내는 클래스
  class TTemplet
  {
    var $Source;
    var $Result;
    var $ResultLine;

    // 문자열 $Value를 $Split를 기준으로 잘라낸다.
    function TTemplet($Value, $Split)
    {
      $iTop = 0;
      $iPos = 0;

      while ($iPos < strlen($Value)) {
        if (substr($Value, $iPos, strlen($Split)) == $Split) {
          $this->Source[] = substr($Value, $iTop, $iPos-$iTop);
          $this->Result[] = "";
          $iPos += strlen($Split);
          $iTop = $iPos;
        } else {
          $iPos += 1;
        }
      }
      if ($iTop < $iPos) {
        $this->Source[] = substr($Value, $iTop, $iPos-$iTop);
        $this->Result[] = "";
      }
    }

    // 템플릿의 원본 데이터에 있는 문자열에서 $Source를 찾아내어, $Target으로 대체한다.
    function Translate($Source, $Target)
    {
      for ($i=0; $i<count($this->Source); $i++)
        $this->Source[$i] = str_replace($Source, $Target, $this->Source[$i]);
    }

    // 템플릿의 원본 데이터에 있는 문자열에서 $Source를 찾아내어, $Target으로 대체한다.
    function Replace($Source, $Target, $Index)
    {
      $this->Source[$Index] = str_replace($Source, $Target, $this->Source[$Index]);
    }

    // $Index 부분은 삭제한다.
    function Delete($Index)
    {
      $this->Source[$i] = "";
      $this->Result[$i] = "";
    }

    // $Query의 레코드 수만큼 반복하면서 잘라낸 $Index번째 부분의 #필드명을 데이터의
    // 내용과 치환하며 붙여나간다.
    function DataSetProducer($Query, $Index)
    {
      // DataSetProducer($Query, $Index)를 통해서 치환된 부분이라는 표식
      // $Query에 레코드가 없을 때, 원본 템플릿이 출력되는 것을 방지하는 효과가 남
      $this->Result[$Index] .= "<!DataSetProducer Output #$Index >";

      $Query->First();

      while (!$Query->EOF) {
        $Record = $this->Source[$Index];

        $Record = str_replace("@RecNoDesc", $Query->RecNoDesc, $Record);
        $Record = str_replace("@RecNo",     $Query->RecNo,     $Record);
        $this->Result[$Index] .= $Query->Translate($Record);

        $Query->Next();
      }
    }

    // 잘라낸 $Index번째 부분의 #필드명을 $Query의 데이터 내용과 치환한다.
    function DataSetTranslate($Query, $Index)
    {
      $this->Result[$Index] = $Query->Translate($this->Source[$Index]);
    }

    function DataSetTranslateTag($Query, $Tag, $Index)
    {
      $this->Result[$Index] = $Query->TranslateTag($this->Source[$Index], $Tag);
    }

    // 모든 템플릿 치환작업이 끝나면 그것을 하나의 문자열 $TTemplete->ResultLine을 합쳐준다.
    function PageProducer()
    {
      $this->ResultLine = "";
      for ($i=0; $i<count($this->Result); $i++)
        if ($this->Result[$i] == "") $this->ResultLine .= $this->Source[$i];
        else $this->ResultLine .= $this->Result[$i];
    }

  }


  // 리스트가 한 페이지가 아닐 때, 네비게이션에 필요한 버턴을 생성한다.
  class TNaviBar
  {

    var $ImagePath = "images";

    var $enFirst;
    var $enPrev;
    var $enNext;
    var $enLast;
    var $dsFirst;
    var $dsPrev;
    var $dsNext;
    var $dsLast;
    var $NaviBar;
    var $enNaviBar;
    var $dsNaviBar;

    var $Size = 10;

    var $ResultLine;
    var $CurrentPage;
    var $LastPage;

    function  TNaviBar($ResultLine, $CurrentPage, $LastPage)
    {
      $this->ResultLine = $ResultLine;
      $this->CurrentPage = $CurrentPage;
      $this->LastPage = $LastPage;
    }

    function MakeButton()
    {
      $PHP_SELF = $_SERVER[PHP_SELF];

      // GET 방식으로 전달된 값들
      $QueryString = $_SERVER[QUERY_STRING];
      if (substr($QueryString, 0, 5) == "Page=") {
        $QueryString = substr($QueryString, strpos($QueryString, "&")+1, strlen($QueryString));
      }

      $this->enFirst = "<a href='$PHP_SELF?Page=1&$QueryString'><img src='$this->ImagePath/icFirst.gif' border='0'></a>";
      $this->enPrev  = "<a href='$PHP_SELF?Page=@Page&$QueryString'><img src='$this->ImagePath/icPrev.gif' border='0'></a>";
      $this->enNext  = "<a href='$PHP_SELF?Page=@Page&$QueryString'><img src='$this->ImagePath/icNext.gif' border='0'></a>";
      $this->enLast  = "<a href='$PHP_SELF?Page=@Page&$QueryString'><img src='$this->ImagePath/icLast.gif' border='0'></a>";
      $this->dsFirst = "<img src='$this->ImagePath/igFirst.gif' border='0'>";
      $this->dsPrev  = "<img src='$this->ImagePath/igPrev.gif'  border='0'>";
      $this->dsNext  = "<img src='$this->ImagePath/igNext.gif'  border='0'>";
      $this->dsLast  = "<img src='$this->ImagePath/igLast.gif'  border='0'>";
      $this->NaviBar = "";
      $this->enNaviBar = "<a href='$PHP_SELF?Page=@Page&$QueryString'>[@Page]</a>&nbsp;";
      $this->dsNaviBar = "<b>@Page</b>&nbsp;";

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
      $NaviEnd = $NaviStart + $this->Size-1;
      if ($NaviEnd > $this->LastPage) {$NaviEnd = $this->LastPage;}

      $stTemp = "";
      for ($i=$NaviStart; $i<=$NaviEnd; $i++) {
        if ($this->enNaviBar) {
          if ($i == $this->CurrentPage) $stTemp .= str_replace("@Page", $i, $this->dsNaviBar);
          else $stTemp .= str_replace("@Page", $i, $this->enNaviBar);
        } else $stTemp .= str_replace("@Page", $i, $this->NaviBar);
      }
      $this->ResultLine = str_replace("@NaviBar", $stTemp, $this->ResultLine);
    }

  }

?>
