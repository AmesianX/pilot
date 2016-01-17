<?
// Title  : ���ø��� ���� ���̺귯��
// �ۼ��� : ������ (ryujt658@hanmail.net)
// �ۼ��� : 2002�� 5�� 16��


  //// Class ���� �κ�

  // ���ø� ������ �������� Ŭ����
  class TTemplet
  {
    var $Source;
    var $Result;
    var $ResultLine;

    // ���ڿ� $Value�� $Split�� �������� �߶󳽴�.
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

    // ���ø��� ���� �����Ϳ� �ִ� ���ڿ����� $Source�� ã�Ƴ���, $Target���� ��ü�Ѵ�.
    function Translate($Source, $Target)
    {
      for ($i=0; $i<count($this->Source); $i++)
        $this->Source[$i] = str_replace($Source, $Target, $this->Source[$i]);
    }

    // ���ø��� ���� �����Ϳ� �ִ� ���ڿ����� $Source�� ã�Ƴ���, $Target���� ��ü�Ѵ�.
    function Replace($Source, $Target, $Index)
    {
      $this->Source[$Index] = str_replace($Source, $Target, $this->Source[$Index]);
    }

    // $Index �κ��� �����Ѵ�.
    function Delete($Index)
    {
      $this->Source[$i] = "";
      $this->Result[$i] = "";
    }

    // $Query�� ���ڵ� ����ŭ �ݺ��ϸ鼭 �߶� $Index��° �κ��� #�ʵ���� ��������
    // ����� ġȯ�ϸ� �ٿ�������.
    function DataSetProducer($Query, $Index)
    {
      // DataSetProducer($Query, $Index)�� ���ؼ� ġȯ�� �κ��̶�� ǥ��
      // $Query�� ���ڵ尡 ���� ��, ���� ���ø��� ��µǴ� ���� �����ϴ� ȿ���� ��
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

    // �߶� $Index��° �κ��� #�ʵ���� $Query�� ������ ����� ġȯ�Ѵ�.
    function DataSetTranslate($Query, $Index)
    {
      $this->Result[$Index] = $Query->Translate($this->Source[$Index]);
    }

    function DataSetTranslateTag($Query, $Tag, $Index)
    {
      $this->Result[$Index] = $Query->TranslateTag($this->Source[$Index], $Tag);
    }

    // ��� ���ø� ġȯ�۾��� ������ �װ��� �ϳ��� ���ڿ� $TTemplete->ResultLine�� �����ش�.
    function PageProducer()
    {
      $this->ResultLine = "";
      for ($i=0; $i<count($this->Result); $i++)
        if ($this->Result[$i] == "") $this->ResultLine .= $this->Source[$i];
        else $this->ResultLine .= $this->Result[$i];
    }

  }


  // ����Ʈ�� �� �������� �ƴ� ��, �׺���̼ǿ� �ʿ��� ������ �����Ѵ�.
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

      // GET ������� ���޵� ����
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
