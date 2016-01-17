<?
// Title  : 쇼핑몰의 장바구니를 구현한 콤포넌트
//          Minseok Choi(eraser95@korea.com)님께서 공개해주신 것을 수정하였습니다.
// 작성자 : 류종택 (ryujt658@hanmail.net)
// 작성일 : 2001년 9월 25일


  #### Class 정의 부분

  // 쇼핑몰의 장바구니를 구현한 Class
  class TBasket
  { // Begin of Class

    var $Record;
    var $RecNo = 0;
    var $RecNoDesc = 0;
    var $Fields;
    var $FieldCount = 5;
    var $Now; // 현재 직업중인 Item
              // 현재 작업대상인 Item을 표시하여 방금 삭제되거나 변경된 데이터를
              // 다루는데 사용한다.

    var $EOF;

    function TBasket()
    {
      $Session_Basket = Array();
    }

    function Size()
    {
      global $Session_Basket;

      if (!is_array($Session_Basket)) {
        return 0;
      } else {
        return count($Session_Basket);
      }
    }

    function AddItem($Code, $Name, $Quantity, $Price, $Mile)
    {
      global $Session_Basket;

      // 현재 작업중인 Item
      $this->Now = null;

      // same code exist??  그래도 넣어준다 -.- 코드는 같고 옵션이 다른 넘 때문에
//      if (!$this->EditItem($Code, $Name, $Quantity, $Price, $Mile)) {
        $Item = new TItem($Code, $Name, $Quantity, $Price, $Mile);
        $Session_Basket[] = $Item;

        // 현재 작업중인 Item
        $this->Now = $Item;
//      }
    }

    function EditItem($Code, $Name, $Quantity, $Price, $Mile)
    {
      global $Session_Basket;

      // 현재 작업중인 Item
      $this->Now = null;

      if ($Session_Basket == "") { return false; }

      reset($Session_Basket);
      while (list($k, $v) = each($Session_Basket)) {
        if ($Session_Basket[$k]->Code == $Code) {
          $Session_Basket[$k]->Name = $Name;
          $Session_Basket[$k]->Quantity = $Quantity;
          $Session_Basket[$k]->Price = $Price;
          $Session_Basket[$k]->Mile = $Mile;

          // 현재 작업중인 Item, 변경된 데이터를 남겨둔다
          $this->Now = $Session_Basket[$k];

          return true;
        }
    }

      return false;
    }

    function DropItem($Code)
    {
      global $Session_Basket;

      // 현재 작업중인 Item
      $this->Now = null;

      if ($Session_Basket == "") { return false; }

      reset($Session_Basket);
      while (list($k, $v) = each($Session_Basket)) {
        if ($Session_Basket[$k]->Code == $Code) {
          // 현재 작업중인 Item, 지워진 데이터가 무엇인지 남겨둔다.
          $this->Now = $Session_Basket[$k];

          // 해당 데이터를 지운다.
          unset($Session_Basket[$k]);

          return true;
        }
      }
    }

    function EditQuantity($Code, $Quantity)
    {
      global $Session_Basket;

      // 현재 작업중인 Item
      $this->Now = null;

      if ($Session_Basket == "") { return false; }

      reset($Session_Basket);
      while (list($k, $v) = each($Session_Basket)) {
        if ($Session_Basket[$k]->Code == $Code) {
          // 현재 작업중인 Item, 변경이전 데이터가 무엇인지 남겨둔다.
          $this->Now = $Session_Basket[$k];

          // 수량을 변경한다.
          $Session_Basket[$k]->Quantity += $Quantity;

          return true;
        }
      }
    }

    function First()
    {
      global $Session_Basket;

      if ($this->Size() == 0) {
        $this->EOF = 1;
      } else {
        reset($Session_Basket);
        $this->Next();
        $this->RecNo = 1;
        $this->RecNoDesc = $this->Size();
      }
    }

    function Next()
    {
      global $Session_Basket;

      // 현재 작업중인 Item
      $this->Now = null;

      unset($this->Fields);
      unset($this->Record);
      if (list($k, $v) = each($Session_Basket)) {
        while (list($Key, $Value) = each($Session_Basket[$k])) {
          $this->Fields[] = $Key;
          $this->Record[] = $Value;
        }

        // 현재 작업중인 Item
        $this->Now = $Session_Basket[$k];

        $this->RecNo += 1;
        $this->RecNoDesc -= 1;
        $this->EOF = 0;
        $this->Now = $Session_Basket[$k];
      } else {
        $this->EOF = 1;
      }
    }

    function Clear()
    {
      global $Session_Basket;

      // 현재 작업중인 Item
      $this->Now = null;

      $Session_Basket = null;
    }

    function AsInteger($Index)
    {
      $Result = $this->Record[$Index];
      $Result = str_replace(",", "", $Result);

      return $Result;
    }

    function Translate($Text)
    {
      $ResultLine = $Text;

      for ($i=0; $i<$this->FieldCount; $i++) {
        $Record = stripslashes($this->Record[$i]);
        $ResultLine = str_replace("#".$this->Fields[$i], $Record, $ResultLine);
        $ResultLine = str_replace("@RecNoDesc", $this->RecNoDesc, $ResultLine);
        $ResultLine = str_replace("@RecNo", $this->RecNo, $ResultLine);
      }

      return $ResultLine;
    }

  } // End of Class

?>
