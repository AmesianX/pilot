<?
// Title  : ���θ��� ��ٱ��ϸ� ������ ������Ʈ
//          Minseok Choi(eraser95@korea.com)�Բ��� �������ֽ� ���� �����Ͽ����ϴ�.
// �ۼ��� : ������ (ryujt658@hanmail.net)
// �ۼ��� : 2001�� 9�� 25��


  #### Class ���� �κ�

  // ���θ��� ��ٱ��ϸ� ������ Class
  class TBasket
  { // Begin of Class

    var $Record;
    var $RecNo = 0;
    var $RecNoDesc = 0;
    var $Fields;
    var $FieldCount = 5;
    var $Now; // ���� �������� Item
              // ���� �۾������ Item�� ǥ���Ͽ� ��� �����ǰų� ����� �����͸�
              // �ٷ�µ� ����Ѵ�.

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

      // ���� �۾����� Item
      $this->Now = null;

      // same code exist??  �׷��� �־��ش� -.- �ڵ�� ���� �ɼ��� �ٸ� �� ������
//      if (!$this->EditItem($Code, $Name, $Quantity, $Price, $Mile)) {
        $Item = new TItem($Code, $Name, $Quantity, $Price, $Mile);
        $Session_Basket[] = $Item;

        // ���� �۾����� Item
        $this->Now = $Item;
//      }
    }

    function EditItem($Code, $Name, $Quantity, $Price, $Mile)
    {
      global $Session_Basket;

      // ���� �۾����� Item
      $this->Now = null;

      if ($Session_Basket == "") { return false; }

      reset($Session_Basket);
      while (list($k, $v) = each($Session_Basket)) {
        if ($Session_Basket[$k]->Code == $Code) {
          $Session_Basket[$k]->Name = $Name;
          $Session_Basket[$k]->Quantity = $Quantity;
          $Session_Basket[$k]->Price = $Price;
          $Session_Basket[$k]->Mile = $Mile;

          // ���� �۾����� Item, ����� �����͸� ���ܵд�
          $this->Now = $Session_Basket[$k];

          return true;
        }
    }

      return false;
    }

    function DropItem($Code)
    {
      global $Session_Basket;

      // ���� �۾����� Item
      $this->Now = null;

      if ($Session_Basket == "") { return false; }

      reset($Session_Basket);
      while (list($k, $v) = each($Session_Basket)) {
        if ($Session_Basket[$k]->Code == $Code) {
          // ���� �۾����� Item, ������ �����Ͱ� �������� ���ܵд�.
          $this->Now = $Session_Basket[$k];

          // �ش� �����͸� �����.
          unset($Session_Basket[$k]);

          return true;
        }
      }
    }

    function EditQuantity($Code, $Quantity)
    {
      global $Session_Basket;

      // ���� �۾����� Item
      $this->Now = null;

      if ($Session_Basket == "") { return false; }

      reset($Session_Basket);
      while (list($k, $v) = each($Session_Basket)) {
        if ($Session_Basket[$k]->Code == $Code) {
          // ���� �۾����� Item, �������� �����Ͱ� �������� ���ܵд�.
          $this->Now = $Session_Basket[$k];

          // ������ �����Ѵ�.
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

      // ���� �۾����� Item
      $this->Now = null;

      unset($this->Fields);
      unset($this->Record);
      if (list($k, $v) = each($Session_Basket)) {
        while (list($Key, $Value) = each($Session_Basket[$k])) {
          $this->Fields[] = $Key;
          $this->Record[] = $Value;
        }

        // ���� �۾����� Item
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

      // ���� �۾����� Item
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
