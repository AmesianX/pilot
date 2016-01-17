<?
// Title  : Form Field의 콤포넌트들
// 작성자 : 류종택 (ryujt658@hanmail.net)
// 작성일 : 2001년 9월 24일


  //// Class 정의 부분

  // TComboBox Class, <Select> </Select>
  class TComboBox
  {
    private $_Value;
    private $_OnChange = "";

    var $Name;
    var $Names;
    var $Values;
    var $ResultLine;
    var $ItemIndex = -1;

    function TComboBox($Name, $Names, $Values)
    {
      $this->Name = $Name;
      $this->Names = explode("|", $Names);
      $this->Values = explode("|", $Values);
      
      $this->make_ResultLine();
    }
    
    function getValue()
    {
      return $this->_Value;
    }
    
    function getOnChange()
    {
      return $this->_OnChange;
    }
    
    function setValue($Value)
    {
      $this->_Value = $Value;
      $this->make_ResultLine();
    }
    
    function setOnChange($OnChange)
    {
      $this->_OnChange = $OnChange;
      $this->make_ResultLine();
    }
    
    function LoadFromQuery($Query)
    {
      $this->Names = array();
      $this->Values = array();

      $Query->First();
      while (!$Query->EOF) {
        $this->Names[] = $Query->Record[0];
        $this->Values[] = $Query->Record[1];

        $Query->Next();
      }
      
      $this->make_ResultLine();
    }

    private function make_ResultLine()
    {
      $this->ItemIndex = -1;
      for ($i=0; $i<count($this->Names); $i++) {
        if ($this->_Value == $this->Values[$i]) {
          $this->ItemIndex = $i;
          break;
        }
      }

      if ($this->_OnChange) {
        $this->ResultLine = "<select name='$this->Name' onchange='$this->_OnChange(this.value)'>";
      } else {
        $this->ResultLine = "<select name='$this->Name'>";
      }
      
      for ($i=0; $i<count($this->Names); $i++) {
        $Names = $this->Names[$i];
        $Values = $this->Values[$i];

        // $Names가 빈문자열이 아닐 때
        if ($Names) {
          $this->ResultLine .= "<option ";
          if ($i == $this->ItemIndex) $this->ResultLine .= "selected ";
          $this->ResultLine .= "value='$Values'>$Names</option>";
        }
      }
      $this->ResultLine .= "</select>";
    }
  }

?>

