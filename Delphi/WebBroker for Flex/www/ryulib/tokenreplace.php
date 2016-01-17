<?

  class TTokenReplace 
  {
    private $_Token;
    private $_Tokens = array();
    private $_Targets = array();

    private $_State;

    var $IgnoreCase = true;
    var $WhiteSpace = "\n\r ~!$%^&*()+`-={}|[]\\:;\"'<>?,./";
    var $InputStr;
    var $OutputStr;
    
    function TTokenReplace()
    {
    }
    
    function Clear()
    {
      $this->InputStr = "";
      $this->_Tokens = array();
      $this->_Targets = array();
    }
    
    function Replace($AToken, $ATarget)
    {
      $this->_Tokens[] = $AToken;
      $this->_Targets[] = $ATarget;
    }
    
    private function do_Base($Ch)
    {
      $iPos = strpos($this->WhiteSpace, $Ch);
      if ($iPos === FALSE) {
        $this->_Token = "";
        $this->_State = "Token";
        $this->do_Token($Ch);
      } else {
        $this->_State = "WhiteSpace";
        $this->do_WhiteSpace($Ch);
      }
    }
    
    private function do_WhiteSpace($Ch)
    {
      if (!$Ch) return;
      
      $iPos = strpos($this->WhiteSpace, $Ch);
      if ($iPos === FALSE) {
        $this->_Token = "";
        $this->_State = "Token";
        $this->do_Token($Ch);
      } else {
        $this->OutputStr .= $Ch;
      }
    }
    
    private function get_Target($Token)
    {
      for ($i=0; $i<count($this->_Tokens); $i++) {
        if ($this->_Tokens[$i] == $Token) {
          return $this->_Targets[$i];
        }
      }

      return $Token;
    }

    private function do_Token($Ch)
    {
      if (!$Ch) {
        if ($this->_Token) {
          $this->OutputStr .= $this->get_Target($this->_Token);
        }

        return;
      }

      $iPos = strpos($this->WhiteSpace, $Ch);
      if ($iPos === FALSE) {
        $this->_Token .= $Ch;
      } else {
        if ($this->_Token) {
          $this->OutputStr .= $this->get_Target($this->_Token);
        }
        
        $this->_State = "WhiteSpace";
        $this->do_WhiteSpace($Ch);
      }
    }
    
    private function do_Execute($Ch)
    {
      switch ($this->_State) {
        case "Base": $this->do_Base($Ch); break;
        case "WhiteSpace": $this->do_WhiteSpace($Ch); break;
        case "Token": $this->do_Token($Ch); break;
      }
    }
    
    function Execute()
    {
      $this->OutputStr = "";
      $this->_Token = "";
      
      $this->_State = "Base";
      for ($i=0; $i<strlen($this->InputStr); $i++) {
        $this->do_Execute($this->InputStr[$i]);
      } 

      $this->do_Execute("");
    }
  }
?>
