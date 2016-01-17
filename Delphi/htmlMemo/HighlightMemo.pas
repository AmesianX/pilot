UNIT HighlightMemo;

INTERFACE

USES
  Windows, SysUtils, Classes, Controls, StdCtrls, Messages, Graphics;

TYPE
  THighlightMemo = CLASS(TMemo)
  PRIVATE
    //FLabel: TLabel;
    FKeyword: TStringList;
    FKeyword2: TStringList;
    //PROCEDURE SetLabel(value: TLabel);
    //PROCEDURE SetKeyword(value: TStringList);
    //function GetLabel: TLabel;
    PROCEDURE WMPaint(VAR Message: TWMPaint); MESSAGE WM_PAINT;
    PROCEDURE WMSize(VAR Message: TWMSize); MESSAGE WM_SIZE;
    PROCEDURE WMMove(VAR Message: TWMMove); MESSAGE WM_MOVE;
    PROCEDURE WMVScroll(VAR Message: TWMMove); MESSAGE WM_VSCROLL;
    PROCEDURE WMMousewheel(VAR Message: TWMMove); MESSAGE WM_MOUSEWHEEL;
  PROTECTED
    PROCEDURE Change; OVERRIDE;
    PROCEDURE KeyDown(VAR Key: Word; Shift: TShiftState); OVERRIDE;
    PROCEDURE KeyUp(VAR Key: Word; Shift: TShiftState); OVERRIDE;
    PROCEDURE MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      OVERRIDE;
    PROCEDURE MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      OVERRIDE;
  PUBLIC
    PROCEDURE Update_label;
    PROCEDURE GotoXY(mCol, mLine: Integer);
    FUNCTION Line: Integer;
    FUNCTION Col: Integer;
    FUNCTION TopLine: Integer;
    FUNCTION VisibleLines: Integer;
    FUNCTION IsKeyWord(s: STRING): Boolean;
    FUNCTION IsKeyWord2(s: STRING): Boolean;
    CONSTRUCTOR Create(AOwer: TComponent); OVERRIDE;
    DESTRUCTOR Destroy; OVERRIDE;
  PUBLISHED
    //PROPERTY PosLabel: TLabel READ GetLabel WRITE SetLabel;
    //property KeywordList: TStringList READ FKeyword WRITE SetKeyword;
  END;

PROCEDURE Register;

IMPLEMENTATION

PROCEDURE Register;
BEGIN
  RegisterComponents('Standard', [THighlightMemo]);
END;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

FUNCTION IsSeparator(Car: Char): Boolean;
BEGIN
  CASE Car OF
    '/', '<', '>', ' ', '=', '"', '''': Result := True;
  ELSE
    Result := False;
  END;
END;
////////////////////////////////////////////////////////////////////////////////

FUNCTION NextWord(VAR s: STRING; VAR PrevWord: STRING): STRING;
BEGIN
  Result := '';
  PrevWord := '';
  IF s = '' THEN Exit;
  WHILE (s <> '') AND IsSeparator(s[1]) DO
  BEGIN
    PrevWord := PrevWord + s[1];
    Delete(s, 1, 1);
  END;
  WHILE (s <> '') AND NOT IsSeparator(s[1]) DO
  BEGIN
    Result := Result + s[1];
    Delete(s, 1, 1);
  END;
END;
////////////////////////////////////////////////////////////////////////////////

FUNCTION THighlightMemo.IsKeyWord(s: STRING): Boolean;
BEGIN
  Result := False;
  IF s = '' THEN Exit;
  IF FKeyword.IndexOf(lowercase(s)) <> -1 THEN
    Result := true;
END;
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

FUNCTION THighlightMemo.IsKeyWord2(s: STRING): Boolean;
BEGIN
  Result := False;
  IF s = '' THEN Exit;
  IF FKeyword2.IndexOf(lowercase(s)) <> -1 THEN
    Result := true;
END;
////////////////////////////////////////////////////////////////////////////////

FUNCTION IsNumber(s: STRING): Boolean;
VAR
  i: Integer;
BEGIN
  Result := False;
  FOR i := 1 TO Length(s) DO
    CASE s[i] OF
      '0'..'9': ;
    ELSE
      Exit;
    END;
  Result := True;
END;
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

CONSTRUCTOR THighlightMemo.Create(AOwer: TComponent);
BEGIN
  INHERITED Create(AOwer);
  //FLabel := NIL;
  FKeyword := TStringList.Create;
  FKeyword2 := TStringList.Create;
  Font.Name:='MS Sans Serif';
  FKeyword.CommaText := 'html,head,title,body,font,table,b,i,u,s,sub,sup,center,brink,pre,xmp,'
    + 'p,hr,a,img,ul,ol,li,tr,td,th,caption,frameset,frame,form,input,select,option,textarea,base,'
    + 'br,div,meta,script,iframe,style,tbody,fieldset,strong,samp';
  FKeyword2.CommaText := 'src,alt,name,href,height,width,border,colspan,cellpadding,cellspacing,'
    + 'bgcolor,align,valign,margin,leftmargin,rightmargin,topmargin,link,target,id,class,'
    + 'color,face,size,content,type,nowrap,vspace,hspace';
END;

DESTRUCTOR THighlightMemo.Destroy;
BEGIN
 // IF (FLabel <> NIL) THEN FLabel := NIL;
  FKeyword.Free;
  FKeyword2.Free;
  INHERITED Destroy;
END;

{function THighlightMemo.GetLabel: TLabel;
begin
  Result := FLabel;
end;

PROCEDURE THighlightMemo.SetLabel(Value: TLabel);
BEGIN
  FLabel := Value;
  IF Value <> NIL THEN
  BEGIN
    Value.FreeNotification(Self);
    Invalidate;
  END;
END;

PROCEDURE THighlightMemo.SetKeyword(Value: TStringList);
BEGIN
   FKeyword.Assign(Value);
END;}

FUNCTION THighlightMemo.VisibleLines: Integer;
BEGIN
  Result := Height DIV (Abs(Self.Font.Height) + 2);
END;
////////////////////////////////////////////////////////////////////////////////

PROCEDURE THighlightMemo.GotoXY(mCol, mLine: Integer);
BEGIN
  Dec(mLine);
  SelStart := 0;
  SelLength := 0;
  SelStart := mCol + Self.Perform(EM_LINEINDEX, mLine, 0);
  SelLength := 0;
  SetFocus;
END;
////////////////////////////////////////////////////////////////////////////////

PROCEDURE THighlightMemo.Update_label;
BEGIN
  //IF PosLabel = NIL THEN Exit;
  //PosLabel.Caption := '(' + IntToStr(Line + 1) + ',' + IntToStr(Col) + ')';
  //Invalidate;
END;
////////////////////////////////////////////////////////////////////////////////

FUNCTION THighlightMemo.TopLine: Integer;
BEGIN
  Result := SendMessage(Self.Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
END;
////////////////////////////////////////////////////////////////////////////////

FUNCTION THighlightMemo.Line: Integer;
BEGIN
  Result := SendMessage(Self.Handle, EM_LINEFROMCHAR, Self.SelStart, 0);
END;
////////////////////////////////////////////////////////////////////////////////

FUNCTION THighlightMemo.Col: Integer;
BEGIN
  Result := Self.SelStart - SendMessage(Self.Handle, EM_LINEINDEX,
    SendMessage(Self.Handle,
    EM_LINEFROMCHAR, Self.SelStart, 0), 0);
END;
////////////////////////////////////////////////////////////////////////////////

PROCEDURE THighlightMemo.WMVScroll(VAR Message: TWMMove);
BEGIN
  Update_label;
  Invalidate;
  INHERITED;
END;
////////////////////////////////////////////////////////////////////////////////

PROCEDURE THighlightMemo.WMSize(VAR Message: TWMSize);
BEGIN
  Invalidate;
  INHERITED;
END;
////////////////////////////////////////////////////////////////////////////////

PROCEDURE THighlightMemo.WMMove(VAR Message: TWMMove);
BEGIN
  Invalidate;
  INHERITED;
END;
////////////////////////////////////////////////////////////////////////////////

PROCEDURE THighlightMemo.WMMousewheel(VAR Message: TWMMove);
BEGIN
  Invalidate;
  INHERITED;
END;
////////////////////////////////////////////////////////////////////////////////

PROCEDURE THighlightMemo.Change;
BEGIN
  Update_label;
  Invalidate;
  INHERITED Change;
END;
////////////////////////////////////////////////////////////////////////////////

PROCEDURE THighlightMemo.KeyDown(VAR Key: Word; Shift: TShiftState);
BEGIN
  Update_label;
  INHERITED KeyDown(Key, Shift);
END;
////////////////////////////////////////////////////////////////////////////////

PROCEDURE THighlightMemo.KeyUp(VAR Key: Word; Shift: TShiftState);
BEGIN
  Update_label;
  INHERITED KeyUp(Key, Shift);
END;
////////////////////////////////////////////////////////////////////////////////

PROCEDURE THighlightMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
BEGIN
  Update_label;
  Invalidate;
  INHERITED MouseDown(Button, Shift, X, Y);
END;
////////////////////////////////////////////////////////////////////////////////

PROCEDURE THighlightMemo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
BEGIN
  Update_label;
  INHERITED MouseUp(Button, Shift, X, Y);
END;
////////////////////////////////////////////////////////////////////////////////

PROCEDURE THighlightMemo.WMPaint(VAR Message: TWMPaint);
VAR
  PS: TPaintStruct;
  DC: HDC;
  Canvas: TCanvas;
  i: Integer;
  X, Y: Integer;
  OldColor: TColor;
  Size: TSize;
  Max: Integer;
  s, Palabra, PrevWord: STRING;
BEGIN
if SelLength<1 then
 begin
    DC := Message.DC;
    IF DC = 0 THEN DC := BeginPaint(Handle, PS);
    Canvas := TCanvas.Create;
    TRY
      OldColor := Font.Color;
      Canvas.Handle := DC;
      Canvas.Font.Name := Font.Name;
      Canvas.Font.Size := Font.Size;
      WITH Canvas DO
      BEGIN
        Max := TopLine + VisibleLines;
        IF Max > Pred(Lines.Count) THEN Max := Pred(Lines.Count);

      //Limpio la seccion visible
        Brush.Color := Self.Color;
        FillRect(Self.ClientRect);
        Y := 1;
        FOR i := TopLine TO Max DO
        BEGIN
          X := 2;
          s := Lines[i];

        //Detecto todas las palabras de esta linea
          Palabra := NextWord(s, PrevWord);
          WHILE Palabra <> '' DO
          BEGIN
            Font.Color := OldColor;
            TextOut(X, Y, PrevWord);
            GetTextExtentPoint32(DC, PChar(PrevWord), Length(PrevWord), Size);
            Inc(X, Size.cx);

            Font.Color := clBlack;
            IF IsKeyWord(Palabra) THEN
            BEGIN
              IF (LastDelimiter('<', PrevWord) = Length(PrevWord)) OR
                (LastDelimiter('/', PrevWord) = Length(PrevWord)) THEN
                Font.Color := clBlue;
              TextOut(X, Y, Palabra);
             {
             //Draw dot underline
             Pen.Color := clHighlight;
             Pen.Style := psDot;
             PolyLine([ Point(X,Y+13), Point(X+TextWidth(Palabra),Y+13)]);
             }
            END

            ELSE IF IsNumber(Palabra) THEN
            BEGIN
              Font.Color := clRed;
              TextOut(X, Y, Palabra);
            END
            ELSE

              IF IsKeyWord2(Palabra) and ((LastDelimiter(' ', PrevWord) = Length(PrevWord)) or
              (LastDelimiter('"', PrevWord) = Length(PrevWord)) or
              (LastDelimiter('''', PrevWord) = Length(PrevWord))) THEN
              BEGIN
                Font.Color := clPurple;
                TextOut(X, Y, Palabra);
              END
              ELSE
                TextOut(X, Y, Palabra);

            GetTextExtentPoint32(DC, PChar(Palabra), Length(Palabra), Size);
            Inc(X, Size.cx);

            Palabra := NextWord(s, PrevWord);
            IF (s = '') AND (PrevWord <> '') THEN
            BEGIN
              Font.Color := OldColor;
              TextOut(X, Y, PrevWord);
            END;
          END;
          IF (s = '') AND (PrevWord <> '') THEN
          BEGIN
            Font.Color := OldColor;
            TextOut(X, Y, PrevWord);
          END;

          s := 'W';
          GetTextExtentPoint32(DC, PChar(s), Length(s), Size);
          Inc(Y, Size.cy);
        END;
      END;
    FINALLY
      IF Message.DC = 0 THEN EndPaint(Handle, PS);
    END;
    Canvas.Free;
    end;
  INHERITED;
END;
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


END.

