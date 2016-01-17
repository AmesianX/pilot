unit _fmMain;

interface

uses
  ParserUtils, Strg, Calculator,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmMain = class(TForm)
    moMsg: TMemo;
    procedure moMsgKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCalculator : TCalculator;
  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FCalculator := TCalculator.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCalculator);
end;

procedure TfmMain.moMsgKeyPress(Sender: TObject; var Key: Char);
var
  sLine, sErrorMsg : string;
  iIndex, iErrorIndex, iTokenSize : integer;
begin
  if Key = '=' then begin
    Key := #0;

    iIndex := moMsg.Perform(EM_LINEFROMCHAR, moMsg.SelStart, 0);
    sLine := moMsg.Lines[iIndex];

    if Pos('=', sLine) > 0 then sLine := DeleteRightPlus(sLine, '=');
    sLine := Trim(sLine);

    iErrorIndex := -1;
    iTokenSize := 1;
    try
      FCalculator.Execute(sLine);
      moMsg.Lines[iIndex] := sLine + ' = ' + FloatToStr(FCalculator.Result);
    except
      on E : EScanError do begin
        iErrorIndex := E.Index;
        iTokenSize := E.Size;
        sErrorMsg := E.Message;
      end;

      on E : EParsingError do begin
        iErrorIndex := E.Index;
        iTokenSize := E.Size;
        sErrorMsg := E.Message;
      end;

      else begin
        iErrorIndex := 0;
        sErrorMsg := '';
      end;
    end;

    if iErrorIndex > -1 then begin
      moMsg.Lines[iIndex] := sLine + ' = Error! (' + sErrorMsg +')';
      moMsg.SelStart := moMsg.Perform(EM_LINEINDEX, iIndex, 0) + iErrorIndex - 1;
      moMsg.SelLength := iTokenSize;
    end;
  end;
end;

end.
