unit freBar;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Menus, StdCtrls;

type
  TFrame1 = class(TFrame)
    Button1: TButton;
    PopupMenu1: TPopupMenu;
    test1: TMenuItem;
    procedure test1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFrame1.test1Click(Sender: TObject);
begin
  ShowMessage('Hi');
end;

end.
