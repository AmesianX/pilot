unit _frShellControl;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.Shell.ShellCtrls, Vcl.ExtCtrls;

type
  TfrShellControl = class(TFrame)
    ShellTreeView: TShellTreeView;
    ShellListView: TShellListView;
    Splitter1: TSplitter;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
