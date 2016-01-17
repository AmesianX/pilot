program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  bbSlice in '..\bbSlice.pas',
  bbThreadPool in '..\bbThreadPool.pas',
  bbUtils in '..\bbUtils.pas',
  BlackBoard in '..\BlackBoard.pas',
  bbSnapShot in '..\bbSnapShot.pas',
  bbMonoBlocks in '..\bbMonoBlocks.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
