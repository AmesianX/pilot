{

 TinyPascal compiler v0.01. Copyright Bloodshed Software.
 See ReadMe.txt for more informations.

 Utils.pas : ExecuteFile() function for the TinyPascal compiler

 The sources can be distributed if no changes if made.
 You can use the sources:
 - for educational purpose.
 - to participate in the improvment of this program ( how about making a real pascal compiler ?)
   Please then send me your sources to:
   Colin.Laplace@Wanadoo.fr

   www: http://www.bloodshed.nu/
}

unit Utils;

interface

uses ShellApi, Forms, Windows, SysUtils;

function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;

implementation

function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;
var
  zFileName, zParams, zDir: array[0..79] of Char;
begin
  Result := ShellExecute(0, nil,
    StrPCopy(zFileName, FileName), StrPCopy(zParams, Params),
    StrPCopy(zDir, DefaultDir), ShowCmd);
end;

end.
 