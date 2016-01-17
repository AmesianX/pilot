{

 TinyPascal compiler v0.01. Copyright Bloodshed Software.
 See ReadMe.txt for more informations.

 This program implements a compiler for a Pascal like
 called "TinyPascal". The only things it can accept are
 Write() and Writeln() procedures.
 The compiler generates x86 assembly that can be assembled
 and linked with Pass32 assembler (thanks to Dieter Pawelczak for this).

 EBNF grammar of the TinyPascal language:

 tinypascal_program ::= "PROGRAM" identifier ";" main_block "."
 main_block ::= "BEGIN" write_procedure | writeln_procedure "END"
 write_procedure ::= "WRITE "(" string ")" ";"
 writeln_procedure ::= "WRITELN" "(" string ")" ";"

 The sources can be distributed if no changes if made.
 You can use the sources:
 - for educational purpose.
 - to participate in the improvement of this program ( how about making a real pascal compiler ?)
   Please then send me your sources to:
   Colin.Laplace@Wanadoo.fr

   www: http://www.bloodshed.nu/
}

{$APPTYPE CONSOLE}

program TinyPas;

uses Errors, Utils, SysUtils, Forms, Windows, Dialogs;

type t_write = (write_,writeln_);

var
    sourcefile, asmfile   : Text;
    param, s, asmfilename : string;
    c,Look                : char;
    Value                 : string[16];
    i                     : integer;


const TAB = ^I;            // Char for a TAB
      CR  = ^M;            // Char for a CR
      LF  = ^J;            // Char for a LF
      NCount: integer = 0; // number of message to display

label Compile;
label Block;
label Main;
{----------------------------------------------------------}
{ Get next character }
procedure GetChar;
begin
   Read(sourcefile,Look);
end;

{ Recognize an Alpha Character }
function IsAlpha(c: char): boolean;
begin
   IsAlpha := UpCase(c) in ['A'..'Z'];
end;

{ Skip A Comment Field }
procedure SkipComment;
begin
   while Look <> '}' do begin
      GetChar;
      if Look = '{' then SkipComment;
   end;
   GetChar;
end;

{ Recognize White Space }
function IsWhite(c: char): boolean;
begin
   IsWhite := c in [' ', TAB, CR, LF, '{'];
end;

{ Skip Over Leading White Space }
procedure SkipWhite;
begin
   while IsWhite(Look) do begin
      if Look = '{' then
         SkipComment
      else
         GetChar;
   end;
end;

{ Get an Identifier }
procedure GetIdent;
begin
   SkipWhite;
   Value := '';
   repeat
      Value := Value + UpCase(Look);
      GetChar;
   until not IsAlpha(Look);
end;

{ Get next ident }
procedure Next;
begin
   SkipWhite;
   if IsAlpha(Look) then GetIdent;
end;

{ Match a keyword }
function Match(x: string) : boolean;
begin
   if x = 'WRITE' then
   begin
        if Value = x then Match:= true;
        exit;
   end;

   if x = 'WRITELN' then
   begin
        if Value = x then Match:= true;
        exit;
   end;

   if Value = x then Match:= true;
   Next;
end;

{ Generate a new name for each string to compile }
function NewName: string;
var S: string;
begin
   Str(NCount, S);
   NewName := 'N' + S;
   Inc(NCount);
end;

{ Generate the x86 assembly codes }
procedure Gen_Asm(s : string; w_type : t_write);
var n : string;
begin
    i := i + 1;

    if i = 1 then begin
       Writeln(asmfile,'.MODEL TINY');
       Writeln(asmfile,'.DATA');
    end;

    n := NewName;
    case w_type of
    write_ : Writeln(asmfile,TAB,n +' db '''+s+''''+','+'''$''');
    writeln_ : Writeln(asmfile,TAB,n +' db '''+s+''''+',10,13,'+'''$''');
    end;
end;

{ Generate the final x86 assembly }
procedure End_GenAsm;
var j : integer;
begin
    Writeln(asmfile,'.CODE');
    Writeln(asmfile,'Start:');
    Writeln(asmfile,TAB,'mov ah,9');

    for j:=0 to NCount-1 do begin
        Writeln(asmfile,TAB,'mov dx,offset '+'N',j );
        Writeln(asmfile,TAB,'int 21h');
    end;

    Writeln(asmfile,TAB,'mov ah,4ch');
    Writeln(asmfile,TAB,'int 21h');
    Writeln(asmfile,'End Start' );
    Writeln(asmfile,'End');
end;

{ Process a Write or Writeln Statement }
procedure DoWrite(w_type : t_write);
begin
   s := '';

   SkipWhite;
   if Look = '(' then GetChar
   else Fatal_Error(e_paren_open);

   SkipWhite;
   if Look = '''' then GetChar
   else Fatal_Error(e_quote);

   while Look <> '''' do
   begin
        s := s + Look;
        GetChar;
   end;
   GetChar;

   SkipWhite;
   if Look = ')' then
         begin
              GetChar;
              SkipWhite;
              if Look = ';' then
                 Gen_Asm(s,w_type)
              else Fatal_Error(e_semicolon);
         end
      else Fatal_Error(e_paren_close);
   Next;
end;

{ Init parser }
procedure Init;
begin
   i := 0;
   GetChar;
   Next;
end;

{ Main }
begin
     Param := '';
     Writeln('TinyPascal compiler v0.01 - Copyright Bloodshed Software');
     Writeln;
     if paramcount=0 then
        begin
             Writeln('Usage: TinyPas <filename>');
             Writeln('TinyPascal compiler for the Tiny Pascal language.');
             Writeln;
             Writeln('See ReadMe.txt for more informations.');
             Writeln;
             Writeln('Please the filename of the file to compile:');
             ReadLn(Param);
             Writeln;
             // if no file name is given in input, ask the user for one
             Goto Main;
        end
     else begin
               Param := ParamStr(1);

               Main:

               if not FileExists(Param) then
               begin
                    Writeln('File '+ Param +' not found.');
                    Halt;
               end;

               AssignFile(sourcefile,Param);
               Reset(sourcefile);

               asmfilename := ExtractFilePath(Param) +  ChangeFileExt(ExtractFileName(Param),'.asm');
               { asmfilename get the name of the source file but with a .asm extension }

               AssignFile(asmfile,asmfilename);
               Rewrite(asmfile);

               Init;           // Init parser

     { Parse source file }
     if Match('PROGRAM') = false then Fatal_Error(e_program);
     Next;
     if Look = ';' then begin
        GetChar; Next;
     end
     else Fatal_Error(e_semicolon);

     if Match('BEGIN') = false then Fatal_Error(e_begin);

     Block :

     if Match('WRITE') then
        DoWrite(write_);
     if Match('WRITELN') then
        DoWrite(writeln_);

     GetChar;
     Next;

     while Value = 'WRITE' do goto Block;
     while Value = 'WRITELN' do goto Block;

     if Match('END') = false then Fatal_Error(e_end);
     SkipWhite;

     if Look <> '.' then Fatal_Error(e_point);

     { End of parsing }

     End_GenAsm; // Generate final x86 assembly

     end;
               CloseFile(sourcefile);
               CloseFile(asmfile);
               { Close both files }

                    Writeln('Compilation successful !');
                    Writeln;
                    Write('Do you want to generate a Dos executable file (y/n)? ');
                    Read(c);
                    Writeln;
                    case c of
                    'y': goto Compile;
                    'Y': goto Compile
                    else Halt;

                    Compile: begin
                             ExecuteFile(ExtractFilePath(application.exename)+'\pass32.exe', asmfilename +' -t','',SW_SHOW);
                             // execute the assembler and linker to generate a .com executable file
                     end;
       end;
end.
