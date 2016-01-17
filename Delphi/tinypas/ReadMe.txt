**********************************************************************
*								     *	
*		 - TinyPascal compiler v0.01 -			     *	
*	         Copyright © Bloodshed Software			     *
*								     *	
*		email:  webmaster@bloodshed.nu	       		     *		
*		www:	http://www.bloodshed.nu/ 		     *
*								     *
**********************************************************************

TinyPascal implements a small compiler for a very simple Pascal 
language. It compiles Write and Writeln procedures into x86 assembly
that can be then assemble and link with Pass32 (public domain tool).
The sources have been compiled under Delphi 3. I think it should
work with others version.

- Installation :

First extract the tinypas.zip archive to a directory. Then load 
TinyPas.pas into Delphi. Just compile it and you'll have built the 
TinyPascal compiler executable.

- Compiling a file with TinyPascal :

You can compile a file by calling the compiler like this:
tinypas <filename>
Where <filename> is the path/name to the file to compile.
I you do not give any parameters, you'll be prompt for a filename.

If you got no errors, the compiler will ask you if you want to create
an executable (.com Msdos executable. If you want to have a .exe file,
then you should remove the -t parameter when you call Pass32. I decided
to have by default a .com executable because if i take a .exe file generated,
it doesn't work on Windows).

- Writing programs with TinyPascal :)

A TinyPascal source composed only of the 5 keywords: program,begin,end,
write,writeln. In can only compile Write and Writeln procedures into
x86 assembly that can be the assemble and link to an executable file.
The compiler can also encapsulate comments.

A TinyPascal source file has the following look:

{ Sample example to use with the TinyPascal compiler }
program Hello;

begin
   writeln('Hello');   { Write Hello to the console with a line break }
   write('World !');   { Write World ! to the console without a line feed }
end.


Here is the EBNF grammar of the TinyPascal language :

 tinypascal_program ::= "PROGRAM" identifier ";" main_block "."
 main_block ::= "BEGIN" write_procedure | writeln_procedure "END"
 write_procedure ::= "WRITE "(" string ")" ";"
 writeln_procedure ::= "WRITELN" "(" string ")" ";"

- Disclaimer of warranty:
  - We're not responsible of any damages, loss of data or anything else
    this program could cause your computer.
  - This program is freeware but you can't make money from it.
  - The sources can be distributed if no changes if made.
  - You can use the sources:
    * for educational purpose.
    * to participate in the improvement of this program (see below)

- Participating in the improvement of TinyPascal:
   If you want to participate on the improvement of this program,
   please send me your modified sources to: Colin.Laplace@Wanadoo.fr

www: http://www.bloodshed.nu/