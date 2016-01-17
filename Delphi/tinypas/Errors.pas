{

 TinyPascal compiler v0.01. Copyright Bloodshed Software.
 See ReadMe.txt for more informations.
 
 Errors.pas : Error unit for the TinyPascal compiler

 The sources can be distributed if no changes if made.
 You can use the sources:
 - for educational purpose.
 - to participate in the improvment of this program ( how about making a real pascal compiler ?)
   Please then send me your sources to:
   Colin.Laplace@Wanadoo.fr

   www: http://www.bloodshed.nu/
}

unit Errors;

interface
type
    t_error = (e_str_not_finished, e_str_too_long, e_program, e_begin, e_end,
               e_point, e_paren_open, e_paren_close, e_unknown_char, e_semicolon, e_quote);

procedure Fatal_error(err : t_error);

implementation

function err_message(err : t_error) : string;
begin
     case err of
{ N/A }   e_str_not_finished : err_message:= 'String not finsihed';
{ N/A }   e_str_too_long     : err_message:= 'String > 255 characters';
          e_program          : err_message:= 'PROGRAM missing';
          e_begin            : err_message:= 'BEGIN missing';
          e_end              : err_message:= 'END missing';
          e_point            : err_message:= '"." missing';
          e_paren_open       : err_message:= '"(" missing';
          e_paren_close      : err_message:= '")" missing';
{ N/A }   e_unknown_char     : err_message:= 'Unknown character';
          e_semicolon        : err_message:= '";" missing';
          e_quote            : err_message:= '"''" missing';
     end;
end;

procedure Fatal_Error(err : t_error);
begin
    Writeln('*** ERROR : ' + err_message(err) + ' ***');
    Writeln;
    Writeln('Compilation aborted.');
    Halt;
end;

begin
end.
