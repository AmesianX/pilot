{
IAA-pascal - interpreter jêzyka Pascal
Copyright (C) 2005 Artur Moœcicki, Igor Kruk, Adam Miler

Niniejszy program jest wolnym oprogramowaniem; mo¿esz go
rozprowadzaæ dalej i/lub modyfikowaæ na warunkach Powszechnej
Licencji Publicznej GNU, wydanej przez Fundacjê Wolnego
Oprogramowania - wed³ug wersji 2-giej tej Licencji lub którejœ
z póŸniejszych wersji.

Niniejszy program rozpowszechniany jest z nadziej¹, i¿ bêdzie on
u¿yteczny - jednak BEZ JAKIEJKOLWIEK GWARANCJI, nawet domyœlnej
gwarancji PRZYDATNOŒCI HANDLOWEJ albo PRZYDATNOŒCI DO OKREŒLONYCH
ZASTOSOWAÑ. W celu uzyskania bli¿szych informacji - Powszechna
Licencja Publiczna GNU.

Z pewnoœci¹ wraz z niniejszym programem otrzyma³eœ te¿ egzemplarz
Powszechnej Licencji Publicznej GNU (GNU General Public License);
jeœli nie - napisz do Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.

-------------------------------------------------------------------

IAA-pascal - the interpreter of Pascal language
Copyright (C) 2005 Artur Moœcicki, Igor Kruk, Adam Miler

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
}

unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Matematical, ExtCtrls;

type
  TForm3 = class(TForm)
    RichEdit1: TRichEdit;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure RichEdit1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;
  indeks_przypisania:integer;
  ktora_tablica:string;

  f,g:TextFile;
  text:string;
  zacznij_wykonywac,czy_zmienna,koniec_petli:boolean;
  do_petli_integer,do_petli_real:integer;
  dzialanie,operator_porownania,liczba_w_richedit:string;
  pierwszy_argument_porownania,drugi_argument_porownania:double;
  ilosc_begin:integer;
  poczatek_petli_for,koniec_petli_for:integer;
  zapamietaj_ilosc_przeczytanych_wierszy_do_for:integer;
  zapamietaj_polecenia_w_petli:TStringList;
  petla_stringlist, petla_stringlist1, petla_przepisywania:integer;
  ilosc_przeczytanych_wierszy:integer;
  koniec_programu:boolean;
  instrukcja:string;
  read_czy_readln:string;
  otworzony_plik:boolean;

implementation

uses Unit2,Unit1;

{$R *.DFM}

procedure wykonaj_write;
begin
    readln(f,text);
    readln(f,text);
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+2;
    if text[1]='''' then
        Form3.RichEdit1.SelText:=copy(text,2,length(text)-2)
    else
    begin
        //wyœwietlanie wartosci zmiennych
        for do_petli_integer:=0 to (Form2.MDIChildren[0] as TForm1).i-1 do
            if (Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].nazwa=text then
                Form3.RichEdit1.SelText:=IntToStr((Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].wartosc);
        for do_petli_real:=0 to (Form2.MDIChildren[0] as TForm1).r-1 do
            if (Form2.MDIChildren[0] as TForm1).zmienne_real[do_petli_real].nazwa=text then
                Form3.RichEdit1.SelText:=FloatToStr((Form2.MDIChildren[0] as TForm1).zmienne_real[do_petli_real].wartosc);
    end;
    //przeczyta nawias zamykaj¹cy i œrednik
    readln(f,text);
    readln(f,text);
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+2;
end;

procedure wykonaj_writeln;
begin
    readln(f,text);
    readln(f,text);
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+2;
    if text[1]='''' then
    begin
        Form3.RichEdit1.SelText:=copy(text,2,length(text)-2);
        Form3.RichEdit1.Lines.Add('');
    end
    else
    begin
       //wyœwietlanie wartosci zmiennych
        for do_petli_integer:=0 to (Form2.MDIChildren[0] as TForm1).i-1 do
             if (Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].nazwa=text then
                 Form3.RichEdit1.SelText:=IntToStr((Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].wartosc);
        for do_petli_real:=0 to (Form2.MDIChildren[0] as TForm1).r-1 do
             if (Form2.MDIChildren[0] as TForm1).zmienne_real[do_petli_real].nazwa=text then
                 Form3.RichEdit1.SelText:=FloatToStr((Form2.MDIChildren[0] as TForm1).zmienne_real[do_petli_real].wartosc);
        Form3.RichEdit1.Lines.Add('');
    end;
    //przeczyta nawias zamykaj¹cy i œrednik
    readln(f,text);
    readln(f,text);
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+2;
end;

procedure wykonaj_readln;
begin
    //czytamy nawias
    readln(f,text);
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
    //czytamy zamienn¹
    readln(f,text);
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
    for do_petli_integer:=0 to (Form2.MDIChildren[0] as TForm1).i-1 do
        if (Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].nazwa=text then
        begin
            ktora_tablica:='tablica_integer';
            indeks_przypisania:=do_petli_integer;
        end;
    for do_petli_real:=0 to (Form2.MDIChildren[0] as TForm1).r-1 do
        if (Form2.MDIChildren[0] as TForm1).zmienne_real[do_petli_real].nazwa=text then
        begin
            ktora_tablica:='tablica_real';
            indeks_przypisania:=do_petli_real;
        end;
    //przeczyta nawias zamykaj¹cy i œrednik
    readln(f,text);
    readln(f,text);
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+2; 
end;

procedure wykonaj_dzialania_matematyczne;
begin
    ktora_tablica:='zadna';
    for do_petli_integer:=0 to (Form2.MDIChildren[0] as TForm1).i-1 do
        if (Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].nazwa=instrukcja then
        begin
            ktora_tablica:='tablica_integer';
            indeks_przypisania:=do_petli_integer;
        end;
    for do_petli_real:=0 to (Form2.MDIChildren[0] as TForm1).r-1 do
        if (Form2.MDIChildren[0] as TForm1).zmienne_real[do_petli_real].nazwa=instrukcja then
        begin
            ktora_tablica:='tablica_real';
            indeks_przypisania:=do_petli_real;
        end;
    if ktora_tablica='tablica_integer' then
    begin
        readln(f,text);//przeczyta znak przypisania
        ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
        koniec_petli:=false;
        while text<>';' do
        begin
            czy_zmienna:=false;
            readln(f,text);
            ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
            if text=';' then
                koniec_petli:=true;

            if koniec_petli=false then
            begin
                for do_petli_integer:=0 to (Form2.MDIChildren[0] as TForm1).i-1 do
                    if (Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].nazwa=text then
                    begin
                        czy_zmienna:=true;
                        dzialanie:=dzialanie+IntToStr((Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].wartosc);
                    end;
                if czy_zmienna=false then
                    for do_petli_real:=0 to (Form2.MDIChildren[0] as TForm1).r-1 do
                        if (Form2.MDIChildren[0] as TForm1).zmienne_real[do_petli_real].nazwa=text then
                        begin
                            czy_zmienna:=true;
                            dzialanie:=dzialanie+FloatToStr((Form2.MDIChildren[0] as TForm1).zmienne_real[do_petli_real].wartosc);
                        end;
                if czy_zmienna=false then
                     dzialanie:=dzialanie+text;
            end;
        end;
        try
            (Form2.MDIChildren[0] as TForm1).zmienne_integer[indeks_przypisania].wartosc:=round(StrToFloat(AtomIntoValue(dzialanie)));
        except
            Showmessage('Dzielenie przez zero lub krytyczny b³¹d');
            Application.Terminate;
        end;
        dzialanie:='';
        ktora_tablica:='zadna';
        indeks_przypisania:=-1;
    end;
    if ktora_tablica='tablica_real' then
    begin
        readln(f,text);//przeczyta znak przypisania
        ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
        koniec_petli:=false;
        while text<>';' do
        begin
            czy_zmienna:=false;
            readln(f,text);
            ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
            if text=';' then
                koniec_petli:=true;

            if koniec_petli=false then
            begin
                for do_petli_integer:=0 to (Form2.MDIChildren[0] as TForm1).i-1 do
                    if (Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].nazwa=text then
                    begin
                        czy_zmienna:=true;
                        dzialanie:=dzialanie+IntToStr((Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].wartosc);
                    end;
                if czy_zmienna=false then
                    for do_petli_real:=0 to (Form2.MDIChildren[0] as TForm1).r-1 do
                        if (Form2.MDIChildren[0] as TForm1).zmienne_real[do_petli_real].nazwa=text then
                        begin
                            czy_zmienna:=true;
                            dzialanie:=dzialanie+FloatToStr((Form2.MDIChildren[0] as TForm1).zmienne_real[do_petli_real].wartosc);
                        end;
                if czy_zmienna=false then
                    dzialanie:=dzialanie+text;
            end;
        end;
        try
           (Form2.MDIChildren[0] as TForm1).zmienne_real[indeks_przypisania].wartosc:=StrToFloat(AtomIntoValue(dzialanie));
        except
           Showmessage('Dzielenie przez zero lub krytyczny b³¹d');
        end;
        dzialanie:='';
        ktora_tablica:='zadna';
        indeks_przypisania:=-1;
    end;
end;

procedure wykonaj_petle;
var
    minus1,minus2:boolean;
begin
    minus1:=false;
    minus2:=false;
    zapamietaj_ilosc_przeczytanych_wierszy_do_for:=ilosc_przeczytanych_wierszy-1;
    //czytamy zmienn¹ steruj¹c¹
    readln(f,text);
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
    for do_petli_integer:=0 to (Form2.MDIChildren[0] as TForm1).i-1 do
         if (Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].nazwa=text then
             indeks_przypisania:=do_petli_integer;
    //czytamy :=
    readln(f,text);
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
    //czytamy to co jest po znaku :=
    readln(f,text);
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
    if text='-' then  //jeœli minus to czytamy to co jest po tym minusie
    begin
        minus1:=true;
        readln(f,text);
        ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
    end;
    try
        if minus1=true then
        begin
            (Form2.MDIChildren[0] as TForm1).zmienne_integer[indeks_przypisania].wartosc:=(-1)*StrToInt(text);
            poczatek_petli_for:=(-1)*StrToInt(text);
        end
        else
        begin
            (Form2.MDIChildren[0] as TForm1).zmienne_integer[indeks_przypisania].wartosc:=StrToInt(text);
            poczatek_petli_for:=StrToInt(text);
        end;
    except
    end;
    for do_petli_integer:=0 to (Form2.MDIChildren[0] as TForm1).i-1 do
        if (Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].nazwa=text then
        begin
            if minus1=true then
            begin
                (Form2.MDIChildren[0] as TForm1).zmienne_integer[indeks_przypisania].wartosc:=(-1)*(Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].wartosc;
                poczatek_petli_for:=(-1)*(Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].wartosc;
            end
            else
            begin
                (Form2.MDIChildren[0] as TForm1).zmienne_integer[indeks_przypisania].wartosc:=(Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].wartosc;
                poczatek_petli_for:=(Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].wartosc;
            end;
        end;
    //czytamy to
    readln(f,text);
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
    //czytamy zakres pêtli lub minusa
    readln(f,text);
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
    if text='-' then  //jeœli minus to czytamy to co jest po tym minusie
    begin
        minus2:=true;
        readln(f,text);
        ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
    end;
    try
        if minus2=true then
            koniec_petli_for:=(-1)*StrToInt(text)
        else
            koniec_petli_for:=StrToInt(text);
    except
    end;
    for do_petli_integer:=0 to (Form2.MDIChildren[0] as TForm1).i-1 do
        if (Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].nazwa=text then
        begin
            if minus2=true then
                koniec_petli_for:=(-1)*(Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].wartosc
            else
                koniec_petli_for:=(Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].wartosc;
        end;
    //czytamy do
    readln(f,text);
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;

    zapamietaj_polecenia_w_petli:=TStringList.Create;
    //czytamy pierwsz¹ rzecz po do
    readln(f,text);
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
    zapamietaj_polecenia_w_petli.Add(text);
    if (text='begin') or (text='for') or (text='if') then    //gdy po do jest begin, for lub if
    begin
        if text='begin' then
            ilosc_begin:=ilosc_begin+1;
        repeat
            readln(f,text);
            ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
            zapamietaj_polecenia_w_petli.Add(text);
            if text='begin' then
                ilosc_begin:=ilosc_begin+1;
            if text='end' then
                ilosc_begin:=ilosc_begin-1;
        until (ilosc_begin=0) and ((text='end') or (text=';'));
        if text<>';' then //czytamy œrednik
        begin
            readln(f,text);
            zapamietaj_polecenia_w_petli.Add(text);
            ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
        end;
    end
    else                   //gdy po do jest instrukcja
        repeat
             readln(f,text);
             zapamietaj_polecenia_w_petli.Add(text);
             ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
        until text=';';
    zapamietaj_polecenia_w_petli.Add((Form2.MDIChildren[0] as TForm1).zmienne_integer[indeks_przypisania].nazwa);
    zapamietaj_polecenia_w_petli.Add(':=');
    zapamietaj_polecenia_w_petli.Add((Form2.MDIChildren[0] as TForm1).zmienne_integer[indeks_przypisania].nazwa);
    zapamietaj_polecenia_w_petli.Add('+');
    zapamietaj_polecenia_w_petli.Add('1');
    zapamietaj_polecenia_w_petli.Add(';');




    assignfile(g,ExtractFilePath(Application.ExeName)+'pomoc27.txt');
    reset(f);
    otworzony_plik:=true;
    rewrite(g);
    //showmessage(inttostr(zapamietaj_ilosc_przeczytanych_wierszy_do_for));
    for petla_przepisywania:=1 to zapamietaj_ilosc_przeczytanych_wierszy_do_for do
    begin
        readln(f,text);
        writeln(g,text);
    end;

    for petla_stringlist1:=poczatek_petli_for to koniec_petli_for do
        for petla_stringlist:=0 to zapamietaj_polecenia_w_petli.Count-1 do
            writeln(g,zapamietaj_polecenia_w_petli.Strings[petla_stringlist]);

    zapamietaj_polecenia_w_petli.Destroy;

    for petla_przepisywania:=zapamietaj_ilosc_przeczytanych_wierszy_do_for+1 to ilosc_przeczytanych_wierszy do
        readln(f,text);

    while not eof(f) do
    begin
        readln(f,text);
        writeln(g,text);
    end;

    closefile(g);
    closefile(f);
    otworzony_plik:=false;

    copyfile(PChar(ExtractFilePath(Application.ExeName)+'pomoc27.txt'),PChar(ExtractFilePath(Application.ExeName)+'pomoc26.txt'),false);

    reset(f);
    otworzony_plik:=true;
    for petla_przepisywania:=1 to zapamietaj_ilosc_przeczytanych_wierszy_do_for do
        readln(f,text);
    ilosc_przeczytanych_wierszy:=zapamietaj_ilosc_przeczytanych_wierszy_do_for;
end;

procedure wykonaj_if;
var
    minus1,minus2:boolean;
begin
    minus1:=false;
    minus2:=false;
    readln(f,text);
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
    if text='-' then
    begin
        minus1:=true;
        readln(f,text);
        ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
    end;
    try
        if minus1=true then
           pierwszy_argument_porownania:=(-1)*StrToFloat(text)
        else
           pierwszy_argument_porownania:=StrToFloat(text);
    except
    end;
    for do_petli_integer:=0 to (Form2.MDIChildren[0] as TForm1).i-1 do
        if (Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].nazwa=text then
        begin
            if minus1=true then
                pierwszy_argument_porownania:=(-1)*(Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].wartosc
            else
                pierwszy_argument_porownania:=(Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].wartosc;
        end;
    for do_petli_real:=0 to (Form2.MDIChildren[0] as TForm1).r-1 do
        if (Form2.MDIChildren[0] as TForm1).zmienne_real[do_petli_real].nazwa=text then
        begin
            if minus1=true then
                pierwszy_argument_porownania:=(-1)*(Form2.MDIChildren[0] as TForm1).zmienne_real[do_petli_real].wartosc
            else
                pierwszy_argument_porownania:=(Form2.MDIChildren[0] as TForm1).zmienne_real[do_petli_real].wartosc;
        end;
    readln(f,text);
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
    operator_porownania:=text;
    readln(f,text);
    if text='-' then
    begin
        minus2:=true;
        readln(f,text);
        ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
    end;
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
    try
         if minus2=true then
             drugi_argument_porownania:=(-1)*StrToFloat(text)
         else
             drugi_argument_porownania:=StrToFloat(text);
    except
    end;
    for do_petli_integer:=0 to (Form2.MDIChildren[0] as TForm1).i-1 do
        if (Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].nazwa=text then
        begin
            if minus2=true then
                drugi_argument_porownania:=(-1)*(Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].wartosc
            else
                drugi_argument_porownania:=(Form2.MDIChildren[0] as TForm1).zmienne_integer[do_petli_integer].wartosc;
        end;
    for do_petli_real:=0 to (Form2.MDIChildren[0] as TForm1).r-1 do
        if (Form2.MDIChildren[0] as TForm1).zmienne_real[do_petli_real].nazwa=text then
        begin
            if minus2=true then
                drugi_argument_porownania:=(-1)*(Form2.MDIChildren[0] as TForm1).zmienne_real[do_petli_real].wartosc
            else
                drugi_argument_porownania:=(Form2.MDIChildren[0] as TForm1).zmienne_real[do_petli_real].wartosc;
        end;

    readln(f,text); //czytamy s³owo then
    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;

    //sprawdzamy jaki mamy operator i jeœli operacja logiczna nie zachodzi to czytamy instrukcje do end-a albo do ;
    ilosc_begin:=0;
    if operator_porownania='=' then
    begin
        if not (pierwszy_argument_porownania=drugi_argument_porownania) then
        begin
            readln(f,text);
            ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
            if (text='begin') or (text='for') or (text='if') then    //gdy po then jest begin, for lub if
            begin
                if text='begin' then
                   ilosc_begin:=ilosc_begin+1;
                repeat
                    readln(f,text);
                    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                    if text='begin' then
                        ilosc_begin:=ilosc_begin+1;
                    if text='end' then
                        ilosc_begin:=ilosc_begin-1;
                until (ilosc_begin=0) and ((text='end') or (text=';'));
                if text<>';' then  //czytamy œrednik
                begin
                    readln(f,text);
                    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                end;
           end
           else                   //gdy po then jest instrukcja
                repeat
                     readln(f,text);
                     ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                until text=';';
        end;
    end;
    if operator_porownania='>' then
    begin
        if not (pierwszy_argument_porownania>drugi_argument_porownania) then
        begin
            readln(f,text);
            ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
            if (text='begin') or (text='for') or (text='if') then    //gdy po then jest begin, for lub if
            begin
                 if text='begin' then
                     ilosc_begin:=ilosc_begin+1;
                     repeat
                         readln(f,text);
                         ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                         if text='begin' then
                             ilosc_begin:=ilosc_begin+1;
                         if text='end' then
                             ilosc_begin:=ilosc_begin-1;
                     until (ilosc_begin=0) and ((text='end') or (text=';'));
                     if text<>';' then  //czytamy œrednik
                     begin
                         readln(f,text);
                         ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                     end;
                end
                else                   //gdy po then jest instrukcja
                    repeat
                         readln(f,text);
                         ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                    until text=';';
                end;
            end;
            if operator_porownania='<' then
            begin
                if not (pierwszy_argument_porownania<drugi_argument_porownania) then
                begin
                    readln(f,text);
                    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                    if (text='begin') or (text='for') or (text='if') then    //gdy po then jest begin, for lub if
                    begin
                        if text='begin' then
                            ilosc_begin:=ilosc_begin+1;
                        repeat
                            readln(f,text);
                            ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                            if text='begin' then
                                ilosc_begin:=ilosc_begin+1;
                            if text='end' then
                                ilosc_begin:=ilosc_begin-1;
                         until (ilosc_begin=0) and ((text='end') or (text=';'));
                         if text<>';' then  //czytamy œrednik
                         begin
                             readln(f,text);
                             ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                         end;
                    end
                    else                   //gdy po then jest instrukcja
                         repeat
                              readln(f,text);
                              ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                         until text=';';
                end;
            end;
            if operator_porownania='>=' then
            begin
                if not (pierwszy_argument_porownania>=drugi_argument_porownania) then
                begin
                    readln(f,text);
                    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                    if (text='begin') or (text='for') or (text='if') then    //gdy po then jest begin, for lub if
                    begin
                        if text='begin' then
                            ilosc_begin:=ilosc_begin+1;
                        repeat
                            readln(f,text);
                            ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                            if text='begin' then
                                ilosc_begin:=ilosc_begin+1;
                            if text='end' then
                                ilosc_begin:=ilosc_begin-1;
                        until (ilosc_begin=0) and ((text='end') or (text=';'));
                        if text<>';' then  //czytamy œrednik
                        begin
                            readln(f,text);
                            ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                        end;
                    end
                    else                   //gdy po then jest instrukcja
                        repeat
                             readln(f,text);
                             ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                        until text=';';
                end;
            end;
            if operator_porownania='<=' then
            begin
                if not (pierwszy_argument_porownania<=drugi_argument_porownania) then
                begin
                    readln(f,text);
                    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                    if (text='begin') or (text='for') or (text='if') then    //gdy po then jest begin, for lub if
                    begin
                        if text='begin' then
                            ilosc_begin:=ilosc_begin+1;
                        repeat
                            readln(f,text);
                            ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                            if text='begin' then
                                ilosc_begin:=ilosc_begin+1;
                            if text='end' then
                                ilosc_begin:=ilosc_begin-1;
                        until (ilosc_begin=0) and ((text='end') or (text=';'));
                        if text<>';' then  //czytamy œrednik
                        begin
                            readln(f,text);
                            ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                        end;
                     end
                     else                   //gdy po then jest instrukcja
                        repeat
                             readln(f,text);
                             ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                        until text=';';
                end;
            end;
            if operator_porownania='<>' then
            begin
                if not (pierwszy_argument_porownania<>drugi_argument_porownania) then
                begin
                    readln(f,text);
                    ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                    if (text='begin') or (text='for') or (text='if') then    //gdy po then jest begin, for lub if
                    begin
                        if text='begin' then
                            ilosc_begin:=ilosc_begin+1;
                        repeat
                            readln(f,text);
                            ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                            if text='begin' then
                                ilosc_begin:=ilosc_begin+1;
                            if text='end' then
                                ilosc_begin:=ilosc_begin-1;
                        until (ilosc_begin=0) and ((text='end') or (text=';'));
                        if text<>';' then  //czytamy œrednik
                        begin
                            readln(f,text);
                            ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                        end;
                    end
                    else                   //gdy po then jest instrukcja
                        repeat
                            readln(f,text);
                            ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
                        until text=';';
                end;
            end;
end;


procedure TForm3.FormShow(Sender: TObject);
begin
    koniec_programu:=false;
    ktora_tablica:='zadna';
    dzialanie:='';
    liczba_w_richedit:='';
    indeks_przypisania:=-1;
    zacznij_wykonywac:=false;
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc26.txt');
    reset(f);
    otworzony_plik:=true;
    Timer1.Enabled:=true;
end;

procedure TForm3.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Form3.RichEdit1.Lines.Clear;
    Form3.Caption:='NoName';
    if otworzony_plik then
    begin
       closefile(f);
       otworzony_plik:=false;
       Timer1.Enabled:=False;
    end;
end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
    if not eof(f) then
    begin
       readln(f,instrukcja);
       ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
       if instrukcja='program' then
       begin
           readln(f,instrukcja);
           ilosc_przeczytanych_wierszy:=ilosc_przeczytanych_wierszy+1;
           Form3.Caption:=instrukcja;
       end;
       if instrukcja='begin' then
          zacznij_wykonywac:=true;

       //wykonujemy readln
       if (zacznij_wykonywac=true) and ((instrukcja='readln') or (instrukcja='read')) then
       begin
           read_czy_readln:=instrukcja;
           Timer1.Enabled:=false;
           wykonaj_readln;
           RichEdit1.ReadOnly:=false;
       end
       else
       //wykonujemy write
       if (zacznij_wykonywac=true) and (instrukcja='write') then
          wykonaj_write
       else
       //wykonujemy writeln
       if (zacznij_wykonywac=true) and (instrukcja='writeln') then
          wykonaj_writeln
       else
       //wykonujemy pêtle
       if (zacznij_wykonywac=true) and (instrukcja='for') then
          wykonaj_petle
       else
       //wykonujemy if-y
       if (zacznij_wykonywac=true) and (instrukcja='if') then
          wykonaj_if
       else
       //wykonujemy dzialania matematyczne
       if (zacznij_wykonywac=true) then
          wykonaj_dzialania_matematyczne;
    end
    else
    begin
        closefile(f);
        otworzony_plik:=false;
        Timer1.Enabled:=False;
        koniec_programu:=true;
        Form3.Caption:=Form3.Caption+'   ---program zakoñczy³ dzia³anie---';
    end;
end;

procedure TForm3.RichEdit1KeyPress(Sender: TObject; var Key: Char);
begin
    if (koniec_programu=false) and (RichEdit1.ReadOnly=False) then
    begin
        if (Key<>'-') and (Key<>',') and ((Key<'0') or (Key>'9')) and (Key<>#8) and (Key<>#13) then
           Key:=#0
        else
        begin
           if (Key<>#8) and (Key<>#13) then
              liczba_w_richedit:=liczba_w_richedit+Key;
           if Key=#8 then
              delete(liczba_w_richedit,length(liczba_w_richedit),1);
        end;
        if Key=#13 then
        begin
            try
                if ktora_tablica='tablica_integer' then
                   (Form2.MDIChildren[0] as TForm1).zmienne_integer[indeks_przypisania].wartosc:=round(StrToFloat(liczba_w_richedit));
                if ktora_tablica='tablica_real' then
                   (Form2.MDIChildren[0] as TForm1).zmienne_real[indeks_przypisania].wartosc:=StrToFloat(liczba_w_richedit);
            except
                Showmessage('B³¹d wejœcia');
                Application.Terminate;
            end;
            liczba_w_richedit:='';
            if read_czy_readln='readln' then
                RichEdit1.Lines.Add('');
            RichEdit1.ReadOnly:=true;
            Timer1.Enabled:=True;
        end;
    end;
end;

end.
