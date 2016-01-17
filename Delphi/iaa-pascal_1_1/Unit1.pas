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

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SynEdit, SynEditHighlighter, SynHighlighterPas, SynHighlighterDfm,
  SynHighlighterGeneral, StdCtrls, ExtCtrls, ToolWin, ComCtrls;

type
  //znaki dopuszczalne przy nazwach zmiennych
  Tznaki_dopuszczalne = set of Char;
  //typ przechouj¹cy cyfry
  Tcyfry = set of Char;
  //do deklaracji zmiennych
  Tzmienne_integer = record
                         wartosc:integer;
                         nazwa:string[255];
                     end;
  Tzmienne_real =  record
                       wartosc:double;
                       nazwa:string[255];
                   end;
  Tzmienne_tymczasowe =  record
                             nazwa:string[255];
                         end;
  TForm1 = class(TForm)
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    Memo1: TMemo;
    Splitter1: TSplitter;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ok:boolean; //zmienna której wartoœæ mówi czy jest wszystko ok
    znaki_dopuszczalne:Tznaki_dopuszczalne;
    cyfry:Tcyfry;
    //tablice dynamiczne do deklaracji zmiennych
    zmienne_integer:array of Tzmienne_integer;
    zmienne_real:array of Tzmienne_real;
    zmienne_tymczasowe:array of Tzmienne_tymczasowe;
    nie_mozna_takich_zmiennych:array of string[255];
    i,r:integer; //aktualne wielkoœci tablic dynamicznych
    procedure usun_plik(plik:string);
    procedure rozdziel_wg_srednikow(SynEdit1:TSynEdit);
    procedure usuniecie_komentarzy;
    procedure zamien_na_male_litery;
    procedure rozdzielenie_wg_slow_kluczowych;
    procedure usuniecie_spacji;
    procedure dodanie_opisu;
    procedure rozdzielenie_wg_znakow_matematycznych;
    procedure usun_instrukcje_puste;
    procedure usun_wszystko_po_koncu_programu;
    procedure sprawdz_program;
    procedure sprawdz_poczatek_programu;
    procedure sprawdz_var;
    procedure sprawdz_deklaracje_zmiennych;
    procedure sprawdz_begin_i_end;
    procedure sprawdz_sredniki_po_end;
    procedure sprawdz_ilosc_begin_i_end;
    procedure sprawdz_if;
    procedure sprawdz_ilosc_if_then;
    procedure sprawdz_petle;
    procedure sprawdz_zmienne_w_petli;
    procedure sprawdz_ilosc_for_to;
    procedure sprawdz_ilosc_for_do;
    procedure sprawdz_dzialania_arytmetyczne;
    procedure sprawdz_koniec_programu;
    procedure deklaruj_zmienne;
    procedure sprawdz_czy_zadeklarowano_zmiene;
    procedure sprawdz_sredniki_przed_i_po_oraz_poprwnosc_read_write;
    procedure sprawdz_sredniki_przed_if_i_for;
    procedure sprawdz_sredniki_przed_operacja_arytmetyczna;
    procedure sprawdz_czy_znaki_arytmetyczne_sa_bez_przypisania;
    procedure sprawdz_czy_operatory_porownania_sa_poza_if_then;
    procedure sprawdz_czy_nie_ma_pojedynczych_zmiennych;
    function sprawdz_poprawnosc_zmiennej(zmienna:string):integer;
    procedure przygotuj_do_uruchomienia;
  end;


var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.DFM}

procedure wyczysc_plik(plik:string);
var
    f:TextFile;
begin
    assignfile(f,plik);
    rewrite(f);
    closefile(f);
end;

procedure TForm1.usun_plik(plik:string);
var
    f:TextFile;
begin
    assignfile(f,plik);
    if FileExists(plik) then
       erase(f);
end;

procedure zapisz_do_pliku(plik,text:string);
var
    f:TextFile;
begin
    assignfile(f,plik);
    if not FileExists(plik) then
       rewrite(f);
    append(f);
    writeln(f,text);
    closefile(f);
end;

procedure rozdziel_wg_znaku(text:string; znak:string; dlugosc:integer; plik: string; czy_rozdzielac:boolean);
var
    poczatek_stringa,koniec_stringa,pozycja_znaku:integer;
    kopia_textu:string;
    zapamietaj:boolean;
begin
    poczatek_stringa:=0;
    koniec_stringa:=0;
    pozycja_znaku:=0;
    kopia_textu:=text;
    zapamietaj:=false;
    if czy_rozdzielac=true then
    repeat
        if zapamietaj=false then
        begin
            poczatek_stringa:=pos('''',kopia_textu);
            if poczatek_stringa>0 then
            begin
                delete(kopia_textu,poczatek_stringa,1);
                insert('#',kopia_textu,poczatek_stringa);
                koniec_stringa:=pos('''',kopia_textu);
                zapamietaj:=true;
            end;
        end;
        pozycja_znaku:=pos(znak,kopia_textu);
        if (pozycja_znaku>poczatek_stringa) and (pozycja_znaku<koniec_stringa) then
        begin
            delete(kopia_textu,pozycja_znaku,dlugosc);
            if dlugosc=2 then
               insert('##',kopia_textu,pozycja_znaku);
            if dlugosc=1 then
               insert('#',kopia_textu,pozycja_znaku);
        end
        else
        begin
            if copy(text,1,pozycja_znaku-1)<>'' then
            begin
               zapisz_do_pliku(plik,copy(text,1,pozycja_znaku-1));
               zapisz_do_pliku(plik,znak);
               if dlugosc=1 then
                   delete(text,1,pozycja_znaku);
               if dlugosc=2 then
                   delete(text,1,pozycja_znaku+1);
               kopia_textu:=text;
            end
            else
               if (copy(text,1,pozycja_znaku-1)='') and (pozycja_znaku=1) then
               begin
                   zapisz_do_pliku(plik,znak);
                   delete(text,1,dlugosc);
                   kopia_textu:=text;
               end
               else
                   pozycja_znaku:=0;
        end;
    until pozycja_znaku=0;
    if text<>'' then
       zapisz_do_pliku(plik,text);
end;

procedure TForm1.rozdziel_wg_srednikow(SynEdit1:TSynEdit);
var
    i,j,dl:integer;
    text,instrukcja:string;
    lancuch:boolean;
begin
    wyczysc_plik(ExtractFilePath(Application.ExeName)+'\pomoc0.txt');
    lancuch:=false;
    for i:=0 to SynEdit1.Lines.Count-1 do
    begin
        text:=SynEdit1.Lines[i];
        dl:=length(text);
        j:=1;
        zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'\pomoc0.txt','###linia'+IntToStr(i));
        while j<=dl do
        begin
            if (text[j]='''') then
                lancuch:= not lancuch;
            if (text[j]=';') and (lancuch=false) then
            begin
                instrukcja:=copy(text,1,j);
                zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'\pomoc0.txt',instrukcja);
                delete(text,1,j);
                j:=1;
                dl:=length(text);
            end
            else
            begin
                if (j=dl) and (text[dl]<>';') then
                begin
                   zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'\pomoc0.txt',text);
                   delete(text,1,length(text));
                end;
                j:=j+1;
            end;    
        end;
        if text<>'' then
           zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'\pomoc0.txt',text);
        if lancuch=true then
           lancuch:=false;
    end;
end;

procedure TForm1.usuniecie_komentarzy;
var
    text,instrukcja:string;
    pozycja_poczatku_komentarza,pozycja_konca_komentarza,linia:integer;
    komentarz:boolean;
    f:TextFile;
begin
    wyczysc_plik(ExtractFilePath(Application.ExeName)+'\pomoc2.txt');
    komentarz:=false;
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc0.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        pozycja_poczatku_komentarza:=pos('{',text);
        pozycja_konca_komentarza:=pos('}',text);
        if (pozycja_poczatku_komentarza>0) and (komentarz=false) then
        begin
            komentarz:=true;
            instrukcja:=copy(text,1,pozycja_poczatku_komentarza-1);
            zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'\pomoc2.txt',instrukcja);
        end;
        if (pozycja_konca_komentarza>0) and (komentarz=true) then
        begin
            komentarz:=false;
            instrukcja:=copy(text,pozycja_konca_komentarza+1,length(text)-pozycja_konca_komentarza);
            zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'\pomoc2.txt',instrukcja);
        end;
        if (komentarz=false) and (pozycja_konca_komentarza=0) then
            zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'\pomoc2.txt',text);
        if (komentarz=false) and (pozycja_konca_komentarza>0) then
            zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'\pomoc2.txt',copy(text,pozycja_konca_komentarza,length(text)-pozycja_konca_komentarza-1));
        if (komentarz=true) and (pos('###linia',text)>0) then
            zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'\pomoc2.txt',text);
    end;
    closefile(f);
end;

procedure TForm1.zamien_na_male_litery;
var
    f:TextFile;
    text:string;
begin
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc2.txt');
    reset(f);
    while not eof (f) do
    begin
        readln(f,text);
        if text<>'' then
           if text[1]<>'''' then
              text:=LowerCase(text);
        if text<>'' then
           zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc3.txt',text);
    end;
    closefile(f);
end;

procedure TForm1.rozdzielenie_wg_slow_kluczowych;
var
    text,instrukcja,znak:string;
    f:TextFile;
    i,dl,poczatek,pomoc,pozycja_writeln,pozycja_write:integer;
    lancuch:boolean;
begin
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc3.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        dl:=length(text);
        i:=1;
        poczatek:=1;
        lancuch:=false;
        while i<dl do
        begin
            pomoc:=0;
            instrukcja:=copy(text,i,5);
            if instrukcja='write' then
                pozycja_write:=i;
            instrukcja:=copy(text,i,7);
            if instrukcja='writeln' then
                pozycja_writeln:=i;
            znak:=copy(text,i,1);
            if znak='''' then
               if ((pozycja_write<i) or (pozycja_writeln<i)) and ((pozycja_write<>0) or (pozycja_writeln<>0)) then
                  lancuch:=not lancuch;

            instrukcja:=copy(text,i,7);
            if   ((instrukcja='program') and not(text[i+7] in znaki_dopuszczalne) and not(text[i-1] in znaki_dopuszczalne) and (lancuch=false))
               or((instrukcja='program') and (i=1) and not(text[i+7] in znaki_dopuszczalne) and (lancuch=false))
               or((instrukcja='program') and (i=1) and (length(text)=7))
            then
            begin
                zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc4.txt',copy(text,poczatek,i-poczatek+8));
                poczatek:=i+7;
                pomoc:=7;
            end;
            instrukcja:=copy(text,i,4);
            if   ((instrukcja='then') and not(text[i-1] in znaki_dopuszczalne) and not(text[i+4] in znaki_dopuszczalne) and (lancuch=false))
               or((instrukcja='then') and (i=1) and not(text[i+4] in znaki_dopuszczalne) and (lancuch=false))
               or((instrukcja='then') and (i=1) and (length(text)=4))
            then
            begin
                zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc4.txt',copy(text,poczatek,i-poczatek));
                poczatek:=i;
                zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc4.txt',copy(text,poczatek,i-poczatek+5));
                poczatek:=i+4;
                pomoc:=4;
            end;
            instrukcja:=copy(text,i,3);
            if   ((instrukcja='var') and not(text[i+3] in znaki_dopuszczalne) and not(text[i-1] in znaki_dopuszczalne) and (lancuch=false))
               or((instrukcja='var') and (i=1) and not(text[i+3] in znaki_dopuszczalne) and (lancuch=false))
               or((instrukcja='var') and (i=1) and (length(text)=3))
            then
            begin
                zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc4.txt',copy(text,poczatek,i-poczatek+4));
                poczatek:=i+3;
                pomoc:=3;
            end;
            instrukcja:=copy(text,i,3);
            if   ((instrukcja='for') and not(text[i+3] in znaki_dopuszczalne) and not(text[i-1] in znaki_dopuszczalne) and (lancuch=false))
               or((instrukcja='for') and (i=1) and not(text[i+3] in znaki_dopuszczalne) and (lancuch=false))
               or((instrukcja='for') and (i=1) and (length(text)=3))
            then
            begin
                zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc4.txt',copy(text,poczatek,i-poczatek+4));
                poczatek:=i+3;
                pomoc:=3;
            end;
            instrukcja:=copy(text,i,2);
            if   ((instrukcja='to') and not(text[i-1] in znaki_dopuszczalne) and not(text[i+2] in znaki_dopuszczalne) and (lancuch=false))
               or((instrukcja='to') and (i=1) and not(text[i+2] in znaki_dopuszczalne) and (lancuch=false))
               or((instrukcja='to') and (i=1) and (length(text)=2))
            then
            begin
                zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc4.txt',copy(text,poczatek,i-poczatek));
                poczatek:=i;
                zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc4.txt',copy(text,poczatek,i-poczatek+3));
                poczatek:=i+2;
                pomoc:=2;
            end;
            instrukcja:=copy(text,i,2);
            if   ((instrukcja='do') and not(text[i-1] in znaki_dopuszczalne) and not(text[i+2] in znaki_dopuszczalne) and (lancuch=false))
               or((instrukcja='do') and (i=1) and not(text[i+2] in znaki_dopuszczalne) and (lancuch=false))
               or((instrukcja='do') and (i=1) and (length(text)=2))
            then
            begin
                zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc4.txt',copy(text,poczatek,i-poczatek));
                poczatek:=i;
                zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc4.txt',copy(text,poczatek,i-poczatek+3));
                poczatek:=i+2;
                pomoc:=2;
            end;
            instrukcja:=copy(text,i,5);
            if   ((instrukcja='begin') and not(text[i+5] in znaki_dopuszczalne) and (lancuch=false) and (i=1))
               or((instrukcja='begin') and not(text[i-1] in znaki_dopuszczalne) and (lancuch=false) and (i+5=dl))
               or((instrukcja='begin') and not(text[i-1] in znaki_dopuszczalne) and not(text[i+5] in znaki_dopuszczalne) and (lancuch=false))
            then
            begin
                zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc4.txt',copy(text,poczatek,i-poczatek+6));
                poczatek:=i+5;
                pomoc:=5;
            end;
            instrukcja:=copy(text,i,3);
            if   ((instrukcja='end') and not((text[i+3] in znaki_dopuszczalne) or (text[i+3]='.')) and (lancuch=false) and (i=1))
               or((instrukcja='end') and not(text[i-1] in znaki_dopuszczalne) and (lancuch=false) and (not(text[i+3] in znaki_dopuszczalne)or(text[i+3]='.')))
               or((instrukcja='end') and not(text[i-1] in znaki_dopuszczalne) and not(text[i+3] in znaki_dopuszczalne) and (lancuch=false))
            then
            begin
                zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc4.txt',copy(text,poczatek,i-poczatek+4));
                poczatek:=i+3;
                pomoc:=3;
            end;
            instrukcja:=copy(text,i,2);
            if   ((instrukcja='if') and not(text[i-1] in znaki_dopuszczalne) and not(text[i+2] in znaki_dopuszczalne) and (lancuch=false))
               or((instrukcja='if') and (i=1) and not(text[i+2] in znaki_dopuszczalne) and (lancuch=false))
               or((instrukcja='if') and (i=1) and (length(text)=2))
            then
            begin
                zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc4.txt',copy(text,poczatek,i-poczatek+3));
                poczatek:=i+2;
                pomoc:=2;
            end;
            i:=i+pomoc+1;
        end;
        zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc4.txt',copy(text,poczatek,dl-poczatek+1));
    end;
    closefile(f);
end;

procedure TForm1.usuniecie_spacji;
var
    text,kopia_textu:string;
    f:TextFile;
    poczatek_stringa,koniec_stringa,pozycja_spacji,pozycja_write,pozycja_writeln,pozycja_tabulatora:integer;
begin
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc4.txt');
    reset(f);
    while not eof(f) do
    begin
        poczatek_stringa:=0;
        koniec_stringa:=0;
        pozycja_spacji:=0;
        readln(f,text);
        kopia_textu:=text;
        pozycja_write:=pos('write(',kopia_textu);
        pozycja_writeln:=pos('writeln(',kopia_textu);
        if ((pozycja_write<>0) or (pozycja_writeln<>0)) then
        begin
            poczatek_stringa:=pos('''',kopia_textu);
            if (poczatek_stringa<pozycja_write) or (poczatek_stringa<pozycja_writeln) then
                poczatek_stringa:=0;
            if poczatek_stringa>0 then
            begin
               delete(kopia_textu,poczatek_stringa,1);
               insert('#',kopia_textu,poczatek_stringa);
               koniec_stringa:=pos('''',kopia_textu);
               if (koniec_stringa<pozycja_write) or (koniec_stringa<pozycja_writeln) then
                   koniec_stringa:=0;
            end;
        end;

        repeat
            pozycja_tabulatora:=pos('	',kopia_textu);
            if (pozycja_tabulatora>poczatek_stringa) and (pozycja_tabulatora<koniec_stringa)  then
            begin
                delete(kopia_textu,pozycja_tabulatora,1);
                insert('#',kopia_textu,pozycja_tabulatora);
            end
            else
            begin
                if (pozycja_tabulatora<length(kopia_textu)) and (kopia_textu[pozycja_tabulatora-1] in znaki_dopuszczalne) and (kopia_textu[pozycja_tabulatora+1] in znaki_dopuszczalne) then
                begin
                    delete(kopia_textu,pozycja_tabulatora,1);
                    insert('#',kopia_textu,pozycja_tabulatora);
                end
                else
                begin
                    delete(kopia_textu,pozycja_tabulatora,1);
                    delete(text,pozycja_tabulatora,1);
                    pozycja_write:=pozycja_write-1;
                    pozycja_writeln:=pozycja_writeln-1;
                    poczatek_stringa:=poczatek_stringa-1;
                    koniec_stringa:=koniec_stringa-1;
                end;
            end;
        until pozycja_tabulatora=0;

        repeat
            pozycja_spacji:=pos(' ',kopia_textu);
            if (pozycja_spacji>poczatek_stringa) and (pozycja_spacji<koniec_stringa)  then
            begin
                delete(kopia_textu,pozycja_spacji,1);
                insert('#',kopia_textu,pozycja_spacji);
            end
            else
            begin
                if (pozycja_spacji<length(kopia_textu)) and (kopia_textu[pozycja_spacji-1] in znaki_dopuszczalne) and (kopia_textu[pozycja_spacji+1] in znaki_dopuszczalne) then
                begin
                    delete(kopia_textu,pozycja_spacji,1);
                    insert('#',kopia_textu,pozycja_spacji);
                end
                else
                begin
                    delete(kopia_textu,pozycja_spacji,1);
                    delete(text,pozycja_spacji,1);
                    pozycja_write:=pozycja_write-1;
                    pozycja_writeln:=pozycja_writeln-1;
                    poczatek_stringa:=poczatek_stringa-1;
                    koniec_stringa:=koniec_stringa-1;
                end;
            end;
        until pozycja_spacji=0;

        if (text<>'') then
        begin
            if text[length(text)]=';' then
            begin
                zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc5.txt',copy(text,1,length(text)-1));
                zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc5.txt',';');
            end
            else
                zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc5.txt',text);
        end;
    end;
    closefile(f);
end;

procedure TForm1.rozdzielenie_wg_znakow_matematycznych;
var
    text:string;
    f:TextFile;
    wystapilo_begin:boolean;
begin
    wystapilo_begin:=false;
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc5.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        rozdziel_wg_znaku(text,':=',2,ExtractFilePath(Application.ExeName)+'pomoc6.txt',true);
    end;
    closefile(f);
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc6.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        if text='begin' then
            wystapilo_begin:=true;
        if wystapilo_begin=false then
            rozdziel_wg_znaku(text,',',1,ExtractFilePath(Application.ExeName)+'pomoc7.txt',true)
        else
            rozdziel_wg_znaku(text,',',1,ExtractFilePath(Application.ExeName)+'pomoc7.txt',false);
    end;
    closefile(f);
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc7.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        if (text<>'<>') and (text<>'>=') and (text<>'<=') and (text<>':=') then
           rozdziel_wg_znaku(text,':',1,ExtractFilePath(Application.ExeName)+'pomoc8.txt',true)
        else
           zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc8.txt',text);
    end;
    closefile(f);
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc8.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        rozdziel_wg_znaku(text,'<>',2,ExtractFilePath(Application.ExeName)+'pomoc9.txt',true);
    end;
    closefile(f);
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc9.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        rozdziel_wg_znaku(text,'<=',2,ExtractFilePath(Application.ExeName)+'pomoc10.txt',true);
    end;
    closefile(f);
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc10.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        rozdziel_wg_znaku(text,'>=',2,ExtractFilePath(Application.ExeName)+'pomoc11.txt',true);
    end;
    closefile(f);
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc11.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        if (text<>'<>') and (text<>'>=') and (text<>'<=') and (text<>':=') then
           rozdziel_wg_znaku(text,'<',1,ExtractFilePath(Application.ExeName)+'pomoc12.txt',true)
        else
           zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc12.txt',text);
    end;
    closefile(f);
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc12.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        if (text<>'<>') and (text<>'>=') and (text<>'<=') and (text<>':=') then
           rozdziel_wg_znaku(text,'>',1,ExtractFilePath(Application.ExeName)+'pomoc13.txt',true)
        else
           zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc13.txt',text);
    end;
    closefile(f);
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc13.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        if (text<>'<>') and (text<>'>=') and (text<>'<=') and (text<>':=') then
           rozdziel_wg_znaku(text,'=',1,ExtractFilePath(Application.ExeName)+'pomoc14.txt',true)
        else
           zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc14.txt',text);
    end;
    closefile(f);
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc14.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        rozdziel_wg_znaku(text,'(',1,ExtractFilePath(Application.ExeName)+'pomoc15.txt',true);
    end;
    closefile(f);
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc15.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        rozdziel_wg_znaku(text,'(',1,ExtractFilePath(Application.ExeName)+'pomoc16.txt',true);
    end;
    closefile(f);
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc16.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        rozdziel_wg_znaku(text,')',1,ExtractFilePath(Application.ExeName)+'pomoc17.txt',true);
    end;
    closefile(f);
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc17.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        rozdziel_wg_znaku(text,'+',1,ExtractFilePath(Application.ExeName)+'pomoc18.txt',true);
    end;
    closefile(f);
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc18.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        rozdziel_wg_znaku(text,'-',1,ExtractFilePath(Application.ExeName)+'pomoc19.txt',true);
    end;
    closefile(f);
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc19.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        rozdziel_wg_znaku(text,'*',1,ExtractFilePath(Application.ExeName)+'pomoc20.txt',true);
    end;
    closefile(f);
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc20.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        rozdziel_wg_znaku(text,'/',1,ExtractFilePath(Application.ExeName)+'pomoc21.txt',true);
    end;
    closefile(f);
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc21.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        rozdziel_wg_znaku(text,'^',1,ExtractFilePath(Application.ExeName)+'pomoc22.txt',true);
    end;
    closefile(f);
end;

procedure TForm1.dodanie_opisu;
var
    f:TextFile;
    text:string;
begin
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc22.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        if text='program' then
            zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc23.txt','[###pocz¹tek programu###]');
        if text='var' then
            zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc23.txt','[###deklaracja zmiennych###]');
        if text='begin' then
            zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc23.txt','[###pocz¹tek bloku###]');
        if text='end' then
            zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc23.txt','[###koniec bloku###]');
        if text='if' then
            zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc23.txt','[###instrukcja warunkowa###]');
        if text='for' then
            zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc23.txt','[###pêtla###]');
        if text='.' then
            zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc23.txt','[###koniec programu###]');
        if pos(':=',text)>0 then
            zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc23.txt','[###operacja przypisania###]');
        zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc23.txt',text);
    end;
    closefile(f);
end;

procedure TForm1.usun_instrukcje_puste;
var
    f:TextFile;
    text,text1:string;
begin
    text:='';
    text1:='';
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc23.txt');
    reset(f);
    if not eof(f) then
    begin
       readln(f,text);
       zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc24.txt',text);
    end;
    while not eof(f) do
    begin
        text1:=text;
        readln(f,text);
        if (text<>';') or (text1<>';') then
           zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc24.txt',text);
    end;
    closefile(f);
end;

procedure TForm1.usun_wszystko_po_koncu_programu;
var
    f:TextFile;
    text:string;
    koniec_programu:boolean;
begin
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc24.txt');
    reset(f);
    koniec_programu:=false;
    while not eof(f) do
    begin
        readln(f,text);
        if koniec_programu=false then
           zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc25.txt',text);
        if (text='[###koniec programu###]') then
           koniec_programu:=true;
        if text='end.' then
        begin
           koniec_programu:=true;
           zapisz_do_pliku(ExtractFilePath(Application.ExeName)+'pomoc25.txt','[###koniec programu###]');
        end;
    end;
    closefile(f);
end;

function TForm1.sprawdz_poprawnosc_zmiennej(zmienna:string):integer;
var
    kod_bledu,i:integer;
begin
    kod_bledu:=0;
    if (zmienna='begin') or (zmienna='end') or (zmienna='program') or (zmienna='if') or (zmienna='then') or (zmienna='for') or (zmienna='do') or (zmienna='var') or (zmienna='readln') or (zmienna='read') or (zmienna='writeln') or (zmienna='write') then
        kod_bledu:=3;
    if (zmienna[1] in cyfry) then
        kod_bledu:=1
    else
    begin
        for i:=1 to length(zmienna) do
           if not (zmienna[i] in znaki_dopuszczalne) then
              kod_bledu:=2;
    end;
    result:=kod_bledu;
end;

procedure TForm1.sprawdz_poczatek_programu;
var
    f:TextFile;
    text:string;
    linia:integer;
    czy_linia, koniec_sprawdzania:boolean;
begin
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    czy_linia:=false;
    koniec_sprawdzania:=false;
    while (not eof (f)) and (not koniec_sprawdzania) do
    begin
        readln(f,text);
        if pos('###linia',text)=1 then
        begin
           linia:=StrToInt(copy(text,9,length(text)-8));
           czy_linia:=true;
        end
        else
           czy_linia:=false;
        if czy_linia=false then
        begin
            if text='[###deklaracja zmiennych###]' then
               koniec_sprawdzania:=true
            else if text='[###pocz¹tek programu###]' then
                    koniec_sprawdzania:=true
                 else if text='[###pocz¹tek bloku###]' then
                         koniec_sprawdzania:=true
                      else
                      begin
                         ok:=false;
                         koniec_sprawdzania:=true;
                         Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Brak pocz¹tku programu');
                      end;
        end;
    end;
    closefile(f);
end;

procedure TForm1.sprawdz_program;
var
    f:TextFile;
    text:string;
    linia,kod_bledu:integer;
begin
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    while not eof (f) do
    begin
        readln(f,text);
        if text='[###pocz¹tek programu###]' then
        begin
            try
            readln(f,text);
            if text='program' then
            begin
                readln(f,text);
                if pos('###linia',text)=1 then
                repeat
                   linia:=StrToInt(copy(text,9,length(text)-8));
                   readln(f,text);
                until pos('###linia',text)=0;
                kod_bledu:=sprawdz_poprawnosc_zmiennej(text);
                if kod_bledu<>0 then
                begin
                    ok:=false;
                    if kod_bledu=1 then
                       Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Na pocz¹tku nazwy programu nie mo¿e byæ cyfry');
                    if kod_bledu=2 then
                       Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Niedopuszczalny znak w nazwie programu');
                    if kod_bledu=3 then
                       Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Nazw¹ programu jest s³owo kluczowe - tak byæ nie mo¿e');
                end
                else
                begin
                   readln(f,text);
                   if pos('###linia',text)=1 then
                   repeat
                       linia:=StrToInt(copy(text,9,length(text)-8));
                       readln(f,text);
                   until pos('###linia',text)=0;
                   if text<>';' then
                   begin
                       ok:=false;
                       Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Brak œrednika ";"');
                   end;
                end;
            end
            else
            begin
                ok:=false;
                Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Spodziewano siê s³owa kluczowego "program" - brak tego s³owa');
            end;
            except
                ok:=false;
                Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Nieoczekiwany koniec programu');
            end;
        end;
        if pos('###linia',text)=1 then
           linia:=StrToInt(copy(text,9,length(text)-8));
    end;
    closefile(f);
end;

procedure TForm1.sprawdz_var;
var
    f:TextFile;
    text:string;
    linia:integer;
    czy_linia,nie_mozna_var:boolean;
begin
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    czy_linia:=false;
    nie_mozna_var:=false;
    while not eof (f) do
    begin
        try
        readln(f,text);
        if pos('###linia',text)=1 then
        begin
            linia:=StrToInt(copy(text,9,length(text)-8));
            czy_linia:=true;
        end
        else
            czy_linia:=false;
        if czy_linia=false then
        begin
            if text='[###pocz¹tek bloku###]' then
                nie_mozna_var:=true;
            if (nie_mozna_var=true) and (text='var') then
            begin
                ok:=false;
                Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : S³owo kluczowe "var" nie mo¿e byæ w tym miejscu u¿yte');
            end;
            if (text='[###pocz¹tek programu###]') then
            begin
                //czyta s³owo program
                readln(f,text);
                if pos('###linia',text)=1 then
                repeat
                    linia:=StrToInt(copy(text,9,length(text)-8));
                    readln(f,text);
                until pos('###linia',text)=0;
                //czyta nazwe programu
                readln(f,text);
                if pos('###linia',text)=1 then
                repeat
                    linia:=StrToInt(copy(text,9,length(text)-8));
                    readln(f,text);
                until pos('###linia',text)=0;
                //czyta œrednik
                readln(f,text);
                if pos('###linia',text)=1 then
                repeat
                    linia:=StrToInt(copy(text,9,length(text)-8));
                    readln(f,text);
                until pos('###linia',text)=0;
                //czyta kolejn¹ liniê
                readln(f,text);
                if pos('###linia',text)=1 then
                repeat
                    linia:=StrToInt(copy(text,9,length(text)-8));
                    readln(f,text);
                until pos('###linia',text)=0;
                if (text<>'[###pocz¹tek bloku###]') and (text<>'[###deklaracja zmiennych###]') then
                begin
                    ok:=false;
                    Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Oczekiwane s³owo kluczowe "var" lub "begin" - brakuje tych s³ów - zamiast nich jest - '+text);
                end;
            end;
        end;
        except
            ok:=false;
            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Nieoczekiwany koniec programu');
        end;
    end;
    closefile(f);
end;

procedure TForm1.sprawdz_deklaracje_zmiennych;
var
    f:TextFile;
    text:string;
    linia,kod_bledu:integer;
    koniec_sprawdzania,zmienna:boolean;
begin
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    zmienna:=false;
    while (not eof (f)) and (koniec_sprawdzania=false) do
    begin
        try
        //czyta [###deklaracja zmiennych###]
        if zmienna=false then
        begin
            readln(f,text);
            if pos('###linia',text)=1 then
            repeat
                linia:=StrToInt(copy(text,9,length(text)-8));
                readln(f,text);
            until pos('###linia',text)=0;
        end;
        if (text='[###deklaracja zmiennych###]') or (zmienna=true) then
        begin
            //przeczyta var lub begin
            if zmienna=false then
            begin
               readln(f,text);
               if pos('###linia',text)=1 then
               repeat
                   linia:=StrToInt(copy(text,9,length(text)-8));
                   readln(f,text);
               until pos('###linia',text)=0;
               zmienna:=true;
               if text='[###pocz¹tek bloku###]' then
               begin
                   koniec_sprawdzania:=true;
                   zmienna:=false;
               end;
            end;
            //przeczyta nazwe zmiennej
            if (zmienna=true) and (koniec_sprawdzania=false) then
            begin
               readln(f,text);
               if pos('###linia',text)=1 then
               repeat
                   linia:=StrToInt(copy(text,9,length(text)-8));
                   readln(f,text);
               until pos('###linia',text)=0;
               //to jest po to jakby by³o drugie wyst¹pienie s³owa var w sekcji dekaratywnej
               if text='[###deklaracja zmiennych###]' then
               begin
                   readln(f,text);
                   if pos('###linia',text)=1 then
                   repeat
                       linia:=StrToInt(copy(text,9,length(text)-8));
                       readln(f,text);
                   until pos('###linia',text)=0;
                   readln(f,text);
                   if pos('###linia',text)=1 then
                   repeat
                       linia:=StrToInt(copy(text,9,length(text)-8));
                       readln(f,text);
                   until pos('###linia',text)=0;
               end;
               if text='[###pocz¹tek bloku###]' then
                   koniec_sprawdzania:=true;
               if koniec_sprawdzania=false then
               begin
                  kod_bledu:=sprawdz_poprawnosc_zmiennej(text);
                  if kod_bledu<>0 then
                  begin
                      ok:=false;
                      koniec_sprawdzania:=true;
                      if kod_bledu=1 then
                         Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Na pocz¹tku nazwy zmiennej nie mo¿e byæ cyfry');
                      if kod_bledu=2 then
                         Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Niedopuszczalny znak w nazwie zmiennej');
                      if kod_bledu=3 then
                         Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Nazw¹ zmiennej jest s³owo zastrze¿one - tak byæ nie mo¿e');
                  end;
                  //teraz przeczyta albo dwukropek, albo przecinek
                  if koniec_sprawdzania=false then
                  begin
                      readln(f,text);
                      if pos('###linia',text)=1 then
                      repeat
                          linia:=StrToInt(copy(text,9,length(text)-8));
                          readln(f,text);
                      until pos('###linia',text)=0;
                      if (text<>':') and (text<>',') then
                      begin
                          ok:=false;
                          koniec_sprawdzania:=true;
                          Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Oczekiwano znaku dwukropka ":" lub przecinka "," - brak tych znaków');
                      end
                      else
                      begin
                          //jeden ze znaków
                          if text=':' then
                          begin
                             //czytam typ
                             zmienna:=false;
                             readln(f,text);
                             if pos('###linia',text)=1 then
                             repeat
                                 linia:=StrToInt(copy(text,9,length(text)-8));
                                 readln(f,text);
                             until pos('###linia',text)=0;
                             if (text<>'integer') and (text<>'double') then
                             begin
                                 ok:=false;
                                 koniec_sprawdzania:=true;
                                 Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Nierozpoznawalny typ zmiennej lub brak œrednika');
                             end
                             else//czytamy œrednik
                             begin
                                 readln(f,text);
                                 if pos('###linia',text)=1 then
                                 repeat
                                     linia:=StrToInt(copy(text,9,length(text)-8));
                                     readln(f,text);
                                 until pos('###linia',text)=0;
                                 if (text<>';') then
                                 begin
                                     ok:=false;
                                     koniec_sprawdzania:=true;
                                     Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Brak œrednika - zamiast niego jest : '+text);
                                 end
                                 else
                                     zmienna:=true;
                             end;
                          end
                          else//jeœli jest to przecinek
                             zmienna:=true
                      end;
                  end;
               end;
            end;
        end;
        except
            ok:=false;
            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Nieoczekiwany koniec programu');
        end;
    end;
    closefile(f);
end;

//powinno byæ ok - ale trzeba sprawdziæ warunki
procedure TForm1.sprawdz_begin_i_end;
var
    f:TextFile;
    text1,text2,text3:string;
    linia,kod_bledu1,kod_bledu3:integer;
    koniec_sprawdzania:boolean;
begin
    text1:='';
    text2:='';
    text3:='';
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    kod_bledu1:=0;
    kod_bledu3:=0;
    koniec_sprawdzania:=false;
    while (not eof(f)) and (koniec_sprawdzania=false) do
    begin
        try
        if (text3<>'[###pocz¹tek bloku###]') and (text3<>'[###koniec bloku###]') and (text3<>'[###koniec programu###]') and (pos('###linia',text3)=0) then
        begin
            text1:=text2;
            text2:=text3;
        end;
        readln(f,text3);
        if pos('###linia',text3)=1 then
        repeat
            linia:=StrToInt(copy(text3,9,length(text3)-8));
            if (text3<>'[###pocz¹tek bloku###]') and (text3<>'[###koniec bloku###]') and (text3<>'[###koniec programu###]') and (pos('###linia',text3)=0) then
            begin
               text1:=text2;
               text2:=text3;
            end;
            readln(f,text3);
        until pos('###linia',text3)=0;
        if text1<>'' then
           kod_bledu1:=sprawdz_poprawnosc_zmiennej(text1);
        if text3<>'' then
           kod_bledu3:=sprawdz_poprawnosc_zmiennej(text3);
        if (text2='begin') and (kod_bledu1<>0) and (text1<>';') and (text1<>'begin') and (text1<>'then') and (text1<>'end') and (text1<>'do') then
        begin
            ok:=false;
            koniec_sprawdzania:=true;
            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Niedozwolony znak lub s³owo przed s³owem "begin" lub brak œrednika');
        end;
        if (text2='begin') and (kod_bledu3<>0) and (text3<>'[###pocz¹tek bloku###]') and (text3<>'[###pêtla###]') and (text3<>'[###koniec bloku###]')  and (text3<>'end.') and (text3<>'[###instrukcja warunkowa###]') and (pos('###linia',text3)=0) and (text3<>'begin') and (text3<>'if') and (text3<>'for') and (text3<>'end') and (text3<>'writeln') and (text3<>'write') and (text3<>'read') and (text3<>'readln') then
        begin
            ok:=false;
            koniec_sprawdzania:=true;
            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Niedozwolony znak lub s³owo po s³owie "begin"');
        end;
        if ((text2='end') or (text2='end.')) and (kod_bledu1<>0) and (text1<>';') and (text1<>'begin') and (text1<>'if') and (text1<>'for') and (text1<>'end') then
        begin
            ok:=false;
            koniec_sprawdzania:=true;
            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Niedozwolony znak lub s³owo przed s³owem "end" lub brak œrednika');
        end;
        if ((text2='end') or (text2='end.')) and (kod_bledu3<>0) and (text3<>';') and (text3<>'.') and (text3<>'[###koniec programu###]')  and (text3<>'[###pocz¹tek bloku###]') and (text3<>'[###pêtla###]') and (text3<>'[###koniec bloku###]') and (text3<>'[###instrukcja warunkowa###]') and (pos('###linia',text3)=0) and (text3<>'begin') and (text3<>'if') and (text3<>'for') and (text3<>'end')  then
        begin
            ok:=false;
            koniec_sprawdzania:=true;
            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Niedozwolony znak lub niedozwolone s³owo po s³owie "end"');
        end;
        except
            ok:=false;
            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Nieoczekiwany koniec programu');
        end;
    end;
    closefile(f);
end;

procedure TForm1.sprawdz_sredniki_po_end;
var
    f:TextFile;
    text:string;
    koniec_sprawdzania:boolean;
    linia:integer;
begin
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    while (not eof(f)) and (koniec_sprawdzania=false) do
    begin
        readln(f,text);
        if pos('###linia',text)=1 then
        repeat
             linia:=StrToInt(copy(text,9,length(text)-8));
             readln(f,text);
        until pos('###linia',text)=0;
        if text='end' then
        begin
            readln(f,text);
            if pos('###linia',text)=1 then
            repeat
                //tu nie potrzebne jest zliczanie linii
                readln(f,text);
            until pos('###linia',text)=0;
            if text<>';' then
            begin
                ok:=false;
                koniec_sprawdzania:=true;
                Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Brak œrednika po s³owie "end"');
            end;
        end;
    end;
    closefile(f);
end;

procedure TForm1.sprawdz_ilosc_begin_i_end;
var
    f:TextFile;
    text:string;
    linia,ilosc:integer;
    koniec_sprawdzania:boolean;
    poczatek_sprawdzania:boolean;
begin
    ilosc:=0;
    poczatek_sprawdzania:=false;
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    while (not eof(f)) and (koniec_sprawdzania=false) do
    begin
        readln(f,text);
        if pos('###linia',text)=1 then
        repeat
             linia:=StrToInt(copy(text,9,length(text)-8));
             readln(f,text);
        until pos('###linia',text)=0;
        if text='begin' then
           poczatek_sprawdzania:=true;
        if text='begin' then
            ilosc:=ilosc+1;
        if (text='end') or (text='end.') then
            ilosc:=ilosc-1;
        if (text='end.') then
            poczatek_sprawdzania:=false;
        if (ilosc=0) and (poczatek_sprawdzania=true) then
        begin
            ok:=false;
            koniec_sprawdzania:=true;
            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Wymagany jest znak kropki "." - brak tego znaku');
        end;
        if ilosc<0 then
        begin
            ok:=false;
            koniec_sprawdzania:=true;
            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Wyst¹pi³o s³owo "end" bez wczeœniejszego wyst¹pienia s³owa "begin"');
        end;
    end;
    if (koniec_sprawdzania=false) and (ilosc<>0) then
    begin
        ok:=false;
        Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Iloœæ wyst¹pieñ s³ów "begin" i "end" nie zgadza siê');
    end;
    closefile(f);
end;

procedure TForm1.sprawdz_ilosc_if_then;
var
    f:TextFile;
    text:string;
    linia,ilosc:integer;
    koniec_sprawdzania:boolean;
begin
    ilosc:=0;
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    while (not eof(f)) and (koniec_sprawdzania=false) do
    begin
        readln(f,text);
        if pos('###linia',text)=1 then
        repeat
             linia:=StrToInt(copy(text,9,length(text)-8));
             readln(f,text);
        until pos('###linia',text)=0;
        if text='if' then
            ilosc:=ilosc+1;
        if (text='then') then
            ilosc:=ilosc-1;
        if ilosc<0 then
        begin
            ok:=false;
            koniec_sprawdzania:=true;
            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Wyst¹pi³o s³owo "then" bez wczeœniejszego wyst¹pienia s³owa "if"');
        end;
    end;
    if (koniec_sprawdzania=false) and (ilosc<>0) then
    begin
        ok:=false;
        Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Iloœæ wyst¹pieñ s³ów "if" i "then" nie zgadza siê');
    end;
    closefile(f);
end;

procedure TForm1.sprawdz_ilosc_for_to;
var
    f:TextFile;
    text:string;
    linia,ilosc:integer;
    koniec_sprawdzania:boolean;
begin
    ilosc:=0;
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    while (not eof(f)) and (koniec_sprawdzania=false) do
    begin
        readln(f,text);
        if pos('###linia',text)=1 then
        repeat
             linia:=StrToInt(copy(text,9,length(text)-8));
             readln(f,text);
        until pos('###linia',text)=0;
        if text='for' then
            ilosc:=ilosc+1;
        if (text='to') then
            ilosc:=ilosc-1;
        if ilosc<0 then
        begin
            ok:=false;
            koniec_sprawdzania:=true;
            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Wyst¹pi³o s³owo "to" bez wczeœniejszego wyst¹pienia s³owa "for"');
        end;
    end;
    if (koniec_sprawdzania=false) and (ilosc<>0) then
    begin
        ok:=false;
        Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Iloœæ wyst¹pieñ s³ów "for" i "to" nie zgadza siê');
    end;
    closefile(f);
end;

procedure TForm1.sprawdz_ilosc_for_do;
var
    f:TextFile;
    text:string;
    linia,ilosc:integer;
    koniec_sprawdzania:boolean;
begin
    ilosc:=0;
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    while (not eof(f)) and (koniec_sprawdzania=false) do
    begin
        readln(f,text);
        if pos('###linia',text)=1 then
        repeat
             linia:=StrToInt(copy(text,9,length(text)-8));
             readln(f,text);
        until pos('###linia',text)=0;
        if text='for' then
            ilosc:=ilosc+1;
        if (text='do') then
            ilosc:=ilosc-1;
        if ilosc<0 then
        begin
            ok:=false;
            koniec_sprawdzania:=true;
            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Wyst¹pi³o s³owo "do" bez wczeœniejszego wyst¹pienia s³owa "for"');
        end;
    end;
    if (koniec_sprawdzania=false) and (ilosc<>0) then
    begin
        ok:=false;
        Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Iloœæ wyst¹pieñ s³ów "for" i "do" nie zgadza siê');
    end;
    closefile(f);
end;

procedure TForm1.sprawdz_if;
var
    f:TextFile;
    text:string;
    linia,ilosc,probna_calkowita,kod_bledu:integer;
    tymczasowe_spr:integer;
    probna_rzeczywista:double;
    koniec_sprawdzania:boolean;
begin
    ilosc:=0;
    linia:=0;
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    tymczasowe_spr:=0;
    while (not eof(f)) and (koniec_sprawdzania=false) do
    begin
        readln(f,text);
        if pos('###linia',text)=1 then
        repeat
             linia:=StrToInt(copy(text,9,length(text)-8));
             readln(f,text);
        until pos('###linia',text)=0;
        if text='[###instrukcja warunkowa###]' then
        begin
            readln(f,text);
            if pos('###linia',text)=1 then
            repeat
                linia:=StrToInt(copy(text,9,length(text)-8));
                readln(f,text);
            until pos('###linia',text)=0;
            //przeczyta pierwsz¹ rzecz po ifie
            readln(f,text);
            if pos('###linia',text)=1 then
            repeat
                linia:=StrToInt(copy(text,9,length(text)-8));
                readln(f,text);
            until pos('###linia',text)=0;
            //jeœli minus to czytamy zmienn¹ lub liczbê
            if text='-' then
            begin
                readln(f,text);
                if pos('###linia',text)=1 then
                repeat
                    linia:=StrToInt(copy(text,9,length(text)-8));
                    readln(f,text);
                until pos('###linia',text)=0;
            end;
            try
                 probna_calkowita:=StrToInt(text);
            except
                 tymczasowe_spr:=1;
            end;
            if tymczasowe_spr=1 then
            begin
                 tymczasowe_spr:=0;
                 try
                     probna_rzeczywista:=StrToFloat(text);
                 except
                     tymczasowe_spr:=1;
                 end;
            end;
            if tymczasowe_spr=1 then
            begin
                kod_bledu:=sprawdz_poprawnosc_zmiennej(text);
                if kod_bledu<>0 then
                begin
                    tymczasowe_spr:=0;
                    ok:=false;
                    koniec_sprawdzania:=true;
                    if kod_bledu=1 then
                       Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Na pocz¹tku nazwy zmiennej nie mo¿e byæ cyfry');
                    if kod_bledu=2 then
                       Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Niedopuszczalny znak w nazwie zmiennej lub niedopuszczalny znak w warunku');
                    if kod_bledu=3 then
                       Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Nazw¹ zmiennej jest s³owo kluczowe - tak byæ nie mo¿e');
                end
            end;
            if koniec_sprawdzania=false then
            begin
                //przeczyta operator
                readln(f,text);
                if pos('###linia',text)=1 then
                repeat
                   linia:=StrToInt(copy(text,9,length(text)-8));
                   readln(f,text);
                until pos('###linia',text)=0;
                if (text<>'>') and (text<>'<') and (text<>'>=') and (text<>'<=') and (text<>'=') and (text<>'<>') then
                begin
                    ok:=false;
                    koniec_sprawdzania:=true;
                    Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +'Nieznany operator porównania');
                end
                else
                begin
                    //przeczyta kolejn¹ zmienn¹/liczbê po operatorze
                    readln(f,text);
                    //jeœli minus to czytamy zmienn¹ lub liczbê
                    if text='-' then
                    begin
                       readln(f,text);
                       if pos('###linia',text)=1 then
                       repeat
                           linia:=StrToInt(copy(text,9,length(text)-8));
                           readln(f,text);
                       until pos('###linia',text)=0;
                    end;
                    if pos('###linia',text)=1 then
                    repeat
                        linia:=StrToInt(copy(text,9,length(text)-8));
                        readln(f,text);
                    until pos('###linia',text)=0;
                    try
                       probna_calkowita:=StrToInt(text);
                    except
                       tymczasowe_spr:=1;
                    end;
                    if tymczasowe_spr=1 then
                    begin
                       tymczasowe_spr:=0;
                       try
                          probna_rzeczywista:=StrToFloat(text);
                       except
                          tymczasowe_spr:=1;
                       end;
                    end;

                    if tymczasowe_spr=1 then
                    begin
                        kod_bledu:=sprawdz_poprawnosc_zmiennej(text);
                        if kod_bledu<>0 then
                        begin
                            tymczasowe_spr:=0;
                            ok:=false;
                            koniec_sprawdzania:=true;
                            if kod_bledu=1 then
                               Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Na pocz¹tku nazwy zmiennej nie mo¿e byæ cyfry');
                            if kod_bledu=2 then
                               Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Niedopuszczalny znak w nazwie zmiennej lub niedopuszczalny znak w warunku');
                            if kod_bledu=3 then
                               Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Nazw¹ zmiennej jest s³owo kluczowe - tak byæ nie mo¿e');
                        end
                    end;

                    if koniec_sprawdzania=false then
                    begin
                        //czytamy then
                        readln(f,text);
                        if pos('###linia',text)=1 then
                        repeat
                            linia:=StrToInt(copy(text,9,length(text)-8));
                            readln(f,text);
                        until pos('###linia',text)=0;
                        if text<>'then' then
                        begin
                            ok:=false;
                            koniec_sprawdzania:=true;
                            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Oczekiwano s³owa kluczowego "then" - brak tego s³owa - zamiast niego jest : '+text);
                        end
                    end;
                end;
            end;
        end;
    end;
    closefile(f);
end;


procedure TForm1.sprawdz_petle;
var
    f:TextFile;
    text,zapamietaj:string;
    linia,ilosc,probna_calkowita,kod_bledu:integer;
    tymczasowe_spr:integer;
    koniec_sprawdzania:boolean;
    do_petli_real:integer;
begin
    ilosc:=0;
    linia:=0;
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    tymczasowe_spr:=0;
    while (not eof(f)) and (koniec_sprawdzania=false) do
    begin
        readln(f,text);
        if pos('###linia',text)=1 then
        repeat
             linia:=StrToInt(copy(text,9,length(text)-8));
             readln(f,text);
        until pos('###linia',text)=0;
        if text='[###pêtla###]' then
        begin
            //przeczyta s³owo for
            readln(f,text);
            if pos('###linia',text)=1 then
            repeat
                linia:=StrToInt(copy(text,9,length(text)-8));
                readln(f,text);
            until pos('###linia',text)=0;
            //przeczyta pierwsz¹ rzecz po for
            readln(f,text);
            if pos('###linia',text)=1 then
            repeat
                linia:=StrToInt(copy(text,9,length(text)-8));
                readln(f,text);
            until pos('###linia',text)=0;
            zapamietaj:=text;//zapamiêtujemy zmiann¹
            kod_bledu:=sprawdz_poprawnosc_zmiennej(text);
            if kod_bledu<>0 then
            begin
                ok:=false;
                koniec_sprawdzania:=true;
                if kod_bledu=1 then
                    Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Na pocz¹tku nazwy zmiennej nie mo¿e byæ cyfry');
                if kod_bledu=2 then
                    Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Niedopuszczalny znak w nazwie zmiennej');
                if kod_bledu=3 then
                    Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Nazw¹ zmiennej jest s³owo kluczowe - tak byæ nie mo¿e');
            end;
            for do_petli_real:=0 to r-1 do
                if zmienne_real[do_petli_real].nazwa=zapamietaj then
                begin
                    ok:=false;
                    koniec_sprawdzania:=true;
                    Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : zmienna rzeczywista nie mo¿e byæ zmienn¹ sterujac¹ pêtli');
                end;

            if koniec_sprawdzania=false then
            begin
                //czytamy [###operacja przypisania###]
                readln(f,text);
                if pos('###linia',text)=1 then
                repeat
                    linia:=StrToInt(copy(text,9,length(text)-8));
                    readln(f,text);
                until pos('###linia',text)=0;
                if text<>'[###operacja przypisania###]' then
                begin
                    ok:=false;
                    koniec_sprawdzania:=true;
                    Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Wymagany jest operator przypisania - brak go - zamiast niego jest : '+text);
                end
                else
                begin
                    //przeczyta operator przypisania
                    readln(f,text);
                    if pos('###linia',text)=1 then
                    repeat
                       linia:=StrToInt(copy(text,9,length(text)-8));
                       readln(f,text);
                    until pos('###linia',text)=0;
                end;
                if koniec_sprawdzania=false then
                begin
                    //czytamy zmienn¹ lub liczbê po operatorze przypisania lub minus
                    readln(f,text);
                    if pos('###linia',text)=1 then
                    repeat
                       linia:=StrToInt(copy(text,9,length(text)-8));
                       readln(f,text);
                    until pos('###linia',text)=0;
                    if text='-' then
                    begin
                        readln(f,text);
                        if pos('###linia',text)=1 then
                        repeat
                            linia:=StrToInt(copy(text,9,length(text)-8));
                            readln(f,text);
                        until pos('###linia',text)=0;
                    end;

                    tymczasowe_spr:=0;
                    try
                       probna_calkowita:=StrToInt(text);
                    except
                       tymczasowe_spr:=1;
                    end;

                    if tymczasowe_spr=1 then
                    begin
                        kod_bledu:=sprawdz_poprawnosc_zmiennej(text);
                        if kod_bledu<>0 then
                        begin
                            tymczasowe_spr:=0;
                            ok:=false;
                            koniec_sprawdzania:=true;
                            if kod_bledu=1 then
                               Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Na pocz¹tku nazwy zmiennej nie mo¿e byæ cyfry');
                            if kod_bledu=2 then
                               Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Niedopuszczalny znak w nazwie zmiennej');
                            if kod_bledu=3 then
                               Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Nazw¹ zmiennej jest s³owo kluczowe - tak byæ nie mo¿e');
                        end
                    end;
                    //nie mo¿e byæ to samo przed i po znaku przypisania
                    if zapamietaj=text then
                    begin
                        ok:=false;
                        koniec_sprawdzania:=true;
                        Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Nazwa zmiennej przed operatorem przypisania jest taka sama jak zmiennej po operatorze przypisania - tak byæ nie mo¿e');
                    end;
                end;
                if koniec_sprawdzania=false then
                begin
                    //czytamy s³owo to
                    readln(f,text);
                    if pos('###linia',text)=1 then
                    repeat
                       linia:=StrToInt(copy(text,9,length(text)-8));
                       readln(f,text);
                    until pos('###linia',text)=0;
                    if text<>'to' then
                    begin
                        ok:=false;
                        koniec_sprawdzania:=true;
                        Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Wymagany s³owo kluczowe "to" - brak go - zamiast niego jest : '+text);
                    end;
                end;
                if koniec_sprawdzania=false then
                begin
                    //czytamy ogranicznik pêtli - mo¿e byæ minus
                    readln(f,text);
                    if pos('###linia',text)=1 then
                    repeat
                       linia:=StrToInt(copy(text,9,length(text)-8));
                       readln(f,text);
                    until pos('###linia',text)=0;
                    if text='-' then
                    begin
                        readln(f,text);
                        if pos('###linia',text)=1 then
                        repeat
                            linia:=StrToInt(copy(text,9,length(text)-8));
                            readln(f,text);
                        until pos('###linia',text)=0;
                    end;

                    tymczasowe_spr:=0;
                    try
                       probna_calkowita:=StrToInt(text);
                    except
                       tymczasowe_spr:=1;
                    end;

                    if tymczasowe_spr=1 then
                    begin
                        kod_bledu:=sprawdz_poprawnosc_zmiennej(text);
                        if kod_bledu<>0 then
                        begin
                            tymczasowe_spr:=0;
                            ok:=false;
                            koniec_sprawdzania:=true;
                            if kod_bledu=1 then
                               Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Na pocz¹tku nazwy zmiennej nie mo¿e byæ cyfry');
                            if kod_bledu=2 then
                               Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Niedopuszczalny znak w nazwie zmiennej lub niedopuszczalny znak w warunku');
                            if kod_bledu=3 then
                               Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Nazw¹ zmiennej jest s³owo kluczowe - tak byæ nie mo¿e');
                        end
                    end;
                    //nie moze byæ to samo po s³owie to i przed operatorem przypisania
                    if zapamietaj=text then
                    begin
                        ok:=false;
                        koniec_sprawdzania:=true;
                        Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Nazwa zmiennej przed operatorem przypisania jest taka sama jak ogranicznika pêtli - tak byæ nie mo¿e');
                    end
                end;
                if koniec_sprawdzania=false then
                begin
                    //czytamy s³owo "do"
                    readln(f,text);
                    if pos('###linia',text)=1 then
                    repeat
                       linia:=StrToInt(copy(text,9,length(text)-8));
                       readln(f,text);
                    until pos('###linia',text)=0;
                    if text<>'do' then
                    begin
                        ok:=false;
                        koniec_sprawdzania:=true;
                        Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Wymagany s³owo kluczowe "do" - brak go - zamiast niego jest : '+text);
                    end;
                end;
            end;
        end;
    end;
    closefile(f);
end;

procedure TForm1.sprawdz_zmienne_w_petli;
var
    f:TextFile;
    text,zapamietaj,zapamietaj_for:string;
    linia,ilosc,do_petli:integer;
    koniec_sprawdzania:boolean;
    ilosc_begin_end:integer;
begin
    linia:=0;
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    while (not eof(f)) and (koniec_sprawdzania=false) do
    begin
        ilosc:=0;
        setlength(nie_mozna_takich_zmiennych,ilosc);
        readln(f,text);
        if pos('###linia',text)=1 then
        repeat
             linia:=StrToInt(copy(text,9,length(text)-8));
             readln(f,text);
        until pos('###linia',text)=0;
        if text='[###pêtla###]' then
        begin
            //przeczyta s³owo for
            readln(f,text);
            if pos('###linia',text)=1 then
            repeat
                linia:=StrToInt(copy(text,9,length(text)-8));
                readln(f,text);
            until pos('###linia',text)=0;
            //przeczyta pierwsz¹ rzecz po for
            readln(f,text);
            if pos('###linia',text)=1 then
            repeat
                linia:=StrToInt(copy(text,9,length(text)-8));
                readln(f,text);
            until pos('###linia',text)=0;
            ilosc:=ilosc+1;
            setlength(nie_mozna_takich_zmiennych,ilosc);
            nie_mozna_takich_zmiennych[ilosc-1]:=text;//zapamiêtujemy zmiann¹
            //czytamy a¿ do do
            repeat
                readln(f,text);
                if pos('###linia',text)=1 then
                repeat
                   linia:=StrToInt(copy(text,9,length(text)-8));
                   readln(f,text);
                until pos('###linia',text)=0;
            until text='do';
            //czytamy pierwsz¹ rzecz po do
            readln(f,text);
            if pos('###linia',text)=1 then
            repeat
                linia:=StrToInt(copy(text,9,length(text)-8));
                readln(f,text);
            until pos('###linia',text)=0;
            //sprawdzamy czy to pocz¹tek bloku
            //jeœli nie to sprawdzamy czy jest przypisanie
            if text<>'[###pocz¹tek bloku###]' then
            begin
                while (text<>';') and (koniec_sprawdzania=false) do
                begin
                    if text<>'[###operacja przypisania###]' then
                    begin
                       zapamietaj_for:=zapamietaj;
                       zapamietaj:=text;
                    end;
                    readln(f,text);
                    if pos('###linia',text)=1 then
                    repeat
                        linia:=StrToInt(copy(text,9,length(text)-8));
                        readln(f,text);
                    until pos('###linia',text)=0;
                    if text=':=' then
                    begin
                        for do_petli:=0 to ilosc-1 do
                            if nie_mozna_takich_zmiennych[do_petli]=zapamietaj then
                            begin
                                ok:=false;
                                koniec_sprawdzania:=true;
                                Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+zapamietaj+ ' - ta zmienna nie mo¿e byæ u¿yta po lewej stronie znaku przypisania');
                            end;
                        if zapamietaj_for='for' then
                        begin
                            ilosc:=ilosc+1;
                            setlength(nie_mozna_takich_zmiennych,ilosc);
                            nie_mozna_takich_zmiennych[ilosc-1]:=zapamietaj;//zapamiêtujemy zmiann¹
                        end;
                   end;
                end;
            end
            else  //jeœli jest begin po for
            begin
                 ilosc_begin_end:=0;
                 repeat
                    if text<>'[###operacja przypisania###]' then
                    begin
                       zapamietaj_for:=zapamietaj;
                       zapamietaj:=text;
                    end;
                    readln(f,text);
                    if pos('###linia',text)=1 then
                    repeat
                        linia:=StrToInt(copy(text,9,length(text)-8));
                        readln(f,text);
                    until pos('###linia',text)=0;

                    if text='begin' then
                       ilosc_begin_end:=ilosc_begin_end+1;
                    if text='end' then
                       ilosc_begin_end:=ilosc_begin_end-1;

                    if text=':=' then
                    begin
                        for do_petli:=0 to ilosc-1 do
                            if nie_mozna_takich_zmiennych[do_petli]=zapamietaj then
                            begin
                                ok:=false;
                                koniec_sprawdzania:=true;
                                Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+zapamietaj+ ' - ta zmienna nie mo¿e byæ u¿yta po lewej stronie znaku przypisania');
                            end;
                        if zapamietaj_for='for' then
                        begin
                           ilosc:=ilosc+1;
                           setlength(nie_mozna_takich_zmiennych,ilosc);
                           nie_mozna_takich_zmiennych[ilosc-1]:=zapamietaj;//zapamiêtujemy zmiann¹
                        end;
                    end;
                 until ((text=';') and (ilosc_begin_end=0)) or (koniec_sprawdzania=true);
            end;
        end;
        
    end;
    closefile(f);
end;

procedure TForm1.sprawdz_dzialania_arytmetyczne;
var
    f:TextFile;
    text,zapamietaj_zmienna,co_mozna_przeczytac:string;
    linia,probna_calkowita,kod_bledu,ilosc_nawiasow:integer;
    tymczasowe_spr:integer;
    probna_rzeczywista:double;
    koniec_sprawdzania:boolean;
begin
    ilosc_nawiasow:=0;
    linia:=0;
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    tymczasowe_spr:=0;
    while (not eof(f))  do
    begin
        koniec_sprawdzania:=false;
        //przeczyta [###operacja przypisania###] i zapamiêta co jest przed nim
        readln(f,text);
        if pos('###linia',text)=1 then
        repeat
             linia:=StrToInt(copy(text,9,length(text)-8));
             readln(f,text);
        until pos('###linia',text)=0;

        //to co teraz mo¿e byæ w czytane
           co_mozna_przeczytac:='funkcja_zmienna_liczba_nawiasotwierajacy_minus';

        if text<>'[###operacja przypisania###]' then
           zapamietaj_zmienna:=text
        else
        begin
           //przeczyta :=
           readln(f,text);
           if pos('###linia',text)=1 then
           repeat
                linia:=StrToInt(copy(text,9,length(text)-8));
                readln(f,text);
           until pos('###linia',text)=0;

           //sprawdzamy poprawnoœæ zmiennej przed operatorem
           kod_bledu:=sprawdz_poprawnosc_zmiennej(zapamietaj_zmienna);
           if kod_bledu<>0 then
           begin
               tymczasowe_spr:=0;
               ok:=false;
               koniec_sprawdzania:=true;
               if kod_bledu=1 then
                   Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Na pocz¹tku nazwy zmiennej nie mo¿e byæ cyfry');
               if kod_bledu=2 then
                   Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Niedopuszczalny znak w nazwie zmiennej');
               if kod_bledu=3 then
                   Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Nazw¹ zmiennej jest s³owo kluczowe - tak byæ nie mo¿e');
           end;
           if koniec_sprawdzania=false then
           begin
               while (text<>';') and (koniec_sprawdzania=false) do
               begin
                   //showmessage(co_mozna_przeczytac);
                   readln(f,text);
                   if pos('###linia',text)=1 then
                   repeat
                       linia:=StrToInt(copy(text,9,length(text)-8));
                       readln(f,text);
                   until pos('###linia',text)=0;
                   if co_mozna_przeczytac='funkcja_zmienna_liczba_nawiasotwierajacy_minus' then
                   begin
                       tymczasowe_spr:=0;
                       //sprawdzamy czy funkcja...
                       if (text<>'sin') and (text<>'cos') and (text<>'tan') and (text<>'cotan') and (text<>'arcsin') and (text<>'arccos') and (text<>'arctan') and (text<>'ln') and (text<>'exp') and (text<>'sqrt') then
                           tymczasowe_spr:=1
                       else
                       begin
                           tymczasowe_spr:=0;
                           co_mozna_przeczytac:='nawias_otwierajacy';  //
                       end;
                       //...jeœli nie funkcja to sprawdzamy czy zmienna...
                       if tymczasowe_spr=1 then
                       begin
                           kod_bledu:=sprawdz_poprawnosc_zmiennej(text);
                           if kod_bledu=0 then
                           begin
                               tymczasowe_spr:=0;
                               co_mozna_przeczytac:='operator_nawiaszamykajacy_srednik'; //
                           end;
                       end;
                       //...jeœli nie zmienna to sprawdzamy czy liczba...
                       if tymczasowe_spr=1 then
                       begin
                           tymczasowe_spr:=0;
                           try
                              probna_calkowita:=StrToInt(text);
                           except
                              tymczasowe_spr:=1;
                           end;
                           if tymczasowe_spr=1 then
                           begin
                              tymczasowe_spr:=0;
                              try
                                 probna_rzeczywista:=StrToFloat(text);
                              except
                                 tymczasowe_spr:=1;
                              end;
                           end;
                           if tymczasowe_spr=0 then
                              co_mozna_przeczytac:='operator_nawiaszamykajacy_srednik'; //
                       end;
                       //...jeœli nie liczba to sprawdzamy czy nawias otwierajacy...
                       if tymczasowe_spr=1 then
                       begin
                           if text='(' then
                           begin
                               tymczasowe_spr:=0;
                               ilosc_nawiasow:=ilosc_nawiasow+1;
                               co_mozna_przeczytac:='funkcja_zmienna_liczba_nawiasotwierajacy_minus'; //
                           end;
                       end;
                       //...jeœli nie nawias otwierajacy to sprawdzamy czy minus...
                       if tymczasowe_spr=1 then
                       begin
                           if text='-' then
                           begin
                               tymczasowe_spr:=0;
                               co_mozna_przeczytac:='funkcja_zmienna_liczba_nawiasotwierajacy'; //
                           end;
                       end;
                       //..jeœli nic z powy¿ej wymienionych rzeczy to b³¹d
                       if tymczasowe_spr=1 then
                       begin
                           ok:=false;
                           koniec_sprawdzania:=true;
                           Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Wymagana jest funkcja, zmienna, liczba b¹dŸ nawias otwieraj¹cy - brak któregokolwiek z tych leksemów - zamiast nich jest : '+text);
                       end;
                   end
                   else

                   if co_mozna_przeczytac='funkcja_zmienna_liczba_nawiasotwierajacy' then
                   begin
                       tymczasowe_spr:=0;
                       //sprawdzamy czy funkcja...
                       if (text<>'sin') and (text<>'cos') and (text<>'tan') and (text<>'cotan') and (text<>'arcsin') and (text<>'arccos') and (text<>'arctan') and (text<>'ln') and (text<>'exp') and (text<>'sqrt') then
                           tymczasowe_spr:=1
                       else
                       begin
                           tymczasowe_spr:=0;
                           co_mozna_przeczytac:='nawias_otwierajacy';  //
                       end;
                       //...jeœli nie funkcja to sprawdzamy czy zmienna...
                       if tymczasowe_spr=1 then
                       begin
                           kod_bledu:=sprawdz_poprawnosc_zmiennej(text);
                           if kod_bledu=0 then
                           begin
                               tymczasowe_spr:=0;
                               co_mozna_przeczytac:='operator_nawiaszamykajacy_srednik'; //
                           end;
                       end;
                       //...jeœli nie zmienna to sprawdzamy czy liczba...
                       if tymczasowe_spr=1 then
                       begin
                           tymczasowe_spr:=0;
                           try
                              probna_calkowita:=StrToInt(text);
                           except
                              tymczasowe_spr:=1;
                           end;
                           if tymczasowe_spr=1 then
                           begin
                              tymczasowe_spr:=0;
                              try
                                 probna_rzeczywista:=StrToFloat(text);
                              except
                                 tymczasowe_spr:=1;
                              end;
                           end;
                           if tymczasowe_spr=0 then
                              co_mozna_przeczytac:='operator_nawiaszamykajacy_srednik'; //
                       end;
                       //...jeœli nie liczba to sprawdzamy czy nawias otwierajacy...
                       if tymczasowe_spr=1 then
                       begin
                           if text='(' then
                           begin
                               tymczasowe_spr:=0;
                               ilosc_nawiasow:=ilosc_nawiasow+1;
                               co_mozna_przeczytac:='funkcja_zmienna_liczba_nawiasotwierajacy_minus'; //
                           end;
                       end;
                       //..jeœli nic z powy¿ej wymienionych rzeczy to b³¹d
                       if tymczasowe_spr=1 then
                       begin
                           ok:=false;
                           koniec_sprawdzania:=true;
                           Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Wymagana jest funkcja, zmienna, liczba b¹dŸ nawias otwieraj¹cy - brak któregokolwiek z tych leksemów - zamiast nich jest : '+text);
                       end;
                   end
                   else

                   if co_mozna_przeczytac='nawias_otwierajacy' then
                   begin
                       tymczasowe_spr:=0;
                       if text<>'(' then
                           tymczasowe_spr:=0
                       else
                       begin
                           ilosc_nawiasow:=ilosc_nawiasow+1;
                           co_mozna_przeczytac:='funkcja_zmienna_liczba_nawiasotwierajacy_minus'; //
                       end;
                       if tymczasowe_spr=1 then
                       begin
                           ok:=false;
                           koniec_sprawdzania:=true;
                           Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Wymagany jest nawias otwieraj¹cy - brak tego leksema - zamiast niego jest : '+text);
                       end;
                   end
                   else
                   if co_mozna_przeczytac='operator_nawiaszamykajacy_srednik' then
                   begin
                       //sprawdzamy czy to jest operator...
                       tymczasowe_spr:=0;
                       if (text<>'+') and (text<>'-') and (text<>'*') and (text<>'/') and (text<>'^') then
                           tymczasowe_spr:=1
                       else
                           co_mozna_przeczytac:='funkcja_zmienna_liczba_nawiasotwierajacy'; //
                       //...jeœli nie to sprawdzamy czy jest to nawias zamyk¹j¹cy...
                       if tymczasowe_spr=1 then
                       begin
                           if text=')' then
                           begin
                               tymczasowe_spr:=0;
                               ilosc_nawiasow:=ilosc_nawiasow-1;
                               if ilosc_nawiasow<0 then
                               begin
                                   ok:=false;
                                   koniec_sprawdzania:=true;
                                   Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Wystêpuje nawias zamykajacy bez wczeœniejszego wyst¹pienia nawiasu otwieraj¹cego');
                               end;
                               co_mozna_przeczytac:='operator_nawiaszamykajacy_srednik'; //
                           end;
                       end;
                       //...jeœli nie to sprawdzamy cz jest to srednik...
                       if (tymczasowe_spr=1) and (koniec_sprawdzania=false) then
                       begin
                           if (text=';') or (text='to') then
                           begin
                               tymczasowe_spr:=0;
                               koniec_sprawdzania:=true;
                           end;
                       end;
                       //...jeœli nic z powy¿ej wymienionych rzeczy to b³¹d
                       if (tymczasowe_spr=1) and (koniec_sprawdzania=false) then
                       begin
                           ok:=false;
                           koniec_sprawdzania:=true;
                           Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Wymagany jest operator, nawias zamykaj¹cy lub œrednik - brak któregokolwiek z tych leksemów - zamiast nich jest : '+text);
                       end;
                   end;

               end;
           end;
           zapamietaj_zmienna:=' ';
        end;  //--
        if (ilosc_nawiasow>0) then
        begin
            ok:=false;
            koniec_sprawdzania:=true;
            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Liczba nawiasów otwieraj¹cych jest wiêksza niz liczba nawiasów zamykajacych');
            break;
        end;
    end;
    closefile(f);
end;

procedure TForm1.sprawdz_koniec_programu;
var
    f:TextFile;
    text:string;
    linia:integer;
begin
    linia:=0;
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    while not eof(f) do
    begin
        readln(f,text);
        if pos('###linia',text)=1 then
        repeat
             linia:=StrToInt(copy(text,9,length(text)-8));
             readln(f,text);
        until pos('###linia',text)=0;
    end;
    if text<>'[###koniec programu###]' then
    begin
        ok:=false;
        Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Brak koñca programu');
    end;
    closefile(f);
end;

procedure TForm1.deklaruj_zmienne;
var
    f:TextFile;
    text:string;
    linia,kod_bledu:integer;
    koniec_sprawdzania,zmienna:boolean;
    v,do_petli_integer,do_petli_real:integer;
    //do sprawdzenia czy wszystko oki
    g:TextFile;
begin
    i:=0;
    r:=0;
    v:=0;
    setlength(zmienne_integer,i);
    setlength(zmienne_real,r);
    setlength(zmienne_tymczasowe,v);

    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    zmienna:=false;
    while (not eof (f)) and (koniec_sprawdzania=false) do
    begin
        //czyta [###deklaracja zmiennych###]
        if zmienna=false then
        begin
            readln(f,text);
            if pos('###linia',text)=1 then
            repeat
                linia:=StrToInt(copy(text,9,length(text)-8));
                readln(f,text);
            until pos('###linia',text)=0;
        end;
        if (text='[###deklaracja zmiennych###]') or (zmienna=true) then
        begin
            //przeczyta var lub begin
            if zmienna=false then
            begin
               readln(f,text);
               if pos('###linia',text)=1 then
               repeat
                   linia:=StrToInt(copy(text,9,length(text)-8));
                   readln(f,text);
               until pos('###linia',text)=0;
               zmienna:=true;
               if text='[###pocz¹tek bloku###]' then
               begin
                   koniec_sprawdzania:=true;
                   zmienna:=false;
               end;
            end;
            //przeczyta nazwe zmiennej
            if (zmienna=true) and (koniec_sprawdzania=false) then
            begin
               readln(f,text);
               if pos('###linia',text)=1 then
               repeat
                   linia:=StrToInt(copy(text,9,length(text)-8));
                   readln(f,text);
               until pos('###linia',text)=0;
               //to jest po to jakby by³o drugie wyst¹pienie s³owa var w sekcji dekaratywnej
               if text='[###deklaracja zmiennych###]' then
               begin
                   readln(f,text);
                   if pos('###linia',text)=1 then
                   repeat
                       linia:=StrToInt(copy(text,9,length(text)-8));
                       readln(f,text);
                   until pos('###linia',text)=0;
                   readln(f,text);
                   if pos('###linia',text)=1 then
                   repeat
                       linia:=StrToInt(copy(text,9,length(text)-8));
                       readln(f,text);
                   until pos('###linia',text)=0;
               end;
               if text='[###pocz¹tek bloku###]' then
                   koniec_sprawdzania:=true;
               //sprawdzanie niepowtarzalnoœci nazw zmiennych
               if koniec_sprawdzania=false then
               begin
                   for do_petli_integer:=0 to i-1 do
                       if zmienne_integer[do_petli_integer].nazwa=text then
                           koniec_sprawdzania:=true;
                   for do_petli_real:=0 to r-1 do
                       if zmienne_real[do_petli_real].nazwa=text then
                           koniec_sprawdzania:=true;
                   for do_petli_real:=0 to v-1 do
                       if zmienne_tymczasowe[do_petli_real].nazwa=text then
                           koniec_sprawdzania:=true;
                   if koniec_sprawdzania=true then
                   begin
                       ok:=false;
                       Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Zmienna o tej nazwie by³a ju¿ wczeœniej zadeklarowana');
                   end;
               end;
               if koniec_sprawdzania=false then
               begin
                   v:=v+1;
                   setlength(zmienne_tymczasowe,v);
                   zmienne_tymczasowe[v-1].nazwa:=text;
               end;
               if koniec_sprawdzania=false then
               begin
                  //teraz przeczyta albo dwukropek, albo przecinek
                  if koniec_sprawdzania=false then
                  begin
                      readln(f,text);
                      if pos('###linia',text)=1 then
                      repeat
                          linia:=StrToInt(copy(text,9,length(text)-8));
                          readln(f,text);
                      until pos('###linia',text)=0;

                      //jeden ze znaków
                      if text=':' then
                      begin
                          //czytam typ
                          zmienna:=false;
                          readln(f,text);
                          if pos('###linia',text)=1 then
                          repeat
                               linia:=StrToInt(copy(text,9,length(text)-8));
                               readln(f,text);
                          until pos('###linia',text)=0;

                          if text='integer' then
                          begin
                              setlength(zmienne_integer,i+v);
                              for do_petli_integer:=i to i+v-1 do
                              begin
                                  zmienne_integer[do_petli_integer].nazwa:=zmienne_tymczasowe[do_petli_integer-i].nazwa;
                                  zmienne_integer[do_petli_integer].wartosc:=0;
                              end;
                              i:=i+v;
                              v:=0;
                              setlength(zmienne_tymczasowe,v);
                          end
                          else
                          if text='double' then
                          begin
                              setlength(zmienne_real,r+v);
                              for do_petli_real:=r to r+v-1 do
                              begin
                                 // showmessage(inttostr(do_petli_real-r));
                                  zmienne_real[do_petli_real].nazwa:=zmienne_tymczasowe[do_petli_real-r].nazwa;
                                  zmienne_real[do_petli_real].wartosc:=0;
                              end;
                              r:=r+v;
                              v:=0;
                              setlength(zmienne_tymczasowe,v);
                          end;

                          //czyta œrednik
                          readln(f,text);
                          if pos('###linia',text)=1 then
                          repeat
                               linia:=StrToInt(copy(text,9,length(text)-8));
                               readln(f,text);
                          until pos('###linia',text)=0;
                          if (text=';') then
                               zmienna:=true;
                      end
                      else//jeœli jest to przecinek
                          zmienna:=true
                  end;
               end;
           end;
        end;
    end;
    closefile(f);

    //poni¿ej jest do sprawdzenia
{    assignfile(g,'trrrrrr.txt');
    rewrite(g);
    writeln(g,'-------------zmienne integer-------------');
    for do_petli_integer:=0 to i-1 do
        writeln(g,zmienne_integer[do_petli_integer].nazwa,'  ',zmienne_integer[do_petli_integer].wartosc);
    writeln(g,'-------------zmienne real-------------');
    for do_petli_real:=0 to r-1 do
        writeln(g,zmienne_real[do_petli_real].nazwa,'  ',zmienne_real[do_petli_real].wartosc);
    closefile(g);}
end;

procedure TForm1.sprawdz_czy_nie_ma_pojedynczych_zmiennych;
var
    f:TextFile;
    text1,text2,text3:string;
    linia:integer;
    koniec_sprawdzania:boolean;
begin
    text1:='';
    text2:='';
    text3:='';
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    while (not eof(f)) and (koniec_sprawdzania=false) do
    begin
        text1:=text2;
        text2:=text3;
        readln(f,text3);
        if pos('###linia',text3)=1 then
        repeat
            linia:=StrToInt(copy(text3,9,length(text3)-8));
            readln(f,text3);
        until pos('###linia',text3)=0;
        if ((text1=';') or (text1='begin') or (text1='then') or (text1='do')) and (text3=';') then
        begin
            ok:=false;
            koniec_sprawdzania:=true;
            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Brak operacji lub za du¿o œredników');
        end;
    end;
    closefile(f);
end;

procedure TForm1.sprawdz_czy_zadeklarowano_zmiene;
var
    f:TextFile;
    text:string;
    linia,tymczasowe_spr,probna_calkowita:integer;
    koniec_sprawdzania, zacznij_sprawdzac, jest_zadeklarowana:boolean;
    probna_rzeczywista:double;
    do_petli_integer,do_petli_real:integer;
begin
    linia:=0;
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    zacznij_sprawdzac:=false;
    while (not eof(f)) and (koniec_sprawdzania=false) do
    begin
        try
        readln(f,text);
        if pos('###linia',text)=1 then
        repeat
            linia:=StrToInt(copy(text,9,length(text)-8));
            readln(f,text);
        until pos('###linia',text)=0;
        if text='[###pocz¹tek bloku###]' then
            zacznij_sprawdzac:=true;
        if zacznij_sprawdzac=true then
        begin
            if     (text<>'[###pocz¹tek bloku###]') and (text<>'[###koniec bloku###]') and (text<>'[###pêtla###]') and (text<>'[###instrukcja warunkowa###]') and (text<>'[###operacja przypisania###]')
               and (text<>'[###instrukcja warunkowa###]') and (text<>'[###koniec programu###]') and (text<>'begin') and (text<>'end') and (text<>'for') and (text<>'to') and (text<>'do') and (text<>'if')
               and (text<>'then') and (text<>'write') and (text<>'writeln')  and (text<>'read') and (text<>'readln') and (text<>'end.') and (text<>':=') and (text<>'=') and (text<>'>=') and (text<>'<=')
               and (text<>'<') and (text<>'>') and (text<>'<>') and (text<>'+') and (text<>'-') and (text<>'*') and (text<>'/') and (text<>'^') and (text<>'sin') and (text<>'cos') and (text<>'tan')
               and (text<>'cotan') and (text<>'arcsin') and (text<>'arccos') and (text<>'arctan') and (text<>'ln') and (text<>'exp') and (text<>'sqrt') and (text<>'(') and (text<>')') and (text<>';') and (text[1]<>'''')
            then
            begin
                tymczasowe_spr:=0;
                try
                    probna_calkowita:=StrToInt(text);
                except
                    tymczasowe_spr:=1;
                end;
                if tymczasowe_spr=1 then
                begin
                    tymczasowe_spr:=0;
                    try
                        probna_rzeczywista:=StrToFloat(text);
                    except
                        tymczasowe_spr:=1;
                    end;
                end;
                //sprawdziæ zmienn¹
                if tymczasowe_spr=1 then
                begin
                    jest_zadeklarowana:=false;
                    for do_petli_integer:=0 to i-1 do
                         if text=zmienne_integer[do_petli_integer].nazwa then
                             jest_zadeklarowana:=true;
                    for do_petli_real:=0 to r-1 do
                         if text=zmienne_real[do_petli_real].nazwa then
                             jest_zadeklarowana:=true;
                    if (jest_zadeklarowana=false) then
                    begin
                        ok:=false;
                        koniec_sprawdzania:=true;
                        Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+text+' - Niezadeklarowana zmienna, lub nieznany symbol');
                    end;
                end;
            end;
        end;
        except
             ok:=false;
             Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Niespodziewany koniec programu');
        end;
    end;
    closefile(f);
end;

procedure TForm1.sprawdz_sredniki_przed_i_po_oraz_poprwnosc_read_write;
var
    f:TextFile;
    text,zapamietaj_zmienna,zapamietaj_slowo_write_lub_read:string;
    linia:integer;
    koniec_sprawdzania:boolean;
    do_petli:integer;
    ilosc_apostrofow,probna_calkowita,tymczasowe_spr:integer;
    probna_real:double;
begin
    linia:=0;
    zapamietaj_zmienna:='';
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    while (not eof(f)) and (koniec_sprawdzania=false) do
    begin
        ilosc_apostrofow:=0;
        readln(f,text);
        if pos('###linia',text)=1 then
        repeat
             linia:=StrToInt(copy(text,9,length(text)-8));
             readln(f,text);
        until pos('###linia',text)=0;

        if (text<>'read') and (text<>'write') and (text<>'readln') and (text<>'writeln') then
            zapamietaj_zmienna:=text
        else
        begin
            zapamietaj_slowo_write_lub_read:=text;
            if (zapamietaj_zmienna<>';') and (zapamietaj_zmienna<>'begin') and (zapamietaj_zmienna<>'do') and (zapamietaj_zmienna<>'then') then
            begin
                ok:=false;
                koniec_sprawdzania:=true;
                Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia) +' : Przed operacj¹ wejœcia/wyjœcia spodziewano siê œrednika, s³owa "do", s³owa "then", lub s³owa "begin" - brak tych leksemów - zamiast nich jest : '+text);
            end;
            if koniec_sprawdzania=false then
            begin
                //czytamy nawias otwieraj¹cy
                readln(f,text);
                if pos('###linia',text)=1 then
                repeat
                   linia:=StrToInt(copy(text,9,length(text)-8));
                   readln(f,text);
                until pos('###linia',text)=0;
                if text<>'(' then
                begin
                     ok:=false;
                     koniec_sprawdzania:=true;
                     Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) + ' : Oczekiwano nawiasu otwierajacego "(" - brak go - zamiast niego jest : '+text);
                end;
                if koniec_sprawdzania=false then
                begin
                   //czytamy to co w nawiasie
                   readln(f,text);
                   if pos('###linia',text)=1 then
                   repeat
                      linia:=StrToInt(copy(text,9,length(text)-8));
                      readln(f,text);
                   until pos('###linia',text)=0;

                   //sprawdzamy czy jest apostrof na koñcu tekstu
                   if (text<>'') then
                      if (text[1]='''') and (text[length(text)]<>'''') then
                      begin
                          ok:=false;
                          koniec_sprawdzania:=true;
                          Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+ text+ ' : Brakuje apostrofu na koñcu tekstu');
                      end;
                   if (text<>'') then
                      if (zapamietaj_slowo_write_lub_read='read') or (zapamietaj_slowo_write_lub_read='readln') then
                      begin
                          if pos('''',text)>0 then
                          begin
                             ok:=false;
                             koniec_sprawdzania:=true;
                             Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+ text+ ' : Nie mo¿e byæ apostrofu w nazwie zmiennej czytanej przez read/readln');
                          end;
                      end;
                   //sprawdzamy czy w tekœcie nie ma za du¿o apostrofów
                   for do_petli:=1 to length(text) do
                       if (text[do_petli]='''') then
                           ilosc_apostrofow:=ilosc_apostrofow+1;
                   if (ilosc_apostrofow<>0) and (ilosc_apostrofow>2) then
                   begin
                       ok:=false;
                       koniec_sprawdzania:=true;
                       Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+ text+ ' : Wewn¹trz tekstu jest za du¿o apostrofów');
                   end;
                   //sprawdzamy czy tekst nie jest liczb¹
                   tymczasowe_spr:=0;
                   try
                       probna_calkowita:=StrToInt(text);
                       ok:=false;
                       koniec_sprawdzania:=true;
                       Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+ text+ ' : Wewn¹trz nie mo¿e byæ liczby ca³kowitej');
                       tymczasowe_spr:=1;
                   except
                   end;

                   if tymczasowe_spr=0 then
                   try
                       probna_real:=StrToFloat(text);
                       ok:=false;
                       koniec_sprawdzania:=true;
                       Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : '+ text+ ' : Wewn¹trz nie mo¿e byæ liczby zmiennoprzecinkowej');
                   except
                   end;

                   if koniec_sprawdzania=false then
                   begin
                      //czytamy nawias zamykajacy
                      readln(f,text);
                      if pos('###linia',text)=1 then
                      repeat
                         linia:=StrToInt(copy(text,9,length(text)-8));
                         readln(f,text);
                      until pos('###linia',text)=0;
                      if text<>')' then
                      begin
                          ok:=false;
                          koniec_sprawdzania:=true;
                          Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) + ' : Oczekiwano nawiasu zamykaj¹cego ")" - brak go - zamiast niego jest : '+text);
                      end;
                      if koniec_sprawdzania=false then
                      begin

                      //czytamy œrednik
                         readln(f,text);
                         if pos('###linia',text)=1 then
                         repeat
                            linia:=StrToInt(copy(text,9,length(text)-8));
                            readln(f,text);
                         until pos('###linia',text)=0;
                         if text<>';' then
                         begin
                             ok:=false;
                             koniec_sprawdzania:=true;
                             Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Brak œrednika na koñcu instrukcji');
                         end;
                      end
                   end;

                end;

            end;
        end;
    end;
    closefile(f);
end;

procedure TForm1.sprawdz_sredniki_przed_if_i_for;
var
    f:TextFile;
    text,zapamietaj_zmienna:string;
    linia:integer;
    koniec_sprawdzania:boolean;
begin
    linia:=0;
    zapamietaj_zmienna:='';
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    while (not eof(f)) and (koniec_sprawdzania=false) do
    begin
        readln(f,text);
        if pos('###linia',text)=1 then
        repeat
             linia:=StrToInt(copy(text,9,length(text)-8));
             readln(f,text);
        until pos('###linia',text)=0;
        if (text<>'[###pêtla###]') and (text<>'[###instrukcja warunkowa###]') then
            zapamietaj_zmienna:=text
        else
        begin
            if (zapamietaj_zmienna<>';') and (zapamietaj_zmienna<>'begin') and (zapamietaj_zmienna<>'do') and (zapamietaj_zmienna<>'then') then
            begin
                ok:=false;
                koniec_sprawdzania:=true;
                Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia) +' : Brak œrednika na koñcu instrukcji - zamiast œrednika jest : '+text);
            end;
        end;
   end;
   closefile(f);
end;

procedure TForm1.sprawdz_sredniki_przed_operacja_arytmetyczna;
var
    f:TextFile;
    text,zapamietaj_zmienna,pomoc:string;
    linia:integer;
    koniec_sprawdzania:boolean;
begin
    linia:=0;
    zapamietaj_zmienna:='';
    pomoc:='';
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    while (not eof(f)) and (koniec_sprawdzania=false) do
    begin
        readln(f,text);
        if pos('###linia',text)=1 then
        repeat
             linia:=StrToInt(copy(text,9,length(text)-8));
             readln(f,text);
        until pos('###linia',text)=0;
        if (text<>'[###operacja arytmatyczna###]') then
        begin
            zapamietaj_zmienna:=pomoc;
            pomoc:=text;
        end
        else
        begin
            if (zapamietaj_zmienna<>';') and (zapamietaj_zmienna<>'begin') and (zapamietaj_zmienna<>'do') and (zapamietaj_zmienna<>'then') then
            begin
                ok:=false;
                koniec_sprawdzania:=true;
                Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia) +' : Brak œrednika na koñcu instrukcji - zamiast œrednika jest : '+text);
            end;
        end;
   end;
   closefile(f);
end;

procedure TForm1.sprawdz_czy_znaki_arytmetyczne_sa_bez_przypisania;
var
    f:TextFile;
    text:string;
    linia:integer;
    koniec_sprawdzania:boolean;
    czy_jest_przypisanie:boolean;
begin
    linia:=0;
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    czy_jest_przypisanie:=false;
    while (not eof(f)) and (koniec_sprawdzania=false) do
    begin
        readln(f,text);
        if pos('###linia',text)=1 then
        repeat
             linia:=StrToInt(copy(text,9,length(text)-8));
             readln(f,text);
        until pos('###linia',text)=0;
        if (text=':=') or (text='if') or (text='while') then
            czy_jest_przypisanie:=true;
            //zmiana zwi¹zana z minusami
        if (czy_jest_przypisanie=true) and ((text=';') or (text='then') or (text='do')) then
            czy_jest_przypisanie:=false;
        if (czy_jest_przypisanie=false) and ((text='+') or (text='-') or (text='*') or (text='/') or (text='^')) then
        begin
            ok:=false;
            koniec_sprawdzania:=true;
            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Wystêpuje operator arytmetyczny bez wczeœniejszego wyst¹pienia operatora przypisania');
        end;
    end;
    closefile(f);
end;

procedure TForm1.sprawdz_czy_operatory_porownania_sa_poza_if_then;
var
    f:TextFile;
    text:string;
    linia:integer;
    koniec_sprawdzania:boolean;
    czy_jest_wewnatrz_if:boolean;
begin
    linia:=0;
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    reset(f);
    koniec_sprawdzania:=false;
    czy_jest_wewnatrz_if:=false;
    while (not eof(f)) and (koniec_sprawdzania=false) do
    begin
        readln(f,text);
        if pos('###linia',text)=1 then
        repeat
             linia:=StrToInt(copy(text,9,length(text)-8));
             readln(f,text);
        until pos('###linia',text)=0;
        if text='if' then
            czy_jest_wewnatrz_if:=true;
        if (czy_jest_wewnatrz_if=true) and (text='then') then
            czy_jest_wewnatrz_if:=false;
        if (czy_jest_wewnatrz_if=false) and ((text='<>') or (text='<') or (text='>') or (text='=') or (text='>=') or (text='<=')) then
        begin
            ok:=false;
            koniec_sprawdzania:=true;
            Memo1.Lines.Add('B³¹d!!!: Linia '+ IntToStr(linia+1) +' : Wystêpuje operator porównania na zewn¹trz s³ów "if" i "then"');
        end;
    end;
    closefile(f);
end;

procedure TForm1.przygotuj_do_uruchomienia;
var
    f,g:TextFile;
    text:string;
begin
    assignfile(f,ExtractFilePath(Application.ExeName)+'pomoc25.txt');
    assignfile(g,ExtractFilePath(Application.ExeName)+'pomoc26.txt');
    reset(f);
    rewrite(g);
    while not eof(f) do
    begin
        readln(f,text);
        if      (text<>'[###pocz¹tek programu###]') and (text<>'[###deklaracja zmiennych###]') and (text<>'[###pocz¹tek bloku###]') and (text<>'[###operacja przypisania###]')
            and (text<>'[###instrukcja warunkowa###]') and (text<>'[###koniec bloku###]') and (text<>'[###koniec programu###]')  and (text<>'[###pêtla###]') and (pos('###linia',text)=0)
        then
           writeln(g,text);
    end;
    closefile(f);
    closefile(g);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if SynEdit1.Modified then
          if Application.MessageBox(PChar('Dokument '+ Form1.Caption +' zosta³ zmieniony. Czy zapisaæ zmiany?'),'Czy zapisaæ zmiany?',MB_OkCancel+MB_DefButton1+MB_IconQuestion)=IdOk then
             Form2.Zapisz1Click(Sender);
    action:=caFree;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    znaki_dopuszczalne:=['a'..'z', 'A'..'Z', '0'..'9', '_'];
    cyfry:=['0'..'9'];
end;

end.

