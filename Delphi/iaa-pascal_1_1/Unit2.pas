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

unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, jpeg, ExtCtrls, StdCtrls, Buttons, ToolWin, ComCtrls, SynEditPrint,
  ImgList, ShellApi, XPMenu;

type
  TForm2 = class(TForm)
    MainMenu1: TMainMenu;
    Plik1: TMenuItem;
    Nowy1: TMenuItem;
    Kompiluj1: TMenuItem;
    Kompiluj2: TMenuItem;
    Okno1: TMenuItem;
    Kaskada1: TMenuItem;
    Ssiadujaco1: TMenuItem;
    Uporzdkuj1: TMenuItem;
    Pomoc1: TMenuItem;
    Oprogramie1: TMenuItem;
    Pomoc2: TMenuItem;
    Edycja1: TMenuItem;
    Cofnijpisanie1: TMenuItem;
    Ponwpisanie1: TMenuItem;
    N1: TMenuItem;
    ToolBar1: TToolBar;
    Kopiuj1: TMenuItem;
    Wytnij1: TMenuItem;
    Wklej1: TMenuItem;
    N2: TMenuItem;
    Zaznaczwszystko1: TMenuItem;
    SynEditPrint1: TSynEditPrint;
    N3: TMenuItem;
    Otwrz1: TMenuItem;
    Drukuj1: TMenuItem;
    Ustawieniawydruku1: TMenuItem;
    N4: TMenuItem;
    Zamknij1: TMenuItem;
    PrinterSetupDialog1: TPrinterSetupDialog;
    OpenDialog1: TOpenDialog;
    N5: TMenuItem;
    Zapisz1: TMenuItem;
    Zapiszjako1: TMenuItem;
    ImageList1: TImageList;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton16: TToolButton;
    SaveDialog1: TSaveDialog;
    ToolButton15: TToolButton;
    ToolButton17: TToolButton;
    Licencja1: TMenuItem;
    N6: TMenuItem;
    XPMenu1: TXPMenu;
    procedure Nowy1Click(Sender: TObject);
    procedure Kompiluj2Click(Sender: TObject);
    procedure Kaskada1Click(Sender: TObject);
    procedure Ssiadujaco1Click(Sender: TObject);
    procedure Uporzdkuj1Click(Sender: TObject);
    procedure Oprogramie1Click(Sender: TObject);
    procedure Cofnijpisanie1Click(Sender: TObject);
    procedure Ponwpisanie1Click(Sender: TObject);
    procedure Kopiuj1Click(Sender: TObject);
    procedure Wytnij1Click(Sender: TObject);
    procedure Wklej1Click(Sender: TObject);
    procedure Zaznaczwszystko1Click(Sender: TObject);
    procedure Zamknij1Click(Sender: TObject);
    procedure Drukuj1Click(Sender: TObject);
    procedure Ustawieniawydruku1Click(Sender: TObject);
    procedure Otwrz1Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure ToolButton12Click(Sender: TObject);
    procedure ToolButton13Click(Sender: TObject);
    procedure ToolButton14Click(Sender: TObject);
    procedure Pomoc2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Zapisz1Click(Sender: TObject);
    procedure Zapiszjako1Click(Sender: TObject);
    procedure ToolButton17Click(Sender: TObject);
    procedure Licencja1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses Unit1, Unit3, Unit4, Unit5;

{$R *.DFM}

procedure TForm2.Nowy1Click(Sender: TObject);
var
    Form1:TForm1;
begin
    Form1:=TForm1.Create(Self);
    Form1.Caption:='Nowy';
    Form1.Show;
end;

procedure TForm2.Kompiluj2Click(Sender: TObject);
var
    il:integer;
begin
    if ((Form2.MDIChildren[0] as TForm1)<>nil) and ((Form2.MDIChildren[0] as TForm1).SynEdit1.Lines.Count>0) then
    begin
        ilosc_przeczytanych_wierszy:=0;
        //wyczyszczenie pola z kodem
        (Form2.MDIChildren[0] as TForm1).Memo1.Lines.Clear;
        //rozbijanie na leksemy
        for il:=0 to 25 do
           (Form2.MDIChildren[0] as TForm1).usun_plik(ExtractFilePath(Application.ExeName)+'pomoc'+IntToStr(il)+'.txt');
        (Form2.MDIChildren[0] as TForm1).rozdziel_wg_srednikow((Form2.MDIChildren[0] as TForm1).SynEdit1);
        (Form2.MDIChildren[0] as TForm1).usuniecie_komentarzy;
        (Form2.MDIChildren[0] as TForm1).zamien_na_male_litery;
        (Form2.MDIChildren[0] as TForm1).rozdzielenie_wg_slow_kluczowych;
        (Form2.MDIChildren[0] as TForm1).usuniecie_spacji;
        (Form2.MDIChildren[0] as TForm1).rozdzielenie_wg_znakow_matematycznych;
        (Form2.MDIChildren[0] as TForm1).dodanie_opisu;
        (Form2.MDIChildren[0] as TForm1).usun_instrukcje_puste;
        (Form2.MDIChildren[0] as TForm1).usun_wszystko_po_koncu_programu;
        for il:=0 to 24 do
           (Form2.MDIChildren[0] as TForm1).usun_plik(ExtractFilePath(Application.ExeName)+'pomoc'+IntToStr(il)+'.txt');
        //sprawdzanie poprawnoœci kodu
        (Form2.MDIChildren[0] as TForm1).ok:=true;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_program;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_poczatek_programu;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_var;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_deklaracje_zmiennych;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_begin_i_end;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_sredniki_po_end;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).deklaruj_zmienne;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_ilosc_begin_i_end;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_if;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_ilosc_if_then;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_petle;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_ilosc_for_to;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_ilosc_for_do;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_dzialania_arytmetyczne;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_czy_znaki_arytmetyczne_sa_bez_przypisania;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_czy_operatory_porownania_sa_poza_if_then;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_czy_zadeklarowano_zmiene;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_sredniki_przed_i_po_oraz_poprwnosc_read_write;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_sredniki_przed_if_i_for;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_sredniki_przed_operacja_arytmetyczna;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_czy_nie_ma_pojedynczych_zmiennych;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_koniec_programu;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).sprawdz_zmienne_w_petli;   
        //uruchomienie
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           (Form2.MDIChildren[0] as TForm1).przygotuj_do_uruchomienia;
        if (Form2.MDIChildren[0] as TForm1).ok=true then
           Form3.ShowModal;
    end;
end;

procedure TForm2.Kaskada1Click(Sender: TObject);
begin
    Cascade;
end;

procedure TForm2.Ssiadujaco1Click(Sender: TObject);
begin
    Tile;
end;

procedure TForm2.Uporzdkuj1Click(Sender: TObject);
begin
    ArrangeIcons;
end;

procedure TForm2.Oprogramie1Click(Sender: TObject);
begin
    Form4.Showmodal
end;

procedure TForm2.Cofnijpisanie1Click(Sender: TObject);
begin
    if ((Form2.MDIChildren[0] as TForm1)<>nil) then
        (Form2.MDIChildren[0] as TForm1).SynEdit1.Undo;
end;

procedure TForm2.Ponwpisanie1Click(Sender: TObject);
begin
    if ((Form2.MDIChildren[0] as TForm1)<>nil) then
        (Form2.MDIChildren[0] as TForm1).SynEdit1.Redo;
end;

procedure TForm2.Kopiuj1Click(Sender: TObject);
begin
    if ((Form2.MDIChildren[0] as TForm1)<>nil) then
        (Form2.MDIChildren[0] as TForm1).SynEdit1.CopyToClipboard;
end;

procedure TForm2.Wytnij1Click(Sender: TObject);
begin
    if ((Form2.MDIChildren[0] as TForm1)<>nil) then
        (Form2.MDIChildren[0] as TForm1).SynEdit1.CutToClipboard;
end;

procedure TForm2.Wklej1Click(Sender: TObject);
begin
    if ((Form2.MDIChildren[0] as TForm1)<>nil) then
        (Form2.MDIChildren[0] as TForm1).SynEdit1.PasteFromClipboard;
end;

procedure TForm2.Zaznaczwszystko1Click(Sender: TObject);
begin
    (Form2.MDIChildren[0] as TForm1).SynEdit1.SelectAll;
end;

procedure TForm2.Zamknij1Click(Sender: TObject);
begin
    close;
end;

procedure TForm2.Drukuj1Click(Sender: TObject);
begin
    if ((Form2.MDIChildren[0] as TForm1)<>nil) then
    begin
        SynEditPrint1.SynEdit := (Form2.MDIChildren[0] as TForm1).SynEdit1;
        SynEditPrint1.DocTitle := (Form2.MDIChildren[0] as TForm1).Caption;
        SynEditPrint1.Print;
    end;
end;

procedure TForm2.Ustawieniawydruku1Click(Sender: TObject);
begin
    PrinterSetupDialog1.Execute;
end;

procedure TForm2.Otwrz1Click(Sender: TObject);
var
    Form1:TForm1;
begin
    if OpenDialog1.Execute then
    begin
       if ((Form2.MDIChildren[0] as TForm1)=nil) then
       begin
           Form1:=TForm1.Create(Self);
           Form1.Caption:='Nowy';
           Form1.Show;
       end;
       (Form2.MDIChildren[0] as TForm1).SynEdit1.Lines.LoadFromFile(OpenDialog1.FileName);
       (Form2.MDIChildren[0] as TForm1).Caption:=OpenDialog1.FileName;
    end;
end;

procedure TForm2.ToolButton1Click(Sender: TObject);
begin
    Nowy1Click(Sender);
end;

procedure TForm2.ToolButton2Click(Sender: TObject);
begin
    Otwrz1Click(Sender);
end;

procedure TForm2.ToolButton3Click(Sender: TObject);
begin
    Zapisz1Click(Sender);
end;

procedure TForm2.ToolButton4Click(Sender: TObject);
begin
    Zapiszjako1Click(Sender);
end;

procedure TForm2.ToolButton6Click(Sender: TObject);
begin
    Drukuj1Click(Sender);
end;

procedure TForm2.ToolButton8Click(Sender: TObject);
begin
    Kompiluj2Click(Sender);
end;

procedure TForm2.ToolButton11Click(Sender: TObject);
begin
    Oprogramie1Click(Sender);
end;

procedure TForm2.ToolButton10Click(Sender: TObject);
begin
    Wytnij1Click(Sender);
end;

procedure TForm2.ToolButton12Click(Sender: TObject);
begin
    Kopiuj1Click(Sender);
end;

procedure TForm2.ToolButton13Click(Sender: TObject);
begin
    Pomoc2Click(Sender)
end;

procedure TForm2.ToolButton14Click(Sender: TObject);
begin
    Wklej1Click(Sender);
end;

procedure TForm2.Pomoc2Click(Sender: TObject);
begin
    ShellExecute(Handle, 'open', PChar(ExtractFilePath(Application.ExeName)+'Untitled.hlp'), nil, nil, SW_Show);
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
var liczba:integer;
begin
    for liczba:=0 to Form2.MDIChildCount - 1 do
       if (Form2.MDIChildren[liczba] as TForm1).SynEdit1.Modified then
          if Application.MessageBox(PChar('Dokument '+ (Form2.MDIChildren[liczba] as TForm1).Caption +' zosta³ zmieniony. Czy zapisaæ zmiany?'),'Czy zapisaæ zmiany?',MB_OkCancel+MB_DefButton1+MB_IconQuestion)=IdOk then
             Zapisz1Click(Sender);
end;

procedure TForm2.Zapisz1Click(Sender: TObject);
begin
    if ((Form2.MDIChildren[0] as TForm1)<>nil) then
    begin
       (Form2.MDIChildren[0] as TForm1).SynEdit1.Modified:=False;
       if (Form2.MDIChildren[0] as TForm1).Caption<>'Nowy' then
          (Form2.MDIChildren[0] as TForm1).SynEdit1.Lines.SaveToFile((Form2.MDIChildren[0] as TForm1).Caption)
       else
          Zapiszjako1Click(Sender);
    end;
end;

procedure TForm2.Zapiszjako1Click(Sender: TObject);
begin
    if ((Form2.MDIChildren[0] as TForm1)<>nil) then
       if SaveDialog1.Execute then
       begin
          (Form2.MDIChildren[0] as TForm1).SynEdit1.Modified:=False;
          (Form2.MDIChildren[0] as TForm1).SynEdit1.Lines.SaveToFile(SaveDialog1.Filename);
          (Form2.MDIChildren[0] as TForm1).Caption:=SaveDialog1.Filename;
       end;   
end;

procedure TForm2.ToolButton17Click(Sender: TObject);
begin
    Form5.ShowModal;
end;

procedure TForm2.Licencja1Click(Sender: TObject);
begin
    Form5.ShowModal;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
    Form5.ShowModal;
end;

end.
