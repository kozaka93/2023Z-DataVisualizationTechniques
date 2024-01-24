import pathlib
import re
import os
import pandas as pd
from collections import Counter
from datetime import datetime

def zlicz_linie(tekst_z_pliku):
    liczba = 0
    for linia in tekst_z_pliku.splitlines():
        liczba += 1
    return liczba

def zlicz_wyrazy(tekst_z_pliku):
    liczba = 0
    for wyraz in tekst_z_pliku.split():
        liczba += 1
    return liczba

def zlicz_znaki(tekst_z_pliku):
    liczba = 0
    for znak in tekst_z_pliku:
        liczba += 1
    return liczba

def data_utworzenia_pliku(sciezka):
    # To nie ma sensu, bo pliki przeniesione z pendrivea mają odświeżoną datę utworzenia
    if os.path.exists(sciezka):
        timestamp = os.path.getctime(sciezka)
        date = datetime.fromtimestamp(timestamp)
        return date.strftime('%Y-%m-%d')
    return None

def data_modefikacji_pliku(sciezka):
    if os.path.exists(sciezka):
        timestamp = os.path.getmtime(sciezka)
        date = datetime.fromtimestamp(timestamp)
        return date.strftime('%Y-%m-%d')
    return None

def stworz_licznik_mniejszeLubRowne_wiekszeLubRowne_mniejsze_wieksze(tekst_z_pliku):
    licznik_wyrazow = Counter()
    for wyraz in tekst_z_pliku.split():
        licznik_wyrazow[wyraz] += 1
    licznik = Counter()
    licznik["<="] = 0
    licznik[">="] = 0
    licznik["<"] = 0
    licznik[">"] = 0
    licznik["=="] = 0
    for wyraz in licznik_wyrazow:
        # operator między liczbami [0-9 ][><]=?[0-9 ]
        # operator między liczbą, a wyrazem [0-9 ][><]=?
        # operator między wyraze,, a liczbą [><][0-9 ]=?
        # operator '<' i '>' może być częścią nawiasu
        if re.search("<=", wyraz):
            licznik["<="] += licznik_wyrazow[wyraz]
        elif re.search(">=", wyraz):
            licznik[">="] += licznik_wyrazow[wyraz]
        elif re.search("<[0-9 ]", wyraz):
            licznik["<"] += licznik_wyrazow[wyraz]
        elif re.search("[0-9 ]>", wyraz):
            licznik[">"] += licznik_wyrazow[wyraz]
        elif re.search("==", wyraz):
            licznik["=="] += licznik_wyrazow[wyraz]
    return licznik

def stworz_licznik_wyrazow_java(tekst_z_pliku):
    licznik_wyrazow = Counter()
    for wyraz in tekst_z_pliku.split():
        licznik_wyrazow[wyraz] += 1
    licznik = Counter()

    licznik["abstract"] = licznik_wyrazow["abstract"]
    licznik["assert"] = licznik_wyrazow["assert"]
    licznik["boolean"] = licznik_wyrazow["boolean"]
    licznik["break"] = 0
    for wyraz in licznik_wyrazow:
        if re.search("break", wyraz):
            licznik["break"] += licznik_wyrazow[wyraz]
    licznik["byte"] = licznik_wyrazow["byte"]
    licznik["case"] = 0
    for wyraz in licznik_wyrazow:
        if re.search("case", wyraz):
            licznik["case"] += licznik_wyrazow[wyraz]
    licznik["catch"] = 0
    for wyraz in licznik_wyrazow:
        if re.search("catch", wyraz):
            licznik["catch"] += licznik_wyrazow[wyraz]
    licznik["char"] = licznik_wyrazow["char"]
    licznik["class"] = licznik_wyrazow["class"]
    licznik["continue"] = licznik_wyrazow["continue"]
    licznik["default"] = licznik_wyrazow["default"]
    licznik["do"] = 0
    for wyraz in licznik_wyrazow:
        if re.search("do", wyraz):
            licznik["do"] += licznik_wyrazow[wyraz]
    licznik["double"] = licznik_wyrazow["double"]
    licznik["else"] = 0
    for wyraz in licznik_wyrazow:
        if re.search("else", wyraz):
            licznik["else"] += licznik_wyrazow[wyraz]
    licznik["enum"] = licznik_wyrazow["enum"]
    licznik["extends"] = licznik_wyrazow["extends"]
    licznik["final"] = licznik_wyrazow["final"]
    licznik["finally"] = licznik_wyrazow["finally"]
    licznik["for"] = 0
    for wyraz in licznik_wyrazow:
        if re.search("for", wyraz):
            licznik["for"] += licznik_wyrazow[wyraz]
    licznik["if"] = 0
    for wyraz in licznik_wyrazow:
        if re.search("if", wyraz):
            licznik["if"] += licznik_wyrazow[wyraz]
    licznik["implements"] = licznik_wyrazow["implements"]
    licznik["import"] = licznik_wyrazow["import"]
    licznik["instanceof"] = licznik_wyrazow["instanceof"]
    licznik["int"] = licznik_wyrazow["int"]
    licznik["interface"] = licznik_wyrazow["interface"]
    licznik["long"] = licznik_wyrazow["long"]
    licznik["native"] = licznik_wyrazow["native"]
    licznik["new"] = licznik_wyrazow["new"]
    licznik["package"] = licznik_wyrazow["package"]
    licznik["private"] = licznik_wyrazow["private"]
    licznik["protected"] = licznik_wyrazow["protected"]
    licznik["public"] = licznik_wyrazow["public"]
    licznik["return"] = licznik_wyrazow["return"]
    licznik["short"] = licznik_wyrazow["short"]
    licznik["static"] = licznik_wyrazow["static"]
    licznik["super"] = 0
    for wyraz in licznik_wyrazow:
        if re.search("super", wyraz):
            licznik["super"] += licznik_wyrazow[wyraz]
    licznik["switch"] = 0
    for wyraz in licznik_wyrazow:
        if re.search("switch", wyraz):
            licznik["switch"] += licznik_wyrazow[wyraz]
    licznik["synchronized"] = licznik_wyrazow["synchronized"]
    licznik["this"] = 0
    for wyraz in licznik_wyrazow:
        if re.search("this", wyraz):
            licznik["this"] += licznik_wyrazow[wyraz]
    licznik["throw"] = licznik_wyrazow["throw"]
    licznik["throws"] = licznik_wyrazow["throws"]
    licznik["transient"] = licznik_wyrazow["transient"]
    licznik["try"] = 0
    for wyraz in licznik_wyrazow:
        if re.search("try", wyraz):
            licznik["try"] += licznik_wyrazow[wyraz]
    licznik["void"] = licznik_wyrazow["void"]
    licznik["volatile"] = licznik_wyrazow["volatile"]
    licznik["while"] = 0
    for wyraz in licznik_wyrazow:
        if re.search("while", wyraz):
            licznik["while"] += licznik_wyrazow[wyraz]
    licznik["exports"] = licznik_wyrazow["exports"]
    licznik["module"] = licznik_wyrazow["module"]
    licznik["non-sealed"] = licznik_wyrazow["non-sealed"]
    licznik["open"] = licznik_wyrazow["open"]
    licznik["opens"] = licznik_wyrazow["opens"]
    licznik["permits"] = licznik_wyrazow["permits"]
    licznik["provides"] = licznik_wyrazow["provides"]
    licznik["record"] = licznik_wyrazow["record"]
    licznik["requires"] = licznik_wyrazow["requires"]
    licznik["sealed"] = licznik_wyrazow["sealed"]
    licznik["to"] = licznik_wyrazow["to"]
    licznik["transitive"] = licznik_wyrazow["transitive"]
    licznik["uses"] = licznik_wyrazow["uses"]
    licznik["var"] = licznik_wyrazow["var"]
    licznik["with"] = licznik_wyrazow["with"]
    licznik["yield"] = 0
    for wyraz in licznik_wyrazow:
        if re.search("yield", wyraz):
            licznik["yield"] += licznik_wyrazow[wyraz]
    licznik["true"] = 0
    for wyraz in licznik_wyrazow:
        if re.search("true", wyraz):
            licznik["true"] += licznik_wyrazow[wyraz]
    licznik["false"] = 0
    for wyraz in licznik_wyrazow:
        if re.search("false", wyraz):
            licznik["false"] += licznik_wyrazow[wyraz]
    licznik["null"] = 0
    for wyraz in licznik_wyrazow:
        if re.search("null", wyraz):
            licznik["null"] += licznik_wyrazow[wyraz]

    return licznik

def stworz_licznik_ilosci_linii_odstepu(tekst_z_pliku):
    licznik = Counter()
    ilosc_odstepow_po_kolei = 0
    for wyraz in tekst_z_pliku:
        if wyraz == "\n":
            ilosc_odstepow_po_kolei += 1
            licznik["Liczba linijek odstepu"] += 1
        else:
            if ilosc_odstepow_po_kolei == 0:
                licznik["Brak odstepu"] += 1
            else:
                licznik[f"{ilosc_odstepow_po_kolei} linijki odstepu"] += 1
                ilosc_odstepow_po_kolei = 0
    if ilosc_odstepow_po_kolei != 0:
        licznik[f"{ilosc_odstepow_po_kolei} linijki odstepu"] += 1
    return licznik

def stworz_statystyki_komentarzy(tekst_z_pliku):
    licznik = Counter()
    liczba_znakow_w_komantarzu_w_linii = 0
    liczba_komentarzy_w_linii = 0
    for linia in tekst_z_pliku.splitlines():
        for i in range(1, len(linia)):
            if linia[i] == '/' and linia[i-1] == '/':
                liczba_komentarzy_w_linii += 1
                liczba_znakow_w_komantarzu_w_linii += len(linia[i+1:])
                if '\n' in linia[i+1:]:
                    liczba_znakow_w_komantarzu_w_linii -= 1
    licznik['liczba_znakow_w_komantarzu_w_linii'] += liczba_znakow_w_komantarzu_w_linii
    licznik['liczba_komentarzy_w_linii'] += liczba_komentarzy_w_linii
    liczba_znakow_w_komentarzu_w_bloku = 0
    liczba_komentarzy_w_bloku = 0
    for i in range(1, len(tekst_z_pliku)):
        if tekst_z_pliku[i-1] == '/' and tekst_z_pliku[i] == '*':
            liczba_komentarzy_w_bloku += 1
            j = i+2
            while tekst_z_pliku[j-1] != '*' and tekst_z_pliku[j] != '/':
                if tekst_z_pliku[j] == '\n':
                    liczba_znakow_w_komentarzu_w_bloku -= 1
                liczba_znakow_w_komentarzu_w_bloku += 1
                j += 1
    licznik['liczba_znakow_w_komentarzu_w_bloku'] += liczba_znakow_w_komentarzu_w_bloku
    licznik['liczba_komentarzy_w_bloku'] += liczba_komentarzy_w_bloku
    return licznik

def stworz_statystyki_cudzyslowow(tekst_z_pliku):
    licznik = Counter()
    liczba_cudzyslowow_apostrofowych = 0
    liczba_cudzyslowow_pojedynczych = 0
    for znak in tekst_z_pliku:
        if znak == '"':
            liczba_cudzyslowow_apostrofowych += 1
        elif znak == "'":
            liczba_cudzyslowow_pojedynczych += 1
    licznik['liczba_cudzyslowow_apostrofowych'] += liczba_cudzyslowow_apostrofowych
    licznik['liczba_cudzyslowow_pojedynczych'] += liczba_cudzyslowow_pojedynczych
    return licznik

def znajdz_najdluzszy_wyraz(tekst_z_pliku):
    najdluzszy_wyraz = ''
    for wyraz in tekst_z_pliku.split():
        if not re.search("[\"'@\\/<>()\[\]!$%\^&*\-+=~`?;:{},.\0-9]", wyraz):
            if len(wyraz) > len(najdluzszy_wyraz):
                najdluzszy_wyraz = wyraz
                # print(najdluzszy_wyraz)
    return najdluzszy_wyraz

# def replace_all(repls, str):
#     return re.sub('|'.join(re.escape(key) for key in repls.keys()), lambda k: repls[k.group(0)], str)

def main():

    # # ----------------------------------------------------------------------
    # # Ścieżki, z których korzysta program
    # # To należy zmienić
    sciezka_do_folderu_z_plikami_java = 'C:/Users/Sebastian/Desktop/Semestr 3/Zaawansowane programowanie obiektowe i funkcyjne/Laboratorium'
    sciezka_do_miejsca_zapisu_plikow = 'C:/Users/Sebastian/Desktop/Semestr 3/Techniki wizualizacji danych/Projekt/PRO_2/skrypt_java'
    imie = 'Sebastian'
    # # ----------------------------------------------------------------------
    # # Tworzy listę ścieżek do wszystkich plików od podanej ścieżki, mających dane rozszerzenie
    sciezki_do_plikow = list(pathlib.Path(sciezka_do_folderu_z_plikami_java).rglob('*.java'))
    # # ----------------------------------------------------------------------
    data = []
    for sciezka_do_pliku in sciezki_do_plikow:
        with open(sciezka_do_pliku, 'r', encoding='utf-8') as plik:
            tekst_z_pliku = plik.read()
            data.append(dict({"Imie": imie,
                              "Rozszerzenie": "java",
                              "Nazwa_pliku": sciezka_do_pliku.name,
                              "Data_utworzenia": data_utworzenia_pliku(sciezka_do_pliku),
                              "Data_ostatniej_modefikacji": data_modefikacji_pliku(sciezka_do_pliku),
                              "Liczba_linii": zlicz_linie(tekst_z_pliku),
                              "Liczba_wierszy": zlicz_wyrazy(tekst_z_pliku),
                              "Liczba_znaków": zlicz_znaki(tekst_z_pliku)},
                             # **stworz_licznik_mniejszeLubRowne_wiekszeLubRowne_mniejsze_wieksze(tekst_z_pliku),
                             **stworz_licznik_wyrazow_java(tekst_z_pliku),
                             **stworz_statystyki_komentarzy(tekst_z_pliku),
                             # **stworz_statystyki_cudzyslowow(tekst_z_pliku),
                             **{"Najdluzszy_wyraz": znajdz_najdluzszy_wyraz(tekst_z_pliku)},
                             # **stworz_licznik_ilosci_linii_odstepu(tekst_z_pliku)
                             ))
        df = pd.DataFrame(data)
        df.to_csv(f"{sciezka_do_miejsca_zapisu_plikow}/{imie}_java.csv", sep=',')
    # # ----------------------------------------------------------------------

if __name__ == '__main__':
    main()