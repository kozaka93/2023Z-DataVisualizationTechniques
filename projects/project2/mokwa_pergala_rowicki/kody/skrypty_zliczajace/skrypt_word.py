import pathlib
from datetime import datetime
import docx2txt
import re
import os
import pandas as pd

# zliczanie wszystkich słów
def count_words(docx_path):
    result = docx2txt.process(docx_path)
    lista = result.split()
    return len(lista)


# zliczanie wszystkich kropek
def count_dots(docx_path):
    result = docx2txt.process(docx_path)
    return result.count(".")


# zliczanie wszystkich przecinkow
def count_przecinki(docx_path):
    result = docx2txt.process(docx_path)
    return result.count(",")


# zliczanie wszystkich wykrzyknikow
def count_wykrzyknik(docx_path):
    result = docx2txt.process(docx_path)
    return result.count("!")


# zliczanie wszystkich znkaow zapytania
def count_pytajnik(docx_path):
    result = docx2txt.process(docx_path)
    return result.count("?")


# zliczanie wszystkich myslnikow
def count_myslnik(docx_path):
    result = docx2txt.process(docx_path)
    return result.count("-")


# zliczanie wszystkich dwukropkow
def count_dwukropek(docx_path):
    result = docx2txt.process(docx_path)
    return result.count(":")


# zliczanie innych znaków interpunkcyjnych
def count_others(docx_path):
    result = docx2txt.process(docx_path)
    return result.count(";") + result.count("'") + result.count("...")


# zlozonosc zdan
def zlozonosc_zdan(docx_path):
    result = docx2txt.process(docx_path)
    wielokropek = result.count("...")
    liczba_zdan = result.count(".") - 2 * wielokropek - count_pytajnik(docx_path) - count_wykrzyknik(docx_path)
    if liczba_zdan == 0:
        return None
    return count_przecinki(docx_path) / liczba_zdan


def znajdz_najdluzsze_zdanie(docx_path):
    result = docx2txt.process(docx_path)
    lista_zdan = re.split(r'[.!?]', result)
    najwiecej_slow_zdanie = max(lista_zdan, key=lambda zdanie: len(re.findall(r'\b\w+\b', zdanie)))
    return najwiecej_slow_zdanie


# znajdowanie najdluzszego słowa w tekście
def znajdz_najdluzsze_slowo(docx_path):
    result = docx2txt.process(docx_path)
    lista = result.split()
    lista = [s.replace(',', '').replace('\n', '').replace(".", "").replace(":", "").replace("\"", "") for s in lista]
    if not lista:
        lista = [""]
    return max(lista, key=len)


def data_utworzenia_pliku(docx_path):
    if os.path.exists(docx_path):
        timestamp_utworzenia = os.path.getmtime(docx_path)
        data_utworzenia = datetime.fromtimestamp(timestamp_utworzenia)
        return data_utworzenia.strftime('%Y-%m-%d')
    else:
        return None


def main():
    folder_path = "C:/Users/Sebastian/Desktop/testa"
    sciezki_do_plikow = list(pathlib.Path(folder_path).rglob('*.docx'))

    imie = "Sebastian"
    data = []

    if os.path.exists(folder_path) and os.path.isdir(folder_path):
        for el in sciezki_do_plikow:
            data.append({"Imie": imie,
                         "Data utworzenia pliku": data_utworzenia_pliku(el),
                         "Rozszerzenie": "docx",
                         "Nazwa pliku": el.name,
                         "Ilosc słow:": count_words(el),
                         "Ilosc przecinkow": count_przecinki(el),
                         "Ilosc pytajnikow": count_pytajnik(el),
                         "Ilosc wykrzyknikow": count_wykrzyknik(el),
                         "Ilosc kropek": count_dots(el),
                         "Ilosc myslnikow": count_myslnik(el),
                         "Ilosc dwukropkow": count_dwukropek(el),
                         "Ilosc pozostalych znakow": count_others(el),
                         "Zlozonosc zdan": zlozonosc_zdan(el),
                         "Najdluzsze slowo": znajdz_najdluzsze_slowo(el)
                         })
    df = pd.DataFrame(data)
    # print(df)
    df.to_csv("Sebastian_word.csv", sep=',')


if __name__ == "__main__":
    main()

