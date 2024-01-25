import pandas as pd

def wybierz_i_zapisz_kolumny(ścieżka_wejściowa, ścieżka_wyjściowa):
    # Wczytaj ramkę danych z pliku CSV
    df = pd.read_csv(ścieżka_wejściowa)

    # Wybierz tylko wybrane kolumny
    wybrane_kolumny = df[["reactions", "timestamp_ms", "sender_name"]]

    # Zapisz ramkę danych z wybranymi kolumnami do nowego pliku
    wybrane_kolumny.to_csv(ścieżka_wyjściowa, index=False)

if __name__ == "__main__":
    # Wprowadź ścieżki plików wejściowego i wyjściowego
    plik_wejściowy = r"C:\Users\wojow\Downloads\PROJEKT_TWD\master_count.csv"
    plik_wyjściowy = r"C:\Users\wojow\Downloads\PROJEKT_TWD\odczyt_danych_do_aplikacji\dane_kacper\hour_reactions_kacper.csv"

    # Wywołaj funkcję do wyboru i zapisu kolumn
    wybierz_i_zapisz_kolumny(plik_wejściowy, plik_wyjściowy)
