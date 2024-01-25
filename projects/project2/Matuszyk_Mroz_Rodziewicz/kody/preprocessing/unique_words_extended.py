import pandas as pd
import spacy
import re

# moze byc problem z spacy.load("pl_core_news_sm") trzeba w anaconda prompt, (a przynajmniej ja tak zrobilem) wpisac:
# python -m spacy download pl_core_news_sm

# Wczytywanie danych
df = pd.read_csv("C:/Users/wojow/Downloads/PROJEKT_TWD/master_count.csv", usecols=[2, 4, 9], dtype={2: str, 4: str, 9: 'int64'})

# Filtruj tylko wiadomości od "Kacper Rodziewicz"
df = df[df['sender_name'].str.contains('Kacper Rodziewicz', na=False)]

# Konwersja timestamp_ms na datę i rok
df['date'] = pd.to_datetime(df['timestamp_ms'], unit='ms')
df['year'] = df['date'].dt.year

nlp = spacy.load("pl_core_news_sm")

# Słownik do przechowywania unikalnych słów dla każdego roku
unique_words_by_year = {year: set() for year in range(df['year'].min(), 2024)}

# Przetwarzanie wiadomości
for _, row in df.iterrows():
    message = row['content']
    year = row['year']

    # Oczyszczanie tekstu
    clean_message = re.sub(r'\W+', ' ', str(message)).lower()
    doc = nlp(clean_message)

    # Dodajemy tylko lematy do naszego zbioru unikalnych słów
    words = {token.lemma_ for token in doc if not token.is_punct and not token.is_stop}

    # Aktualizowanie zbioru unikalnych słów dla danego roku i wszystkich kolejnych lat
    for y in range(year, 2024):
        unique_words_by_year[y].update(words)

# Obliczanie sumy unikalnych słów do danego roku włącznie i wyświetlanie wyników
for year in range(df['year'].min(), 2024):
    print(f"Rok: {year}, Suma unikalnych słów do tego roku włącznie: {int(0.975*len(unique_words_by_year[year]))}")
# te *0.975 dlatego ze ta biblioteka do nlp nie dziala idealnie tak raz na 40 (lekka reka liczac) nieslusznie jest dodawane slowo
