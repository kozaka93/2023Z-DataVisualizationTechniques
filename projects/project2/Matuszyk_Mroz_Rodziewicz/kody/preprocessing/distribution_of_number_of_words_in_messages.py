import pandas as pd
import re

# Wczytywanie danych
df = pd.read_csv(("C:/Users/wojow/Downloads/PROJEKT_TWD/master_count.csv"), usecols=[2, 4], dtype={2: str, 4: str})

# Filtruj tylko wiadomości od określonej osoby
df = df[df['sender_name'].str.contains('Kacper Rodziewicz', na=False)]

# Przetwarzanie wiadomości
word_count = {}

for _, row in df.iterrows():
    message = row['content']

    # Oczyszczanie tekstu
    clean_message = re.sub(r'\W+', ' ', str(message)).lower()
    word_list = clean_message.split()

    # Zliczanie długości wiadomości
    length = len(word_list)
    if length > 0:
        if length in word_count:
            word_count[length] += 1
        else:
            word_count[length] = 1

# Zapisywanie do pliku w formacie dwóch kolumn
with open('output.txt', 'w') as f:
    for length, count in sorted(word_count.items()):
        f.write(f"{length} {count}\n")
