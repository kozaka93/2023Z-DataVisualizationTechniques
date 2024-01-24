# skrypt do czyszczenia tresci wiadomosci

import os
import json
import codecs


def process_json_files(source_path):
    # Tworzymy ścieżkę do folderu 'nowe'
    destination_path = os.path.join(os.path.dirname(source_path), 'ciemnoszare')

    # Sprawdzamy, czy folder docelowy istnieje, jeśli nie, to go tworzymy
    if not os.path.exists(destination_path):
        os.makedirs(destination_path)

    # Inicjalizujemy licznik wiadomości
    message_count = 0
    ileZ = 0
    ileA = 0
    ileF = 0

    # Przeszukujemy w głąb folderu źródłowego
    for root, dirs, files in os.walk(source_path):
        for file in files:
            file_path = os.path.join(root, file)

            # Sprawdzamy, czy plik jest w formacie JSON
            if file.lower().endswith('.json'):
                # Odczytujemy zawartość pliku JSON
                with codecs.open(file_path, 'r', encoding='utf-8-sig') as json_file:
                    data = json.load(json_file)

                # Przeprowadzamy modyfikacje w kopii danych
                modified_data = data.copy()
                for message in modified_data.get('messages', []):
                    if 'content' in message:
                        # Wstawiamy pusty string do pola 'content'
                        message['content'] = ''

                        # Zliczamy wystąpienia słowa 'content'
                        message_count += 1
                    if 'sender_name' in message:
                        if message['sender_name'] == "Zosia Kaminska":
                            ileZ += 1
                        if message['sender_name'] == "Anna Ostrowska":
                            ileA += 1
                        if message['sender_name'] == "Filip Langiewicz":
                            ileF += 1

                # Tworzymy ścieżkę do pliku w folderze 'nowe'
                destination_file_path = os.path.join(destination_path, file)

                # Zapisujemy zmodyfikowane dane do nowego pliku JSON
                with open(destination_file_path, 'w', encoding='utf-8') as new_json_file:
                    json.dump(modified_data, new_json_file, ensure_ascii=False, indent=2)

                print(f"Przetworzono: {file_path} -> {destination_file_path}")

    print(f"Zliczono wystąpień słowa 'content': {message_count}")
    print(f"Dostalam': {message_count - ileZ}")
    print(f"Z:{ileZ}")
    print(f"A:{ileA}")
    print(f"F:{ileF}")


# path z folderu brudne
path_to_search = r'C:\Users\Zosia\Desktop\AAAPROJEKT2\poufne_dane\instagram\brudne'
process_json_files(path_to_search)

