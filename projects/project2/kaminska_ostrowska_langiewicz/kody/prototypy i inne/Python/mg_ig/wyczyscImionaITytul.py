# skrypt do czyszczenia imion i tytulow konwersacji

import os
import json
import codecs


def clear_fields(source_path):
    # Tworzymy ścieżkę do folderu 'nowe'
    destination_path = os.path.join(os.path.dirname(source_path), 'biale')

    # Sprawdzamy, czy folder docelowy istnieje, jeśli nie, to go tworzymy
    if not os.path.exists(destination_path):
        os.makedirs(destination_path)

    # Inicjalizujemy liczniki
    name_count = 0
    sender_name_count = 0
    title_count = 0

    # Przeszukujemy w głąb folderu źródłowego
    for root, dirs, files in os.walk(source_path):
        for file in files:
            file_path = os.path.join(root, file)

            # Sprawdzamy, czy plik jest w formacie JSON
            if file.lower().endswith('.json'):
                # Otwieramy plik z użyciem opcji utf-8-sig
                with codecs.open(file_path, 'r', encoding='utf-8-sig') as json_file:
                    data = json.load(json_file)

                # Przeprowadzamy modyfikacje w kopii danych
                modified_data = data.copy()

                # Czyszczenie pola 'name' dla uczestników
                for participant in modified_data.get('participants', []):
                    if 'name' in participant:
                        participant['name'] = ''
                        name_count += 1

                # Czyszczenie pola 'sender_name' dla wiadomości
                for message in modified_data.get('messages', []):
                    if 'sender_name' in message:
                        message['sender_name'] = ''
                        sender_name_count += 1

                # Czyszczenie pola 'title'
                if 'title' in modified_data:
                    modified_data['title'] = ''
                    title_count += 1

                # Tworzymy ścieżkę do pliku w folderze 'nowe'
                destination_file_path = os.path.join(destination_path, file)

                # Zapisujemy zmodyfikowane dane do nowego pliku JSON
                with open(destination_file_path, 'w', encoding='utf-8') as new_json_file:
                    json.dump(modified_data, new_json_file, ensure_ascii=False, indent=2)

                print(f"Przetworzono: {file_path} -> {destination_file_path}")

    print(f"Zliczono wystąpień słowa 'name' w polu 'name': {name_count}")
    print(f"Zliczono wystąpień słowa 'name' w polu 'sender_name': {sender_name_count}")
    print(f"Zliczono wystąpień słowa 'title' w polu 'title': {title_count}")


# path z folderu ciemnoszare
path_to_search = r'C:\Users\Zosia\Desktop\AAAPROJEKT2\poufne_dane\instagram\ciemnoszare'
clear_fields(path_to_search)
