# skrypt czysci pola actor, thread_path i uri,
# w ktorych pojawiaja sie imiona i nazwiska

import os
import json
import codecs


def clear_actor_and_uri_fields(source_path):
    # Tworzymy ścieżkę do folderu 'perlowe'
    destination_path = os.path.join(os.path.dirname(source_path), 'perlowe')

    # Sprawdzamy, czy folder docelowy istnieje, jeśli nie, to go tworzymy
    if not os.path.exists(destination_path):
        os.makedirs(destination_path)

    # Inicjalizujemy liczniki
    actor_count = 0
    uri_count = 0
    thread_path_count = 0

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

                # Czyszczenie pola 'actor' w sekcji 'reactions'
                for message in modified_data.get('messages', []):
                    for reaction in message.get('reactions', []):
                        if 'actor' in reaction:
                            reaction['actor'] = ''
                            actor_count += 1

                    # Czyszczenie pola 'uri' w sekcji 'photos'
                    for photo in message.get('photos', []):
                        if 'uri' in photo:
                            photo['uri'] = ''
                            uri_count += 1

                    for gif in message.get('gifs', []):
                        if 'uri' in gif:
                            gif['uri'] = ''

                    for video in message.get('videos', []):
                        if 'uri' in video:
                            video['uri'] = ''

                    for audio_file in message.get('audio_files', []):
                        if 'uri' in audio_file:
                            audio_file['uri'] = ''

                    for filee in message.get('files', []):
                        if 'uri' in filee:
                            filee['uri'] = ''

                    if 'sticker' in message:
                        message['sticker'] = ''

                    if 'ip' in message:
                        message['ip'] = ""


                    # tylko do Instagrama {

                    if 'share' in message:
                        message['share'] = ''
                    # }
                if 'thread_path' in modified_data:
                    modified_data['thread_path'] = ''
                    thread_path_count += 1

                # Tworzymy ścieżkę do pliku w folderze 'nowe'
                destination_file_path = os.path.join(destination_path, file)

                # Zapisujemy zmodyfikowane dane do nowego pliku JSON
                with open(destination_file_path, 'w', encoding='utf-8') as new_json_file:
                    json.dump(modified_data, new_json_file, ensure_ascii=False, indent=2)

                print(f"Przetworzono: {file_path} -> {destination_file_path}")

    print(f"Zliczono wystąpień słowa 'actor' w polu 'actor': {actor_count}")
    print(f"Zliczono wystąpień słowa 'uri' w polu 'uri': {uri_count}")


# path z folderu biale
path_to_search = r'C:\Users\anost\OneDrive\Dokumenty\twd_proj2\poufne_dane\messenger\your_activity_across_facebook\messages\biale'
clear_actor_and_uri_fields(path_to_search)

