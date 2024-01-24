# skrypt do stworzenia kopii jsonow

import os
import shutil


def copy_and_rename_json_files(source_path):
    # Sprawdzamy, czy folder źródłowy istnieje
    if not os.path.exists(source_path):
        print(f"Podany folder '{source_path}' nie istnieje.")
        return

    # Tworzymy ścieżkę do folderu 'czyste'
    destination_path = os.path.join(os.path.dirname(source_path), 'brudne')

    # Sprawdzamy, czy folder docelowy istnieje, jeśli nie, to go tworzymy
    if not os.path.exists(destination_path):
        os.makedirs(destination_path)

    # Przeszukujemy w głąb folderu źródłowego
    for root, dirs, files in os.walk(source_path):
        for file in files:
            file_path = os.path.join(root, file)

            # Sprawdzamy, czy plik jest w formacie JSON i czy znajduje się w odpowiednim podkatalogu
            if file.lower().endswith('.json'):
                # Rozdzielamy nazwę użytkownika i numer użytkownika z nazwy katalogu
                only_root = root.split("\\")[-1]
                username, user_num = only_root.split('_')
                only_file = file.split(".jso")[0]
                # Tworzymy nową nazwę pliku
                new_filename = f"{username}_{only_file.split('_')[-1]}"

                # Tworzymy ścieżkę do pliku w folderze 'czyste'
                destination_file_path = os.path.join(destination_path, new_filename + ".json")

                # Kopiujemy plik do folderu 'czyste' i zmieniamy jego nazwę
                shutil.copyfile(file_path, destination_file_path)
                print(f"Skopiowano: {file_path} -> {destination_file_path}")

# path ma folder wszystko, czyli wszystkie foldery w ktorych siedza jsony
path_to_search = r'C:\Users\Zosia\Desktop\AAAPROJEKT2\poufne_dane\instagram\inbox'
copy_and_rename_json_files(path_to_search)

