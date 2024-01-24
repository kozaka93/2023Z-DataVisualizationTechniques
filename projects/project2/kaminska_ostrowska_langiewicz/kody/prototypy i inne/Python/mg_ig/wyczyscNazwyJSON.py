import os
import shutil


def rename_files_with_prefix(source_folder_path):
    # Sprawdzamy, czy podana ścieżka źródłowa jest folderem
    if not os.path.isdir(source_folder_path):
        print(f"Podana ścieżka źródłowa '{source_folder_path}' nie jest folderem.")
        return

    # Tworzymy nowy folder o nazwie 'nowy' na tym samym poziomie co ścieżka źródłowa
    destination_folder_path = os.path.join(os.path.dirname(source_folder_path), 'czyste')

    # Sprawdzamy, czy podana ścieżka docelowa istnieje, jeśli nie, to tworzymy folder
    if not os.path.exists(destination_folder_path):
        os.makedirs(destination_folder_path)

    # Pobieramy listę plików w folderze źródłowym
    files = os.listdir(source_folder_path)

    # Licznik dla numerów porządkowych
    counter = 1

    # Iterujemy przez pliki w folderze źródłowym
    for file_name in files:
        # Pełna ścieżka do pliku źródłowego
        source_file_path = os.path.join(source_folder_path, file_name)

        # Sprawdzamy, czy to jest plik
        if os.path.isfile(source_file_path):
            # Nowa nazwa pliku
            new_file_name = f"konw_{counter:04d}"

            # Pełna ścieżka do pliku docelowego w nowym folderze
            destination_file_path = os.path.join(destination_folder_path, new_file_name + ".json")

            # Kopiujemy plik z nową nazwą do nowego foldera
            shutil.copy(source_file_path, destination_file_path)

            # Zwiększamy licznik
            counter += 1

            print(f"Skopiowano i zmieniono nazwę pliku: {source_file_path} -> {destination_file_path}")

    print(f"Utworzono nowy folder: {destination_folder_path}")


# path z folderem perlowe
path_to_source_folder = r'C:\Users\Zosia\Desktop\AAAPROJEKT2\poufne_dane\instagram\perlowe'
rename_files_with_prefix(path_to_source_folder)

