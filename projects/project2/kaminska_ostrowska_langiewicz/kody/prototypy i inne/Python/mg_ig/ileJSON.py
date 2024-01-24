# skrypt liczy JSONy by wiedziec
# w ktory folder wchodzic
# ewentualnie mozna sie dowiedziec z kim ma sie najwiecej wiadomosci

import os

folder_path = r'C:\Users\Zosia\Downloads\instagram-zosiek_cosiek-2023-12-05-TkmIUkQI\messages'
suma = 0

def policzJsony(item):
    count = 0
    path = os.path.join(folder_path, item)
    # print(path)
    count += count_json_files(path)
    return count

def count_json_files(path):
    json_count = 0

    # Przeszukiwanie katalogu
    for root, dirs, files in os.walk(path):
        for file in files:
            file_path = os.path.join(root, file)

            # Sprawdzenie, czy plik jest w formacie JSON
            if file.lower().endswith('.json'):
                json_count += 1

    print(f"Liczba json w pliku {path}: {json_count}\n")
    return json_count

def main():

    # Sprawdzanie istnienia folderu
    # Wyświetlanie zawartości folderu
    content = os.listdir(folder_path)
    count = 0
    for item in content:
        # print(item)
        count += policzJsony(item)
    print(f"w sumie jsonow {count}")


if __name__ == "__main__":
    main()