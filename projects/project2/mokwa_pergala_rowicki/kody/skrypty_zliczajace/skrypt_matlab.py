import os
import codecs
import pandas as pd
import pathlib
from datetime import datetime
from statistics import mean



def get_matlab_code_stats(matlab_file_path):
    # Check if the file exists
    if not os.path.exists(matlab_file_path):
        print(f"Error: File {matlab_file_path} not found.")
        return None

    code_length = 0
    num_lines = 0
    comments_lines = 0
    empty_lines = 0

    # Read the content of the MATLAB file
    with codecs.open(matlab_file_path, 'r', encoding='utf-8', errors='ignore') as file:
        for line in file:
            code_length += len(line) - 1
            num_lines += 1
            if line.startswith("%"):
                comments_lines += 1
            if line.isspace():
                empty_lines += 1

    code_length += 1

    return code_length, num_lines, comments_lines, empty_lines


def find_longest_word(matlab_file_path):
    # Check if the file exists
    if not os.path.exists(matlab_file_path):
        print(f"Error: File {matlab_file_path} not found.")
        return None

    # Read the content of the MATLAB file
    with open(matlab_file_path, 'r', encoding='utf-8', errors='ignore') as file:
        matlab_code = file.read()

    # Split the content into words
    words = matlab_code.split()

    # Find the longest word
    longest_word = max(words, key=len)

    return longest_word


def count_equal_sign_occurrences(matlab_file_path):
    # Check if the file exists
    if not os.path.exists(matlab_file_path):
        print(f"Error: File {matlab_file_path} not found.")
        return None

    list_of_strings = ["+","-" "*", "/", "=", ":", "./", ".*", "<=", ">=", "<", ">", "||", "&&", "~="]
    total_number_of_occurences = 0
    total_number_of_occurences_with_two_spaces = 0

    # Read the content of the MATLAB file
    with open(matlab_file_path, 'r', encoding='utf-8', errors='ignore') as file:
        for line in file:
            for string in list_of_strings:
                if string != "<=" and string != ">=" and string != "~=" and string != "./" and string != ".*":
                    total_number_of_occurences += line.strip().count(string)
                if string == "<=" or string == ">=":
                    total_number_of_occurences -= line.strip().count(string)
                total_number_of_occurences_with_two_spaces += line.strip().count(" " + string + " ")

    return total_number_of_occurences, total_number_of_occurences_with_two_spaces


def get_characters_per_line(matlab_file_path):
    # Check if the file exists
    if not os.path.exists(matlab_file_path):
        print(f"Error: File {matlab_file_path} not found.")
        return None

    # Read the content of the MATLAB file
    with open(matlab_file_path, 'r', encoding='utf-8', errors='ignore') as file:
        lines = file.readlines()

    # Get the number of characters in each line
    characters_per_line = [len(line) - 1 for line in lines]
    characters_per_line[-1] += 1

    return characters_per_line


def get_length_of_first_comment(matlab_file_path):
    # Check if the file exists
    if not os.path.exists(matlab_file_path):
        print(f"Error: File {matlab_file_path} not found.")
        return None

    # Read the content of the MATLAB file
    with open(matlab_file_path, 'r', encoding='utf-8', errors='ignore') as file:
        lines = file.readlines()

    # Find the first comment and get its length
    first_comment_length = 0
    has_started = False

    for line in lines:
        stripped_line = line.strip()

        # Check if the line is a comment (starts with '%')
        if stripped_line.startswith('%'):
            first_comment_length += 1
            has_started = True

        if not stripped_line.startswith('%') and has_started:
            break

    return first_comment_length
def data_modyfikacji_pliku(sciezka):
    if os.path.exists(sciezka):
        timestamp = os.path.getmtime(sciezka)
        date = datetime.fromtimestamp(timestamp)
        return date.strftime('%Y-%m-%d')
    return None


def count_lines_with_semicolon_and_conditions(matlab_file_path):
    # Check if the file exists
    if not os.path.exists(matlab_file_path):
        print(f"Error: File {matlab_file_path} not found.")
        return None

    # Read the content of the MATLAB file
    with open(matlab_file_path, 'r', encoding='utf-8', errors='ignore') as file:
        lines = file.readlines()

    # Count lines ending with ';' and meeting specified conditions
    lines_with_semicolon = 0
    lines_that_should_have_semicolon = 0

    for line in lines:

        # Check conditions for valid lines
        if line.strip().endswith(';') and not line.lstrip().startswith('%'):
            lines_with_semicolon += 1
        if not line.isspace() and not line.lstrip().startswith(('%', 'end', 'function', 'if', 'for', 'else')) and not line.strip().endswith("..."):
            lines_that_should_have_semicolon += 1

    return lines_with_semicolon, lines_that_should_have_semicolon



def main():
    # Example usage
    # s = ")"
    # matlab_file_path = 'data/dokladneWartosci.m'
    # length, num_lines, commentslines, emptylines = get_matlab_code_stats(matlab_file_path)
    # longest_word = find_longest_word(matlab_file_path)
    # equal_sign_count, equal_sign_with_spaces_count = count_equal_sign_occurrences(matlab_file_path)
    # characters_per_line = get_characters_per_line(matlab_file_path)
    #
    # first_comment_length = get_length_of_first_comment(matlab_file_path)
    #
    # lines_with_semicolon, lines_that_should_have_semicolon = count_lines_with_semicolon_and_conditions(matlab_file_path)
    #
    # if first_comment_length > 0:
    #     print(f"The length of the first comment is: {first_comment_length} lines.")
    # else:
    #     print("No comments found in the file.")
    #
    # print(f"The number of lines ending with ';' is: {lines_with_semicolon}.")
    # print(f"The number of lines that should end with ';' is: {lines_that_should_have_semicolon}")
    #
    # print("Number of characters per line:")
    # print(characters_per_line)
    # print(f"The number of '{s}' occurrences is: {equal_sign_count}.")
    # print(f"The number of ' {s} ' occurrences is: {equal_sign_with_spaces_count}.")
    # print(f"The longest word in the MATLAB code is: {longest_word}.")
    # print(f"The length of the MATLAB code is: {length} characters.")
    # print(f"The number of lines in the MATLAB code is: {num_lines}.")
    # print(f"The number of commented lines in the MATLAB code is: {commentslines}.")
    # print(f"The number of empty lines in the MATLAB code is: {emptylines}.")

    imie = "Sebastian"
    folder_path = "C:/Users/Sebastian/Desktop/Semestr 3/Metody numeryczne/Laboratorium"
    sciezki_do_plikow = list(pathlib.Path(folder_path).rglob('*.m'))
    data = []
    if os.path.exists(folder_path) and os.path.isdir(folder_path):
        for el in sciezki_do_plikow:
            length, num_lines, commentslines, emptylines = get_matlab_code_stats(el)
            longest_word = find_longest_word(el)
            sign_count, sign_with_spaces_count = count_equal_sign_occurrences(el)
            characters_per_line = get_characters_per_line(el)
            mean_characters_per_line = mean(characters_per_line)
            too_long_lines = 0
            for line in characters_per_line:
                if line > 75:
                    too_long_lines += 1
            first_comment_length = get_length_of_first_comment(el)

            lines_with_semicolon, lines_that_should_have_semicolon = count_lines_with_semicolon_and_conditions(
                el)
            data.append({"Imie": imie,
                         "Data modyfikacji": data_modyfikacji_pliku(el),
                         "Rozszerzenie": "m",
                         "Nazwa pliku": el.name,
                         "Liczba znaków:": length,
                         "Liczba wierszy": num_lines,
                         "Laczna dlugosc komentarzy": commentslines,
                         "Liczba pustych linii": emptylines,
                         "Najdłuższe słowo": longest_word,
                         "Długośc pierwszego komentarza": first_comment_length,
                         "Srednia liczba znaków w wierszu":mean_characters_per_line,
                         "Liczba wierszy zakonczonych srednikiem": lines_with_semicolon,
                         "Liczba wierszy, ktore powinny sie konczyc srednikiem": lines_that_should_have_semicolon,
                         "Liczba operatorów: +, -, *, /, =, :, ./, .*": sign_count,
                         "Liczba operatorow otoczonych spacjami" : sign_with_spaces_count,
                         "Liczba zbyt długich linii" : too_long_lines})
    df = pd.DataFrame(data)
    print(df)
    df.to_csv("Sebastian_matlab.csv")


if __name__ == "__main__":
    main()