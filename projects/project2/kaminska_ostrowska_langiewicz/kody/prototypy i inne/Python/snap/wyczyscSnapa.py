# skrypt czysci jsonowy plik ze Snapchata

import json
import os


def generate_alphabet_letters():
    return [chr(i) for i in range(ord('A'), ord('Z') + 1)]


def generate_user_names():
    alphabet_letters = generate_alphabet_letters()
    user_names = [letter for letter in alphabet_letters]

    for letter1 in alphabet_letters:
        for letter2 in alphabet_letters:
            user_names.append(letter1 + letter2)

    return user_names


def modify_and_save_json(input_path, output_path):
    user_names = generate_user_names()

    with open(input_path, 'r', encoding='utf-8') as file:
        data = json.load(file)

    new_data = {}
    for i, (user, entries) in enumerate(data.items()):
        new_user = user_names[i]
        new_data[new_user] = entries
        for entry in new_data[new_user]:
            entry['From'] = ""
            entry['Content'] = ""
            entry['Conversation Title'] = ""
        print(f"Wyczyszczono pola dla użytkownika: {user}")

    with open(output_path, 'w', encoding='utf-8') as file:
        json.dump(new_data, file, indent=2, ensure_ascii=False)


if __name__ == "__main__":
    # sciezke do pliku brudne, ktorym jest json ze wszystkimi wiadomosciami
    input_file_path = r"C:\twd_proj2\poufne_dane\snapchat\chat_history.json"
    output_file_path = os.path.join(os.path.dirname(input_file_path), "czyste.json")

    modify_and_save_json(input_file_path, output_file_path)
    print(f"Plik został zmodyfikowany i zapisany jako {output_file_path}")

