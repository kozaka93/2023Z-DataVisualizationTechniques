# skrypt zmienia znaki na polskie dla jednego pliku json

import codecs


def fix_unicode(text):
    # ą, ć, ę, ł, ń, ó, ś, ź, ż
    text = text.replace(r'\u00c4\u0085', 'ą')
    text = text.replace(r'\u00c4\u0084', 'Ą')  ##
    text = text.replace(r'\u00c4\u0087', 'ć')
    text = text.replace(r'\u00c4\u0086', 'Ć')  ##
    text = text.replace(r'\u00c4\u0099', 'ę')
    text = text.replace(r'\u00c4\u0098', 'Ę')  ##
    text = text.replace(r'\u00c5\u0082', 'ł')  # \u00c5\u0081
    text = text.replace(r'\u00c5\u0081', 'Ł')  # \u00c5\u0081
    text = text.replace(r'\u00c5\u0084', 'ń')
    text = text.replace(r'\u00c5\u0083', 'Ń')
    text = text.replace(r'\u00c3\u00b3', 'ó')
    text = text.replace(r'\u00c3\u00b2', 'Ó')  ##
    text = text.replace(r'\u00c5\u009b', 'ś')  # \u00c5\u009a
    text = text.replace(r'\u00c5\u009a', 'Ś')  # \u00c5\u009a
    text = text.replace(r'\u00c5\u00ba', 'ź')
    text = text.replace(r'\u00c5\u00b9', 'Ź')  ##
    text = text.replace(r'\u00c5\u00bc', 'ż')  # \u00c5\u00bb
    text = text.replace(r'\u00c5\u00bb', 'Ż')  # \u00c5\u00bb

    return text


# Otwórz plik oryginalny do odczytu
input_filename = r''
output_filename = r''
i = 1

# Otwórz plik docelowy do zapisu
with codecs.open(input_filename, 'r', encoding='utf-8') as input_file:
    with open(output_filename, 'w', encoding='utf-8') as output_file:
        # Odczytaj linijkę po linijce
        for line in input_file:
            # Zastosuj funkcję do każdego elementu
            line_with_fixed_unicode = fix_unicode(line)
            # Zapisz do pliku docelowego
            output_file.write(line_with_fixed_unicode)
            print(f"Przerobiono {i} linię")
            i += 1
