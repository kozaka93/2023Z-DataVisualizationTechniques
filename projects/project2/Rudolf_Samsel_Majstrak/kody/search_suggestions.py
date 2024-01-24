from googlesearch import search
import re


def give_search_result(user_input):
    search_suggestions = list(search(user_input, num=6, stop=6, pause=2))

    formatted_urls = []

    for search_item in search_suggestions:
        match = re.search(r'https://([^/]+)/.*?/([^/]+)', search_item)

        if match:
            url_part = match.group(1)

            if url_part not in formatted_urls:
                formatted_urls.append(url_part)

                if len(formatted_urls) == 6:
                    break

    return formatted_urls


# results = give_search_result('Henry Cavill')
#
# print(results)
