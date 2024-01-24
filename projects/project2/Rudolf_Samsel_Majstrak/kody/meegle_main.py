from shiny import ui
from shiny.ui import div, h1, HTML


def colored_letters(word):
    return HTML("".join(f'<span class="letter-{i}">{letter}</span>' for i, letter in enumerate(word)))


def main_page_fun():

    main_page = div(
        div(
            div(
                ui.img(src="left_arrow.png", id="small_button"),
                ui.img(src="right_arrow.png", id="small_button"),
                ui.img(src="reload.png", id="small_button"),
                id="fun_buttons"
            ),
            div(
                ui.input_text(label="", id="url", value="https://meegle.com/main_page"),
                id="url_div"
            ),
            id="header_div"
        ),
        div(
            div(
                div(
                    ui.input_action_button(id="menu_button", label="", style="height: 40px; width: 40px;"),
                    ui.img(src="menu.png", id="menu_image"),
                    id="menu_button_div"
                ),
                id="menu_div"
            ),
            id="lower_header_div"
        ),
        div(
            div(
                h1(colored_letters("Meegle"), id="logo"),
                id="logo_div"
            ),
            div(
                div(
                    ui.img(src="lupa.png", id="lupa"),
                    id="lupa_div"
                ),
                div(
                    ui.input_text(label="", id="search", value="Patrzymy w przeszłość."),
                    id="search_div"
                ),
                id="search_bar_div"
            ),
            ui.output_ui("search_result"),

            div(
                ui.input_action_button(id="locker_button", label="", style="height: 55px; width: 40px;"),
                HTML("<div class='hide'>Prosimy o wybranie profilu przed wyszukiwaniem informacji.</div>"),
                ui.img(src="padlock.png", id="padlock_graphics"),

                id="locker_button_div"
            ),
            div(
                id="popup_window_div"
            ),
            id="master_div"
        ),
        div(
            id="footer_div"
        ),
        id="body_div"
    )

    return main_page
