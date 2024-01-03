import turtle
import random
import rabit


FRAMES_COUNT = 300
WIDTH = 600
HEIGHT = 600

FONT_SIZE = 30

BALL_SIZE = 20
BALL_COLORS = ["orange", "blue", "red", "gold", "violet", "white"]
BALL_POS = [
    (-WIDTH / 30, WIDTH / 4),
    (3 * WIDTH / 40, WIDTH / 5),
    (-WIDTH / 20, WIDTH / 6),
    (WIDTH / 30, WIDTH / 9),
    (-WIDTH / 12, WIDTH / 30),
    (WIDTH / 12, WIDTH / 24),
    (-WIDTH / 9, -WIDTH / 20),
    (WIDTH / 8, -WIDTH / 15),
    (0, -WIDTH / 6),
    (-WIDTH / 6, -WIDTH / 6),
    (WIDTH / 5, -WIDTH / 7.5),
]

SNOW_SIZE = 7
SNOW_SPEED = 2
TURTLE_SPEED = 10
SNOW_NUM = 0.5

GROUND_LEVEL = -200

screen = turtle.Screen()
screen.setup(WIDTH, HEIGHT)
screen.title("Merry Christmas")
screen.bgcolor("sky blue")

tt = turtle.Turtle()
tt.hideturtle()
tt.penup()
tt.speed(4)


def make_triangle(x, y, size, outline, color="forest green", pensize=3):
    tt.penup()
    tt.setposition(x, y)
    tt.pensize(pensize)

    if outline:
        tt.pendown()
    else:
        tt.fillcolor(color)
        tt.begin_fill()

    tt.setposition(x + size, y - size)
    tt.setposition(x - size, y - size)
    tt.setposition(x, y)
    if not outline:
        tt.end_fill()


def make_ball(x, y, turtle, color="white", size=BALL_SIZE):
    turtle.penup()
    turtle.setposition(x, y)
    turtle.color(color)
    turtle.dot(size)


def move_snow(turtle):
    position = turtle.position()
    turtle.clear()
    make_ball(position[0], position[1] - SNOW_SPEED, turtle, size=SNOW_SIZE)


def snow_fall(list_of_snow):
    if random.random() <= SNOW_NUM:
        tt_ = turtle.Turtle()
        tt_.hideturtle()
        list_of_snow.append(tt_)
        make_ball(
            random.randint(-WIDTH / 2, WIDTH / 2), HEIGHT / 2, tt_, size=SNOW_SIZE
        )

    for tt_ in list_of_snow:
        move_snow(tt_)

        if tt_.position()[1] <= GROUND_LEVEL:
            tt_.clear()
            list_of_snow.remove(tt_)
            del tt_
    screen.update()


def make_tree_up():
    outline = True
    for _ in range(2):
        make_triangle(0, WIDTH / 3, WIDTH / 6, outline)
        make_triangle(0, WIDTH / 4, WIDTH / 4, outline)
        make_triangle(0, WIDTH / 8, WIDTH / 3, outline)
        outline = False


def make_tree_down(color="brown"):
    tt.penup()
    tt.color(color)
    tt.setposition(-WIDTH / 30, -WIDTH / 4.8)
    tt.pendown()
    tt.begin_fill()
    tt.setposition(WIDTH / 30, -WIDTH / 4.8)
    tt.setposition(WIDTH / 30, -3 * WIDTH / 8)
    tt.setposition(-WIDTH / 30, -3 * WIDTH / 8)
    tt.setposition(-WIDTH / 30, -WIDTH / 4.8)
    tt.end_fill()


def make_ground(level=GROUND_LEVEL, color="white"):
    tt.penup()
    tt.setposition(-WIDTH, level)
    tt.color(color)
    tt.begin_fill()
    tt.setposition(WIDTH, level)
    tt.setposition(WIDTH, -HEIGHT / 2)
    tt.setposition(-WIDTH, -HEIGHT / 2)
    tt.end_fill()
    screen.update()


def make_decorations(size=BALL_SIZE, colors=BALL_COLORS, positions=BALL_POS):
    for x, y in positions:
        make_ball(x, y, tt, color=random.choice(colors), size=size)
        screen.update()


def make_star(color="yellow", pos_x=WIDTH / 100, pos_y=HEIGHT / 2.7, size=20):
    tt.speed(2)
    tt.penup()
    tt.color(color)
    tt.goto(pos_x, pos_y)
    tt.begin_fill()
    tt.pendown()
    for _ in range(5):
        tt.forward(size)
        tt.right(120)
        tt.forward(size)
        tt.right(72 - 120)
    tt.end_fill()


def draw():
    make_tree_up()
    make_ground()
    make_tree_down()
    make_decorations()
    make_star()

    # snow is falling…
    list_of_snow = []
    screen.tracer(0)
    for _ in range(50):
        snow_fall(list_of_snow)

    tt.penup()
    tt.setposition(0, WIDTH / 2.4)
    tt.color("red")
    tt.write(
        "Merry Christmas", font=("Apple Chancery", FONT_SIZE, "bold"), align="center"
    )

    # for _ in range(FRAMES_COUNT - 50):
    while True:
        snow_fall(list_of_snow)


if __name__ == "__main__":
    # will crush yet will save eps frames
    # rabit.convert2gif(draw, 60, 60)
    draw()
