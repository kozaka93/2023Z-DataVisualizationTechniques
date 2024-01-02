from turtle import *
import random
"""
Ogolnie napisałem to na początku przy uzyciu turtle i nie przeczytałem, ze ma to byc przy uzyciu bibliotek do wizualizacji danych,
coś na szybko wymyśliłem, zeby działalo, ale załączam oba pliki, bo fajna choineczka mi z turtlem wyszła + sie czegos nauczyłem

"""
class ChristmasTree:
    speed(0)
    def __init__(self, height: int) -> None:
        self.height = height
        self.end_coordinates = []
    def Background(self) -> None:
        screensize(100,300)
        pensize(2)
        bgcolor('red')
    def MerryChristmas(self):
        penup()
        goto(0, 180)
        pendown()
        color("white")
        write("Merry Christmas!", align="center", font=("Arial", "36", "bold"))

    def makeTree(self, x: int, height:int) -> None:
        pensize(6)
        assert height > 0
        if x <= 0:
            end = pos()
            self.end_coordinates.append(end)
            return
        forward(height)
        self.makeTree(x-1, height*.8)
        right(120)
        self.makeTree(x-3, height*.6)
        right(120)
        self.makeTree(x-3, height*.6)
        right(120)
        backward(height)

    def makeStar(self) -> None:
        penup()
        goto(0,-130)
        pendown()
        left(90)
        forward(3*self.height)
        color("yellow", "yellow")
        begin_fill()
        left(126)
        for i in range(5):
            forward(self.height/5)
            right(144)
            forward(self.height/5)
            left(72)
        end_fill()
        right(126)

    def makeBaubles(self) -> None:
        hideturtle()
        colors = ["yellow", "light yellow", "cyan", "blue"]
        pensize(2)
        for tup in self.end_coordinates:
            penup()
            goto(*tup)
            pendown()
            if random.randint(0,13) < 1:
                begin_fill()
                col = random.choice(colors)
                color(col)
                circle(5)
                end_fill()

    def executor(self):
        self.Background()
        self.MerryChristmas()
        self.makeStar()
        
        color("green")
        backward(self.height*4.8)
        self.makeTree(12, self.height)
        self.makeBaubles()
        done()
def main():
    CT = ChristmasTree(75)
    CT.executor()
    print(CT.end_coordinates)
if __name__ == '__main__':
    main()