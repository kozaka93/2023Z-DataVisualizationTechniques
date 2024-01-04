from turtle import *

speed(0)

penup()
goto(0, -230)
pendown()
color("lightskyblue")
begin_fill()
circle(210)
end_fill()

# Tree Trunk
penup()
goto(-15, -50)
pendown()
color("brown")
begin_fill()
for i in range(2):
    forward(30)
    right(90)
    forward(40)
    right(90)
end_fill()


y = -50
width = 200
height = 25

while width > 20:
    x = 0 - width / 2 
    color("green")
    penup()
    goto(x, y)
    pendown()
    begin_fill()
    
    forward(width)
    left(135)
    forward(35.355)
    left(45)
    
    forward(width - 2*height)
    left(45)
    forward(35.355)
    left(135)
    end_fill()    
    
    width = width - 30 
    y = y + height 

penup()
goto(-15, 115)
pendown()
color("yellow")
begin_fill()
for i in range(5):
    forward(30)
    right(144)
end_fill()

penup()
goto(-130, -150)
color("red")
write("MERRY CHRISTMAS", font=("Arial", 20, "bold"))

hideturtle()
mainloop()