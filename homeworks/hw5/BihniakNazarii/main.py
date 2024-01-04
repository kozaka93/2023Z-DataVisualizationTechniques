import matplotlib.pyplot as plt
import numpy as np
from matplotlib.animation import FuncAnimation

def draw_triangle(ax, base_x, base_y, size, color):
    triangle = plt.Polygon([(base_x - size/2, base_y), (base_x, base_y + size), (base_x + size/2, base_y)], closed=True, edgecolor=color, facecolor=color)
    ax.add_patch(triangle)

def draw_rectangle(ax, base_x, base_y, width, height, color):
    rectangle = plt.Rectangle((base_x - width/2, base_y), width, height, edgecolor=color, facecolor=color)
    ax.add_patch(rectangle)

def draw_star(ax, base_x, base_y, size, color):
    star_points = np.array([
        [base_x, base_y + size],
        [base_x - 0.2 * size, base_y + 0.3 * size],
        [base_x - size, base_y + 0.3 * size],
        [base_x - 0.4 * size, base_y - 0.1 * size],
        [base_x - 0.6 * size, base_y - size],
        [base_x, base_y - 0.5 * size],
        [base_x + 0.6 * size, base_y - size],
        [base_x + 0.4 * size, base_y - 0.1 * size],
        [base_x + size, base_y + 0.3 * size],
        [base_x + 0.2 * size, base_y + 0.3 * size],
    ])
    star = plt.Polygon(star_points, closed=True, edgecolor=color, facecolor=color)
    ax.add_patch(star)

def draw_christmas_tree(levels):
    base_width = 1.0/4
    triangle_height = 1.5/4
    trunk_width = 0.4/4
    trunk_height = 0.8/4
    tree_color = "green"
    trunk_color = "brown"
    star_color = "yellow"

    fig, ax = plt.subplots(figsize=(6/10, 6/10))

    text = ax.text(0, 1.5, '', fontsize=30, color='#ff00ea', ha='center', va='center')

    def update(frame):
        text.set_text('202' + str(frame % 5 + 1))

    # rysujemy choinke
    for level in range(levels,0,-1):
        draw_triangle(ax, 0, level * triangle_height, base_width, tree_color)
        base_width += 0.3

    # Dodamy male trojkaciki dla ozdoby
    for level in range(1, levels+10):
        for _ in range(level):
            x = np.random.uniform(-base_width, base_width)
            y = np.random.uniform((level - 1) * triangle_height, level * triangle_height)-3
            draw_triangle(ax, x, y, 0.1, "red")

    for level in range(1, levels+10):
        for _ in range(level):
            x = np.random.uniform(-base_width, base_width)
            y = np.random.uniform((level - 1) * triangle_height, level * triangle_height)-3
            draw_triangle(ax, x, y, 0.1, "yellow")

    # Dodamy gwiazde
    draw_star(ax, 0, levels * triangle_height + trunk_height, 0.4/4, star_color)

    draw_rectangle(ax, 0, 0 * triangle_height+0.25, trunk_width+0.25, trunk_height, trunk_color)

    ani = FuncAnimation(fig, update, frames=range(1, 6), interval=500, repeat=True)
    ax.set_aspect('equal', adjustable='datalim')
    ax.axis('off')

    plt.show()

draw_christmas_tree(6)
