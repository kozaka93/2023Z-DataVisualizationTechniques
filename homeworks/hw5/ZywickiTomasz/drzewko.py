import math
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import animation

fig = plt.figure(figsize=(10, 10))
ax = fig.add_subplot(111, projection="3d")


def init():
    k = 300
    Z = [i for i in range(k)]
    X = [math.cos(i/5)*(k-i) for i in range(k)]
    Y = [math.sin(i/5)*(k-i) for i in range(k)]

    tree_sizes = np.linspace(80, 50, len(Z))
    ax.scatter(X, Y, Z, c="green", marker="^", s=tree_sizes)
    step = 2

    c = [np.random.rand(3) for _ in range(1, k, step)]
    Z = [i for i in range(1, k, step)]
    X = [math.cos(i/5+2)*(k-i+10) for i in range(1, k, step)]
    Y = [math.sin(i/5+2)*(k-i+10) for i in range(1, k, step)]

    ball_sizes = np.linspace(60, 5, len(Z))
    ax.scatter(X, Y, Z, c=c, marker="o", s=ball_sizes)

    plt.xlim(-500, 500)
    plt.ylim(-500, 500)
    ax.set_axis_off()
    ax.set_facecolor('#171717')  # tło
    return fig,


def init_snow():
    snow_x = np.random.uniform(-1500, 1500, 1000)
    snow_y = np.random.uniform(-1500, 1500, 1000)
    snow_z = np.random.uniform(0, 300, 1000)

    return snow_x, snow_y, snow_z


snow_positions = init_snow()
snow_positions_frames = [init_snow() for _ in range(8)]


def animate(f):
    fig.clear()
    ax = fig.add_subplot(111, projection="3d")

    k = 300
    Z = [i for i in range(k)]
    X = [math.cos(i/5+f/10)*(k-i) for i in range(k)]
    Y = [math.sin(i/5+f/10)*(k-i) for i in range(k)]

    tree_sizes = np.linspace(80, 30, len(Z))
    ax.scatter(X, Y, Z, c="green", marker="^", s=tree_sizes)

    step = 2
    c = [np.random.rand(3) for _ in range(1, k, step)]
    Z = [i for i in range(1, k, step)]
    X = [math.cos(i/5+2+f/10)*(k-i+10) for i in range(1, k, step)]
    Y = [math.sin(i/5+2+f/10)*(k-i+10) for i in range(1, k, step)]

    ball_sizes = np.linspace(70, 10, len(Z))
    ax.scatter(X, Y, Z, c=c, marker="o", s=ball_sizes)

    ax.set_axis_off()
    ax.set_facecolor('#171717')  # tło

    plt.xlim(-500, 500)
    plt.ylim(-500, 500)

    ax.scatter([-10], [0], [k+10], c="yellow", marker="*", s=800)  # Gwiazda

    global snow_positions, snow_positions_frames

    if f % 24 == 0:
        snow_positions = init_snow()
        snow_positions_frames = [init_snow() for _ in range(8)]

    current_frame_positions = snow_positions_frames[f % 24 // 3]

    snow_x, snow_y, snow_z = current_frame_positions

    ax.scatter(snow_x, snow_y, snow_z, c="white", marker=".", s=10)  # Śnieg

    return fig,


ani = animation.FuncAnimation(fig, animate, init_func=init,
                               frames=240, interval=60, blit=True)
ani.save("drzewko.gif", writer="pillow")

# https://www.youtube.com/shorts/uLCRVrqYEFw :))
