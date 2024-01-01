import math
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.animation import FuncAnimation

fig = plt.figure(figsize = (10, 10))
ax = fig.add_subplot(projection = '3d')

def update(frame):
    ax.clear()

    ax.set_title('Merry Christmas!', color='red', fontsize = 30)

    # Zielone dekoracje
    for s in reversed(range(0, 300, 25)):
        z = s
        x = [math.cos(i) * (300 - s) for i in range(1, 360, 5)]
        y = [math.sin(i) * (300 - s) for i in range(1, 360, 5)]
        ax.scatter(x, y, z, c = 'green')

        # Czerwone dekoracje
        if s % 50 == 0:
            z = s
            x = [math.cos(i) * (300 - s) for i in range(1, 360, 5)]
            y = [math.sin(i) * (300 - s) for i in range(1, 360, 5)]
            ax.scatter(x, y, z, c = 'red')

    # Gwiazdka na górze choinki
    ax.scatter(0, 0, 301, s = 250, c = 'blue', marker = '*')

    #Płatki śniegu
    z = np.random.randint(0, 300, 100)
    x = np.random.randint(-500, 500, 100)
    y = np.random.randint(-500, 500, 100)
    ax.scatter(x, y, z, c = 'gray')

    plt.xlim(-500, 500)
    plt.ylim(-500, 500)
    plt.axis('off')

    # Rotacja choinki
    ax.view_init(elev = 30, azim = frame)

# Ustawienie animacji
animation = FuncAnimation(fig, update, frames = np.arange(0, 360, 1), interval = 50)

animation.save('christmas_tree.gif', writer = 'pillow', fps = 20)
plt.close()