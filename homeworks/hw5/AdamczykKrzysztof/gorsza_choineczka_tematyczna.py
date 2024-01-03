import math
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

plt.style.use('dark_background')
fig = plt.figure()
ax = fig.add_subplot(111, projection="3d")

k = 300
Z = [i for i in range(k)]
X = [math.cos(i / 5) * (k - i) for i in range(k)]
Y = [math.sin(i / 5) * (k - i) for i in range(k)]

sc = ax.scatter(X, Y, Z, c="green", marker="^")
ax.set_axis_off()

def update(frame):
    ax.view_init(azim=frame, elev=10)
    return sc,

ani = FuncAnimation(fig, update, frames=range(0, 360), interval=20, blit=True)

ani.save('taki_sobie_gif.gif', writer='pillow', fps=30)
