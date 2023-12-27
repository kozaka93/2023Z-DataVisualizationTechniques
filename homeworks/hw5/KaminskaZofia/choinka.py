import numpy as np
import matplotlib.pyplot as plt

def generate_cone_spiral(radius, height, num_points=100, rotation=0):
    t = np.linspace(0, 4 * np.pi, num_points)
    x = radius * np.cos(t + rotation) * (1 - t / (4 * np.pi))
    y = radius * np.sin(t + rotation) * (1 - t / (4 * np.pi))
    z = height * t / (4 * np.pi)
    return x, y, z

def clear_color(ax):
    ax.w_xaxis.set_pane_color((0, 0, 0, 0))
    ax.w_yaxis.set_pane_color((0, 0, 0, 0))
    ax.w_zaxis.set_pane_color((0, 0, 0, 0))
    ax.xaxis.pane.fill = False
    ax.yaxis.pane.fill = False
    ax.zaxis.pane.fill = False

fig = plt.figure(figsize=(8, 8))
ax = fig.add_subplot(111, projection='3d')

num_spirals = 6

for i in range(num_spirals):
    radius = 0.5
    height = 40
    rotation = i * (2 * np.pi / num_spirals)
    x, y, z = generate_cone_spiral(radius, height, rotation=rotation)
    if i % 2 == 0:
        color = 'green'
    else:
        color = 'gold'
    ax.scatter(x, y, z, color=color, s=20, marker='*')

ax.scatter([0], [0], [41.5], color='white', s=500, marker='*')

num_snowflakes = 150
snowflake_radius = 0.1
snowflake_color = 'white'
for _ in range(num_snowflakes):
    x_snow = np.random.uniform(-1.5, 1.5)
    y_snow = np.random.uniform(-1.5, 1.5)
    z_snow = np.random.uniform(0, 45)
    ax.scatter(x_snow, y_snow, z_snow, color=snowflake_color, s=2, marker='*')

u = np.linspace(0, 2 * np.pi, 100)
v = np.linspace(0, np.pi / 2, 50)
u, v = np.meshgrid(u, v)
x_sphere = 2 * np.cos(u) * np.sin(v)
y_sphere = 2 * np.sin(u) * np.sin(v)
z_sphere = 50 * np.cos(v)
ax.plot_surface(x_sphere, y_sphere, z_sphere, color='white', alpha=0.2, edgecolor='none')

ax.text(0, 0, -15, "Merry Christmas", color='red', fontsize=40, ha='center', va='center', bbox=dict(facecolor='white', alpha=0), fontname='Times New Roman')

ax.set_facecolor('black')

ax.set_xlim([-1.5, 1.5])
ax.set_ylim([-1.5, 1.5])
ax.set_zlim([0, 45])

ax.grid(False)

clear_color(ax)

for angle in range(0, 360):
    ax.view_init(azim=angle, elev=10)
    plt.draw()
    plt.pause(0.01)

plt.show()

