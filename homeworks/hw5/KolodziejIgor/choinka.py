import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

plt.style.use('dark_background')

# Constants
A_0 = 1  # Amplitude
b = 0.8  # Damping coefficient
m = 1  # Mass
ω = 5  # Angular frequency
ϕ = 0  # Phase angle

t = np.linspace(0, 10, 1000)
x = A_0 * np.exp(-b * t / (2 * m)) * np.cos(ω * t + ϕ)

baubles = np.append(np.linspace(0, 9, 9), [0.5, 3.9, 4.8, 1.8, 0.9, 0.3, 2.4])
bauble_colors = ['red', 'blue', 'orange', 'cyan', 'magenta']

n_snowflakes = 300
x_snow = np.random.uniform(-0.75, 0.75, n_snowflakes)
y_snow = np.random.uniform(0, 10, n_snowflakes)

plt.figure(figsize=(10, 8))
plt.axis('off')
plt.plot(x, t, 'g', linewidth=5)
plt.title('Merry Christmas!',
          fontsize=30,
          color='red',
          family='serif',
          fontweight='bold',
          bbox={'facecolor': 'yellow', 'alpha': 0.2, 'pad': 1},
          rotation=-45,
          x=0.8,
          y=0.4)

# Ornaments
plt.scatter(0.01, 10, marker='*', s=1500, color='yellow', zorder=3)
plt.scatter(A_0 * np.exp(-b * baubles / (2 * m)) * np.cos(ω * baubles + ϕ), baubles,
            marker='o',
            s=450,
            color=np.random.choice(bauble_colors, size=baubles.shape[0]),
            zorder=3)

# Snowflakes
scat_snow = plt.scatter(x_snow, y_snow, marker='o', s=10, color='white', zorder=4)


def update(i):
    y_snow[:] -= 0.01  # move snowflakes down
    y_snow[y_snow < 0] = 10  # if snowflake is below the frame, move it to the top
    scat_snow.set_offsets(np.c_[x_snow, y_snow])  # update positions

    if i % 30 == 0:  # update ornaments every 10 frames
        plt.scatter(A_0 * np.exp(-b * baubles / (2 * m)) * np.cos(ω * baubles + ϕ), baubles,
                    marker='o',
                    s=450,
                    color=np.random.choice(bauble_colors, size=baubles.shape[0]),
                    zorder=3)


ani = animation.FuncAnimation(plt.gcf(), update, interval=20, frames=500, cache_frame_data=False)
#ani.save('choinka.gif', writer='pillow', fps=30)
plt.show()
