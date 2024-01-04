import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.animation import FuncAnimation


def create_christmas_tree():
    layers = []

    for layer in range(1, 20):
        num_points = layer * 3 + 1
        x_left = [20 - i * 0.3 + layer * 0.3 for i in range(num_points // 2)]
        x_right = [20 + i * 0.3 - layer * 0.3 for i in range(num_points // 2)]
        x = x_left + x_right
        y = [-1.5 * layer + np.cos(i * 0.5) * 0.5 for i in range(len(x))]
        color = '#006d2c' if layer % 2 == 0 else '#41ae76'
        layer_data = {'x': x, 'y': y, 'color': color}
        layers.append(layer_data)

    trunk = {
        'x': [20, 20.5, 19.6, 20.1, 20.4, 20.9, 20.25, 19.3, 21.2, 19.7, 20.7, 19, 21.5, 20.25, 19.4, 19.9, 20.5, 21.1,
              18.7, 21.8],
        'y': [-29.6, -29.6, -29.9, -29.9, -29.9, -29.9, -30.2, -30.2, -30.2, -30.5, -30.5, -30.5, -30.5, -30.5, -30.8,
              -30.8, -30.8, -30.8, -30.8, -30.8], 'color': '#8c510a'}

    star_1 = {
        'x': [20], 'y': [0.25], 'color': '#FFF200'
    }
    star_2 = {
        'x': [20.5], 'y': [-0.25], 'color': '#FFF201'
    }
    star_3 = {
        'x': [19.5], 'y': [-0.25], 'color': '#FFF202'
    }
    star_4 = {
        'x': [19.8], 'y': [-0.55], 'color': '#FFF203'
    }
    star_5 = {
        'x': [20.2], 'y': [-0.55], 'color': '#FFF204'
    }
    star_6 = {
        'x': [20], 'y': [-0.2], 'color': '#FFF205'
    }

    layers.append(trunk)

    layers.append(star_1)
    layers.append(star_2)
    layers.append(star_3)
    layers.append(star_4)
    layers.append(star_5)
    layers.append(star_6)

    dfs = [pd.DataFrame(layer) for layer in layers]

    df = pd.concat(dfs, ignore_index=True)

    return df


df = create_christmas_tree()
df = df.drop_duplicates(subset=['x', 'y'])

fig, ax = plt.subplots(figsize=(10, 8))
fig.patch.set_facecolor('black')
ax.set_facecolor('black')

scatter = ax.scatter(df['x'], df['y'], c=df['color'], s=10, marker='s', edgecolors='none')

direction = 1
direction_second = -1


def update(frame):
    global direction
    global direction_second

    df.loc[df['color'] == '#41ae76', 'x'] += 0.1 * direction
    df.loc[df['color'] == '#006d2c', 'x'] += 0.1 * direction_second

    df.loc[df['color'] == '#FFF200', 'y'] += 0.1 * direction
    df.loc[df['color'] == '#FFF201', 'x'] += 0.1 * direction
    df.loc[df['color'] == '#FFF202', 'x'] += -0.1 * direction
    df.loc[df['color'] == '#FFF203', 'x'] += -0.1 * direction
    df.loc[df['color'] == '#FFF203', 'y'] += -0.1 * direction
    df.loc[df['color'] == '#FFF204', 'x'] += 0.1 * direction
    df.loc[df['color'] == '#FFF204', 'y'] += -0.1 * direction

    if (df['x'].min() <= 18) or (df['x'].max() >= 22):
        direction *= -1
        direction_second *= -1

    scatter.set_offsets(np.column_stack((df['x'], df['y'])))
    return scatter,


ani = FuncAnimation(fig, update, frames=100, interval=300, blit=True)

plt.title('Wesołych Świąt!', color="white", fontname='Comic Sans MS', fontsize=20)
plt.gca().set_aspect('equal', adjustable='box')
plt.show()
