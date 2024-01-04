import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from mpl_toolkits import mplot3d
import matplotlib.animation as animation


fig = plt.figure(facecolor='black')
ax = plt.axes(projection='3d')
ax.set_facecolor('black')


n_vertices = 8
angle = 2 * np.pi / n_vertices
radius = 0.5
x = radius * np.cos(np.arange(0, 2 * np.pi, angle))
y = radius * np.sin(np.arange(0, 2 * np.pi, angle))



x = np.append(x, x[0])
y = np.append(y, y[0])
z = np.zeros(len(x))
veritices = np.column_stack((x, y,z))
apex = np.array([0, 0, 1])


for i in range(len(x)):
    ax.plot([veritices[i, 0], apex[0]], [veritices[i, 1], apex[1]], [veritices[i, 2], apex[2]], color='green', zorder = 2)


ax.plot_trisurf(np.append(x, apex[0]), np.append(y, apex[1]), np.append(z, apex[2]), color='green', zorder=2)
ax.plot_trisurf(x, y, z, color='green', alpha=1, zorder = 2)

z_new = np.linspace(0, 1, 100)
x_new = z_new * np.sin(25 * z_new)
y_new = z_new * np.cos(25 * z_new)
ax.plot3D(0.6*x_new, 0.6* y_new, -z_new+1, 'red', zorder = 5)

z_new1 = np.random.uniform(0, 1, 50)
x_new1 = z_new1 * np.sin(25 * z_new1)
y_new1 = z_new1 * np.cos(25 * z_new1)
ax.scatter3D(0.6*x_new1, 0.6* y_new1, -z_new1+1, color = 'white',marker = 'o', zorder = 70)

ax.scatter(apex[0], apex[1], apex[2] + 0.07, color='yellow', marker='*', s=100)


plt.title('Merry Christmas', color='white', fontsize=16, fontfamily='serif', fontname='Lucida Handwriting', y =0.92)


def _frame_update(index):
    ax.view_init(index * 0, index * 3)
    return

anim = animation.FuncAnimation(
    fig,
    _frame_update,
    interval=50,
    cache_frame_data=False,
    frames=100,
)

ax.set_axis_off()



anim.save('animationtree.gif', writer='imagemagick')
plt.show()
#nie umiałam zrobić tak aby ta czerowna spirala była na tym ostrosłupie, udajmy że tak miało być