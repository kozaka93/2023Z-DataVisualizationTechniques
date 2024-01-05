import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.patches import Rectangle
arr = []
YEAR=2024
arr.append(YEAR)

THE_PEAK=-35
while True:
    if YEAR!=1:
        if YEAR%2==0:
            YEAR= YEAR / 2
            arr.append(YEAR)
        else:
            YEAR= (YEAR * 3) + 1
            arr.append(YEAR)
    else:
        break


x= np.array([i for i in range(len(arr))])
arr_minus=np.array([-i for i in arr])
max_arr=max(arr)
max_arr_index=arr.index(max_arr)
triangle= patches.Polygon([(0, THE_PEAK), (-max_arr, max_arr_index), (max_arr, max_arr_index)], closed=True, fill=True, color='green')
fig, ax = plt.subplots()
ax.add_patch(triangle)
plt.text(-1800, -5, '2024!', dict(size=22,color='darkred', alpha=0.5))
plt.plot(arr, x, "-o",c="brown", mfc="yellow", mec="black")
plt.plot(arr_minus, x, "-o", c="brown", mfc="yellow", mec="black")
ax.add_patch(Rectangle((-2000, len(arr)-1), 4000, -(len(arr)-1-max_arr_index), fill=True, color='brown',zorder=10))
plt.ylim( len(arr),THE_PEAK)

plt.title("'3x+1 problem' graph for the 2024")
plt.show()