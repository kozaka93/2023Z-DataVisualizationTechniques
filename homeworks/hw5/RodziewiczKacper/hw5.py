# Drawing a Christmas tree with a black square background and stars in an Ulam spiral pattern
import matplotlib.pyplot as plt
import numpy as np

layers = [
    {'x': [18, 22, 22, 18], 'y': [-1, -1, 4, 4], 'color': '#8c510a'},  # trunk
    {'x': [10, 30, 20], 'y': [3, 3, 20], 'color': '#41ae76'},  # bottom layer
    {'x': [11, 29, 20], 'y': [6, 6, 18], 'color': '#006d2c'},
    {'x': [12, 28, 20], 'y': [9, 9, 16], 'color': '#41ae76'},
    {'x': [13, 27, 20], 'y': [12, 12, 22], 'color': '#006d2c'},
    {'x': [14, 26, 20], 'y': [15, 15, 24], 'color': '#41ae76'},
    {'x': [15, 25, 20], 'y': [18, 18, 26], 'color': '#006d2c'},
    {'x': [16, 24, 20], 'y': [21, 21, 28], 'color': '#41ae76'},
    {'x': [17, 23, 20], 'y': [24, 24, 30], 'color': '#006d2c'},
    {'x': [18, 22, 20], 'y': [27, 27, 32], 'color': '#41ae76'},  # top layer
    {'x': [18.2, 21.8, 20], 'y': [30.6, 30.6, 35.6], 'color': 'yellow'},  # star
]

# Function to check if a number is prime
def is_prime(num):
    if num < 2:
        return False
    for i in range(2, int(np.sqrt(num)) + 1):
        if num % i == 0:
            return False
    return True

# Function to create an Ulam spiral with prime numbers highlighted
def ulam_spiral_prime(n):
    x, y = 0, 0
    dx, dy = 0, -1
    prime_coords = []
    all_coords = []
    for i in range(1, n*n + 1):
        if (-n//2 < x <= n//2) and (-n//2 < y <= n//2):
            all_coords.append((x, y))
            if is_prime(i):
                prime_coords.append((x, y))
        if x == y or (x < 0 and x == -y) or (x > 0 and x == 1-y):
            dx, dy = -dy, dx
        x, y = x+dx, y+dy
    return prime_coords, all_coords

# Define size of the spiral
spiral_size = 50  # This will create a 50x50 grid for the Ulam spiral

# Get the prime and all coordinates for the spiral
prime_coords, all_coords = ulam_spiral_prime(spiral_size)

# Setup plot
fig, ax = plt.subplots(figsize=(10, 15))
plt.axis('off')


# Plot all points
for coord in all_coords:
    if(coord!=(0,0)):
      plt.plot(coord[0] + 20, coord[1] + 20, 'o', color='white', markersize=2)
    else:
        plt.plot(coord[0] + 20, coord[1] + 20, 'o', color='black', markersize=4)

# Highlight prime numbers
for prime in prime_coords:
    plt.plot(prime[0] + 20, prime[1] + 20, 'o', color='blue', markersize=2)

# Drawing the tree
for layer in layers:
    plt.fill(layer['x'], layer['y'], layer['color'], edgecolor='black')

plt.xlim(0, 40)
plt.ylim(0, 40)
plt.gca().set_aspect('equal', adjustable='box')
plt.show()
