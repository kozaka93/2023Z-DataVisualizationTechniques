import networkx as nx
import matplotlib.pyplot as plt

file_path = 'Vienna subway.csv'

with open(file_path, 'r') as file:
    edges = [line.strip().split(';') for line in file]

G_new = nx.Graph()

for start, stop, line, color in edges:
    G_new.add_edge(start, stop, color=color)

plt.figure(figsize=(35, 35))
pos_kamada_kawai = nx.kamada_kawai_layout(G_new)
nx.draw_networkx_nodes(G_new, pos_kamada_kawai, node_size=100)
edge_colors_new = [G_new[u][v]['color'] for u, v in G_new.edges()]
nx.draw_networkx_edges(G_new, pos_kamada_kawai, edge_color=edge_colors_new, width=2)

label_pos = pos_kamada_kawai.copy()
label_offset = 0.005
for start, stop, line, color in edges:
    label_pos[start] = (label_pos[start][0], label_pos[start][1] + label_offset)
    label_pos[stop] = (label_pos[stop][0], label_pos[stop][1] + label_offset)

nx.draw_networkx_labels(G_new, label_pos, font_size=15, font_family="sans-serif")

plt.axis("off")
plt.title("Vienna Subway Network", fontsize=20, fontfamily='sans-serif')
plt.savefig("vienna_subway_network.jpg")
plt.show()