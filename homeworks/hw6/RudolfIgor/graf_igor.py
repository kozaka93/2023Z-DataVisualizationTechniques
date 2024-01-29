import networkx as nx
import matplotlib.pyplot as plt
import matplotlib as mpl

import pandas as pd

#zbior danych pochodzi ze strony: https://networks.skewed.de/net/dolphins

csv_file_path = 'data/edges.csv'

df = pd.read_csv(csv_file_path, header=None, names=['source', 'target'], comment='#')

G = nx.Graph()

for row in df.itertuples(index=False):
    source, target = str(row.source), str(row.target)
    G.add_node(source)
    G.add_node(target)
    G.add_edge(source, target)

wierzecholki_struktura = dict(G.degree())
max_degree_node = max(wierzecholki_struktura, key=wierzecholki_struktura.get)

schemat_rysowania = nx.spring_layout(G, k=0.65)

plt.figure(figsize=(10, 8))

node_color = 'skyblue'
node_color_map = [node_color if node != max_degree_node else 'orange' for node in G.nodes]

nx.draw(G, schemat_rysowania, with_labels=True, font_weight='bold', node_size=500, node_color=node_color_map, font_size=8)

nasza_legenda = [
    mpl.lines.Line2D([0], [0], marker='o', color='w', markerfacecolor='orange', markersize=10, label='Most connected dolphin'),
    mpl.lines.Line2D([0], [0], marker='o', color='w', markerfacecolor=node_color, markersize=10, label='Other dolphins')
]
plt.legend(handles=nasza_legenda, title='', loc='lower right')

plt.suptitle('Graph representing frequent associations observed among dolphins', fontweight='bold', fontname='Arial')
avg_interakcje = G.number_of_edges() / G.number_of_nodes()

num_nodes = G.number_of_nodes()
num_edges = G.number_of_edges()
plt.text(0.01, 0.01, f'Number of nodes: {num_nodes}\nNumber of edges: {num_edges}\nAverage interactions: {avg_interakcje:.2f}\n '
                     f'\nSource: https://networks.skewed.de/net/dolphins',
         fontsize=8, fontweight='bold', transform=plt.gcf().transFigure, ha='left', va='bottom')

plt.show()


