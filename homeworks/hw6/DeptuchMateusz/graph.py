import networkx as nx
import matplotlib.pyplot as plt
from matplotlib.colors import Normalize
from matplotlib.cm import ScalarMappable


#DANE: https://gephi.org/datasets/lesmiserables.gml.zip
file_path = '/Users/mateuszdeptuch/RStudio/TWD/lab14/lesmiserables.gml'


G = nx.read_gml(file_path)
edge_values = nx.get_edge_attributes(G, 'value')
edge_colors = [edge_values[edge] for edge in G.edges()]

plt.figure(figsize=(12, 8))
pos = nx.kamada_kawai_layout(G)
plt.axes().set_facecolor('lightgray')
nx.draw(G, pos, with_labels=True, node_size=70, node_color='#EEEEEE', edge_color=edge_colors, edge_cmap=plt.cm.inferno_r, font_size = 5)
norm = Normalize(vmin=min(edge_values.values()), vmax=max(edge_values.values()))
sm = ScalarMappable(cmap=plt.cm.inferno_r, norm=norm)
sm.set_array([])

plt.title("Ile razy postaci z Les Miserables wystąpiły wspólnie w jednym rozdziale")

cbar = plt.colorbar(sm, orientation='vertical', shrink=0.8)
cbar.set_label('Liczba wspólnych rozdziałów')

plt.savefig('/Users/mateuszdeptuch/Desktop/graph.pdf', format='PDF')

plt.show()
