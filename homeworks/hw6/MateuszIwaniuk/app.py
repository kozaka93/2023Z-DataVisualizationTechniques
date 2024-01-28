import networkx as nx
import matplotlib.pyplot as plt

# Lista zwierząt
zwierzeta = [
    "Kot", "Lew", "Tygrys", "Człowiek", "Małpa", "Goryl",
    "Pies", "Wilk", "Lis", "Niedźwiedź", "Koń", "Zebra",
    "Słoń", "Hipopotam", "Nosorożec", "Wieloryb", "Delfin",
    "Rekin", "Orzeł", "Sokół", "Gołąb", "Pingwin",
    "Krokodyl", "Żółw", "Wąż"
]

# Tworzenie grafu
G_zwierzeta = nx.Graph()

# Dodawanie wierzchołków - zwierząt
G_zwierzeta.add_nodes_from(zwierzeta)

# Dodawanie krawędzi w oparciu o pokrewieństwo
pokrewienstwa = [
    ("Kot", "Lew"), ("Kot", "Tygrys"), ("Człowiek", "Małpa"), 
    ("Człowiek", "Goryl"), ("Pies", "Wilk"), ("Koń", "Zebra"), 
    ("Słoń", "Hipopotam"), ("Wieloryb", "Delfin"), ("Rekin", "Delfin"), 
    ("Orzeł", "Sokół"), ("Gołąb", "Pingwin"), ("Krokodyl", "Żółw"),
    ("Niedźwiedź", "Panda"), ("Lis", "Szakal"), ("Nosorożec", "Bawół")
]

G_zwierzeta.add_edges_from(pokrewienstwa)

# Rysowanie grafu z użyciem krawędzi
plt.figure(figsize=(12, 12))
pos = nx.spring_layout(G_zwierzeta,k=0.2)  # układ wierzchołków grafu
nx.draw(G_zwierzeta, pos, with_labels=True, node_color='lightblue', node_size=2000, font_size=10)
nx.draw_networkx_edges(G_zwierzeta, pos, edgelist=pokrewienstwa, edge_color="black")
plt.title("Graf pokrewieństwa zwierząt z krawędziami")
plt.show()