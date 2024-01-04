import networkx as nx
import matplotlib.pyplot as plt

def draw_neural_network():
    nodes = 31
    G = nx.Graph()

    # Add nodes
    G.add_nodes_from(range(1, nodes + 7))

    # Edges
    branches = nodes-1
    edges = []
    for i in range(1, branches):
        for j in range(1, branches):
            if i == j:
                continue
            edges.append((i, j))
    edges.append((nodes, nodes-1))
    edges.append((nodes, nodes-11))
    edges.append((nodes-1, nodes-10))
    # edges for star
    edges.append((nodes+6, nodes+5))
    edges.append((nodes+5, nodes+4))
    edges.append((nodes+6, nodes+4))
    edges.append((nodes + 1, nodes + 2))
    edges.append((nodes + 3, nodes + 2))
    edges.append((nodes + 1, nodes + 3))
    G.add_edges_from(edges)



    # Set positions
    pos = {1: (0, 0.5),

           2: (-0.5, -1.6),
           3: (0.5, -1.6),
           4: (-1.5, -1.6),
           5: (1.5, -1.6),

           6: (0.5, -3.2),
           7: (-0.5, -3.2),
           8: (1.5, -3.2),
           9: (-1.5, -3.2),
           10: (2.5, -3.2),
           11: (-2.5, -3.2),

           12: (0.5, -4.8),
           13: (-0.5, -4.8),
           14: (1.5, -4.8),
           15: (-1.5, -4.8),
           16: (2.5, -4.8),
           17: (-2.5, -4.8),
           18: (3.5, -4.8),
           19: (-3.5, -4.8),

           20: (0.5, -6.4),
           21: (-0.5, -6.4),
           22: (1.5, -6.4),
           23: (-1.5, -6.4),
           24: (2.5, -6.4),
           25: (-2.5, -6.4),
           26: (3.5, -6.4),
           27: (-3.5, -6.4),
           28: (4.5, -6.4),
           29: (-4.5, -6.4),

           30: (-0.5, -8),
           31: (0.5, -8),

            # star
           32: (0, 0),
           33: (0.5, 0.75),
           34: (-0.5, 0.75),
           35: (0, 1),
           36: (0.5, 0.25),
           37: (-0.5, 0.25)
           }



    # Draw
    node_sizes = [600.] * (nodes) + [100.] * 6
    node_colors = ["yellow"] + ["green"] * (nodes-1) + ["yellow"]*6
    nx.draw(G, pos, with_labels=False, node_size=node_sizes, node_color=node_colors, font_size=5, font_color="black", font_weight="bold", linewidths=1, edge_color="green", alpha=0.7)
    plt.show()

draw_neural_network()
