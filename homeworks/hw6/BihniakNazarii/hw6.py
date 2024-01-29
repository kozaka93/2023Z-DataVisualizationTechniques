{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": null,
   "id": "782fc955-baa1-4c29-b56b-779ad1e60742",
   "metadata": {},
   "outputs": [],
   "source": [
    "#Praca domowa 6 - TWD\n",
    "# Nazarii Bihniak\n",
    "\n",
    "# Wizualizacja przedstawia graf Barabási-Alberta z 550 wierzchołkami\n",
    "# W tym modelu grafu niektóre wierzchołki mają znacznie więcej połączeń niż inne, co jest charakterystyczne\n",
    "# dla sieci bezskalowych i może stanowić wyzwanie wizualizacyjne\n",
    "# Właśnie, tak to jest.\n",
    "\n",
    "import networkx as nx\n",
    "import matplotlib.pyplot as plt\n",
    "import numpy as np\n",
    "\n",
    "# Utworzenie grafu Barabási-Alberta z ponad 500 wierzchołkami\n",
    "N = 550  # liczba wierzchołków\n",
    "m = 5    # liczba krawędzi do dołączenia z każdym nowym wierzchołkiem\n",
    "\n",
    "# Tworzenie grafu\n",
    "graph = nx.barabasi_albert_graph(N, m)\n",
    "\n",
    "# Ustalenie wielkości wierzchołków na podstawie stopnia wierzchołka\n",
    "node_sizes = [graph.degree(n) * 10 for n in graph.nodes()]\n",
    "\n",
    "# Ustalenie szerokości krawędzi na podstawie najkrótszej ścieżki (dystansu) między wierzchołkami\n",
    "# (dłuższe ścieżki = cieńsze krawędzie)\n",
    "path_lengths = dict(nx.all_pairs_shortest_path_length(graph))\n",
    "edge_widths = [1 / (path_lengths[u][v] + 1) for u, v in graph.edges()]\n",
    "\n",
    "# Ustalenie kolorów wierzchołków\n",
    "node_colors = [plt.cm.plasma(graph.degree(n) / max(dict(graph.degree()).values())) for n in graph.nodes()]\n",
    "\n",
    "# Rysowanie grafu\n",
    "plt.figure(figsize=(15, 15))\n",
    "nx.draw_networkx(graph, \n",
    "                 node_size=node_sizes, \n",
    "                 width=edge_widths, \n",
    "                 node_color=node_colors, \n",
    "                 with_labels=False,\n",
    "                 edge_color=\"grey\",\n",
    "                 alpha=0.7)\n",
    "\n",
    "# Dodanie legendy dla kolorów\n",
    "sm = plt.cm.ScalarMappable(cmap=plt.cm.plasma, norm=plt.Normalize(vmin=min(node_sizes), vmax=max(node_sizes)))\n",
    "plt.colorbar(sm, label='Stopień wierzchołka')\n",
    "\n",
    "plt.title(\"Wizualizacja grafu Barabási-Alberta z 550 wierzchołkami\")\n",
    "plt.show()\n"
   ]
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3 (ipykernel)",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.11.5"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 5
}
