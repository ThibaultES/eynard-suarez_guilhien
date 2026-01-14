# Projet OCaml

## Auteurs
Corentin Guillien et Thibault Eynard-Suarez
Groupe de TD : A2

## Implémenté

### I. Ford Fulkerson

Nous avons correctement codé l'algorithme de Ford-Fulkerson.

### II. Adaptation à un exemple

Des fichiers de textes permettent de représenter les données pour un traffic de ressources sur un réseau routier.
L'algorithme est cappable d'appliquer l'algorithme de Ford-Fulkerson sur des données comportant plusieures sources et plusieurs puits.

### III. Minimisation des coûts

#### III.1 Objectif

Dans la suite de l'idée de gestion du flux des ressources sur un réseau, nous avons intégrés la possibilité de chercher le graphe maximisant le flux tout en minimisant des coûts. Ces coûts pouvant représenter les prix d'autoroutes sur un réseau routier, ou de places dans des wagons sur des réseaux ferroviaires.
Des fichiers de texte permettent donc de représenter les coûts en plus des capacités des arcs.

#### III.2 Algorithme utilisé

L'algorithme permettant de résoudre ce problème est l'algorithme de Successive Shortest Path. Cet algorithme est une adaptation de Ford-Fulkerson, cependant, à chaque itération nous cherchons comme chemin augmentant le flux un chemin de coût minimal.
Pour cela nous avons implémenté l'algorithme de Bellman-Ford (celui de Dijkstra étant impossible puisque les poids peuvent être négatifs sur le graphe d'écart).
