############################################################################
# Social Networks Final Project by Omer Elgrably and Barr Inbar
############################################################################

#setup
install.packages("igraph")
install.packages("RCurl")
install.packages("XML")
install.packages("rvest")

require(XML)
require(RCurl)
require(igraph)
require(rvest)

############################################################################
# Set up the initial graph, clear the anomalies and simplify it
############################################################################
# Read graph (undirected)
#ga.data <- read.csv('edges.csv', header=TRUE)
#ga.vrtx <- read.csv('nodes.csv', header=TRUE, stringsAsFactors=FALSE)
#initial.graph <- graph.data.frame(ga.data, vertices=ga.vrtx, directed=FALSE)

initial.graph <- read.graph("MKLawsInit.graphml", format = c("graphml"))

V(initial.graph)$label

# Clean data
V(initial.graph)[which(V(initial.graph)$label == "Rachel Azaria")]$Party <- "Kulanu"
V(initial.graph)[which(V(initial.graph)$label == "Yair Lapid")]$Party <- "Yesh Atid"
V(initial.graph)[which(V(initial.graph)$label == "Yael German")]$Party <- "Yesh Atid"
V(initial.graph)[which(V(initial.graph)$label == "Shai Piron")]$Party <- "Yesh Atid"
V(initial.graph)[which(V(initial.graph)$label == "Aliza Lavie")]$Party <- "Yesh Atid"

# Remove the resiged MKs
#initial.graph <- delete.vertices(initial.graph, V(initial.graph)[which(V(initial.graph)$label=="Shai Piron")])
#initial.graph <- delete.vertices(initial.graph, V(initial.graph)[which(V(initial.graph)$label=="Danny Atar")])
#initial.graph <- delete.vertices(initial.graph, V(initial.graph)[which(V(initial.graph)$label=="Meir Porush")])
#initial.graph <- delete.vertices(initial.graph, V(initial.graph)[which(V(initial.graph)$label=="Sharon Gal")])
#initial.graph <- delete.vertices(initial.graph, V(initial.graph)[which(V(initial.graph)$label=="Yinon Magal")])

# Write it to a gephi readable format
#write.graph(initial.graph, file = "InitialGraph.graphml", format = "graphml")

vcount(initial.graph)                       # Number of nodes
ecount(initial.graph)                       # Number of edges
count_components(initial.graph,mode="weak") # Number of connected components
max(clusters(initial.graph)$csize)               # Size of largest connected component
deg <- degree(initial.graph)

# MKs who didn't propose any law
V(initial.graph)[which(deg == 0)]$label

# MKs who proposed the most laws
V(initial.graph)[which(deg == max(deg))]$label

####################
# Q1, Q3
####################

# Remove the MKs who don't work with others
#gclust <- clusters(initial.graph) # Induce a graph with the largest component
#lcg <- induced.subgraph(initial.graph, V(initial.graph)[which(gclust$membership ==

# Simplify the graph with edges weight as amount of edges between nodes
E(initial.graph)$count <- 1
g <- simplify(graph=initial.graph, remove.multiple = TRUE, remove.loops = FALSE, edge.attr.comb = list(weight="sum", count = "sum", "ignore"))

# Write it to a gephi readable format
write.graph(g, file = "MKLawsWeighed.graphml", format="graphml")

solo <- sum(E(g)[which_loop(g)]$count)
total <- ecount(initial.graph)
friends <- total - solo
solo
solo / total * 100
friends
friends / total * 100
total


####################
# Q2, Q4
####################

# Create a parties graph
parties.graph <- g
V(parties.graph)$count <- 1
parties.graph <- contract.vertices(g,
                                  mapping=as.integer(as.factor(V(g)$Party)),
                                  vertex.attr.comb=list(Party="first", count="sum", label="last", "ignore"))

V(parties.graph)$label <- V(parties.graph)$Party

E(parties.graph)$weight <- E(parties.graph)$count

parties.graph <- simplify(graph=parties.graph, remove.multiple = TRUE, remove.loops = FALSE, edge.attr.comb = list(weight="sum", count ="sum", "ignore"))

vcount(parties.graph)
ecount(parties.graph)

# Write it to a gephi readable format
write.graph(parties.graph, file = "MKLawsParties.graphml", format = "graphml")

# Check how many laws are inter-parties and how many intra-party
intraparty <- sum(E(g)[which_loop(parties.graph)]$count)
interparty <- total - intraparty
total <- sum(E(parties.graph)$count)
total
intraparty
intraparty / total * 100
interparty
interparty / total * 100

# Sum the amount of laws each partyis involved in
strength(parties.graph, weights = E(parties.graph)$count)


####################
# Q7
####################

# Create a sides graph
side.graph <- g
V(side.graph)$count <- 1
side.graph <- contract.vertices(g,
                                 mapping=as.integer(as.factor(V(g)$Side)),
                                 vertex.attr.comb=list(Side="first", count="sum", label="last", "ignore"))

V(side.graph)$label <- V(side.graph)$Side

side.graph <- simplify(graph=side.graph, remove.multiple = TRUE, remove.loops = FALSE, edge.attr.comb = list(weight="sum", count ="sum", "ignore"))

vcount(side.graph)
ecount(side.graph)

# Write it to a gephi readable format
write.graph(side.graph, file = "MKLawsSides.graphml", format = "graphml")

# Check how many laws are inter-side and how many intra-side
intraside <- sum(E(side.graph)[which_loop(side.graph)]$count)
total <- sum(E(side.graph)$count)
interside <- total - intraside
total
intraside
intraside / total * 100
interside
interside / total * 100

####################
# Q8
####################

# Study the network's centrality factors
deg <- centralization.degree(g, normalized=F) # How likely is the MK to be part of a law
V(g)[which.max(deg$res)]$label
V(g)[which.min(deg$res)]$label
cls <- centralization.closeness(g, normalized=F) # How close he is to all other MKs
V(g)[which.max(cls$res)]$label
V(g)[which.min(cls$res)]$label
bet <- centralization.betweenness(g, directed = F, normalized = F) # How much is he a bridge between nodes in shortest path
V(g)[which.max(bet$res)]$label
V(g)[which.min(bet$res)]$label
evc <- centralization.evcent(g, directed = F, normalized = F) # How much influence does the MK have in the network?
V(g)[which.max(evc$vector)]$label
V(g)[which.min(evc$vector)]$label
