# wNetwork
R package for calculating topological characteristics of weighted networks

Version 0.1.2

## Usage

An example of measuring small-worldness of a weighted network:
```R
# Create an exemplar network in edgelist
edg <- rbind(c(1,2,4), c(1,3,2), c(2,1,4), c(2,3,4), 
              c(2,4,1), c(2,5,2), c(3,1,2), c(3,2,4), 
              c(4,2,1), c(5,2,2), c(5,6,1), c(6,5,1))
# Convert the edgelist into an adjacency matrix
adj <- edgelist.to.adjacency(edg)
# Calculate the small-worldness of the network
smallworldness(adj)
# Result
      am 
1.146284 
```

An example of measuring centralisations a weighted network:
```R
# Create an exemplar network in edgelist
edg <- rbind(c(1,2,4), c(1,3,2), c(2,1,4), c(2,3,4), 
              c(2,4,1), c(2,5,2), c(3,1,2), c(3,2,4), 
              c(4,2,1), c(5,2,2), c(5,6,1), c(6,5,1))
# Calculate various centralisations of the network
centr.betweenness(edg)
[1] 0.75
centr.closeness(edg)
[1] 0.5044942
centr.degree(edg)
[1] 0.6666667
centr.strength(edg)
[1] 0.4130435
```
