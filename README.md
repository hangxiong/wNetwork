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
Examples of ploting degree distribution and strenght distribution of weighted networks:
```R
g <- barabasi.game(1000, directed = FALSE)
edg <- get.edgelist(g)
degree.dist(edg)
      [,1]  [,2]
 [1,]    0 0.596
 [2,]    1 0.203
 [3,]    2 0.089
 [4,]    3 0.045
 [5,]    4 0.020
 [6,]    5 0.015
 [7,]    6 0.002
 [8,]    7 0.010
 [9,]    8 0.006
[10,]    9 0.003
[11,]   10 0.001
[12,]   11 0.001
[13,]   12 0.001
[14,]   13 0.004
[15,]   14 0.001
[16,]   17 0.001
[17,]   26 0.001
[18,]   29 0.001
degree.dist.plot(edg)
(Intercept)   log(degree) 
  -1.592286   -1.840652 
  
g <- barabasi.game(1000, directed = FALSE)
edg <- get.edgelist(g)
edg = cbind(edg, runif(nrow(edg), 0, 1))
strength.dist(edg)
      [,1]  [,2]
 [1,]    0 0.721
 [2,]    1 0.176
 [3,]    2 0.058
 [4,]    3 0.020
 [5,]    4 0.011
 [6,]    5 0.006
 [7,]    7 0.003
 [8,]    8 0.001
 [9,]    9 0.002
[10,]   11 0.001
[11,]   14 0.001
strength.dist.plot(edg)
  (Intercept) log(strength) 
  -1.603872     -2.170544 
```
