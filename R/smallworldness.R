#' Calculate small-worldness of network
#' 
#' This function calculate the small-worldness of a given weighted or unweighted network 
#' using Humphries or Telesford method.
#' @param adjacency The input adjacency matrix of a network.
#' @param definition The definition of small-worldness.
#' 
#' "Humphries" implies the definition provided by Humphries (2008).
#'
#' "Telesford" implies the definition provided by Telesford (2010).
#' @param option The option of treating the infinite path length in the network.
#'
#' "gconly" implies calculating the path length only for the gaint component (default).
#' 
#' "max" implies treating the infinite path length as the maximum non-infinite length in the network.
#' 
#' "2max" implies treating the infinite path length as the twice of the maximum non-infinite length
#' in the network.
#' 
#' NULL implies leaving the infinite path length as it is.
#' @param measure The method used to measure the value of triplet in the network.
#' 
#' "am" implies the arithmetic mean method (default).
#' 
#' "gm" implies the geometric mean method.
#' 
#' "mi" implies the minimum method.
#' 
#' "ma" implies the maximum method.
#' 
#' "bi" implies the binary measure.
#' @param reshuffle The option the way to reshuffled edges in the network.
#' "weights" implies randomly assigning the weights to the edges.
#' 
#' "links" implies maintaining the degree distribution, but changing the contacts randomly.
#' @references Humphries MD, Gurney K (2008). Network ‘Small-World-Ness’: A Quantitative Method 
#' for Determining Canonical Network Equivalence. PLoS ONE 3(4): e0002051.
#' @references Telesford, Q. K., Joyce, K. E., Hayasaka, S., Burdette, J. H., & Laurienti, P. J. (2011).
#' The Ubiquity of Small-world Networks. Brain Connectivity, 1(5), 367-375.
#' @export
#' @examples
#' edg <- rbind(
#' c(1,2,4),
#' c(1,3,2),
#' c(2,1,4),
#' c(2,3,4),
#' c(2,4,1),
#' c(2,5,2),
#' c(3,1,2),
#' c(3,2,4),
#' c(4,2,1),
#' c(5,2,2),
#' c(5,6,1),
#' c(6,5,1))
#' adj <- edgelist.to.adjacency(edg)
#' smallworldness(adj)

smallworldness <- function(adjacency, definition = "Humphries", option = "gconly",
                           measure = "am", reshuffle = "weights")
{
  nv <- nrow(adjacency)
  edg <- adjacency.to.edgelist(adjacency)
  cc = clustering_w(edg, measure=measure)
  eq_rand <- edg
  num_iter = 4*nv
  for(k in 1:num_iter)
    eq_rand = rg_reshuffling_w(eq_rand, option=reshuffle)
  cc_rand = clustering_w(eq_rand, measure=measure)
  if(is.null(option))
  {
    dis = distance_w(edg, directed=NULL, gconly=FALSE)
    dis_rand = distance_w(eq_rand, directed=NULL, gconly=FALSE)
  }
  if(option=="gconly")
  {
    dis = distance_w(edg, directed=NULL, gconly=TRUE)
    dis_rand = distance_w(eq_rand, directed=NULL, gconly=TRUE)
  }
  if(option=="max")
  {
    dis = distance_w(edg, directed=NULL, gconly=FALSE)
    dis_alt = dis
    dis_alt[is.infinite(dis_alt)] = NA
    max_dis = max(dis_alt, na.rm=T)
    dis[is.infinite(dis)] = max_dis
    dis_rand = distance_w(eq_rand, directed=NULL, gconly=FALSE)
    dis_rand_alt = dis_rand
    dis_rand_alt[is.infinite(dis_rand_alt)] = NA
    max_dis_rand = max(dis_rand_alt, na.rm=T)
    dis_rand[is.infinite(dis_rand)] = max_dis_rand
  }
  if(option=="2max")
  {
    dis = distance_w(edg, directed=NULL, gconly=FALSE)
    dis_alt = dis
    dis_alt[is.infinite(dis_alt)] = NA
    max_dis = max(dis_alt, na.rm=T)
    dis[is.infinite(dis)] = 2*max_dis
    dis_rand = distance_w(eq_rand, directed=NULL, gconly=FALSE)
    dis_rand_alt = dis_rand
    dis_rand_alt[is.infinite(dis_rand_alt)] = NA
    max_dis_rand = max(dis_rand_alt, na.rm=T)
    dis_rand[is.infinite(dis_rand)] = 2*max_dis_rand
  }
  apl = mean(dis, na.rm=T)
  apl_rand = mean(dis_rand, na.rm=T)
  if(definition == "Humphries")
  {
    C_ratio = cc / cc_rand
    L_ratio = apl / apl_rand
    SW.Hump = C_ratio / L_ratio
    return(SW.Hump)
  }
  if(definition == "Telesford")
  {
    eq_latt = eq.lattice(adjacency, measure=measure)  # Create eq lattice network
    cc_latt = clustering_w(eq_latt, measure=measure)
    if(is.null(option))
      dis_latt = distance_w(eq_latt, directed=NULL, gconly=FALSE)
    if(option=="gconly")
      dis_latt = distance_w(eq_latt, directed=NULL, gconly=TRUE)
    if(option=="max")
    {
      dis_latt = distance_w(eq_latt, directed=NULL, gconly=FALSE)
      dis_latt_alt = dis_latt
      dis_latt_alt[is.infinite(dis_latt_alt)] = NA
      max_dis_latt = max(dis_latt_alt, na.rm=T)
      dis_latt[is.infinite(dis_latt)] = max_dis_latt
    }
    if(option=="2max")
    {
      dis_latt = distance_w(eq_latt, directed=NULL, gconly=FALSE)
      dis_latt_alt = dis_latt
      dis_latt_alt[is.infinite(dis_latt_alt)] = NA
      max_dis_latt = max(dis_latt_alt, na.rm=T)
      dis_latt[is.infinite(dis_latt)] = 2*max_dis_latt
    }
    apl_latt = mean(dis_latt, na.rm=T)
    L_ratio_reciprocal = apl_rand / apl
    C_ratio_latt = cc / cc_latt
    SW.Tele = L_ratio_reciprocal - C_ratio_latt
    return(SW.Tele)
  }
}