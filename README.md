# scgraph


AT this link is efforts for threejs: 

https://gist.github.com/mdsumner/0a9207a58898eed30c07fba7ec5e959b

Here's direct ggraph, cannot get it to work with ggmap

```R
#install.packages("ggmap")
#install.packages("ggraph")
#devtools::install_github("mdsumner/scsf")
library(ggmap)

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")

library(USAboundaries)

## pick any county with a hole in it
hc <- USAboundaries::us_counties()[unlist(fortify(USAboundaries::us_counties()) %>% filter(hole) %>% distinct(id)), ]

## this one has two hole
bb <- sp::bbox(hc[4, ])
county <- fortify(hc[4, ])
map <- get_stamenmap(as.vector(bb), zoom = 10, maptype = "toner-lite")
ggmap(map)

library(ggplot2)
## traditional 
ggmap(map) +  geom_polygon(fill = scales::alpha("firebrick", 0.5), 
                                       aes(x = long, y = lat, group = group, colour = id), data = county)  
  
## with proper holes
ggmap(map) +  ggpolypath::geom_polypath(fill = scales::alpha("firebrick", 0.5), 
                           aes(x = long, y = lat, group = group, colour = id), data = county)  


## ggraph from primitives (line segments)
library(scsf)  ## only sf supported in the spatial family so far
prim <- sc::PRIMITIVE(sf::st_as_sf(hc[4, ]))


graph_primitive_layout <- function(prim) {
  g <- igraph::graph_from_data_frame(prim$segment %>% rename(from = .vertex0, to = .vertex1))
  igraph::V(g)$x <- prim$v$x_[match(names(igraph::V(g)), prim$v$vertex_)]
  igraph::V(g)$y <- prim$v$y_[match(names(igraph::V(g)), prim$v$vertex_)]
  g
}

library(ggraph)

g <- graph_primitive_layout(prim)
lyout <- cbind(igraph::V(g)$x, igraph::V(g)$y)
ggraph(g) +
  geom_edge_link(aes(colour = segment_)) + geom_node_point() 

```
