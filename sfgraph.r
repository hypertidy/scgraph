library(ggraph)
library(igraph)
#devtools::install_github(c("mdsumner/sc", "mdsumner/scsf"))
library(scsf)
data("minimal_mesh")
prim <- sc::PRIMITIVE(minimal_mesh)
library(dplyr)
mm_g <- graph_from_data_frame(prim$segment %>% rename(from = .vertex0, to = .vertex1))

library(sf)
nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)

plot(nc[4:10, 1:2])
prim <- PRIMITIVE(nc[4:10, ])
nc_g <- graph_from_data_frame(prim$segment %>% rename(from = .vertex0, to = .vertex1))


ggraph(nc_g, layout = 'kk') +
  geom_edge_link(aes(colour = as.numeric(factor(segment_))))

ggraph(nc_g, layout = 'kk') +
  geom_edge_link(aes(colour = path_))

##devtools::install_github("mdsumner/sfdct")
#data("antarctica", package = "sfdct")

data("wrld_simpl", package = "maptools")
oz <- sp::disaggregate(subset(wrld_simpl, NAME == "Australia"))
tas <- st_as_sf(subset(oz, sp::coordinates(oz)[, 2] < -40))
plot(st_geometry(tas))


prim <- PRIMITIVE(tas)
tas_g <- graph_from_data_frame(prim$segment %>% rename(from = .vertex0, to = .vertex1))
ggraph(tas_g, layout = 'kk') +
  geom_edge_link(aes(colour = path_))


ggraph(tas_g, layout = 'kk') +
  geom_edge_link(aes(colour = as.numeric(factor(segment_))))



## -------------------------------------------------------------

library(ggraph)
library(igraph)
hierarchy <- as.dendrogram(hclust(dist(iris[, 1:4])))

# Classify nodes based on agreement between children
hierarchy <- tree_apply(hierarchy, function(node, children, ...) {
  if (is.leaf(node)) {
    attr(node, 'Class') <- as.character(iris[as.integer(attr(node, 'label')),5])
  } else {
    classes <- unique(sapply(children, attr, which = 'Class'))
    if (length(classes) == 1 && !anyNA(classes)) {
      attr(node, 'Class') <- classes
    } else {
      attr(node, 'Class') <- NA
    }
  }
  attr(node, 'nodePar') <- list(class = attr(node, 'Class'))
  node
}, direction = 'up')

hairball <- graph_from_data_frame(highschool)

# Classify nodes based on popularity gain
pop1957 <- degree(delete_edges(hairball, which(E(hairball)$year == 1957)), 
                  mode = 'in')
pop1958 <- degree(delete_edges(hairball, which(E(hairball)$year == 1958)), 
                  mode = 'in')
V(hairball)$pop_devel <- ifelse(pop1957 < pop1958, 'increased',
                                ifelse(pop1957 > pop1958, 'decreased', 
                                       'unchanged'))
V(hairball)$popularity <- pmax(pop1957, pop1958)
E(hairball)$year <- as.character(E(hairball)$year)
