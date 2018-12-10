#' Convert to tidygraph. 
#' 
#' 
#' @param x model
#' @param ... other args
#'
#' @export 
#' @name as_tbl_graph
#' @importFrom tidygraph as_tbl_graph activate
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_cols %>% 
#' @importFrom silicate sc_uid
#' @importFrom igraph layout.star
#' @importFrom rlang .data
#' @examples 
#' library(tidygraph)
#' library(scgraph)
#' data("minimal_mesh", package = "silicate")
#' as_tbl_graph(minimal_mesh)
#' library(silicate)  
#' umodel <- SC(as_tbl_graph(minimal_mesh))
#' #gibble.PATH <- function(x, ...) {
#' #inner_join(x[["path"]], x[["path_link_vertex"]] %>% group_by(path) %>% summarize(nrow = n()) ) %>%
#' #  dplyr::mutate(ncol = 2, type = "MULTILINESTRING")
#' #}
#' #library(gibble)
#' #geomap <- gibble(prim %>% PATH())
#' #silicate:::build_sf()
#' ## some kind of round trip
#' 
as_tbl_graph.SC <- function(x, ...) {
  as_tbl_graph(as.igraph(x), ...)
}

#' @export 
#' @name as_tbl_graph
as_tbl_graph.sf <- function(x, ...) {
  as_tbl_graph(SC(x), ...)
}
#' @importFrom silicate sc_object
sc_object.tbl_graph <- function(x, ...) {
  tibble::tibble(object_ = silicate::sc_uid(1L))
}
#' @importFrom silicate sc_edge
sc_edge.tbl_graph <- function(x, ...) {
  out <- igraph::as_edgelist(x)
  tibble::tibble(.vx0 = out[,1], .vx1 = out[, 2], edge_ = sc_uid(nrow(out)))
}
#' @importFrom silicate sc_vertex
sc_vertex.tbl_graph <- function(x, ...) {
  outm <- igraph::layout.auto(x)
  edge <- sc_edge(x)
  out <- tibble::tibble(vertex_ = unique(c(edge$.vx0, edge$.vx1)))
  out$x_ <- outm[,1]
  out$y_ <- outm[,2]
  out[c("x_", "y_", "vertex_")]
}
#' SC
#' 
#' tbl_graph methods for the universal model
#' @inheritParams silicate::SC
#' @importFrom silicate SC
#' @export
#' @name SC
SC.tbl_graph <- function(x, ...) {
    O <- sc_object(x)
  E <- sc_edge(x)
  ExO <- tibble::tibble(object_ = O$object_, edge_ = E$edge_)
  structure(list(object = O, object_link_edge = ExO, edge = E, 
                 vertex = sc_vertex(x)), class = c("SC", "sc"))

}

