
#' Convert model to a graph. 
#' 
#' Only segments from an input model are extracted and used to build the graph.  
#' @inheritParams sc::PRIMITIVE
#' @param layout keep the input vertex coordinates x-y as the graph layout, defaults to `TRUE` otherwise no layout is provided
#' ## sf
#' ## spatstat
#' ## raw track data
#' ## make up segments starting with the unjoin thing, that might show the way forward
#' @importFrom igraph graph_from_data_frame V
#' @importFrom sc PRIMITIVE
#' @importFrom dplyr rename
#' @name sc_as_igraph
#' @export
sc_as_igraph <- function(x, layout = TRUE, ...) {
  prim <- sc::PRIMITIVE(x)
  g <- igraph::graph_from_data_frame(dplyr::rename_(prim[["segment"]], from = quote(.vertex0), to = quote(.vertex1)))
  if (layout) {
    igraph::V(g)$x <- prim$v$x_[match(names(igraph::V(g)), prim$v$vertex_)]
    igraph::V(g)$y <- prim$v$y_[match(names(igraph::V(g)), prim$v$vertex_)]
  }
  g
}