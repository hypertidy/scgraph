
#' Convert model to a graph. 
#' 
#' Only segments from an input model are extracted and used to build the graph.  
#' @inheritParams sc::PRIMITIVE
#' @param layout keep the input vertex coordinates x-y as the graph layout, defaults to `TRUE` otherwise no layout is provided
#' ## sf
#' ## spatstat
#' ## raw track data
#' ## make up segments starting with the unjoin thing, that might show the way forward
#' @importFrom igraph graph_from_data_frame V as.igraph
#' @importFrom sc PRIMITIVE
#' @importFrom dplyr rename
#' @importFrom tidygraph as_tbl_graph
#' @importMethodsFrom scsf sc_path
#' @name as.igraph
#' @export
#' @export as.igraph
#' @examples
#' data("minimal_mesh", package = "scsf")
#' as.igraph(minimal_mesh)
as.igraph.PRIMITIVE <- function(x, ..., layout = TRUE) {
  g <- igraph::graph_from_data_frame(dplyr::rename_(x[["segment"]], from = quote(.vertex0), to = quote(.vertex1)))
  if (layout) {
    igraph::V(g)$x <- x$v$x_[match(names(igraph::V(g)), x$v$vertex_)]
    igraph::V(g)$y <- x$v$y_[match(names(igraph::V(g)), x$v$vertex_)]
  }
  g
}

#' @export
#' @name as.igraph
as.igraph.sf <- function(x, ..., layout = TRUE) {
  as.igraph(PRIMITIVE(x), ..., layout = layout)
}
#' @export 
#' @name as.igraph
sc_as_igraph <- as.igraph.PRIMITIVE


