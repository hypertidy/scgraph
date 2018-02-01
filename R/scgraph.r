
#' Convert model to a graph. 
#' 
#' Only segments from an input model are extracted and used to build the graph.  
#' @inheritParams silicate::SC
#' @param layout keep the input vertex coordinates x-y as the graph layout, defaults to `TRUE` otherwise no layout is provided
#' ## sf
#' ## spatstat
#' ## raw track data
#' ## make up segments starting with the unjoin thing, that might show the way forward
#' @importFrom igraph graph_from_data_frame V as.igraph
#' @importFrom dplyr rename
#' @importFrom tidygraph as_tbl_graph
#' @importFrom silicate sc_path SC
#' @name as.igraph
#' @export
#' @export as.igraph
#' @examples
#' data("minimal_mesh", package = "silicate")
#' as.igraph(minimal_mesh)
as.igraph.SC <- function(x, ..., layout = TRUE) {
  g <- igraph::graph_from_data_frame(dplyr::rename_(x[["edge"]], from = quote(.vertex0), to = quote(.vertex1))) 
#                                       dplyr::mutate(from = as.character(as.integer(factor(from, levels = unique(x$path_link_vertex$vertex_)))), 
#                                              to = as.character(as.integer(factor(to, levels = unique(x$path_link_vertex$vertex_))))))
  if (layout) {
    igraph::V(g)$x <- x$v$x_[match(igraph::V(g)$name, x$vertex$vertex_)]
    igraph::V(g)$y <- x$v$y_[match(igraph::V(g)$name, x$vertex$vertex_)]
    
  }
  g
}

#' @export
#' @name as.igraph
as.igraph.sf <- function(x, ..., layout = TRUE) {
  as.igraph(SC(x), ..., layout = layout)
}
#' @export 
#' @name as.igraph
sc_as_igraph <- function(x, ..., layout = TRUE) {
  as.igraph.SC(x, ..., layout = TRUE)
}


