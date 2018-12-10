
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
  g <- igraph::graph_from_data_frame(dplyr::rename_(x[["edge"]], from = quote(.vx0), to = quote(.vx1))) 
  if (layout) {
    igraph::V(g)$x <- x$vertex$x_[match(igraph::V(g)$name, x$vertex$vertex_)]
    igraph::V(g)$y <- x$vertex$y_[match(igraph::V(g)$name, x$vertex$vertex_)]
    
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
#' @export
#' @name as.igraph
as.igraph.SC0 <- function(x, ..., layout = TRUE) {
  nr <- unlist(lapply(x$object$topology_, nrow), use.names = FALSE)
  edge <- do.call(rbind, x$object$topology_)
  edge$object <- rep(seq_len(nrow(x$object)), nr)
  names(edge) <- c("from", "to", "object")
  g <- igraph::graph_from_data_frame(edge)
  if (layout) {
    igraph::V(g)$x <- x$vertex$x_[as.integer(igraph::V(g)$name)]
    igraph::V(g)$y <- x$vertex$y_[as.integer(igraph::V(g)$name)]
  
  }
  g
}
