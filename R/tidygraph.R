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
#' @importFrom sc sc_uid
#' @importFrom igraph layout.star
#' @importFrom rlang .data
#' @examples 
#' library(tidygraph)
#' data("minimal_mesh", package = "scsf")
#' as_tbl_graph(minimal_mesh)
#' ## library(scsf)  ## not quite
#' ##PRIMITIVE(as_tbl_graph(minimal_mesh)) %>% sf()
#' ## some kind of round trip
#' 
as_tbl_graph.PRIMITIVE <- function(x, ...) {
  as_tbl_graph(as.igraph(x), ...)
}

#' @export 
#' @name as_tbl_graph
as_tbl_graph.sf <- function(x, ...) {
  as_tbl_graph(PRIMITIVE(x), ...)
}

PRIMITIVE.tbl_graph <- function(x, ...) {
  object = tibble(object_ = sc_uid(1))
  vertex = bind_cols(as_tibble(layout.star(x)) %>% 
                       rename(x_ = .data$V1, y_ = .data$V2), as_tibble(activate(x, "nodes")))
  vertex$vertex_ <- sc_uid(nrow(vertex))
  segment = rename(as_tibble(activate(x, "edges")), .vertex0 = .data$from, .vertex1 = .data$to)
  segment$.vertex0 <- vertex$vertex_[segment$.vertex0]
  segment$.vertex1 <- vertex$vertex_[segment$.vertex1]
  a <- list(object = object, vertex = vertex, segment = segment)
  attr(a, "join_ramp") <- c("object", "path", "path_link_vertex", "vertex")
  class(a) <- c("PRIMITIVE", "sc")
  a
}
#prim <- PRIMITIVE(as_tbl_graph(graph))

