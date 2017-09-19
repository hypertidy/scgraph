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
#' #library(silicate)  ##not quite
#' #prim <- PRIMITIVE(as_tbl_graph(minimal_mesh))
#' #gibble.PATH <- function(x, ...) {
#' #inner_join(x[["path"]], x[["path_link_vertex"]] %>% group_by(path) %>% summarize(nrow = n()) ) %>%
#' #  dplyr::mutate(ncol = 2, type = "MULTILINESTRING")
#' #}
#' #library(gibble)
#' #geomap <- gibble(prim %>% PATH())
#' #silicate:::build_sf()
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
  object = tibble(object = sc_uid(1))
  vertex = bind_cols(as_tibble(igraph::layout.star(x)) %>% 
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

