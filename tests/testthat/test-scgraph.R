context("scgraph")


data("minimal_mesh", package = "silicate")
test_that("multiplication works", {
  g <- as.igraph(minimal_mesh)
  g %>% expect_s3_class("igraph") 
  igraph::is_dag(g) %>% expect_false()
})
