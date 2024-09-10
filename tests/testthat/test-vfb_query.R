test_that("vfb_neo4j_query works", {
  skip_if_not_installed('neo2R')
  expect_s3_class(
    nclasses <- vfb_neo4j_query("MATCH (n:Neuron:Class) RETURN n.label"),
    'data.frame'
  )
  expect_true(nrow(nclasses)>10e3)
})
