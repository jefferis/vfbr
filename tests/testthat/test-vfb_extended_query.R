context("synonym queries")

test_that("", {
  expect_is(mbondf<-vfb_synonym_query("MBON-??"), 'data.frame')
  expect_equal(nrow(mbondf), 22L)
  mbondf$aso=sapply(mbondf$synonym, function(x) grep(glob2rx("MBON-??"), x, value=TRUE))
  expect_true(all(mbondf$aso %in% sprintf("MBON-%02d", 1:22)))
})
