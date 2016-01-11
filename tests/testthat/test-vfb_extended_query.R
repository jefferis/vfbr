context("synonym queries")

test_that("exact synonym queries work", {
  expect_is(mbondf<-vfb_synonym_query("MBON-??"), 'data.frame')
  expect_equal(nrow(mbondf), 22L)
  mbondf$aso=sapply(mbondf$synonym, function(x) grep(glob2rx("MBON-??"), x, value=TRUE))
  expect_true(all(mbondf$aso %in% sprintf("MBON-%02d", 1:22)))
})

test_that("verbose messages for synonym queries work", {
  expect_message(vfb_synonym_query("rhubarb crumble", verbose=T),
                 "No results found")

  expect_message(vfb_synonym_query("antennal lobe", exact=FALSE, verbose=T),
                 "Return them all by setting rows=Inf")
})

test_that("quoted synonym queries work", {
  expect_gt(nqf<-nrow(vfb_synonym_query("antennal lobe", exact=F, quote = F, rows=Inf)), 0)
  expect_lt(nqt<-nrow(vfb_synonym_query("antennal lobe", exact=F, quote = T, rows=Inf)), nqf)
  expect_true(nqt>0)
})

test_that("can find synonyms only in main label", {
  expect_gt(nrow(vfb_synonym_query("antennal lobe", exact=T)),0)
})
