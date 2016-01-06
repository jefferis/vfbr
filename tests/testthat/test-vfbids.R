context("ids")

testthat::test_that("",{
  fcids=c("VGlut-F-000304", "VGlut-F-200278", "fru-F-200121", "TH-F-300016")
  expect_equal(vfbids<-vfb_tovfbids(fcids),
               c("VFB_00014755", "VFB_00015114", "VFB_00004615", "VFB_00013392"))

  expect_equal(vfb_fromvfbids(vfbids), fcids)
})
