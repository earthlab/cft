
test_that("cft_df generates a tibble", {
  local_dir <- "."
  file_refs <- cftdata(park = "Acadia National Park",
                       years = c(2004, 2005),
                       models = c("bcc-csm1-1"),
                       parameters = c("pr"),
                       scenarios = c("rcp45"),
                       local_dir = local_dir)
  out <- cft_df(file_reference = file_refs)
  expect_s3_class(out, "data.frame")
})
