test_that("cft_df generates a tibble", {
  project_dir <- "test_project"
  dataset = "maca"
  file_refs <- cftdata(park = "Acadia National Park",
                       years = c(2004, 2005),
                       models = c("bcc-csm1-1"),
                       parameters = c("pr"),
                       dataset = dataset,
                       scenarios = c("rcp45"),
                       project_dir = project_dir)
  out <- cft_df(file_reference = file_refs)
  expect_s3_class(out, "data.frame")
})
