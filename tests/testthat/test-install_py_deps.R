test_that("install_py_deps creates the expected conda environment", {
  install_py_deps(method = "conda")
  
  conda_envs <- reticulate::conda_list()$name
  expect_true("cft" %in% conda_envs)
  
  reticulate::use_condaenv("cft")
  xarray <- reticulate::import("xarray")
  expect_s3_class(xarray, "python.builtin.module")
})
