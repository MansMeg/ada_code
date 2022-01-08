context("README")

# Run the line below to run different test suites locally
# See documentation for details.
# adapop:::set_test_stan_basic_on_local(TRUE)
# adapop:::set_test_stan_full_on_local(TRUE)

test_that("README works as expected", {
  skip_if_not(test_stan_basic_on_local() | test_stan_full_on_local() | on_github_actions())

  root_path <- get_working_directory_path()
  fp <- file.path(root_path, "README.Rmd")
  expect_message(output <- capture.output(krm <- knitr::knit(fp, output = "tmp.md")),
                 regexp = "output file: tmp.md")
  file.remove("tmp.md")

})

