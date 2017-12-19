context("Total")

test_that("get_energy_by", {
  expect_equal(get_energy_by("testfolder/00_EQ_a_R1_"), -20)
})


context("Entry")
test_that("Entry Class", {
  path <- "testfolder/10_EQ_1_StartingMaterial_10EQ1"
  entry <- Entry$new(path)
  expect_equal(entry$path, path)
  expect_equal(entry$energy, NULL)
})

