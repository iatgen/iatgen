context("build qsf process")

writeIATfull_canned_recipe <- function() {
  writeIATfull(
    IATname = "flowins",
    posname = "Pleasant",
    negname = "Unpleasant",
    Aname = "Flowers",
    Bname = "Insects",
    catType = "words",
    poswords = c("Gentle", "Enjoy", "Heaven", "Cheer", "Happy", "Love", "Friend"),
    negwords = c("Poison", "Evil", "Gloom", "Damage", "Vomit", "Ugly", "Hurt"),
    tgtType = "words",
    Awords = c("Orchid", "Tulip", "Rose", "Daffodil", "Daisy", "Lilac", "Lily"),
    Bwords = c("Wasp", "Flea", "Roach", "Centipede", "Moth", "Bedbug", "Gnat"),

    # advanced options with recommended IAT settings
    n = c(20, 20, 20, 40, 40, 20, 40),
    qsf = T,
    note = T,
    correct.error = T,
    pause = 250,
    tgtCol = "black",
    catCol = "green"
  )
}

test_that("Function to create qsf generates a qsf file with the correct name", {

  # Remove file from previous testing for clean environment
  if ("iat-flowins.qsf" %in% list.files()) {
    file.remove("iat-flowins.qsf")
  }
  writeIATfull_canned_recipe()

  expect_true("iat-flowins.qsf" %in% list.files())
  # Clean generated files for future tests to run in clean environment
  file.remove("iat-flowins.qsf")
})

# Note: there may be a cleaner way using `expect_snapshot_output_file()`
test_that("Function to create qsf creates a qsf withe the expected contents", {
  #generated_contents <- character()
  #expected_contents <- character()

  # Remove file from previous testing for clean environment
  if ("iat-flowins.qsf" %in% list.files()) {
    file.remove("iat-flowins.qsf")
  }
  writeIATfull_canned_recipe()

  expect_true(compare_file_text("expected_iat-flowins.qsf", "iat-flowins.qsf"))
  # Clean generated files for future tests to run in clean environment
  file.remove("iat-flowins.qsf")
})
