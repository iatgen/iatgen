context("writeIATfull file handling")

# writeIATfull() writes folders and files into a directory on disk and then cleans some
# of them up again. Those side effects were previously untested, which is how it came
# to delete directories it had not created.

test_that("the QSF is written to outdir and nothing is left in the working directory", {
  in_temp_dir({
    dir.create("survey")

    writeIATfull_canned_recipe(outdir = "survey")

    expect_true(file.exists(file.path("survey", "iat-flowins.qsf")))
    # the intermediate folders are cleaned up, and nothing lands beside them
    expect_equal(list.files("survey"), "iat-flowins.qsf")
    expect_equal(list.files("."), "survey")
  })
})

test_that("the working directory is unchanged after building a survey", {
  in_temp_dir({
    before <- getwd()
    writeIATfull_canned_recipe()
    expect_equal(getwd(), before)
  })
})

test_that("outdir defaults to the working directory", {
  in_temp_dir({
    writeIATfull_canned_recipe()
    expect_true(file.exists("iat-flowins.qsf"))
  })
})

test_that("a non-existent outdir is reported clearly", {
  in_temp_dir({
    expect_error(
      writeIATfull_canned_recipe(outdir = "no/such/place"),
      "does not exist"
    )
  })
})

test_that("a pre-existing folder is preserved rather than deleted", {
  # writeIATfull() removes its four working folders once the QSF is built. A folder of
  # the same name that it did not create may hold the user's own files.
  in_temp_dir({
    dir.create("1 flowins_rp")
    writeLines("do not delete", file.path("1 flowins_rp", "notes.txt"))

    expect_warning(
      writeIATfull_canned_recipe(),
      "already existed"
    )

    expect_true(file.exists(file.path("1 flowins_rp", "notes.txt")))
    expect_equal(readLines(file.path("1 flowins_rp", "notes.txt")), "do not delete")
    # the survey is still produced, and the folders it did create are still cleaned up
    expect_true(file.exists("iat-flowins.qsf"))
    expect_false(dir.exists("2 flowins_rn"))
  })
})

test_that("a file named like the QSF template is neither read nor removed", {
  # The template used to be copied into the working directory before being parsed, so a
  # same-named file belonging to the user was picked up instead and then deleted.
  in_temp_dir({
    writeLines("not really a template", "FullTemplate_-_For_Shiny_V11.qsf")

    writeIATfull_canned_recipe()

    expect_true(file.exists("FullTemplate_-_For_Shiny_V11.qsf"))
    expect_equal(readLines("FullTemplate_-_For_Shiny_V11.qsf"), "not really a template")
    expect_true(file.exists("iat-flowins.qsf"))
  })
})

test_that("manual mode leaves the four permutation folders in place", {
  in_temp_dir({
    writeIATfull(
      IATname = "manual",
      posname = "Pleasant", negname = "Unpleasant",
      Aname = "Flowers", Bname = "Insects",
      catType = "words",
      poswords = c("Gentle", "Enjoy", "Heaven", "Cheer", "Happy", "Love", "Friend"),
      negwords = c("Poison", "Evil", "Gloom", "Damage", "Vomit", "Ugly", "Hurt"),
      tgtType = "words",
      Awords = c("Orchid", "Tulip", "Rose", "Daffodil", "Daisy", "Lilac", "Lily"),
      Bwords = c("Wasp", "Flea", "Roach", "Centipede", "Moth", "Bedbug", "Gnat"),
      n = c(20, 20, 20, 40, 40, 20, 40),
      qsf = FALSE
    )

    expect_true(dir.exists("1 manual_rp"))
    expect_true(dir.exists("2 manual_rn"))
    expect_true(dir.exists("3 manual_lp"))
    expect_true(dir.exists("4 manual_ln"))
    expect_false(file.exists("iat-manual.qsf"))
    # each folder holds the HTML and JavaScript to paste into Qualtrics
    expect_true(length(list.files("1 manual_rp", pattern = "\\.txt$")) > 0)
  })
})


## --- argument validation ----------------------------------------------------

test_that("misspecified stimulus types are rejected", {
  in_temp_dir({
    expect_error(
      writeIATfull_canned_recipe(tgtType = "pictures"),
      "tgtType"
    )
    expect_error(
      writeIATfull_canned_recipe(catType = "pictures"),
      "catType"
    )
  })
})

test_that("the wrong number of block lengths is rejected", {
  in_temp_dir({
    expect_error(
      writeIATfull_canned_recipe(n = c(20, 20, 20)),
      "seven blocks"
    )
  })
})

test_that("an invalid swap argument is rejected", {
  in_temp_dir({
    expect_error(
      writeIATfull_canned_recipe(swap = "sideways"),
      "swap"
    )
  })
})
