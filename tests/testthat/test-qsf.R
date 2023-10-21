context("build qsf process")

test_that("Function to create qsf", {
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
  expect_true("iat-flowins.qsf" %in% list.files())
})
