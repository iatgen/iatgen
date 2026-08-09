context("writeIATfull stimulus variants")

# writeIATfull() branches on whether targets and categories are words or images, and on
# which pair swaps sides at block 5. Only the words/words, swap = "target" combination
# was covered; these drive the other branches.
#
# Manual mode (qsf = FALSE) is used throughout: it exercises the same stimulus and
# JavaScript generation but skips the slower QSF assembly, and leaves the generated
# files on disk where they can be inspected.

variant_args <- function(...) {
  utils::modifyList(
    list(
      IATname = "variant",
      posname = "Pleasant",
      negname = "Unpleasant",
      Aname = "Flowers",
      Bname = "Insects",
      n = c(20, 20, 20, 40, 40, 20, 40),
      qsf = FALSE
    ),
    list(...)
  )
}

word_cats <- list(
  catType = "words",
  poswords = c("Gentle", "Enjoy", "Heaven", "Cheer", "Happy", "Love", "Friend"),
  negwords = c("Poison", "Evil", "Gloom", "Damage", "Vomit", "Ugly", "Hurt")
)
word_tgts <- list(
  tgtType = "words",
  Awords = c("Orchid", "Tulip", "Rose", "Daffodil", "Daisy", "Lilac", "Lily"),
  Bwords = c("Wasp", "Flea", "Roach", "Centipede", "Moth", "Bedbug", "Gnat")
)
img_cats <- list(
  catType = "images",
  posimgs = paste0("https://example.com/pleasant", 1:7, ".jpg"),
  negimgs = paste0("https://example.com/unpleasant", 1:7, ".jpg")
)
img_tgts <- list(
  tgtType = "images",
  Aimgs = paste0("https://example.com/flower", 1:7, ".jpg"),
  Bimgs = paste0("https://example.com/insect", 1:7, ".jpg")
)

# All generated text for one IAT, concatenated - enough to check that a given stimulus
# made it into the survey somewhere.
generated_text <- function(iatname = "variant") {
  folders <- paste0(1:4, " ", iatname, c("_rp", "_rn", "_lp", "_ln"))
  files <- list.files(folders, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
  paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = "\n")
}

expect_four_permutation_folders <- function(iatname = "variant") {
  for (folder in paste0(1:4, " ", iatname, c("_rp", "_rn", "_lp", "_ln"))) {
    expect_true(dir.exists(folder))
    # seven blocks, each contributing an HTML and a JavaScript file
    expect_equal(length(list.files(folder, pattern = "html")), 7)
    expect_equal(length(list.files(folder, pattern = "JavaScript")), 7)
  }
}


## --- stimulus types ---------------------------------------------------------

test_that("word targets with word categories build all four permutations", {
  in_temp_dir({
    do.call(writeIATfull, c(variant_args(), word_cats, word_tgts))
    expect_four_permutation_folders()

    txt <- generated_text()
    expect_true(grepl("Orchid", txt, fixed = TRUE))
    expect_true(grepl("Gentle", txt, fixed = TRUE))
  })
})

test_that("image targets with word categories reach the generated survey", {
  in_temp_dir({
    do.call(writeIATfull, c(variant_args(), word_cats, img_tgts))
    expect_four_permutation_folders()

    txt <- generated_text()
    expect_true(grepl("https://example.com/flower1.jpg", txt, fixed = TRUE))
    expect_true(grepl("https://example.com/insect7.jpg", txt, fixed = TRUE))
    # category words still appear alongside the target images
    expect_true(grepl("Gentle", txt, fixed = TRUE))
  })
})

test_that("word targets with image categories reach the generated survey", {
  in_temp_dir({
    do.call(writeIATfull, c(variant_args(), img_cats, word_tgts))
    expect_four_permutation_folders()

    txt <- generated_text()
    expect_true(grepl("https://example.com/pleasant1.jpg", txt, fixed = TRUE))
    expect_true(grepl("https://example.com/unpleasant7.jpg", txt, fixed = TRUE))
    expect_true(grepl("Orchid", txt, fixed = TRUE))
  })
})

test_that("image targets with image categories reach the generated survey", {
  in_temp_dir({
    do.call(writeIATfull, c(variant_args(), img_cats, img_tgts))
    expect_four_permutation_folders()

    txt <- generated_text()
    expect_true(grepl("https://example.com/flower1.jpg", txt, fixed = TRUE))
    expect_true(grepl("https://example.com/pleasant1.jpg", txt, fixed = TRUE))
  })
})

test_that("an image IAT can be packaged as a qsf", {
  # The QSF path splices the generated files into the Qualtrics template; confirm it
  # copes with image stimuli, not just words.
  in_temp_dir({
    do.call(writeIATfull, c(variant_args(qsf = TRUE), word_cats, img_tgts))

    expect_true(file.exists("iat-variant.qsf"))
    contents <- paste(readLines("iat-variant.qsf", warn = FALSE), collapse = "")
    expect_true(grepl("flower1.jpg", contents, fixed = TRUE))
  })
})


## --- which pair swaps sides at block 5 --------------------------------------

test_that("swap = 'category' and swap = 'target' produce different surveys", {
  # Block 5 onwards either moves the targets or moves the categories. Both appear in
  # the literature and they must not silently generate the same thing.
  target.swap <- in_temp_dir({
    do.call(writeIATfull, c(variant_args(swap = "target"), word_cats, word_tgts))
    generated_text()
  })
  category.swap <- in_temp_dir({
    do.call(writeIATfull, c(variant_args(swap = "category"), word_cats, word_tgts))
    generated_text()
  })

  expect_true(nzchar(target.swap))
  expect_true(nzchar(category.swap))
  expect_false(identical(target.swap, category.swap))
})

test_that("swap = 'category' builds a complete set of files", {
  in_temp_dir({
    do.call(writeIATfull, c(variant_args(swap = "category"), word_cats, word_tgts))
    expect_four_permutation_folders()
  })
})


## --- stimulus ordering ------------------------------------------------------

test_that("norepeat = TRUE builds a complete survey and changes the ordering code", {
  no.repeat <- in_temp_dir({
    do.call(writeIATfull, c(variant_args(norepeat = TRUE), word_cats, word_tgts))
    expect_four_permutation_folders()
    generated_text()
  })
  default <- in_temp_dir({
    do.call(writeIATfull, c(variant_args(), word_cats, word_tgts))
    generated_text()
  })

  expect_false(identical(no.repeat, default))
})


## --- block lengths ----------------------------------------------------------

test_that("a shorter IAT than the default can be built", {
  # Blocks 3, 4, 6 and 7 are the combined blocks and must divide by four; the rest
  # only need to be even.
  in_temp_dir({
    do.call(writeIATfull, c(
      variant_args(n = c(10, 10, 12, 20, 20, 12, 20)),
      word_cats, word_tgts
    ))
    expect_four_permutation_folders()
  })
})

test_that("an odd number of trials in a single-category block is rejected", {
  # Trials alternate between the two categories, so each block needs an even count.
  in_temp_dir({
    expect_error(
      do.call(writeIATfull, c(
        variant_args(n = c(21, 20, 20, 40, 40, 20, 40)),
        word_cats, word_tgts
      )),
      "must be even"
    )
  })
})

test_that("a combined block not divisible by four is rejected", {
  # Combined blocks interleave positive, negative, target A and target B stimuli, so
  # their length has to divide by four for the four pools to be sampled evenly.
  in_temp_dir({
    expect_error(
      do.call(writeIATfull, c(
        variant_args(n = c(20, 20, 10, 40, 40, 20, 40)),
        word_cats, word_tgts
      )),
      "divisible by four"
    )
  })
})
