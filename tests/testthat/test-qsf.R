context("build qsf process")

# writeIATfull_canned_recipe() lives in helper-iat.R, shared with test-writeiat-io.R.
# expected_iat-flowins.qsf is a byte-for-byte record of that recipe's output, so the
# recipe's arguments must not be changed without regenerating the expected file.

test_that("Function to create qsf generates a qsf file with the correct name", {
  in_temp_dir({
    writeIATfull_canned_recipe()

    expect_true("iat-flowins.qsf" %in% list.files())
  })
})

# Note: there may be a cleaner way using `expect_snapshot_output_file()`
test_that("Function to create qsf creates a qsf withe the expected contents", {
  expected <- normalizePath("expected_iat-flowins.qsf")

  in_temp_dir({
    writeIATfull_canned_recipe()

    expect_true(compare_file_text(expected, "iat-flowins.qsf"))
  })
})

test_that("the survey name in the qsf follows the IATname argument", {
  in_temp_dir({
    writeIATfull_canned_recipe()

    q <- jsonlite::fromJSON("iat-flowins.qsf")
    expect_equal(q$SurveyEntry$SurveyName, "flowins")
  })
})

test_that("the qsf carries the HTML and JavaScript for all 28 IAT blocks", {
  # Four counterbalanced permutations of seven blocks each. If the code that splices
  # the generated files into the template silently missed some, the survey would import
  # into Qualtrics but fail to run.
  in_temp_dir({
    writeIATfull_canned_recipe()

    q <- jsonlite::fromJSON("iat-flowins.qsf")
    payload <- q$SurveyElements$Payload

    # jsonlite renders Payload either as a list of question objects or as a data frame
    # of columns, depending on the template; writeIATfull handles both, so match both.
    field <- function(name) {
      if (!is.null(payload[[name]]) && !is.list(payload[[name]])) {
        return(payload[[name]])
      }
      vapply(
        payload,
        function(x) if (is.list(x) && !is.null(x[[name]])) as.character(x[[name]])[1] else NA_character_,
        character(1)
      )
    }

    tags <- field("DataExportTag")
    iat.rows <- grep("^Q[0-9]+ [RL][NP][0-9]$", tags)

    expect_equal(length(iat.rows), 28)
    expect_true(all(!is.na(field("QuestionText")[iat.rows])))
    expect_true(all(nzchar(field("QuestionText")[iat.rows])))
    expect_true(all(!is.na(field("QuestionJS")[iat.rows])))
    expect_true(all(nzchar(field("QuestionJS")[iat.rows])))
  })
})

test_that("stimulus words reach the generated survey", {
  in_temp_dir({
    writeIATfull_canned_recipe()

    contents <- paste(readLines("iat-flowins.qsf", warn = FALSE), collapse = "")
    for (word in c("Orchid", "Wasp", "Gentle", "Poison", "Flowers", "Insects")) {
      expect_true(grepl(word, contents, fixed = TRUE))
    }
  })
})
