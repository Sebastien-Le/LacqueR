test_that("empty and invalid responses are rejected", {
  expect_false(.nailqda_valid_response(NULL))
  expect_false(.nailqda_valid_response(character()))
  expect_false(.nailqda_valid_response(""))
  expect_false(.nailqda_valid_response("  \n\t"))
  expect_false(.nailqda_valid_response(NA_character_))
  expect_false(.nailqda_valid_response(c("valid", NA_character_)))
  expect_false(.nailqda_valid_response(list("valid")))
})

test_that("non-empty textual responses are accepted", {
  expect_true(.nailqda_valid_response("Une interprétation."))
  expect_true(.nailqda_valid_response(c("", "Texte")))
})
