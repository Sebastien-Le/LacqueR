technical_block <- function(body = '"product":"A"') {
  paste0("<!-- NAILER_PRODUCT_INTERPRETATION\n", body,
         "\nEND_NAILER_PRODUCT_INTERPRETATION -->")
}

test_that("complete technical blocks are removed", {
  expect_identical(
    clean_response_text(paste("Narration", technical_block(), sep = "\n")),
    "Narration\n"
  )

  text <- paste("Avant", technical_block("one"), "Milieu",
                technical_block("two"), "Après", sep = "\n")
  expect_identical(clean_response_text(text), "Avant\n\nMilieu\n\nAprès")
})

test_that("an incomplete technical block is left intact", {
  text <- paste("Narration", "<!-- NAILER_PRODUCT_INTERPRETATION",
                "incomplete", sep = "\n")
  expect_identical(clean_response_text(text), text)
})

test_that("external fences are removed conservatively", {
  markdown <- paste0("```markdown\n# Résumé\n\nTexte\n",
                     technical_block(), "\n```")
  expect_identical(clean_response_text(markdown), "# Résumé\n\nTexte\n\n")

  html <- paste0("Narration\n```html\n", technical_block(), "\n```\nSuite")
  expect_identical(clean_response_text(html), "Narration\n\nSuite")
})

test_that("Unicode and narration outside metadata are unchanged", {
  text <- "Crème brûlée — très équilibrée.\n\nArômes : café, cacao & mûre."
  expect_identical(clean_response_text(text), text)
})
