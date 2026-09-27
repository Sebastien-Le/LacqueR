test_that("supported Markdown is rendered", {
  text <- paste(
    "# Titre", "## Sous-titre", "### Détail", "", "Un paragraphe", "",
    "- premier", "* second", "", "1. un", "3. trois", "",
    "Texte **gras** et *italique*.", sep = "\n"
  )
  html <- .nailqda_response_to_html(text)

  expect_match(html, "<h1>Titre</h1>", fixed = TRUE)
  expect_match(html, "<h2>Sous-titre</h2>", fixed = TRUE)
  expect_match(html, "<h3>Détail</h3>", fixed = TRUE)
  expect_match(html, "<p>Un paragraphe</p>", fixed = TRUE)
  expect_match(html, "<ul><li>premier</li><li>second</li></ul>", fixed = TRUE)
  expect_match(html, '<ol><li value="1">un</li><li value="3">trois</li></ol>', fixed = TRUE)
  expect_match(html, "<strong>gras</strong>", fixed = TRUE)
  expect_match(html, "<em>italique</em>", fixed = TRUE)
})

test_that("HTML metacharacters and hostile HTML are escaped", {
  text <- paste(
    "5 < 7 > 3 & oui",
    '<script>alert("x")</script>',
    '<img src="x" onerror="alert(1)">',
    '<div style="background:url(javascript:x)">texte</div>',
    sep = "\n"
  )
  html <- .nailqda_response_to_html(text)

  expect_match(html, "5 &lt; 7 &gt; 3 &amp; oui", fixed = TRUE)
  expect_match(html, "&lt;script&gt;alert(&quot;x&quot;)&lt;/script&gt;", fixed = TRUE)
  expect_match(html, "&lt;img src=&quot;x&quot; onerror=&quot;alert(1)&quot;&gt;", fixed = TRUE)
  expect_match(html, "&lt;div style=&quot;background:url(javascript:x)&quot;&gt;", fixed = TRUE)
  expect_false(grepl("<script|<img|<div", html,
                     ignore.case = TRUE, perl = TRUE))
})

test_that("links and remote images remain inactive text", {
  html <- .nailqda_response_to_html(paste(
    "[danger](javascript:alert(1))",
    "![remote](https://example.invalid/image.png)", sep = "\n"
  ))

  expect_match(html, "[danger](javascript:alert(1))", fixed = TRUE)
  expect_match(html, "![remote](https://example.invalid/image.png)", fixed = TRUE)
  expect_false(grepl("<a|<img", html, ignore.case = TRUE, perl = TRUE))
})

test_that("knitr inline code and chunks cannot remain active", {
  inline <- .nailqda_response_to_html("Valeur: `r system('echo unsafe')`")
  chunk <- .nailqda_response_to_html(paste("```{r}", "system('echo unsafe')", "```",
                                  sep = "\n"))
  ordinary <- .nailqda_response_to_html("Des `backticks` ordinaires")

  expect_false(grepl("`", paste(inline, chunk, ordinary), fixed = TRUE))
  expect_match(inline, "&#96;r system(&#39;echo unsafe&#39;)&#96;", fixed = TRUE)
  expect_match(chunk, "&#96;&#96;&#96;{r}", fixed = TRUE)
  expect_match(ordinary, "&#96;backticks&#96;", fixed = TRUE)
})

test_that("empty responses produce empty HTML", {
  expect_identical(.nailqda_response_to_html(""), "")
})
