test_that("report paths are validated conservatively", {
  expect_error(
    .nailqda_validate_report_path(""),
    "Choose a destination path"
  )
  expect_error(
    .nailqda_validate_report_path("report.pptx"),
    "must be absolute"
  )

  parent <- tempfile("LacqueR report été ")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)

  without_extension <- file.path(parent, "Rapport QDA été")
  expected <- file.path(normalizePath(parent, winslash = "/"),
                        "Rapport QDA été.pptx")
  expect_identical(
    .nailqda_validate_report_path(without_extension),
    expected
  )
  expect_identical(
    .nailqda_validate_report_path(expected),
    expected
  )
  expect_error(
    .nailqda_validate_report_path(file.path(parent, "report.pdf")),
    "must use the .pptx extension"
  )
  expect_error(
    .nailqda_validate_report_path(
      file.path(parent, "missing", "report.pptx")),
    "does not exist"
  )

  existing <- file.path(parent, "existing.pptx")
  file.create(existing)
  expect_error(
    .nailqda_validate_report_path(existing),
    "already exists"
  )
})


test_that("a three-slide PowerPoint report is built without writing", {
  response <- paste(
    "# Résumé",
    "Une interprétation.",
    "<!-- NAILER_PRODUCT_INTERPRETATION",
    '"product":"A"',
    "END_NAILER_PRODUCT_INTERPRETATION -->",
    sep = "\n"
  )
  report <- .nailqda_build_pptx("", response)
  content <- officer::pptx_summary(report)$text

  expect_s3_class(report, "rpptx")
  expect_length(report, 3L)
  expect_true(all(c(
    "AI-assisted QDA",
    "Interpretation context",
    "No additional context provided.",
    "AI-assisted interpretation"
  ) %in% content))
  expect_true(any(grepl("Une interprétation", content, fixed = TRUE)))
  expect_false(any(grepl("NAILER_PRODUCT_INTERPRETATION", content,
                         fixed = TRUE)))
})


test_that("a PowerPoint report is written once through a same-folder temporary", {
  parent <- tempfile("LacqueR output ")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)

  target <- .nailqda_validate_report_path(
    file.path(parent, "Rapport final é"))
  report <- .nailqda_build_pptx("Contexte", "Interprétation")
  written <- .nailqda_write_pptx(report, target)

  expect_identical(written, target)
  expect_true(file.exists(target))
  expect_gt(file.info(target)$size, 0)
  expect_true("ppt/presentation.xml" %in% unzip(target, list = TRUE)$Name)
  expect_length(list.files(parent, pattern = "^\\.lacquer-qda-report-"), 0L)
  expect_error(
    .nailqda_write_pptx(report, target),
    "already exists"
  )
})


test_that("report request identifiers are consumed only once", {
  state <- list(lastRequest = "", consumedRequests = character())

  first <- .nailqda_consume_report_request(state, "request-1")
  second <- .nailqda_consume_report_request(first$state, "request-1")
  third <- .nailqda_consume_report_request(second$state, "request-2")

  expect_true(first$fresh)
  expect_false(second$fresh)
  expect_true(third$fresh)
  expect_identical(third$state$consumedRequests,
                   c("request-1", "request-2"))
})
