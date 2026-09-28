report_workflow_fixture <- function(path) {
  data <- data.frame(
    Product = factor(c("A", "B")),
    Panelist = factor(c("P1", "P2")),
    Attribute = c(1, 2)
  )
  options <- nailqdaOptions$new(
    product = "Product",
    panelist = "Panelist",
    attributes = "Attribute",
    reportPath = path,
    reportRequest = ""
  )
  analysis <- nailqdaClass$new(options = options, data = data)
  analysis$results$generationState$setState(list(
    lastRequest = "",
    consumedRequests = character(),
    inputSignature = NULL,
    preparedSignature = NULL,
    responseSignature = NULL,
    response = NULL,
    status = "Ready",
    message = "",
    duration = NULL
  ))
  analysis$results$reportState$setState(list(
    lastRequest = "",
    consumedRequests = character(),
    status = "Ready",
    message = "",
    path = NULL,
    duration = NULL
  ))
  analysis
}


set_report_interpretation <- function(analysis, response_signature = NULL) {
  state <- analysis$results$generationState$state
  if (is.null(response_signature))
    response_signature <- state$inputSignature
  state$responseSignature <- response_signature
  state$response <- "Interprétation courante"
  state$status <- "Complete"
  analysis$results$generationState$setState(state)
  report_request <- analysis$options$option("reportRequest")
  report_request$value <- "report-1"
}


run_report_with_mocked_nailer <- function(analysis, calls) {
  testthat::with_mocked_bindings(
    analysis$run(),
    nail_qda = function(..., generate = FALSE) {
      calls$generate <- c(calls$generate, generate)
      list()
    },
    nail_evidence = function(x, select = NULL) list(preview = TRUE),
    nail_prompt = function(x, select = NULL, print = TRUE) "prompt",
    .package = "NaileR"
  )
}


test_that("one report request writes once and never calls the LLM", {
  target <- tempfile("LacqueR workflow ", fileext = ".pptx")
  on.exit(unlink(target), add = TRUE)
  expected_target <- .nailqda_validate_report_path(target)
  analysis <- report_workflow_fixture(target)
  calls <- new.env(parent = emptyenv())
  calls$generate <- logical()

  # Establish the exact signature produced by the jamovi data wrapper.
  run_report_with_mocked_nailer(analysis, calls)
  set_report_interpretation(analysis)
  run_report_with_mocked_nailer(analysis, calls)
  first_size <- file.info(target)$size

  expect_true(file.exists(target))
  expect_identical(calls$generate, c(FALSE, FALSE))
  expect_identical(analysis$results$reportState$state$status, "Complete")
  expect_identical(analysis$results$reportState$state$path, expected_target)
  expect_identical(
    analysis$results$reportState$state$consumedRequests,
    "report-1"
  )

  second_target <- tempfile("LacqueR changed path ", fileext = ".pptx")
  report_path <- analysis$options$option("reportPath")
  report_path$value <- second_target
  run_report_with_mocked_nailer(analysis, calls)
  expect_identical(calls$generate, c(FALSE, FALSE, FALSE))
  expect_identical(analysis$results$reportState$state$status, "Complete")
  expect_identical(file.info(target)$size, first_size)
  expect_false(file.exists(second_target))
})


test_that("an outdated interpretation prevents report creation without Ollama", {
  target <- tempfile("LacqueR outdated ", fileext = ".pptx")
  analysis <- report_workflow_fixture(target)
  calls <- new.env(parent = emptyenv())
  calls$generate <- logical()

  run_report_with_mocked_nailer(analysis, calls)
  set_report_interpretation(analysis, "outdated-signature")
  run_report_with_mocked_nailer(analysis, calls)

  expect_false(file.exists(target))
  expect_identical(calls$generate, c(FALSE, FALSE))
  expect_identical(analysis$results$reportState$state$status, "Error")
  expect_identical(
    analysis$results$reportState$state$message,
    "Generate an up-to-date interpretation before creating the PowerPoint report."
  )
  expect_identical(
    analysis$results$reportState$state$consumedRequests,
    "report-1"
  )
})
