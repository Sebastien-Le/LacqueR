generation_fixture <- function(request = "request-1") {
  options <- nailqdaOptions$new(
    product = "Product",
    panelist = "Panelist",
    attributes = "Attribute",
    generationRequest = request
  )
  data <- data.frame(
    Product = factor(c("A", "B")),
    Panelist = factor(c("P1", "P2")),
    Attribute = c(1, 2)
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
  analysis
}

run_with_mocked_nailer <- function(analysis, calls) {
  testthat::with_mocked_bindings(
    analysis$run(),
    nail_qda = function(..., generate = FALSE) {
      calls$generate <- c(calls$generate, generate)
      list()
    },
    nail_evidence = function(x, select = NULL) list(preview = TRUE),
    nail_prompt = function(x, select = NULL, print = TRUE) "prompt",
    nail_response = function(x, select = NULL, print = TRUE) "response",
    .package = "NaileR"
  )
}

test_that("a fresh request generates after preparing its preview", {
  analysis <- generation_fixture()
  calls <- new.env(parent = emptyenv())
  calls$generate <- logical()

  run_with_mocked_nailer(analysis, calls)

  expect_identical(calls$generate, c(FALSE, TRUE))
  expect_identical(analysis$results$generationState$state$status, "Complete")
  expect_identical(analysis$results$generationState$state$response, "response")

  run_with_mocked_nailer(analysis, calls)
  expect_identical(calls$generate, c(FALSE, TRUE, FALSE))
})

test_that("a restart after preview prevents generation with stale inputs", {
  analysis <- generation_fixture()
  calls <- new.env(parent = emptyenv())
  calls$generate <- logical()
  analysis$.setCheckpoint(function(results) "restart")

  testthat::with_mocked_bindings(
    error <- expect_error(run_with_mocked_nailer(analysis, calls), "restarting"),
    RProtoBuf_serialize = function(object, connection) raw(),
    .package = "jmvcore"
  )
  expect_identical(error$code, "restart")
  expect_identical(calls$generate, FALSE)
})
