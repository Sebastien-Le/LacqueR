signature_for <- function(data, attributes = c("Sweet", "Sour"),
                          introduction = "Intro", model = "model-a") {
  variables <- c("Product", "Panelist", attributes)
  data_qda <- data[, variables, drop = FALSE]
  data_qda[[1]] <- as.factor(data_qda[[1]])
  data_qda[[2]] <- as.factor(data_qda[[2]])
  names(data_qda)[1:2] <- c(".Product", ".Panelist")
  arguments <- list(
    dataset = data_qda,
    formul = "~.Product+.Panelist",
    firstvar = 3,
    lastvar = ncol(data_qda),
    introduction = introduction,
    isolate.groups = FALSE,
    drop.negative = FALSE,
    proba = 0.05,
    sample.pct = 1,
    sample.method = "stratified",
    prompt_style = "detailed",
    product_knowledge = "known",
    provider = "ollama",
    model = model
  )
  .nailqda_input_signature(variables, arguments)
}

signature_data <- data.frame(
  Product = c("A", "B"),
  Panelist = c("P1", "P2"),
  Sweet = c(1, 2),
  Sour = c(3, 4),
  Bitter = c(5, 6),
  Unused = c("x", "y"),
  check.names = FALSE
)

test_that("identical inputs have identical signatures", {
  expect_identical(signature_for(signature_data), signature_for(signature_data))
})

test_that("selected data changes affect the signature", {
  changed <- signature_data
  changed$Sweet[1] <- 99
  expect_false(identical(signature_for(signature_data), signature_for(changed)))
})

test_that("selected variables and their order affect the signature", {
  base <- signature_for(signature_data)
  expect_false(identical(base, signature_for(signature_data, c("Sweet", "Bitter"))))
  expect_false(identical(base, signature_for(signature_data, c("Sour", "Sweet"))))
})

test_that("introduction and model affect the signature", {
  base <- signature_for(signature_data)
  expect_false(identical(base, signature_for(signature_data, introduction = "Autre")))
  expect_false(identical(base, signature_for(signature_data, model = "model-b")))
})

test_that("unselected columns do not affect the signature", {
  changed <- signature_data
  changed$Unused <- c("changed", "values")
  expect_identical(signature_for(signature_data), signature_for(changed))
})
