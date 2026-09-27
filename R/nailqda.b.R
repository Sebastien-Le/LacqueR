# This file is a generated template, your changes will not be overwritten

nailqdaClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "nailqdaClass",
  inherit = nailqdaBase,
  private = list(
    
    .run = function() {
      
      # ------------------------------------------------------------
      # 1. Wait until the required variables have been selected
      # ------------------------------------------------------------
      
      request <- self$options$generationRequest
      state <- self$results$generationState$state
      if (is.null(state)) {
        # An identifier restored without its state is never a fresh click.
        state <- list(lastRequest = request,
                      consumedRequests = if (nzchar(request)) request else character(),
                      inputSignature = NULL,
                      preparedSignature = NULL, responseSignature = NULL,
                      response = NULL, status = "Ready", message = "",
                      duration = NULL)
      }
      new_request <- nzchar(request) && !request %in% state$consumedRequests
      if (new_request) {
        state$lastRequest <- request
        state$consumedRequests <- c(state$consumedRequests, request)
      }

      publish_status <- function() {
        self$results$generationState$setState(state)
        text <- c(state$status, state$message)
        if (!is.null(state$duration))
          text <- c(text, sprintf("Generation elapsed: %.2f s", state$duration))
        self$results$status$setContent(paste(text[nzchar(text)], collapse = "\n"))
      }

      if (is.null(self$options$product) || is.null(self$options$panelist) ||
          length(self$options$attributes) == 0L) {
        state["response"] <- list(NULL)
        state$preparedSignature <- NULL
        state$status <- "Ready"
        state$message <- if (is.null(state$responseSignature))
          "Select Product, Panelist and Sensory Attributes." else
          "Inputs changed — generate a new interpretation"
        state$duration <- NULL
        self$results$response$setContent("")
        self$results$evidence$setContent("")
        self$results$prompt$setContent("")
        publish_status()
        return()
      }

      product <- as.character(self$options$product)
      panelist <- as.character(self$options$panelist)
      attributes <- as.character(self$options$attributes)
      
      introduction <- self$options$introduction
      
      if (is.null(introduction) ||
          !nzchar(trimws(introduction))) {
        introduction <- NULL
      }
      
      
      # ------------------------------------------------------------
      # 2. Build a minimal QDA dataset for NaileR
      # ------------------------------------------------------------
      
      variables <- c(product, panelist, attributes)
      
      data_qda <- self$data[
        ,
        variables,
        drop = FALSE
      ]
      
      data_qda[[1]] <- as.factor(data_qda[[1]])
      data_qda[[2]] <- as.factor(data_qda[[2]])
      
      # Safe internal names for the two design variables
      names(data_qda)[1:2] <- c(
        ".Product",
        ".Panelist"
      )
      
      
      # ------------------------------------------------------------
      # 3. Prepare the exact inputs and identify a fresh request
      # ------------------------------------------------------------

      model <- trimws(as.character(self$options$model))
      if (!nzchar(model))
        model <- "mistral-small3.2"

      qda_args <- list(
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
      signature <- .nailqda_input_signature(variables, qda_args)
      if (!identical(signature, state$inputSignature)) {
        state["response"] <- list(NULL)
        state$status <- "Ready"
        state$message <- if (is.null(state$responseSignature)) "" else
          "Inputs changed — generate a new interpretation"
        state$duration <- NULL
      } else if (identical(state$status, "Generating interpretation…")) {
        state["response"] <- list(NULL)
        state$status <- "Error"
        state$message <- "Previous generation did not complete. Click Generate interpretation to retry."
      }
      state$inputSignature <- signature
      publish_status()

      # Keep the null graphics device around each NaileR call.
      run_qda <- function(generate) {
        grDevices::pdf(file = NULL)
        null_device <- grDevices::dev.cur()
        on.exit({
          devices <- grDevices::dev.list()
          if (!is.null(devices) && null_device %in% devices)
            grDevices::dev.off(null_device)
        }, add = TRUE)
        do.call(NaileR::nail_qda, c(qda_args, list(generate = generate)))
      }
      failure <- function(e) {
        if (inherits(e, "restart"))
          stop(e)
        list(error = conditionMessage(e))
      }

      # Evidence and prompt are always obtained without calling the LLM.
      preview <- tryCatch({
        res <- run_qda(FALSE)
        list(evidence = NaileR::nail_evidence(res),
             prompt = NaileR::nail_prompt(res, print = FALSE))
      }, error = failure)
      if (!is.null(preview$error)) {
        state$preparedSignature <- NULL
        state["response"] <- list(NULL)
        state$status <- "Error"
        state$message <- paste("NaileR QDA failed:", preview$error)
        state$duration <- NULL
        self$results$response$setContent("")
        self$results$evidence$setContent("")
        self$results$prompt$setContent("")
        publish_status()
        return()
      }
      state$preparedSignature <- signature
      prepared <- identical(signature, state$preparedSignature)
      evidence <- preview$evidence
      prompt <- preview$prompt

      # ------------------------------------------------------------
      # 7. Small formatter for prompt / response
      # ------------------------------------------------------------
      
      to_text <- function(x) {
        
        if (is.null(x))
          return("<not generated>")
        
        if (is.character(x))
          return(paste(x, collapse = "\n"))
        
        if (is.data.frame(x))
          return(
            paste(
              capture.output(print(x)),
              collapse = "\n"
            )
          )
        
        if (is.list(x)) {
          
          parts <- vapply(
            x,
            function(y) {
              
              if (is.character(y))
                paste(y, collapse = "\n")
              else
                paste(
                  capture.output(print(y)),
                  collapse = "\n"
                )
            },
            character(1)
          )
          
          return(
            paste(
              parts,
              collapse = "\n\n--------------------\n\n"
            )
          )
        }
        
        paste(
          capture.output(print(x)),
          collapse = "\n"
        )
      }
      
      
      # ------------------------------------------------------------
      # 8. Evidence
      # ------------------------------------------------------------
      
      evidence_text <- paste(
        capture.output(
          str(
            evidence,
            max.level = 4,
            give.attr = FALSE
          )
        ),
        collapse = "\n"
      )
      
      
      # ------------------------------------------------------------
      # 9. Display results
      # ------------------------------------------------------------
      
      self$results$evidence$setContent(
        evidence_text
      )
      
      self$results$prompt$setContent(
        to_text(prompt)
      )
      
      if (new_request && prepared) {
        state["response"] <- list(NULL)
        state$status <- "Generating interpretation…"
        state$message <- ""
        state$duration <- NULL
        self$results$response$setContent("")
        publish_status()
        # Preserve the consumed identifier for normal engine re-executions.
        if (is.function(private$.statePathSource))
          self$.save()
        private$.checkpoint()

        started <- proc.time()[["elapsed"]]
        generated <- tryCatch({
          res <- run_qda(TRUE)
          response <- NaileR::nail_response(res, print = FALSE)
          if (!.nailqda_valid_response(response))
            stop("NaileR returned an empty or invalid interpretation.")
          list(response = response)
        }, error = failure)
        state$duration <- unname(proc.time()[["elapsed"]] - started)
        if (!is.null(generated$error)) {
          state$status <- "Error"
          state$message <- paste("Generation failed:", generated$error)
        } else {
          state$response <- generated$response
          state$responseSignature <- signature
          state$status <- "Complete"
          state$message <- ""
        }
      } else if (new_request) {
        state$status <- "Ready"
        state$message <- "Inputs changed — generate a new interpretation"
      }
      response <- state$response
      self$results$response$setContent(
        .nailqda_response_to_html(.nailqda_clean_response_text(to_text(response)))
      )
      publish_status()
    }
  )
)
