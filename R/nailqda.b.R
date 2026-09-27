# This file is a generated template, your changes will not be overwritten

nailqdaClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "nailqdaClass",
  inherit = nailqdaBase,
  private = list(
    
    .run = function() {
      
      # ------------------------------------------------------------
      # 1. Wait until the required variables have been selected
      # ------------------------------------------------------------
      
      if (is.null(self$options$product))
        return()
      
      if (is.null(self$options$panelist))
        return()
      
      if (is.null(self$options$attributes) ||
          length(self$options$attributes) == 0)
        return()
      
      
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
      # 3. Decide whether the LLM should be called
      # ------------------------------------------------------------
      
      generate <- isTRUE(self$options$generate)
      
      model <- trimws(as.character(self$options$model))
      
      if (!nzchar(model))
        model <- "mistral-small3.2"
      
      
      
      # ------------------------------------------------------------
      # 5. Run NaileR
      #
      # SensoMineR::decat() calls par() even with graph = FALSE,
      # so provide a null graphics device.
      # ------------------------------------------------------------
      
      grDevices::pdf(file = NULL)
      null_device <- grDevices::dev.cur()
      
      res <- tryCatch(
        
        NaileR::nail_qda(
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
          model = model,
          
          generate = generate
        ),
        
        error = function(e) {
          
          jmvcore::reject(
            paste0(
              "NaileR QDA failed: ",
              conditionMessage(e)
            )
          )
          
          NULL
        },
        
        finally = {
          
          devices <- grDevices::dev.list()
          
          if (!is.null(devices) &&
              null_device %in% devices) {
            
            grDevices::dev.off(null_device)
          }
        }
      )
      
      
      if (is.null(res))
        return()
      
      
      # ------------------------------------------------------------
      # 6. Retrieve NaileR artifacts
      # ------------------------------------------------------------
      
      evidence <- NaileR::nail_evidence(res)
      prompt <- NaileR::nail_prompt(res)
      
      response <- if (generate) {
        NaileR::nail_response(res)
      } else {
        NULL
      }
      
      
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
      # Display-only cleanup; keep response unchanged
      # ------------------------------------------------------------

      clean_response_text <- function(text) {
        block <- paste0(
          "(?s)<!-- NAILER_PRODUCT_INTERPRETATION[[:space:]]+",
          "(?:(?!<!--|-->|END_NAILER_PRODUCT_INTERPRETATION).)*",
          "END_NAILER_PRODUCT_INTERPRETATION[[:space:]]*-->"
        )
        remove_blocks <- function(x) gsub(block, "", x, perl = TRUE)

        # If any technical marker remains, preserve the malformed response.
        if (grepl("NAILER_PRODUCT_INTERPRETATION", remove_blocks(text),
                  fixed = TRUE))
          return(text)

        # Unwrap only metadata-only HTML fences or an external narration fence.
        for (language in c("html", "markdown")) {
          fence <- paste0(
            "(?ms)^[ \\t]*```", language, "[ \\t]*\\r?\\n",
            ".*?^[ \\t]*```[ \\t]*(?:\\r?\\n|$)"
          )
          matches <- gregexpr(fence, text, perl = TRUE)[[1]]
          lengths <- attr(matches, "match.length")
          for (i in rev(seq_along(matches))) {
            if (matches[i] < 0L)
              next
            start <- matches[i]
            end <- start + lengths[i] - 1L
            wrapped <- substr(text, start, end)
            body <- sub("^[^\\n]*\\n", "", wrapped, perl = TRUE)
            body <- sub("(?m)^[ \\t]*```[ \\t]*(?:\\r?\\n)?$", "",
                        body, perl = TRUE)
            before <- substr(text, 1L, start - 1L)
            after <- substring(text, end + 1L)
            metadata_only <- grepl(block, body, perl = TRUE) &&
              !nzchar(trimws(remove_blocks(body)))
            external <- !nzchar(trimws(remove_blocks(paste0(before, after))))
            if (!grepl("(?m)^[ \\t]*```", body, perl = TRUE) &&
                ((language == "html" && metadata_only) ||
                 (language == "markdown" && external)))
              text <- paste0(before, body, after)
          }
        }
        remove_blocks(text)
      }

      # Escape untrusted text before introducing our own HTML tags.
      response_to_html <- function(text) {
        escapes <- c("&" = "&amp;", "<" = "&lt;", ">" = "&gt;",
                     '"' = "&quot;", "'" = "&#39;", "`" = "&#96;",
                     "\\" = "&#92;", "@" = "&#64;", "+" = "&#43;")
        for (symbol in names(escapes))
          text <- gsub(symbol, escapes[[symbol]], text, fixed = TRUE)

        inline <- function(line) {
          # Keep lines containing unsupported inline constructs literal.
          if (grepl("&#96;|&#92;|&lt;|\\[", line, perl = TRUE))
            return(line)
          line <- gsub("(?<!\\*)\\*\\*([^*[:space:]](?:[^*]*[^*[:space:]])?)\\*\\*(?!\\*)",
                       "<strong>\\1</strong>", line, perl = TRUE)
          gsub("(?<!\\*)\\*([^*[:space:]](?:[^*]*[^*[:space:]])?)\\*(?!\\*)",
               "<em>\\1</em>", line, perl = TRUE)
        }

        output <- character()
        paragraph <- character()
        list_type <- ""
        fence <- ""
        flush_paragraph <- function() {
          if (length(paragraph)) {
            output <<- c(output, paste0("<p>", paste(paragraph, collapse = "<br>"), "</p>"))
            paragraph <<- character()
          }
        }
        close_list <- function() {
          if (nzchar(list_type)) {
            output <<- c(output, paste0("</", list_type, ">"))
            list_type <<- ""
          }
        }

        for (line in strsplit(text, "\r\n|\n|\r", perl = TRUE)[[1]]) {
          marker <- regmatches(line, regexpr("^[ \\t]*(?:(&#96;){3,}|~{3,})", line, perl = TRUE))
          if (nzchar(fence) || length(marker)) {
            close_list()
            paragraph <- c(paragraph, line)
            if (!nzchar(fence)) {
              fence <- trimws(marker)
            } else if (identical(trimws(line), fence)) {
              fence <- ""
            }
            next
          }
          if (!nzchar(trimws(line))) {
            flush_paragraph()
            close_list()
            next
          }
          heading <- regexpr("^#{1,3} +", line)
          bullet <- grepl("^[-*] +[^ ]", line)
          numbered <- grepl("^[0-9]{1,9}\\. +[^ ]", line)
          if (heading[1] == 1L) {
            flush_paragraph()
            close_list()
            level <- nchar(sub(" .*", "", line))
            label <- substring(line, attr(heading, "match.length") + 1L)
            output <- c(output, paste0("<h", level, ">", inline(label), "</h", level, ">"))
          } else if (bullet || numbered) {
            flush_paragraph()
            type <- if (bullet) "ul" else "ol"
            if (!identical(list_type, type)) {
              close_list()
              output <- c(output, paste0("<", type, ">"))
              list_type <- type
            }
            value <- if (numbered)
              paste0(' value="', sub("\\..*", "", line), '"') else ""
            label <- sub("^([-*]|[0-9]{1,9}\\.) +", "", line)
            output <- c(output, paste0("<li", value, ">", inline(label), "</li>"))
          } else {
            close_list()
            paragraph <- c(paragraph, if (grepl("^[ \\t]", line)) line else inline(line))
          }
        }
        flush_paragraph()
        close_list()
        # No raw line breaks: untrusted text cannot begin a knitr chunk line.
        paste(output, collapse = "")
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
      
      self$results$status$setContent(
        paste0(
          "SUCCESS\n\n",
          "NaileR version: ",
          as.character(utils::packageVersion("NaileR")),
          "\n",
          "Provider: Ollama\n",
          "Model: ", model, "\n",
          "Product: ", product, "\n",
          "Panelist: ", panelist, "\n",
          "Sensory attributes: ",
          paste(attributes, collapse = ", "),
          "\n\n",
          "generate = ", generate,
          if (generate)
            "\nLLM call completed."
          else
            "\nNo LLM call has been made."
        )
      )
      
      self$results$evidence$setContent(
        evidence_text
      )
      
      self$results$prompt$setContent(
        to_text(prompt)
      )
      
      self$results$response$setContent(
        response_to_html(clean_response_text(to_text(response)))
      )
    }
  )
)