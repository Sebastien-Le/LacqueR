.nailqda_input_signature <- function(variables, arguments) {
  path <- tempfile("lacquer-inputs-")
  on.exit(unlink(path), add = TRUE)
  saveRDS(list(variables = variables, arguments = arguments), path,
          compress = FALSE, version = 2)
  unname(tools::md5sum(path))
}


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


.nailqda_valid_response <- function(response) {
  is.character(response) && length(response) > 0L && !anyNA(response) &&
    any(nzchar(trimws(response)))
}
