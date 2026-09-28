.nailqda_validate_report_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(trimws(path))) {
    stop("Choose a destination path for the PowerPoint report.", call. = FALSE)
  }

  path <- path.expand(trimws(path))
  absolute <- startsWith(path, "/") ||
    grepl("^[A-Za-z]:[/\\\\]", path) ||
    startsWith(path, "\\\\") || startsWith(path, "//")
  if (!absolute) {
    stop("The PowerPoint report path must be absolute.", call. = FALSE)
  }

  extension <- tools::file_ext(basename(path))
  if (!nzchar(extension)) {
    path <- paste0(path, ".pptx")
  } else if (!identical(tolower(extension), "pptx")) {
    stop("The PowerPoint report path must use the .pptx extension.",
         call. = FALSE)
  }

  parent <- dirname(path)
  if (!dir.exists(parent)) {
    stop("The destination folder for the PowerPoint report does not exist.",
         call. = FALSE)
  }
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  path <- file.path(parent, basename(path))

  if (file.access(parent, 2L) != 0L) {
    stop("The destination folder is not writable.", call. = FALSE)
  }
  if (file.exists(path)) {
    stop("The PowerPoint report already exists. Choose another destination.",
         call. = FALSE)
  }

  path
}


.nailqda_build_pptx <- function(introduction, response) {
  context <- if (is.null(introduction) ||
                 !any(nzchar(trimws(as.character(introduction))))) {
    "No additional context provided."
  } else {
    paste(as.character(introduction), collapse = "\n")
  }
  interpretation <- .nailqda_clean_response_text(
    paste(as.character(response), collapse = "\n")
  )

  report <- officer::read_pptx()
  report <- officer::add_slide(
    report, layout = "Title Slide", master = "Office Theme")
  report <- officer::ph_with(
    report, "AI-assisted QDA",
    location = officer::ph_location_type(type = "ctrTitle"))

  report <- officer::add_slide(
    report, layout = "Title and Content", master = "Office Theme")
  report <- officer::ph_with(
    report, "Interpretation context",
    location = officer::ph_location_type(type = "title"))
  report <- officer::ph_with(
    report, context,
    location = officer::ph_location_type(type = "body"))

  report <- officer::add_slide(
    report, layout = "Title and Content", master = "Office Theme")
  report <- officer::ph_with(
    report, "AI-assisted interpretation",
    location = officer::ph_location_type(type = "title"))
  officer::ph_with(
    report, interpretation,
    location = officer::ph_location_type(type = "body"))
}


.nailqda_write_pptx <- function(report, path) {
  if (file.exists(path)) {
    stop("The PowerPoint report already exists. Choose another destination.",
         call. = FALSE)
  }

  temporary <- tempfile(
    pattern = ".lacquer-qda-report-",
    tmpdir = dirname(path),
    fileext = ".pptx"
  )
  on.exit(unlink(temporary), add = TRUE)

  tryCatch(
    print(report, target = temporary),
    error = function(e) {
      stop(
        paste("The PowerPoint report could not be written:",
              conditionMessage(e)),
        call. = FALSE
      )
    }
  )
  if (!file.exists(temporary) || is.na(file.info(temporary)$size) ||
      file.info(temporary)$size <= 0L) {
    stop("The PowerPoint report could not be written.", call. = FALSE)
  }
  if (file.exists(path) || !file.rename(temporary, path)) {
    stop("The PowerPoint report could not be moved to its destination.",
         call. = FALSE)
  }

  path
}


.nailqda_consume_report_request <- function(state, request) {
  if (is.null(state$consumedRequests))
    state$consumedRequests <- character()
  request <- if (is.null(request) || is.na(request)) "" else as.character(request)
  fresh <- nzchar(request) && !request %in% state$consumedRequests
  if (fresh) {
    state$lastRequest <- request
    state$consumedRequests <- c(state$consumedRequests, request)
  }
  list(state = state, fresh = fresh)
}
