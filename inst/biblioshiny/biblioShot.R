# Acknowledgment
# This function is based on the source code of the webshot2 package. 
# We would like to acknowledge and express our gratitude to the authors 
# and contributors of webshot2 for their valuable work in developing tools 
# for web content capture. 
#
# In this implementation, we have adapted and modified parts of the original 
# webshot2 code to address an issue where PNG files were being generated as 
# empty images when used on Windows systems. Our modifications aim to improve 
# compatibility and ensure reliable output across different platforms.
#
# Reference:
# webshot2 package: https://github.com/rstudio/webshot2

biblioShot <- function(
  url = NULL,
  file = "biblioShot.png",
  vwidth = 992,
  vheight = 744,
  selector = NULL,
  cliprect = NULL,
  expand = NULL,
  delay = 0.2,
  zoom = 1,
  useragent = NULL,
  max_concurrent = getOption("biblioShot.concurrent", default = 6),
  verbose = FALSE
) {

  if (length(url) == 0) {
    stop("Need url.")
  }

  url <- vapply(url,
    function(x) {
      if (!is_url(x)) {
        file_url(x)
      } else {
        x
      }
    },
    character(1)
  )

  if (!is.null(cliprect) && !is.list(cliprect)) cliprect <- list(cliprect)
  if (!is.null(selector) && !is.list(selector)) selector <- list(selector)
  if (!is.null(expand)   && !is.list(expand))   expand   <- list(expand)

  if (is.null(selector)) {
    selector <- "html"
  }

  if (length(url) > 1 && length(file) == 1) {
    file <- vapply(1:length(url), FUN.VALUE = character(1), function(i) {
      replacement <- sprintf("%03d.\\1", i)
      gsub("\\.(.{3,4})$", replacement, file)
    })
  }

  args_all <- list(
    url       = url,
    file      = file,
    vwidth    = vwidth,
    vheight   = vheight,
    selector  = selector,
    cliprect  = cliprect,
    expand    = expand,
    delay     = delay,
    zoom      = zoom,
    useragent = useragent,
    verbose = verbose
  )

  n_urls <- length(url)
  args_all <- mapply(args_all, names(args_all),
    FUN = function(arg, name) {
      if (length(arg) == 0) {
        return(vector(mode = "list", n_urls))
      } else if (length(arg) == 1) {
        return(rep(arg, n_urls))
      } else if (length(arg) == n_urls) {
        return(arg)
      } else {
        stop("Argument `", name, "` should be NULL, length 1, or same length as `url`.")
      }
    },
    SIMPLIFY = FALSE
  )

  args_all <- long_to_wide(args_all)

  cm <- default_chromote_object()

  # A list of promises for the screenshots
  res <- lapply(args_all,
    function(args) {
      new_session_screenshot(cm,
        args$url, args$file, args$vwidth, args$vheight, args$selector,
        args$cliprect, args$expand, args$delay, args$zoom, args$useragent,
        verbose
      )
    }
  )

  p <- promises::promise_all(.list = res)
  res <- cm$wait_for(p)
  res <- structure(unlist(res), class = "webshot")
  res
}


new_session_screenshot <- function(
  chromote,
  url,
  file,
  vwidth,
  vheight,
  selector,
  cliprect,
  expand,
  delay,
  zoom,
  useragent,
  verbose = FALSE
) {

  filetype <- tolower(tools::file_ext(file))
  if (filetype != "png" && filetype != "pdf") {
    stop("File extension must be 'png' or 'pdf'")
  }

  if (is.null(selector)) {
    selector <- "html"
  }

  if (is.character(cliprect)) {
    if (cliprect == "viewport") {
      cliprect <- c(0, 0, vwidth, vheight)
    } else {
      stop("Invalid value for cliprect: ", cliprect)
    }
  } else {
    if (!is.null(cliprect) && !(is.numeric(cliprect) && length(cliprect) == 4)) {
      stop("`cliprect` must be a vector with four numbers, or a list of such vectors")
    }
  }

  s <- NULL

  p <- chromote$new_session(wait_ = FALSE,
      width = vwidth,
      height = vheight
    )$
    then(function(session) {
      s <<- session

      if (!is.null(useragent)) {
        s$Network$setUserAgentOverride(userAgent = useragent)
      }
      res <- s$Page$loadEventFired(wait_ = FALSE)
      s$Page$navigate(url, wait_ = FALSE)
      res
    })$
    then(function(value) {
      if (delay > 0) {
        promises::promise(function(resolve, reject) {
          later::later(
            function() {
              resolve(value)
            },
            delay
          )
        })
      } else {
        value
      }
    })$
    then(function(value) {
      if (filetype == "png") {
        s$screenshot(
          filename = file, selector = selector, cliprect = cliprect,
          expand = expand, scale = zoom,
          show = FALSE, wait_ = FALSE
        )

      } else if (filetype == "pdf") {
        s$screenshot_pdf(filename = file, wait_ = FALSE)
      }
    })$
    then(function(value) {
      if (verbose) message(url, " screenshot completed")
      normalizePath(value)
    })$
    finally(function() {
      s$close()
    })

  p
}

is_url <- function(x) {
  grepl("^[a-zA-Z]+://", x)
}

# Given the path to a file, return a file:// URL.
file_url <- function(filename) {
  if (is_windows()) {
    paste0("file://", normalizePath(filename, mustWork = TRUE))
  } else {
    enc2utf8(paste0("file:///", normalizePath(filename, winslash = "/", mustWork = TRUE)))
  }
}

is_windows <- function() .Platform$OS.type == "windows"

is_mac <- function() Sys.info()[["sysname"]] == "Darwin"

is_linux <- function() Sys.info()[["sysname"]] == "Linux"

long_to_wide <- function(x) {
  if (length(x) == 0)
    return(x)

  lapply(seq_along(x[[1]]), function(n) {
    lapply(stats::setNames(names(x), names(x)), function(name) {
      x[[name]][[n]]
    })
  })
}