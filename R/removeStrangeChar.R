removeStrangeChar <- function(D) {
  ind <- numeric(length(D))
  for (i in 1:length(D)) {
    # print(i)
    # ind[i] <- nchar(D[i])

    res <- try(ind[i] <- nchar(D[i]), silent = TRUE)
    if (inherits(res, "try-error")) {
      ind[i] <- 0
      next
    }
  }
  D <- D[ind > 1]
}
