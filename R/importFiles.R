importFiles <- function(...){
  
  arguments <- unlist(list(...))
  k=length(arguments)
  D=list()
  # enc="UTF-8"
  # origEnc=getOption("encoding")
  # if (origEnc=="UTF-8"){options(encoding = "native.enc")}
  for (i in 1:k){
    
    D[[i]] <- read_lines(
      arguments[i],
      skip = 0,
      n_max = -1L,
      locale = default_locale(),
      progress = show_progress()
    )
    
    
    # D[[i]]=suppressWarnings(
    #   iconv(readLines(arguments[i],encoding = "UTF-8"),"latin1", "ASCII", sub="")
    #   #conv(readLines(arguments[[i]]))
    # )
  }
  D=unlist(D)
  # options(encoding = origEnc)
  # Encoding(D) <- "UTF-8"
  return(D)
  
}
