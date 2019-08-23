
# Magic separator for class methods
classSep <- "__CLASS__"

analyzePkg <- function(pkg, rlib, ver){
  cat("\t", pkg, "\n")
  rds <- list.files(file.path(rlib, pkg, "man"))
  lapply(rds, function(rd){
    #cat("\t\t", rd, "\n")
    rd_path <- file.path(rlib, pkg, "man", rd)
    p <- tools::parse_Rd(rd_path)
    pg <<- p

    # TODO: some exported functions aren't documented in usage. See `utils::de.ncols`

    usage <- p[sapply(p, function(x){attr(x, "Rd_tag") == "\\usage"})]
    if (length(usage) == 0){
      return()
    }

    # Replace \\dots with ...
    idx <- vapply(usage[[1]], function(x){ attr(x, "Rd_tag") == "\\dots"}, logical(1))
    usage[[1]][idx] <- "..."

    # The class of S3 methods appears as the second element in the list. i.e. in relist.Rd 3.5.2:
    # [[3]]
    # [[3]][[1]]
    # [[3]][[1]][[1]]
    # [1] "relist"
    # attr(,"Rd_tag")
    # [1] "TEXT"
    #
    #
    # [[3]][[2]]
    # [[3]][[2]][[1]]
    # [1] "default"
    # attr(,"Rd_tag")
    # [1] "TEXT"
    # We want to filter out these second elements when we see this.
    usage[[1]] <- lapply(usage[[1]], function(u){
      if (identical(attr(u, "Rd_tag"), "\\method")) {
        # Combine the two elements into one using a magic string to help us later
        # identify that this is a special function.
        return(paste(as.character(unlist(u)), collapse=classSep))
      }
      u
    })

    str <- unlist(usage[[1]])

    #idx <- vapply(usage[[1]], function(x){ length(x) >= 1 && x != "\n"}, logical(1))
    #str <- unlist(usage[[1]][idx])

    # filter out comments (e.g. in base::base-internal.Rd)
    str <- str[!grepl("^%.*", str, perl = TRUE)]

    fil <- tempfile(fileext = ".Rdmped")
    cat(paste(str, collapse=""), file = fil)
    # TODO: you can recover from partially parsed files. See ?parse
    par <- tryCatch({parse(fil=fil)}, error=function(x){
      warning(pkg, "::", rd, ": ", x)
      NULL
    })
    unlink(fil)

    lapply(par, function(pa){
      # NULL is documented (see ?NULL) which really throws everything off. Treat this as a special case
      if (is.null(pa)){
        return(data.frame(ver=ver, pkg=pkg, file=rd, fun="NULL", class=NA_character_, paramName=NA_character_, placeholder=NA_character_, evaluated=FALSE, stringsAsFactors = FALSE))
      }

      if (is.symbol(pa)){
        # Example may just be an unevaluated call.
        # we'll just represent this with evaluated=FALSE
        return(data.frame(ver=ver, pkg=pkg, file=rd, fun=as.character(pa), class=NA_character_, paramName=NA_character_, placeholder=NA_character_, evaluated=FALSE, stringsAsFactors = FALSE))
      }

      fname <- as.character(pa[[1]])
      class <- NA_character_
      if (grepl(classSep, fname)){
        re <- paste0("^(.*)", classSep, "(.*)$")
        mat <- stringr::str_match(fname, re)

        if (nrow(mat) > 0){
          fname <- mat[1,2]
          class <- mat[1,3]
        }
      }

      # Only explore if there are actually arguments.
      placeholders <- NA_character_
      pnames <- NA_character_
      if (length(pa) > 1){
        placeholders <- as.character(pa[2:length(pa)])
        pnames <- names(pa)[2:length(pa)]

        # If no names, then the stated characters are the names of the parameters.
        if (is.null(pnames)){
          pnames <- placeholders
          placeholders <- rep(NA_character_, times = length(pa)-1)
        }

        # Where unnamed, we have no placeholder and the given text is the pname
        noplace <- pnames == ""
        pnames[noplace] <- placeholders[noplace]
        placeholders[noplace] <- NA_character_
      }

      # TODO: see R 3.5.2's utils::relist.Rd for an example of a documented, unexported function: unlist.relistable

      data.frame(ver=ver, pkg=pkg, file=rd, fun=fname, class=class, paramName=pnames, placeholder=placeholders, evaluated=TRUE, stringsAsFactors = FALSE)
    }) %>% bind_rows()
  }) %>% bind_rows()
}
