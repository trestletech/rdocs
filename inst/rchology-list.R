#! Rscript
#

# From rcheology
# https://github.com/hughjonesd/rcheology/blob/10709a7620cbe402ff16bac3bf28402c3377dbab/list-objects.R#L44

try(options(error = traceback))

rv <- R.Version()
shortRversion <- paste(rv$major, rv$minor, sep = ".")
S4exists <- rv$major > 1 || (rv$major == 1 && rv$minor >= "4.0") # think doing string comparisons OK
if (S4exists) library(methods)


funArgs <- function (fn) {
  a <- deparse(args(fn))
  res <- paste(a[-length(a)], collapse = "")
  res <- sub("^function ", "", res)

  res
}

# backported
is.primitive <- function (x) switch(typeof(x), special = , builtin = TRUE, FALSE)

safelyTestGeneric <- function (fname, ns) {
  if (is.primitive(get(fname, envir=ns))) return(NA)
  if (! S4exists) return(FALSE)

  return(isGeneric(fname, ns)) # can't use namespacing for early R, so no methods::
}


checkExported <- function(objName, pkg) {
  if (pkg == "base") return(TRUE)

  if (rv$major > 1 || (rv$major == 1 && rv$minor >= "7.0")) {
    # some packages used not to have a namespace. In this case we return NA
    cond <- try(objName %in% getNamespaceExports(pkg))
    if (inherits(cond, "try-error")) return(NA)
    return(cond)
  } else {
    return(TRUE)
  }
}

makeData <- function (pkg) {
  #nsName <- paste("package:", pkg, sep = "")
  #pkgObjNames  <- do.call("ls", list(nsName, all.names = TRUE)) # NSE weirdness in early R
  ns <- getNamespace(pkg)
  pkgObjNames  <- ls(ns, all.names=TRUE)
  pkgObjNames  <- sort(pkgObjNames)
  pkgObjs      <- lapply(pkgObjNames, get, envir = ns, inherits = FALSE)
  types        <- sapply(pkgObjs, typeof)
  isExported   <- sapply(pkgObjNames, checkExported, pkg)
  classes      <- sapply(pkgObjs, function (x) paste(class(x), collapse = "/"))
  generics     <- sapply(pkgObjNames, safelyTestGeneric, ns)
  args         <- sapply(pkgObjs, function (x) if (is.function(x)) funArgs(x) else NA)

  thisPkgData <- data.frame(
    name     = pkgObjNames,
    type     = types,
    class    = classes,
    exported = isExported,
    generic  = generics,
    args     = args,
    package  = rep(pkg, length(pkgObjNames)),
    Rversion = rep(shortRversion, length(pkgObjNames)) # necessary for old R
  )

  thisPkgData
}

#
# ip <- installed.packages()[, "Package"]
# pkgData <- data.frame(
#   name     = character(0),
#   type     = character(0),
#   class    = character(0),
#   exported = logical(0),
#   # S3method = character(0),
#   generic  = logical(0),
#   args     = character(0),
#   package  = character(0),
#   Rversion = character(0)
# )
#
# for (pkg in ip) {
#   try({
#     if (! library(pkg, character.only = TRUE, logical.return = TRUE)) {
#       warning(paste("Could not load", pkg))
#       break
#     }
#     thisPkgData <- makeData(pkg)
#     pkgData <- rbind(pkgData, thisPkgData)
#   })
# }
#
#
# if (rv$major == 1 && rv$minor < "2.0") {
#   source("write-table-backport.R")
# }
# # simulate write.csv for older Rs
# write.table(pkgData,
#             file = file.path("docker-data", paste("pkg_data-R-", shortRversion, ".csv", sep = "")),
#             row.names = FALSE,
#             sep       = ",",
#             qmethod   = "double",
#             col.names = TRUE
# )

