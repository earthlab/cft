#' We are chunking the date ranges up into the maximum number of days
#' before getting multiple IO or DAP failures per request. It seems to work
#' fairly well at 3 or 4 years, but starts failing at 5 or more. I'd like
#' to try chunks that exactly line up with the files on northwestknowledge
#' every 5 years (1950 - 1954, 1955 - 1959, 1960 - 1964), but 5 years...
#'
#' Anyway, this file is to parallelize which chunks we choose between however
#' many cores we can manage.

library(climateR)
library(parallel)

parScenario <- function(AOI, method, param, model, scenario, timeRes, location, cc, cl){

  # Get start and end date vector
  starts <- lapply(cc[[1]], function(x) x[[1]])
  ends <- lapply(cc[[1]], function(x) tail(x, n = 1))

  # We need a vector of all possible arguments
  n <- length(cc[[1]])
  arguments <- c(AOI, method, param, model, scenario, as.character(starts[[1]]),
                 as.character(ends[[1]]), timeRes)
  names(arguments) <- c("AOI", "method", "param", "model", "scenario", "startDate",
                        "endDate", "timeRes")
  argument_dictionary <- lapply(datasets, formalArgs)

  # Get arguments associated with the chosen method
  method_args <- argument_dictionary[method]
  args <- lapply(method_args, function(k) arguments[k])[[1]]
  if (args['endDate'] == 'NULL') args <- args[!grepl('endDate', names(args))]

  # Now, we want the remaining arguments without dates
  args <- args[!grepl("startDate", names(args))]
  args <- args[!grepl("endDate", names(args))]

  # Make the list of file names
  locale <- gsub(" ", "_", tolower(location))
  fparam <- paste0(c(method, model, scenario, param), collapse = "_")
  folder <- file.path("data", "rasters", "tests", locale, fparam)
  fstarts <- lapply(starts, function(x) gsub("-", "", x))
  fends <- lapply(ends, function(x) gsub("-", "", x))
  franges <- lapply(1:n, function(i) paste0(c(fstarts[[i]], fends[[i]]), collapse = "_"))
  fileranges <- paste0(franges, ".nc")
  files <- file.path(folder, fileranges)
  if (!file.exists(folder)) dir.create(folder, recursive = TRUE)

  # Try this with clusterMap
  tryCatch({
    # Get the list of rasterbricks
    clusterExport(cl, datasets[[method]])
    data <- clusterMap(cl, datasets[[method]], startDate = starts, endDate = ends,
                       MoreArgs=args)

    # Now save all these to the files
    for(i in 1:n){
      makeNC(data[[i]], savepath = files[[i]], method = method, model = model, param = param,
            location = location, scenario = scenario, naval = -9999)
    }

  }, error = function(e){
    # It will almost always break at least once
    print(paste(e, ": Taking a break and trying again..."))
    Sys.sleep(3)

    # Get the list of rasterbricks
    data <- clusterMap(cl, datasets[[method]], startDate = starts, endDate = ends,
                       MoreArgs=args)

    # Now save all these to the files
    for(i in 1:n){
      makeNC(data[[i]], savepath = files[[i]], method = method, model = model, param = param,
             location = location, scenario = scenario, naval = -9999)
    }
  })

}
