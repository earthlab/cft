
BASE_URL = paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                  "agg_macav2metdata")
CATALOG_URL = paste0("http://thredds.northwestknowledge.net:8080/thredds/",
                     "reacch_climate_CMIP5_macav2_catalog2.html")


#' @title MACA Subset generator
#'
#' @description An R6 class data subset generator for the MACA dataset.
Maca <- R6::R6Class(

  classname = "Maca",
  lock_objects = FALSE,
  portable = TRUE,
  public = list(

    #' @field base_url Base URL for data queries.
    base_url = BASE_URL,

    #' @field catalog_url URL to catalog of links for all available datasets.
    catalog_url = CATALOG_URL,

    #' @field arg_ref Argument options and parameter attributes.
    arg_ref = get_reference("maca"),

    #' @field grid_ref Grid attributes.
    grid_ref = get_reference("grid"),

    #' @field project_dir The local directory in which to save files.
    project_dir = NULL,

    #' @field verbose Print verbose output.
    verbose = NULL,

    #' @description Initialize Maca Object
    #' @param project_dir The local directory in which to save files. (character)
    #' @param verbose Print verbose output. (logical)
    initialize = function(project_dir = ".", verbose = FALSE) {

      # Create and store project directory path
      dir.create(project_dir, showWarnings = FALSE, recursive = TRUE)
      project_dir = normalizePath(project_dir)
      self$project_dir = project_dir
      self$verbose = verbose
  
      # Check that the server is open
      if (!self$server_open) {
        print("Server not available.")
      }
    },

    #' @description Print Maca attributes
    print = function() {

      # Print high level paths
      cat(class(self)[[1]], "Data Generator Class \n")
      cat("  OpenDAP URL: ", self$base_url, "\n", sep = "")
      cat("  Data Catalog URL: ", self$catalog_url, "\n", sep = "")
      cat("  Project Directory: ", self$project_dir, "\n", sep = "")

      # Print sub directories
      if ( "area_dir" %in% ls(self) ) {
        cat("  Current Area Directory: ", self$area_dir, "\n", sep = "")
      }

      invisible(self)
    },

    #' @description Set the area of interest for the next query.
    #' @param shp_path A path to a shapefile with which to clip the resulting data
    #'  sets. This path may be to a local .shp file or a url to a zipped remote
    #'  file. If this option is used, leave the `park` argument empty or set
    #'  to NA. (character)
    #' @param area_name If a shapefile path is used, provide a name to use as a
    #'  reference to the location. This will be used in file names, attributes, and
    #'  directories. (character)'
    #' @param park The name of a national park (e.g., "Yellowstone National
    #'  Park", "Yellowstone Park", "Yellowstone", "yellowstone", etc.). The user may
    #'  use this option in place of a shapefile path. In this case, leave the
    #'  shp_path argument empty or set to NA. (character)
    set_aoi = function(shp_path, area_name, park) {

      # Reformat the area name
      area_name <- gsub(" ", "_", tolower(area_name))
      self$area_name = area_name

      # Reset the directory to a subdirectory for the area of interest
      area_dir <- file.path(self$project_dir, area_name)
      dir.create(area_dir, showWarnings = FALSE, recursive = TRUE)
      area_dir = normalizePath(area_dir)
      self$area_dir = area_dir

      # Get the area of intereset shapefile object
      print("Retrieving area of interest boundaries...")
      aoi <- get_aoi(park, shp_path, area_name, self$project_dir)

      # Match coordinate systems
      aoi <- sp::spTransform(aoi, self$grid_ref$crs)

      # Check if this raster has been saved yet
      raster_dir = file.path(self$project_dir, "rasters")
      raster_path = file.path(raster_dir, paste0(area_name, ".tif"))
      if ( !file.exists(raster_path) ) {
          if ( self$verbose ) {
            print("Building area of interest grid...")
          }
      }

      # Get geographic information about the aoi
      aoi_info <- get_aoi_info(aoi, self$project_dir, area_name, self$grid_ref)

      # Assign as attributes
      self$aoi <- aoi
      self$aoi_info <- aoi_info
    },

    #' @description Retrieve subset of MACA data with currently set parameters.
    #' @param models A list of global circulation models to download. Only available
    #'  for GCMs. If left empty all available models will be downloaded. A list of
    #'  currently available models is available under cft::get_reference("maca")$models.
    #'  (vector)
    #' @param parameters A list of climate parameters to download. If left empty
    #'  all available parameters will be downloaded. A list of available of models is
    #'  available under cft::get_reference(<dataset>)$parameters. (vector)
    #' @param scenarios A list of representative concentration pathways (rcps) to
    #'  download. If left empty all available rcps will be downloaded. A list of
    #'  available of rcps is available under cft::get_reference(<dataset>)$scenarios.
    #'  (vector)
    #' @param ncores The number of cpus to use, which defaults to 1. (numeric)
    #' @param years The first and last years of the desired period. (vector)
    get_subset = function(models, parameters, scenarios, years, ncores = 1) {

      # Separate these out
      aoi_info = self$aoi_info
      area_name = self$area_name
      area_dir = self$area_dir
      arg_ref = self$arg_ref
      
      # Getting the queries list (private to avoid mismatches)
      queries <- private$get_queries(years, models, parameters, scenarios)

      # Setup parallelization
      pbapply::pboptions(use_lb = TRUE)
      cl <- parallel::makeCluster(ncores)
      on.exit(parallel::stopCluster(cl))
      parallel::clusterExport(cl, c("retrieve_subset", "filter_years"),
                              envir = environment())

      # If all that works, signal that the process is starting
      if (self$verbose) {
        print(paste("Retrieving climate data for", self$area_name))
        print(paste("Saving local files to", self$area_dir))
      }

      # Retrieve, subset, and write the files
      refs <- pbapply::pblapply(queries,
                                FUN = retrieve_subset,
                                years = years,
                                aoi_info = aoi_info,
                                area_name = area_name,
                                area_dir = area_dir,
                                arg_ref = arg_ref,
                                cl = cl)

      # Create a data frame from the file references
      file_references <- data.frame(do.call(rbind, refs), stringsAsFactors = FALSE)
      file_references <- tibble::as_tibble(lapply(file_references, unlist))
      file_references$parameter_long <- unlist(lapply(
        file_references$parameter, 
        FUN = function(x) self$arg_ref$variables[x]
      ))

      # Set file_references as an attribute
      self$file_references <- file_references
    }
  ),

  private = list(

    # For building the queries
    get_queries = function(years, models, parameters, scenarios) {
      aoi <- self$aoi
      area_name <- self$area_name
      arg_ref <- self$arg_ref
      grid_ref <- self$grid_ref
  
      # Split year range up
      start_year <- years[1]
      end_year <- years[2]
      
      # Get time information from our grid reference
      ntime_hist <- grid_ref$ntime_historical
      ntime_model <- grid_ref$ntime_model
      
      # Get relative index positions to full grid
      index_pos <- get_aoi_indexes(aoi, grid_ref)
      y1 <- index_pos[["y1"]]
      y2 <- index_pos[["y2"]]
      x1 <- index_pos[["x1"]]
      x2 <- index_pos[["x2"]]

      # Build a list of lists with historical/future model queries and a file name
      queries <- list()
      for (model in models) {
        
        # Available arguments for this model
        args <- arg_ref$get_args(model)
        avail_params <- args$parameters
        avail_scenarios <- args$scenarios
        
        # Available requested arguments
        params <- lapply(parameters, FUN = function(x) if (x %in% avail_params) x)
        rcps <- lapply(scenarios, FUN = function(x) if (x %in% avail_scenarios) x)
        params <- params[params %in% avail_params]
        rcps <- rcps[rcps %in% avail_scenarios]
        
        # Variable reference
        variables <- arg_ref$variables
        
        # Only one model run for each
        ensemble <- args$ensemble
        
        # Loop through all of the available arguments and build queries
        for (param in params) {
          for (rcp in rcps) {
            
            # Get internal variable name
            var <- variables[param]

            # Build local file name
            file_name <- paste(c(param, area_name, model, ensemble, rcp,
                                 "macav2metdata", as.character(start_year),
                                 as.character(end_year), "daily.nc"),
                               collapse = "_")
            
            # Build remote historical and future file names
            historical <- paste(c(self$base_url, param, model, ensemble, "historical",
                                  "1950_2005", "CONUS_daily.nc"), collapse = "_")
            future <- paste(c(self$base_url, param, model, ensemble, rcp,
                              "2006_2099", "CONUS_daily.nc"), collapse = "_")

            # Build the temporal and spatial subsets
            historical_subset <- glue::glue(paste0("?time[0:1:{ntime_hist}],lon[{x1}:{1}:{x2}],",
                                                   "lat[{y1}:{1}:{y2}],crs[0:1:0],",
                                                   "{var}[{0}:{1}:{ntime_hist}]",
                                                   "[{y1}:{1}:{y2}][{x1}:{1}:{x2}]",
                                                   "#fillmismatch"))

            future_subset <- glue::glue(paste0("?time[0:1:{ntime_model}],lon[{x1}:{1}:{x2}],",
                                               "lat[{y1}:{1}:{y2}],crs[0:1:0],",
                                               "{var}[{0}:{1}:{ntime_model}]",
                                               "[{y1}:{1}:{y2}][{x1}:{1}:{x2}]",
                                               "#fillmismatch"))

            # For further reference, create a vector of data set elements
            elements <- c("model" = model,
                          "parameter" = param,
                          "rcp" = rcp,
                          "ensemble" = ensemble,
                          "year1" = as.numeric(years[1]),
                          "year2" = as.numeric(years[2]),
                          "area_name" = area_name,
                          "units" = unname(arg_ref$units[unlist(var)]),
                          "full_varname" = unname(arg_ref$labels[unlist(param)]),
                          "internal_varname" = unname(var))
            
            # Combine everything into a query package and add to query list
            historical_url <- paste0(historical, historical_subset)
            future_url <- paste0(future, future_subset)
            paired_url <- c(historical_url, future_url)
            queries[[length(queries) + 1]] <- list(paired_url, file_name, elements)
          }
        }
      }
      
      return(queries)
    }
  ),

  active = list(

    #' @field server_open
    #'  Check access to server.
    server_open = function() {

      # Load python dependency
      reticulate::use_condaenv('cft', required = TRUE)
      xr <- reticulate::import("xarray")

      # This is very first possible file in our list
      sample_url = paste0(self$base_url,
                          "_vpd_BNU-ESM_r1i1p1_rcp85_2006_2099_CONUS_daily.nc",
                          "#fillmismatch")

      # Try to open a remote file from base url.
      tryCatch({
        xr$open_dataset(sample_url)
        return(TRUE)
      }, error=function() {
        return(FALSE)
      })
    },

    #' @field existing_aois
    #'  Check previously downloaded AOI rasters in currently set project directory.
    existing_aois = function() {

      # The raster directory is set here
      raster_dir <- file.path(self$project_dir, "rasters")
      if ( dir.exists(raster_dir) ) {
        existing_aois = Sys.glob(file.path(raster_dir, "*.tif"))
      } else {
        existing_aois = c()
      }
      return(existing_aois)
    },

    #' @field project_contents
    #'  Check existing climate files for the currently set aoi.
    project_contents = function() {
        existing_dirs = list.dirs(self$project_dir)
        existing_files = list.files(self$project_dir, recursive = TRUE)
        return(c(existing_dirs, existing_files))
    }
  )
)
