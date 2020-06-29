
BASE_URL = paste0("http://thredds.northwestknowledge.net:8080/thredds/",
                  "dodsC/MET")
CATALOG_URL = paste0("http://thredds.northwestknowledge.net:8080/thredds/",
                     "reacch_climate_MET_catalog.html")


#' @title gridMET Subset generator
#'
#' @description An R6 class data subset generator for the gridMET dataset.
GridMet <- R6::R6Class(
  
  classname = "GridMet",
  lock_objects = FALSE,
  portable = TRUE,
  public = list(
    
    #' @field base_url Base URL for data queries.
    base_url = BASE_URL,
    
    #' @field catalog_url URL to catalog of links for all available datasets.
    catalog_url = CATALOG_URL,
    
    #' @field arg_ref Argument options and parameter attributes.
    arg_ref = get_reference("gridmet"),
    
    #' @field grid_ref Grid attributes.
    grid_ref = get_reference("grid"),
    
    #' @field project_dir The local directory in which to save files.
    project_dir = NULL,
    
    #' @field verbose Print verbose output.
    verbose = NULL,
    
    #' @description Initialize GridMet Object
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
    
    #' @description Print GridMet attributes
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

    #' @description Retrieve subset of GridMet data with currently set parameters.
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

      # We need the same arguments here both dont need these. What to do?
      models = NA
      scenarios = NA
  
      # Getting the queries list (private to avoid mismatches)
      queries <- private$get_queries(years, parameters)
      
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
                                aoi_info = self$aoi_info,
                                area_name = self$area_name,
                                area_dir = self$area_dir,
                                arg_ref = self$arg_ref,
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
    get_queries = function(years, parameters) {

      # Split year range up
      start_year <- years[1]
      end_year <- years[2]
      
      # Get relative index positions to full grid
      index_pos <- get_aoi_indexes(self$aoi, self$grid_ref, latflip=TRUE)
      y1 <- index_pos[["y1"]] - 1 
      y2 <- index_pos[["y2"]] - 1
      x1 <- index_pos[["x1"]]
      x2 <- index_pos[["x2"]]
      
      # Build a list of lists with model queries and a file name
      queries <- list()
      
      # Loop through all of the available arguments and build queries
      for (param in parameters) {
        
        # Get internal variable name
        var <- self$arg_ref$variables[param]
        
        # Build local file name
        file_name <- paste(c(param, self$area_name, "gridmet", as.character(start_year),
                             as.character(end_year), "daily.nc"), collapse = "_")
        
        # These come in individual yearly files
        yearly_urls <- c()
        for (y in start_year: end_year) {
          
          # Get the number of days for these years
          ntime <- private$year_days(y, param) - 1
          
          # Build the subsetting query
          subset <- glue::glue(paste0("?day[0:1:{ntime}],lat[{y2}:{1}:{y1}],lon[{x1}:{1}:{x2}],",
                                      "crs[0:1:0],{var}[{0}:{1}:{ntime}][{y2}:{1}:{y1}][{x1}:{1}:{x2}]",
                                      "#fillmismatch"))
          remote_file <- paste0(param, "_", y, ".nc")
          remote_path <- file.path(self$base_url, param, remote_file)
          url = paste0(remote_path, subset)
          yearly_urls[[length(yearly_urls) + 1]] <- url
        }

        # For further reference, create a vector of data set elements
        elements <- c("model" = NA,
                      "parameter" = param,
                      "rcp" = NA,
                      "ensemble" = NA,
                      "year1" = as.numeric(years[1]),
                      "year2" = as.numeric(years[2]),
                      "area_name" = self$area_name,
                      "units" = unname(self$arg_ref$units[unlist(param)]),
                      "full_varname" = unname(self$arg_ref$labels[unlist(param)]),
                      "internal_varname" = unname(var))
        
        # Combine everything into a query package and add to query list
        queries[[length(queries) + 1]] <- list(yearly_urls, file_name, elements)
      }
      return(queries)
    },

    year_days = function(year, parameter) {
      
      # Two scenarios, this year or previous years
      today <- Sys.Date()
      year <- as.character(year)
      
      if (year == format(today, "%Y")) {
        # There is a several day lag for new daily data
        ndays <- private$year_days_current(year, parameter)
        
      } else {
        # Only one possible number for past years
        last_day <- as.Date(paste(year, "12", "31", sep = "-"))
        first_day <- as.Date(paste(year, "01", "01", sep = "-"))
        days <- seq(first_day, last_day, by = "1 day")
        ndays <- length(days)
      }
      
      return(ndays)
    },
    
    year_days_current = function(year, parameter) {
      # Load cft conda environment
      reticulate::use_condaenv("cft", required = TRUE)
      xr <- reticulate::import("xarray")
      
      # How many days are available in current year for this parameter?
      query <- glue::glue(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                                 "MET/{parameter}/{parameter}_{year}.nc?#fillmismatch"))
      ds <- xr$open_dataset(query)
      ndays <- length(ds$day$data)
      return(ndays)
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
      sample_url = file.path(self$base_url, "pr/pr_2020.nc#fillmismatch")
      
      # Try to open a remote file from base url.
      tryCatch({
        xr$open_dataset(sample_url)
        return(TRUE)
      }, error=function(e) {
        print(e)
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
