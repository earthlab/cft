
BASE_URL = paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                  "agg_macav2metdata")
CATALOG_URL = paste0("http://thredds.northwestknowledge.net:8080/thredds/",
                     "reacch_climate_CMIP5_macav2_catalog2.html")


#' @export
Maca <- R6::R6Class(

  classname = "Maca",
  lock_objects = FALSE,
  portable = TRUE,
  public = list(

    # MACA Specific OpenDAP and catalog URLs
    base_url = BASE_URL,
    catalog_url = CATALOG_URL,

    # Referenceobjects
    arg_ref = get_reference("maca"),
    grid_ref = get_reference("grid"),

    # Initalize arguments
    project_dir = NULL,
    verbose = NULL,
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

    # This is like __repr__
    print = function(...) {
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

    # For building the AOI
    set_aoi = function(park, shp_path, area_name) {

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

    # For executing the request
    get_subset = function(years, models, parameters, scenarios) {

      # Getting the queries list
      queries <- private$get_queries(years, models, parameters, scenarios)

      # Setup parallelization
      pbapply::pboptions(use_lb = TRUE)
      cl <- parallel::makeCluster(ncores)
      on.exit(parallel::stopCluster(cl))
      parallel::clusterExport(cl, c("retrieve_subset", "filter_years"),
                              envir = environment())

      # If all that works, signal that the process is starting
      if (verbose) {
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
      queries <- get_queries(aoi, area_name, years, models, parameters,
                             scenarios, arg_ref, grid_ref)
      return(queries)
    }
  ),

  active = list(

    # Check access to server
    server_open = function() {

      # Load python dependency
      reticulate::use_condaenv('cft', required = TRUE)
      xr <- reticulate::import("xarray")

      # This is very first possible file in our list
      sample_url = paste0("http://thredds.northwestknowledge.net:8080/",
                          "thredds/dodsC/MACAV2/BNU-ESM/macav2metdata_",
                          "vpd_BNU-ESM_r1i1p1_rcp85_2096_2099_CONUS_daily.nc",
                          "#fillmismatch")

      # Try to open a remote file from base url (what would the exception be?)
      tryCatch({
        xr$open_dataset(sample_url)
        return(TRUE)
      }, error=function() {
        return(FALSE)
      })
    },

    # Check available AOIs
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

    # Check existing climate files for the currently set aoi  ( will break before aoi is set)
    project_contents = function() {
        existing_dirs = list.dirs(self$project_dir)
        existing_files = list.files(self$project_dir, recursive = TRUE)
        return(c(existing_dirs, existing_files))
    }
  )
)
