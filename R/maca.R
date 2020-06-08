#' Note that older versions of roxygen2 don't recognize R6 classes

BASE_URL = paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/",
                  "agg_macav2metdata")
CATALOG_URL = paste0("http://thredds.northwestknowledge.net:8080/thredds/",
                     "reacch_climate_CMIP5_macav2_catalog2.html")

#' @importFrom R6 R6Class
Maca <- R6::R6Class(

  classname = "Maca",

  public = list(

    # MACA Specific OpenDAP and catalog URLs
    base_url = BASE_URL,
    catalog_url = CATALOG_URL,

    # MACA Specific Argument Reference
    arg_ref = maca_reference,

    # Shared Grid Reference
    grid_ref = grid_reference,

    # Initalize arguments
    project_dir = NULL,
    initialize = function(project_dir = ".") {

      # Create and store project directory path
      dir.create(project_dir, showWarnings = FALSE)
      project_dir = normalizePath(project_dir)
      self$project_dir = project_dir

      # Check that the server is open
      if (!self$server_open) {
        print("Server not available.") 
      }

    },
  
    # This is like __repr__
    print = function(...) {
      cat(class(self)[[1]], " Data Generator Class \n")
      cat("  OpenDAP URL: ", self$base_url, "\n", sep = "")
      cat("  Data Catalog URL: ", self$catalog_url, "\n", sep = "")
      cat("  Project Directory: ", self$project_dir, "\n", sep = "")
      invisible(self)
    },

    # For building the AOI
    set_aoi = function(park, shp_path, area_name, field, verbose=FALSE) {

      if ( missing(field) ) field <- NULL

      if (verbose) print("Retrieving area of interest boundaries...")
      aoi <- get_aoi(park, shp_path, area_name, local_dir)

      # Match coordinate systems
      aoi <- sp::spTransform(aoi, self$grid_ref$crs)

      # Get geographic information about the aoi
      if (verbose) print("Building area of interest grid...")
      aoi_info <- get_aoi_info(aoi, self$grid_ref)

    },

    # For executing the request
    retrieve_subset = function() {
       print("Working on it ...")
    }
  
  ),

  private = list(

    # For building the queries
    queries = function(x) {
      print("Working on it ...")
      
    },
      
    # For subsetting the datset
    subset = function(x) {
      print("Working on it ...")
      
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

    }
  )
)
