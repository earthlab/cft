verify_aws_credentials <- function() {
  environment_var_names <- c("AWS_ACCESS_KEY_ID", 
                             "AWS_SECRET_ACCESS_KEY", 
                             "AWS_DEFAULT_REGION")
  environment_var_vals <- Sys.getenv(environment_var_names)
  if(any(environment_var_vals == "")) {
    stop(
      paste0(
        "To use AWS functionality, you must provide your credentials as ", 
        "environment variables, specifying the following: ",
        paste(environment_var_names, collapse = ", ")
        )
      )
  }
}
