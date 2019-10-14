config_aws <- function(aws_config_dir) {
  aws_config_file <- file.path(aws_config_dir, "cstdata_config.RDS")
  
  # Build configuration file if needed
  if (!file.exists(aws_config_file)) {
    if (!dir.exists(aws_config_dir)) {
      dir.create(aws_config_dir)
    }
    print("Build AWS Configuration File\n")
    bucket <- readline("s3 bucket name: ")
    key <- readline("aws key: ")
    skey <- readline("aws secret key: ")
    region <- readline("aws region: ")
    aws_creds <- c("bucket" = bucket, "key" = key, "skey" = skey,
                   "region" = region)
    print(paste("Saving configuration file to", aws_config_file))
    saveRDS(aws_creds, aws_config_file)
  }
  
  # Initialize AWS access
  aws_creds <- readRDS(aws_config_file)
  Sys.setenv("AWS_ACCESS_KEY_ID" = aws_creds["key"],
             "AWS_SECRET_ACCESS_KEY" = aws_creds["skey"],
             "AWS_DEFAULT_REGION" = aws_creds["region"])
  
  # Return the credentials
  return(aws_creds)
}
