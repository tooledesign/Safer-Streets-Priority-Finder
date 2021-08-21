save_report_to_s3 = function(file, bucket=Sys.getenv("S3_BUCKET"), user_id, run_id) {
  tryCatch({
      print(glue::glue('Uploading {user_id}_{run_id}_report.pdf to S3 bucket'))
      aws.s3::put_object(
          file = file,
          object = glue::glue('{user_id}_{run_id}_report.pdf'),
          bucket = bucket
      )
  }, error = function(cond){
    print('There was an error uploading report.')
    print(cond)
  })
}


fetch_report_from_s3 = function(bucket=Sys.getenv("S3_BUCKET"), user_id, run_id) {
  tryCatch({
      print(glue::glue('Retrieving {user_id}_{run_id}_report.pdf from S3 bucket'))
      if (aws.s3::object_exists(glue::glue('{user_id}_{run_id}_report.pdf'), bucket)) {
          file <- aws.s3::get_object(glue::glue('{user_id}_{run_id}_report.pdf'), bucket)
          return(file)
          # or save object to disc
          # aws.s3::save_object(glue::glue('{user_id}_{run_id}_report.pdf'), bucket, file=glue::glue('{user_id}_{run_id}_report.pdf'))
      } else {
        print(glue::glue('{user_id}_{run_id}_report.pdf not found'))
        return(NULL)
      }
  }, error = function(cond){
    print('There was an error retreiving report.')
    print(cond)
  })
}
