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

