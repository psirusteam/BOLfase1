write_chunk_data <- function(data_path, output_dir, chunk_size = 1000000) {
  #If the output_dir do not exist, it is created
  if (!fs::dir_exists(output_dir)) fs::dir_create(output_dir)
  #It gets the name of the file
  data_name <- fs::path_ext_remove(fs::path_file(data_path))
  #It sets the chunk_num to 0
  chunk_num <- 0
  #Read the file using vroom
  data_chunk <- vroom::vroom(data_path)
  #It gets the variable names
  data_names <- names(data_chunk)
  #It gets the number of rows
  rows<-nrow(data_chunk)
  
  #The following loop creates a parquet file for every [chunk_size] rows
  repeat{
    #It checks if we are over the max rows
    if(rows>(chunk_num+1)*chunk_size){
      arrow::write_parquet(data_chunk[(chunk_num*chunk_size+1):((chunk_num+1)*chunk_size),], 
                           fs::path(output_dir, glue::glue("{data_name}-{chunk_num}.parquet")))
    }
    else{
      arrow::write_parquet(data_chunk[(chunk_num*chunk_size+1):rows,], 
                           fs::path(output_dir, glue::glue("{data_name}-{chunk_num}.parquet"))) 
      break
    }
    chunk_num <- chunk_num + 1
  }
  
  
  
  #This is to recover some memory and space in the disk
  rm(data_chunk)
  tmp_file <- tempdir()
  files <- list.files(tmp_file, full.names = T, pattern = "^vroom")
  file.remove(files)
}
