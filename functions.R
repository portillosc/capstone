return_spec_pfam <- function(values){
  # split up the accession numbers
  
  if(!values == ""){
    split <- strsplit(values,"\\s+")
    full_split <- unlist(split)
    
    
    output <- "SELECT * FROM pfam_ratio WHERE"
    
    count <- 0
    for (val in full_split){
      if(count > 0){
        output <- paste0(output, " OR")
      }
      output <- paste0(output, " pfam_id = \'", val, "\'")
      count = count + 1
    }
    print(output)
    return(output)
  }
  return("")
}

return_pdb_ids <- function(values){
  if(!values == ""){
    split <- strsplit(values,"\\s+")
    full_split <- unlist(split)
    
    
    output <- "WITH temp_data AS (SELECT * FROM pfam_uni WHERE "
    
    count <- 0
    for (val in full_split){
      if(count > 0){
        output <- paste0(output, " OR")
      }
      output <- paste0(output, " pfam_id = \'", val, "\'")
      count = count + 1
    }
    output <- paste0(output, ")")
    
    output <- paste0(output, " SELECT * FROM pfam_ratio INNER JOIN temp_data on pfam_ratio.pfam_id = temp_data.pfam_id INNER JOIN pdb_uni on temp_data.uniprot_id = pdb_uni.uniprot_id")
    return(output)
  }
  return("")
}