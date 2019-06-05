return_spec_pfam <- function(values){
  # split up the accession numbers
  
  if(!values == ""){
    split <- strsplit(values,"\\s+")
    full_split <- unlist(split)
    
    output <- paste0("SELECT all_opt.pfam_id, all_opt.uniprot_id, all_opt.pdb_id, pfam_ratio.pdb_exists, pfam_ratio.total_seqs, pfam_ratio.ratio ",
                     "FROM all_opt ",
                     "INNER JOIN pfam_ratio on pfam_ratio.pfam_id = all_opt.pfam_id ", 
                     "WHERE " 
                     )
    
    count <- 0
    for (val in full_split){
      if(count > 0){
        output <- paste0(output, " OR")
      }
      output <- paste0(output, " all_opt.pfam_id = \'", val, "\'")
      count = count + 1
    }
    
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