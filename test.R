library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")

conn <- dbConnect(drv,
                  dbname="can",
                  host="pyrva.cul6zd3pcwrp.us-east-2.rds.amazonaws.com",
                  user="python",
                  password="pythonuser")
rs <- dbSendQuery(conn, 
                  paste0("WITH unique_ids as (SELECT DISTINCT pfam_id FROM all_opt), ",
                         "ran_ids as (SELECT pfam_id FROM unique_ids ORDER BY RANDOM() LIMIT 1000) ",
                         "select all_opt.pfam_id, all_opt.uniprot_id, all_opt.pdb_id from all_opt ",
                         "inner join ran_ids on ran_ids.pfam_id = all_opt.pfam_id"
                         )
                         #"select * from all_opt",
                         #"INNER JOIN unique_ids on unique_ids.pfam_id = all_opt.pfam_id"
                         #)
                  )
data <- fetch(rs,n=-1)
print(head(data))