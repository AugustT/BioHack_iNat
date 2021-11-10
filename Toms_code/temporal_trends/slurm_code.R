# Cluster code
library('rslurm')

slurm_rnorm <- function(x, id){
  
  y <- rnorm(n = x)
  write.table(y, file = paste0('test', id, '.txt'))
  return(y)
  
}

pars <- data.frame(x = floor(runif(n = 100, min = 1000, max = 2000)),
                   id = 1:100)

# Create the job scipt and the R script needed to run the process on 
# lotus using slurm. Note: you can edit the templates used. These are
# found in the slurm folder in your R library (run '.Library' to find).
# You will need to add the command to load jaspy: module add jaspy
job_id_file <- tempfile()
sink(file = job_id_file)
sjob <- slurm_apply(f = slurm_rnorm,
                    params = pars, 
                    jobname = 'TSA4',
                    nodes = nrow(pars), 
                    cpus_per_node = 1, 
                    submit = TRUE,
                    slurm_options = list(time = '20:00:00', 
                                         partition = 'short-serial',
                                         error = '%a.err'))
sink()
cat(readLines(job_id_file))
job_id <- readLines(job_id_file)
