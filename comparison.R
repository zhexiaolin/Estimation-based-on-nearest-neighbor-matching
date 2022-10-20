# --------------------------------------------------
# setup the session
# --------------------------------------------------
library("optparse")
option_list = list(
  make_option(c("--input_feather_path"), type="character", default="data/exp_generated.feather", 
              help="path to dataset containing treated + controls", metavar="character"),
  make_option(c("--output_feather_path"), type="character", default="result/out.feather", 
              help="output filename", metavar="character"),
  make_option(c("--resume"), type="logical", default=TRUE, 
              help="Resume from previous intermediate results?", metavar="logical"),
  make_option(c("--N_runs"), type="integer", default=2000L, 
              help="Number of runs per estimator", metavar="integer"),
  make_option(c("--N_chunk"), type="integer", default=200L, 
              help="After N_chunk runs, intermediate results will be saved", metavar="integer"),
  make_option(c("--N_workers"), type="integer", default=10, 
              help="If n_workers > 1, runs will be parallelized using 'future.apply'", metavar="integer")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

if (opt$N_workers > 1) {
  library("future.apply")
  plan(multiprocess, workers = opt$N_workers)
  options(future.globals.maxSize = 3000 * 1024 ^ 2)
  replicate = future_replicate
}

# --------------------------------------------------
# specify estimators and covariates
# --------------------------------------------------
covariates = c("black", "hispanic", "married", "nodegree",
               "re74", "re75", "education", "age")

source("estimators.R")
estimators = list(
  BCM_ps   = function(M,x,y,w) BCM(x,y,w,M)
)

# --------------------------------------------------
# define some helper functions to loop over
# --------------------------------------------------
get_sample = function() {
  t = data$t == 1
  s = rbind(data[t,][sample(1:sum(t), N_treated),],
            data[!t,][sample(1:sum(!t), N_control),])
  return(list(X=as.matrix(s[, covariates]), Y=s$re78, W=s$t))
}

iteration = function() {
  sampled = get_sample()
  result = estimators[[method]](M,sampled$X, sampled$Y, sampled$W)
  return(c(est=result$est, se=result$se, AIse=result$AIse))
}


# --------------------------------------------------
# execute iterations and save intermediate results
# --------------------------------------------------
suppressWarnings(data <- feather::read_feather(opt$input_feather_path))
N_runs = rep(opt$N_runs, length(estimators))
names(N_runs) = names(estimators)

N_list = c(600,1200,4800,9600)
M_fix_list = c(1,4,16)
M_poly_list = c(0.5,1,2,5,10)

print(paste("starting iterations with input", opt$input_feather_path))
tictoc::tic("total time: ")
if (opt$resume & file.exists(opt$output_feather_path)) {runs = as.data.frame(feather::read_feather(opt$output_feather_path))} else {runs = NULL}
iter = 0
for (N in N_list){
  N_treated = floor(N*185/(185+260))
  N_control = N-N_treated
  for (M in c(M_fix_list,floor(M_poly_list*N^{1/3}))){
    for (method in (names(estimators))) {
      set.seed(123)
      N_completed = sum(runs$method==method&
                          runs$N==N&
                          runs$M==M)
      print(paste("N:", N, "M:", M, "Method:", method, "Completed:", N_completed))
      while (N_completed < N_runs[method]) {
        tictoc::tic("chunk time: ")
        N_chunk = opt$N_workers*opt$N_chunk
        runs_chunk = replicate(N_chunk, iteration())
        tictoc::toc()
        if (opt$resume & file.exists(opt$output_feather_path) | iter > 0) {runs = as.data.frame(feather::read_feather(opt$output_feather_path))} else {runs = NULL}
        runs = rbind(runs, data.frame(N=N, M=M, method=method, est=runs_chunk["est",], se=runs_chunk["se",], AIse=runs_chunk["AIse",]))
        N_completed = N_completed + ncol(runs_chunk)
        print(paste("N:", N, "M:", M, "Method:", method, "Completed:", N_completed, "| Saving results..."))
        feather::write_feather(runs, opt$output_feather_path)
        iter = iter + 1
      }
    }
  }
}
tictoc::toc()
future:::ClusterRegistry("stop")