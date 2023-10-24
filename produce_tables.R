suppressMessages(library("dplyr"))
suppressMessages(library("xtable"))
print(paste("working directory:", getwd()[1]))

runfile = "result/out.feather"
runsefile = "result/outse.feather"
datafile = "data/exp_generated.feather"
outfile = "result/table.txt"
outsefile = "result/tablese.txt"
outfile_alpha = "result/table_alpha.txt"
outsefile_alpha = "result/tablese_alpha.txt"

gt = feather::read_feather(datafile) %>%
  mutate(te = (((t==1)-(t==0))*(re78-re78_cf)))
ate = 185/(185+260)*mean(gt$te[gt$t==1])+260/(185+260)*mean(gt$te[gt$t==0])
runs = feather::read_feather(runfile) %>% as.tbl %>%
  mutate(error = (est - ate)/1000, se=se/1000, AIse=AIse/1000) %>%
  mutate(covered95 = abs(error)<qnorm(0.975)*se) %>%
  mutate(AIcovered95 = abs(error)<qnorm(0.975)*AIse) %>%
  mutate(covered90 = abs(error)<qnorm(0.95)*se) %>%
  mutate(AIcovered90 = abs(error)<qnorm(0.95)*AIse) %>%
  mutate(method = as.character(method))
runsse = feather::read_feather(runsefile) %>% as.tbl %>%
  summarise(sd = sd(est)/1000) * sqrt(100000)
runsse = as.numeric(runsse)
results = runs %>%
  group_by(N, M, method) %>%
  summarise(rmse = sqrt(mean(error^2)), bias = mean(error),
            sd=sqrt(mean((error-bias)^2)), mae = mean(abs(error)),
            coverage95 = mean(covered95),
            AIcoverage95 = mean(AIcovered95),
            coverage90 = mean(covered90),
            AIcoverage90 = mean(AIcovered90),
            n_runs=as.character(n())) %>%
  ungroup() %>%
  arrange(N, M) %>%
  as.tbl
results_scale = results %>% 
  mutate(rmsesc = rmse*sqrt(N)/runsse,
         biassc = bias*sqrt(N)/runsse,
         sdsc = sd*sqrt(N)/runsse,
         maesc = mae*sqrt(N)/runsse)
labelnames = c("method", "n_runs")
print(xtable(results %>% select(-labelnames) %>% filter(M %in% c(1,4,16)), digits = 3, type = "latex"), file = outfile, include.rownames=FALSE)
results_alpha = results %>% select(-labelnames) %>% 
  filter(M==floor(0.5*N^{1/3})|M==floor(1*N^{1/3})|M==floor(2*N^{1/3})|M==floor(5*N^{1/3})|M==floor(10*N^{1/3}))
results_alpha[,"M"] = data.frame(rep_len(c(0.5,1,2,5,10),length.out = nrow(results_alpha)))
colnames(results_alpha)[2] = "alpha"
print(xtable(results_alpha, digits = 3, type = "latex"), file = outfile_alpha, include.rownames=FALSE)
print(xtable(results_scale %>% 
               select("N", "M", "rmsesc", "biassc", "sdsc", "maesc") %>% filter(M %in% c(1,4,16)), 
             digits = 2, type = "latex"), file = outsefile, include.rownames=FALSE)
results_scale_alpha = results_scale %>% select("N", "M", "rmsesc", "biassc", "sdsc", "maesc") %>% 
  filter(M==floor(0.5*N^{1/3})|M==floor(1*N^{1/3})|M==floor(2*N^{1/3})|M==floor(5*N^{1/3})|M==floor(10*N^{1/3}))
results_scale_alpha[,"M"] = data.frame(rep_len(c(0.5,1,2,5,10),length.out = nrow(results_scale_alpha)))
colnames(results_scale_alpha)[2] = "alpha"
print(xtable(results_scale_alpha, digits = 2, type = "latex"), file = outsefile_alpha, include.rownames=FALSE)
print(runsse)
