suppressMessages(library("dplyr"))
suppressMessages(library("xtable"))
print(paste("working directory:", getwd()[1]))

runfile = "result/out.feather"
datafile = "data/exp_generated.feather"
outfile = "result/table.txt"

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
labelnames = c("method", "n_runs")
print(xtable(results %>% select(-labelnames), digits = 3, type = "latex"), file = outfile, include.rownames=FALSE)
