library(dplyr)
df = read.table("data/shadish.txt")
df = df %>%
  filter(rq=="quasiexperiment") %>%
  mutate(vm=as.numeric(vm=="Mathematics"),
         cauc=as.numeric(cauc=="Caucasian"),
         male=as.numeric(male=="male")) %>%
  select("mathall", "vm", "vocabpre", "mathpre", "numbmath", "age", "momdegr", "daddegr", "hsgpaar", "cauc", "male")
write.csv(df, "data/shadish.csv", row.names = FALSE)