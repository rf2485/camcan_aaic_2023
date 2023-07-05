library(tidyverse)
source('imaging_available.R')

old_dwi = read_csv("old_dwi.csv")

scd = old_dwi %>% filter(scd == TRUE)
write_tsv(scd, "scd.tsv")

ctl = old_dwi %>% filter(scd == FALSE)
write_tsv(ctl,"ctl.tsv")