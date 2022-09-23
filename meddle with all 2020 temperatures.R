library(tidyverse)

## from canvas with manually renamed cols
allT_2020 <- read_csv2("data/prev years/alltempdata2020.csv")

## seperate via loop


site <- colnames(allT_2020)
site <- site[c(-1, -seq(2, 116, by = 2))]

#site <- sub("K", "K_", site)
#site <- sub("N", "N_", site)
length(site)

## seperate via loop
allT_list <- list()
for(i in seq_along(site)) {
  allT_list[[i]] <- allT_2020 %>%
                    select(seq(2, 116, by = 2)[i]:(
                             seq(2, 116, by = 2)[i]+1))%>%
                    mutate(site=site[i])%>%
                    rename(date = 1,
                           temperature = site[i])

  }

names(allT_list) <- site
allT_list[[1]]

allT_2 <- bind_rows(allT_list)
allT_2%>%
  mutate(site = str_replace(site, "K", "K_"),
         site = str_replace(site, "N", "N_"))%>%
  separate(site,
           into= c("site", "elevation", "plot"),
           sep = "_") %>%
  mutate(elevation = str_replace(elevation, "L", "low"),
         elevation = str_replace(elevation, "M", "medium"),
         elevation = str_replace(elevation, "H", "high")) -> allT_2

write_csv2(allT_2, "data/prev years/alltempdata2020 1 3col.csv")
