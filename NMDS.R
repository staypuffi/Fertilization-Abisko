##### NMDS of fertilization plots
library(tidyverse)
library(vegan)

getwd()

All_plots <- read_csv2("data/combined 2022 plots.csv")
All_plots %>%
  mutate(site_elev =  paste(site, elevation, sep = "_"),
         site_treat = paste(site, treatment, sep = "_"),
         site_elev_treat = paste(site, elevation, treatment, sep = "_")) %>%
  mutate(`Carex sp` = `Carex sp`+`Cyperaceae indet. (Foto Felix 6838-40)`,
         `Salix sp 1` =`Salix sp 1`+ `Salix sp 2`+`Salix sp 3`+
           `Salix herbacea`+ `Salix caprea x lapponum`) %>%
  select(-c(`Cyperaceae indet. (Foto Felix 6838-40)`,
            `Salix sp 2`,`Salix sp 3`,
            `Salix herbacea`,`Salix caprea x lapponum`
            ) )-> All_plots

species_chars <- read.csv2("data/species characteristics.csv")
species_chars %>%
  rename(species=1) -> species_chars

All_plots %>%
  select(plot:`Vaccinium vitis-idaea`
         ) -> spec_plots
  

## calculate nmds
spec_plots[,-1] %>%
  metaMDS(distance = "bray", k = 2, 
          autotransform = FALSE,
          trymax=500, 
                  sratmax=0.999) -> spec_nmds
spec_nmds
stressplot(spec_nmds)

#make a vector for colors
colvec <- c("red2", "pink4", "orange", "green3", "blue","black")
#ordi plot
ordiplot(spec_nmds,type="n")
ordiplot(spec_nmds$points)
ordihull(spec_nmds,
         groups=All_plots$site_treat,
         draw="polygon", label=F, col=colvec)
legend('bottomright', legend = tools::toTitleCase(levels(All_plots$site_treat)),
       fill = colvec, bty = 'n')

## ggplot
# plot scores
plot.scores <- as.data.frame(scores(spec_nmds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
plot.scores %>%
  mutate(plot =rownames(plot.scores),
         elevation = All_plots$elevation,
         treatment = All_plots$treatment,
         site_elev = All_plots$site_elev,
         site_treat = All_plots$site_treat,
         site_elev_treat = All_plots$site_elev_treat# create a column of site names, from the rownames of data.scores
  ) -> plot.scores


# species scores
species.scores <- as.data.frame(scores(spec_nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores %>%
  mutate(species = rownames(species.scores)) %>%
  left_join(species_chars
         ) -> species.scores 

head(species.scores)

## find hulls around groups
library(plyr)
find_hull <- function(x) x[chull(x$eff, x$man), ]
hulls <- ddply(plot.scores,
               "site_elev_treat", find_hull)


plot.scores %>%
  ggplot(aes(x=NMDS1, y=NMDS2))+
  #geom_text(data=species.scores,
  #          aes(label=abbriviation),alpha=0.5) +  # add the species labels
  geom_point(aes(col=site_elev,
                 pch=treatment))+
  geom_polygon(aes(group = site_elev_treat),
               alpha = 0.5)

