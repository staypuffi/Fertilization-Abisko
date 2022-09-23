### explore fert data
getwd()
library(tidyverse)

#### load data ####
all_plots <- read_csv2("Fertilizer +control data 2022.csv")

names(all_plots) # we want to see the names 
summary(all_plots)

##
all_plots <- all_plots %>%
  mutate(across(`Andromeda polifolia`:`Vaccinium vitis-idaea`,
                ~if_else(.x == -1, 1, .x)),
        elevation = factor(elevation, levels = c("low", "medium", "high")))

##### long data #####
all_plots %>%
  select(site:`Vaccinium vitis-idaea`) %>% 
  pivot_longer(5:29, 
               names_to = "species",
               values_to = "count") %>%
  filter(count > 0) %>%
mutate(elev_treat = paste(elevation, treatment, sep = "_") 
)-> species_long

### plotting
library(ggplot2)
  
ggplot(species_long)+
  geom_boxplot(aes(x=elevation,
                   y=count,
                   fill= treatment))+
  ylim(c(0,200))+
  facet_wrap(~site)

#### calculate species richness & total count ####
species_long %>%
  group_by(site, elevation, treatment, plot) %>%
  summarize(totcount = sum(count),
            richness = length(unique(species))
            ) -> species_sum

ggplot(species_sum)+
  geom_col(aes(x=elevation,
                   y=totcount,
                   fill= treatment),
               position= "dodge")+
  facet_wrap(~site)


##### # add growthform #####
species_chars <- read.csv2("data/species characteristics.csv")
species_chars %>%
  rename(species = 1) -> species_chars

species_long <- left_join(species_long, species_chars)


ggplot(species_long) +
  geom_col(aes(x= treatment, y= count,
               fill = growthform)) +
  facet_wrap(~site+elevation, scales = "free_y")


#### with other species ####
#all species in a tibble
all_species <- species_long %>%
  select(species) %>%
  unique()

#sort all species with total abundance less than 5%
spec_less5 <- setdiff(all_species, spec_more5)

#name everything in spec_less5 "other"
spec_less5 %>%
  select(species) %>%
  mutate(species_other = "other") -> spec_less5

#name everything in spec_more5 as the species
spec_more5 %>%
  select(species) %>%
  mutate(species_other = species) -> spec_more5

#cbind spec_less5 with spec_more5 (only if columns have same name)
rbind(spec_less5, spec_more5) -> all_species

#lefjoin to species_long
species_long <- left_join(species_long, all_species)

#new gg
ggplot(species_long)+
  geom_col(aes(x=treatment, (y=perc_cover)/5, fill=species_other))+
  facet_wrap(~site+elevation)

#right colour scheme


