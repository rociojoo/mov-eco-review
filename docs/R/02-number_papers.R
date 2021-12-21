##
# calling packages that we will need
library(tidyverse)
library(paletteer)
library(cowplot)


# Indicating paths of dictionary, fulltext data, papers metadata, doi-pmc information, and where we will save the
# dictionary data.frame results and the statistics for each category in each dictionary

path <- "./Data/"


papers <- read.csv(file = paste0(path,"cleaned_papers_all_years_simple.csv"),stringsAsFactors = FALSE)

data_decade <- papers %>% 
  filter(pubyear > 2008 & pubyear < 2019)


# Set ggplot theme for all plots
theme_set(theme_bw() +
            theme(axis.title.y = element_text(angle = 0,vjust=0.5),
                  strip.background = element_blank(),
                  strip.text.y = element_text(angle=0),
                  strip.text.y.right = element_text(angle=0),
                  strip.text.y.left = element_text(angle=0),
                  panel.grid.minor = element_blank()))

# PAPERS PER YEAR
papers_year <- as.data.frame(table(papers$pubyear))
colnames(papers_year) <- c("year","papers")
papers_year$year <- as.numeric(as.character(papers_year$year))

# Filter 
papers_year <- 
  papers_year %>% 
  filter(year > 2008 & year < 2019) 

# other articles
papers_general <- read.csv(file = paste0(path,"all-articles-years.csv"), sep = "\t")
papers_general <- papers_general %>% 
  filter(Publication.Years > 2008 & Publication.Years < 2019) %>% 
  select(-X..of.47.611.456) %>% 
  mutate(year = as.numeric(Publication.Years)) %>% 
  select(-Publication.Years)

# join both and compute proportion 
papers_all <- 
  papers_year %>% 
  left_join(papers_general) %>% 
  mutate(prop = papers/Record.Count)

# Table displayed:
(papers_all)

# Figure
ggplot(data = papers_all, aes(x = year, y = prop)) +
  geom_smooth(alpha = 0.25, colour = "#666666") +
  geom_point(size = 3) + 
  xlab("") +
  ylab("Proportion of papers") +
  theme_classic() + theme(text = element_text(size=20), axis.text.x = element_text(angle=0,vjust=0.5),
                          axis.title.y = element_text(margin = margin(r=10)), 
                          axis.title.x = element_text(margin = margin(t=10)))
