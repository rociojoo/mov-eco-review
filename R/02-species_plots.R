####################################
# "Make Stacked Species Area Plot."
####################################

library(tidyverse)
library(cowplot)
library(paletteer)

###  Read in data

# Read in taxonomy output
animals <- read.csv('Data/animals_in_papers.csv')

# Read in table with our taxa classification categories
ss <- read.csv('Data/Dictionary/Synonyms-Species.csv')
animals <- merge(animals, ss, by.x = 'class', by.y = 'keyword')
# Read in WoS cleaned csv
data <- read.csv('Data/cleaned_papers_all_years_full.csv')

# Summarize data

df1 <- merge(data, animals, by = 'doi')
df1 <- subset(df1, PY>2008 & PY<2019)

df2 <- df1 %>% drop_na(tsn) %>% 
  filter(class != 'Vertebrata' & level == 'genus_sp') %>% 
  group_by(paper_id, tsn) %>% 
  distinct(tsn, year = PY,paper_id,meaning) %>% 
  left_join(
    df1 %>% 
      distinct(paper_id, PY) %>% 
      group_by(PY) %>% 
      tally()
    ,by = c('year' = 'PY')
  )


df2 %>% group_by(year) %>%summarise(n_class = n_distinct(tsn))

df5 <- df2 %>% group_by(year,meaning) %>% summarise(n_class = n_distinct(tsn), n = mean(n))

# the last year has more species, conveniently for ordering
top_classes <- df5$meaning[df5$year==2018][order(rank(df5$n_class[df5$year==2018],ties.method = 'first'), decreasing = T)]
top_classes <- top_classes[1:5]

df5 <- df5 %>% 
  filter(meaning %in% top_classes)

df5$taxa <- factor(df5$meaning, levels = 
                     top_classes
)

classes_especies <- levels(df5$taxa)

taxa_colors <- paletteer_d("colorblindr::OkabeIto") %>%
  as.vector()
taxa_color_palette_current <- taxa_colors[classes_especies]

ggplot(data  = df5, mapping = aes(x = year, y = n_class, color = taxa, fill = taxa)) +
  geom_line(size=1.5, linetype = 3) +
  geom_point(size = 5.5) +
  scale_color_manual(name="Class",values = taxa_color_palette_current) +
  scale_fill_manual(name="Class",values = taxa_color_palette_current) +
  scale_x_continuous(breaks = values_year) +
  # scale_y_continuous(breaks = values_breaks) +
  theme_bw()+xlab("") + ylab("Number of species") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,size=16),axis.text.y = element_text(size=16),
        legend.position = "bottom", legend.justification = "right",legend.text=element_text(size=15),
        axis.title.y = element_text(margin = margin(r=10),size=17), 
        axis.title.x = element_text(margin = margin(t=10)),
        legend.key.size = unit(2,"line"),
        legend.title=element_text(size=16))

