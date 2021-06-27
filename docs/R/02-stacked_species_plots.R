####################################
# "Make Stacked Species Area Plot."
####################################

###  Read in data

# Read in taxonomy output
animals <- read.csv('Data/ProcessedQueries/Taxonomy/animals_in_papers.csv')

# Read in table with our taxa classification categories
ss <- read.csv('Data/Dictionary/Synonyms-Species.csv')
animals <- merge(animals, ss, by.x = 'class', by.y = 'keyword')
# Read in WoS cleaned csv
data <- read.csv('Data/ProcessedQueries/References/cleaned_papers_all_years_full.csv')

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

df5$taxa <- factor(df5$meaning, levels = 
    df5$meaning[df5$year==2018][order(rank(df5$n_class[df5$year==2018],ties.method = 'first'), decreasing = T)]
)

#################################
# Create color ramps and set up the plot
#################################

# These are the color ramps we have to choose from, I like 5 personally.  
color_ramp1 <- colorRampPalette(c( 'darkolivegreen4', '#a6cee3','#1f78b4'))(length(levels(df5$meaning)))
color_ramp2 <- colorRampPalette(c( '#a95aa1', '#85c0f9','#0f2080'))(length(levels(df5$meaning)))
color_ramp3 <- colorRampPalette(c( '#1b7837', "#d9f0d3",'#762a83'))(length(levels(df5$meaning)))
color_ramp4 <- colorRampPalette(c( '#a6611a', "#f5f5f5",'#018571'))(length(levels(df5$meaning)))
color_ramp5 <- colorRampPalette(c( 'darkolivegreen4','wheat2','darkorchid3'))(length(levels(df5$meaning)))

sps <- ggplot(data = df5, aes(x=year, y=n_class, fill = taxa)) + 
  geom_area( stat = 'identity') + theme_bw() + ylab('number of species') + 
  scale_fill_manual(values = color_ramp5)
sps
ggsave(plot =sps ,'plots/species_year_stacked.png', width = 80*2, height = 60*2, units = 'mm')
