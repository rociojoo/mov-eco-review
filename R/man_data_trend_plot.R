#############################
# Making category line plots
#############################

library(tidyverse)
library(topicmodels) #install in ubuntu libgsl-dev and gsl-bin
source("./R/expectation_functions.R")

# Arguments
path <- "./Data/ProcessedQueries/References/"
path.plots <- "./Rocio/Plots/"

# Summarizing
if (!dir.exists(path.plots)){
  dir.create(path.plots)
}
path_processed_dictionaries <- "./Data/Dictionary/Papers-Term/"
path_dictionary_info <- "./Data/Dictionary/"

papers <- read.csv(file = paste0(path,"cleaned_papers_all_years_simple.csv"),stringsAsFactors = FALSE)

data_decade <- papers %>% 
  filter(pubyear > 2008 & pubyear < 2019)
######## Category #############

dictionary <- "Data"
corrected_df <- expectation_functions(dictionary, data_all = data_decade, num_cat=NULL, ini_cat=1,
  paper_cat_out=FALSE, filter_lines=FALSE, suffix=NULL,
  path_processed_dictionaries)

values_prop <- sort(unique(corrected_df$prop_papers))
values_breaks <- seq(from=0,to=max(values_prop)+0.1,by=0.1)
values_year <- seq(from=min(corrected_df$year), to=max(corrected_df$year),by=1)

# We're going to plot our lines and then adjust the color, alpha, and linetype to better read the data

plot_df <- corrected_df
head(plot_df)
plot_df$category <- rep(c('GPS','Radio','Video','Accelerometer',
  'Acoustic','Satellite','Encounter','Radar','Pressure',
  'Light loggers','Body conditions'), each = 10)
# Run a quick linear model to measure which trend lines are positive or negative
# we'll reference this when we choose our colors
here <- by(plot_df, plot_df$category, function(x)
  lm(x$prop_papers ~ x$year)$coefficients[2]
)
plot_df$category <- factor(plot_df$category, levels= names(sort(here)))
# Create a grouping variable based on this value

grouping <- data.frame(category = c(names(here)[here<=0.003 & here>=(-0.003)],names(here)[here<(-0.003)],names(here)[here>0.003]))
grouping$group <- seq_along(grouping$category)

plot_df <- merge(plot_df,grouping, by='category')

# Now to make our aesthetic features which will be added with scale_*_manual()
# Colors
# Make a color ramp where the amount of 'grays' will determine the highlighted categories
# colfunc <- colorRampPalette(c("red",'gray','gray',"blue"))
# colorz <- colfunc(nrow(here))
# names(colorz) <- names(sort(here))
Tol_muted <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#AA4499', '#DDDDDD')

#Okabe_Ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
colorz <- Tol_muted[1:(length(here)+1) %% (length(Tol_muted)+1)]
# change problematic colors to gray60
# gray 60 "#7f7f7f"
# black "#000000"
names(colorz) <- names(sort(here))
colorz
colorz[10] <- "#7f7f7f"
colorz[2] <- "#7f7f7f"
colorz[11] <- '#117733'
# line types
# just need to spread linetypes out enough so that the color and alpha can help distinguish as well
# manual 5 = dash, 3 = dotted, 1 = solid
linetypez <- c(5,3,3,3,3,3,3,3,1,1,1)
# or random
# linetypez <- rep(1:6,times=ceiling(length(levels(plot_df$Topic))/6))
# linetypez <- linetypez[seq_along(levels(plot_df$Topic))]
names(linetypez) <- names(sort(here))

# alpha
# Changing alpha will help to make the important categories pop.
# Create a gradient of alphas from 1 -> .2 -> so none trend lines are grayed out.
nz <- length(here)
# automatically
alphaz <- c((1*nz/2):(.2*nz/2)/nz*2,(.2*nz/2):(1*nz/2)/nz*2,ifelse(nz%%2==0,NULL,1))
# or manually

alphaz <- c(1,.45,.45,.45,.45,.45,.45,.45,.7,1,1)
names(alphaz) <- names(sort(here))

# line width
sizez <-  c(1,1,1,1,1,1,2,1,2,2,2)
#names(sizez) <- names(here)
sizez
sizez <- rep(sizez, each = 10)
sizez
# You have to include color, linetype, and alpha in the mapping even if youre going to override it anyway.

p <- ggplot(
  data  = plot_df) +
  geom_line(size=sizez, 
    mapping = aes(x = year, y = prop_papers, color = category, group = group, linetype = category, alpha = category)
  ) +
  scale_color_manual(name='Data Device',values = colorz) +
  scale_linetype_manual(name='Data Device',values = linetypez) +
  scale_alpha_manual(name='Data Device',values = alphaz)+
  theme_classic()+xlab("") + ylab("Proportion of articles in a year") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1,size=16),axis.text.y = element_text(size=16),
    legend.position = "none", legend.justification = "right",legend.text=element_text(size=15),
    axis.title.y = element_text(margin = margin(r=10),size=17), 
    axis.title.x = element_text(margin = margin(t=10)),
    legend.key.size = unit(2,"line"),
    legend.title=element_text(size=16))

start_pos <- plot_df %>% group_by(category) %>% summarise(y = last(prop_papers)) %>% mutate(x = 2018)
start_pos$colorz <- colorz
start_pos


start_pos$x_new <- start_pos$x + 0.1
start_pos$y_new <- start_pos$y + c(0,0.0125,0,0,0,0.01,0,0,0,0,0)
p + geom_text(data = start_pos, aes(x =x_new ,y=y_new, label = category), color=colorz,hjust=0,size=5)+
  coord_cartesian(xlim = c(2009, 2018),clip = 'off') + 
  theme(plot.margin = unit(c(1,10,1,1), "lines"))

ggsave("Manuscript/Images/devices_ts_all1.png", width=12,height=8)
