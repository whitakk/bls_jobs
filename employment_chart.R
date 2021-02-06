setwd('../Desktop/realworld/newsletter/2021.2.7 - economy/')
library(ggplot2)

### Data input and cleaning ### 

# read in base employment data and industry files
empl <- read.csv('bls_Jan21.csv')
  # source: https://www.bls.gov/webapps/legacy/cesbtab1.htm
  # formatted as multi-series table, comma-delimited
ind <- read.csv('industry_filter.csv')
  # industry_filter filters for custom MECE mapping of NAICS industries at various levels

data <- merge(empl, ind, by='Series.ID')

# INPUT: set months to compare here #
data$curr <- data$Jan.2021
data$prev <- data$Jan.2020

data <- data[c("Series.ID","industry_name","prev","curr")]

data.clean <- data.frame(lapply(data, function(x) {
  gsub("\\(P\\)", "", x)
}))

# tweak how a couple industries displayed
data.clean$industry_name <- gsub('Federal', 'Federal Government', data.clean$industry_name)
data.clean$industry_name <- gsub('General merchandise stores, including warehouse clubs and supercenters', 'General merchandise/Superstores', data.clean$industry_name)


data.clean$prev <- as.numeric(as.character(data.clean$prev))
data.clean$curr <- as.numeric(as.character(data.clean$curr))
data.clean$growth <- with(data.clean, (curr / prev)-1)

### Prep and plot graph ### 

# drop industries with <1M workers
data.clean <- subset(data.clean, prev>=1000)

# convert growth to percentage
data.clean$growth <- data.clean$growth*100

# assign groups for coloring
data.clean$group <- cut(data.clean$growth, breaks = c(-999,-10, 0, 999), 
                                      labels=c("Very Neg", "Neg", "Pos"), 
                                      include.lowest=TRUE)

# sort ascending # not actually necessary for the plot
srt <- order(data.clean$growth) # returns index sorted by col1
data.clean <- data.clean[srt, names(data.clean)] # returns df sorted by col1

# plot 
ggplot(data=data.clean, aes(y=reorder(industry_name, -growth), x=growth, group=1)) + 
  geom_point(aes(color = group, size=curr*1.5)) + 
  scale_color_manual(values = c("darkred", "pink", "dark green")) + 
  scale_size(range = c(2, 10)) +
  theme_classic() + 
  xlab("Change in employment, Jan '21 vs Jan '20 (%)") +
  ylab('Industry') + 
  scale_x_continuous(position = "top") +
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10), 
        axis.ticks = element_blank(), 
        legend.position='none', 
        panel.grid.major.x = element_line(colour = "light grey",size=0.25))

ggsave('bls_Jan21.png', width=7, height=8)


# ad-hoc analysis

tot_loss <- sum(data.clean$curr) - sum(data.clean$prev)
restaurant_loss <- with(subset(data.clean, industry_name=="Accommodation and food services"), sum(curr) - sum(prev))
100*(restaurant_loss / tot_loss)

tot_loss / sum(data.clean$prev)

warehouse_gain <- with(subset(data.clean, industry_name=="Warehousing and storage"), sum(curr) - sum(prev))
warehouse_gain