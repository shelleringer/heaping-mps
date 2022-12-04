##------------------------------------------------------------------------------
## Plot trends in heaping, by country
##------------------------------------------------------------------------------

library(tidyverse)
library(haven)
library(stringr)
library(scales)
library(ggnewscale)

rm(list = ls()) 

setwd("~/heaping-mps")

# The visualization is produced using the summary data set that contains 
# values of Whipple's index for the surveys and censuses conducted in the 6 
# LMICs included in the study (Burkina Faso, Cote d'Ivoire, Ghana, Malawi, 
# Rwanda and Senegal), since 1990.
# To produce this dataset, you will need to register for access to data from 
# multiple survey programs. Instructions about how to do that, and how to access
# other data sources (e.g., censuses) are provided in the supplementary material
# of the data visualization.

data<-read.csv("heaping_series.csv") # read in the summary data

# Clean country names and the names of survey programs
data[data=="census"]<-"PHC" # PHC = Population and Housing Census
heaping.data<-data%>%
      mutate(heaping = heaping*100,
             country = str_to_title(
                        case_when(country_code=="sn"~"senegal",
                                  country_code=="rw"~"rwanda",
                                  country_code=="bf"~"burkina faso",
                                  country_code=="gh"~"ghana",
                                  country_code=="ci"~"cote d'ivoire",
                                  country_code=="mw"~"malawi",
                                  country_code=="malawi"~"malawi",
                                  country_code=="rwanda"~"rwanda",
                                  country_code=="ghana"~"ghana",
                                  country_code=="senegal"~"senegal",
                                  country_code=="burkina faso"~"burkina faso",
                                  country_code=="cote d'ivoire"~"côte d'ivoire"),
                        locale = "fr"),
             program = toupper(program),
             mobile = factor(ifelse(program=="MOBILE","MPS","OTHER")))

# Create breaks to be displayed on the y-axis. 
# These breaks match the classification of age data according to Whipple's
# index often used by the UN:
# 100-109 = Accurate, 110-124 = Approximate, 125-174 = Rough, 175+ = Very rough
heaping.breaks<-data.frame(start = c(100,110,125,175),
                           end = c(110,125,175,300),
                           fill = factor(1:4))

# Create the plot
heaping.mps<-ggplot()+
             geom_point(data = subset(heaping.data,mobile=="OTHER"),
                        aes(x = year, y = heaping, shape = program),
                        color = "black", alpha = 0.5)+
             scale_shape_manual(values = c(15,17,18,0,2,3,4),
                                guide = guide_legend(title = "Household surveys\nand censuses"))+
             new_scale_color()+
             geom_point(data = subset(heaping.data,mobile=="MPS"),
                        aes(x = year, y = heaping, color = mobile),
                        size = 2, shape = 16)+
             scale_color_manual(values = "red",
                     guide = guide_legend(title = "Remote surveys"))+
             stat_smooth(data = subset(heaping.data,mobile == "OTHER"),
                         aes(x = year,
                             y = heaping),
                         linetype = "dashed",
                         fill = "black",
                         color = "black", 
                         size = 0.5,
                         span = 0.8,
                         alpha = .1)+
            scale_y_log10(limits=c(100,300),
                          breaks = c(100, 110, 125, 175, 300),
                          oob = squish)+
            theme_test()+
            labs(title = "Trends in Age Heaping in Selected African LMICs (1990-2022)",
                  y = "Age heaping\n(Whipple Index)")+
            theme(plot.title.position = "plot",
                  plot.title = element_text(size = 14),
                  axis.title.x = element_blank(),
                  axis.title.y = element_text(angle = 0,
                                              vjust = 0.5),
                  panel.grid.major.y = element_line(color="snow2"),
                  panel.grid.minor.y = element_blank(),
                  strip.background = element_blank(),
                  strip.text = element_text(size = 11))+
            facet_wrap(~country, nrow = 3)
                           
# Save the plot

ggsave("heaping-mps.png", heaping.mps, 
       width=6.5, height=6.5, bg = "white", units="in", dpi=300)
