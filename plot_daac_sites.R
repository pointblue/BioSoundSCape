# Purpose: Creates figures found in the BioSoundSCape sound archive data paper based on the metadata .csv file

# Matthew L. Clark, Ph.D.
# Department of Geography, Environment, and Planning
# Sonoma State University, California USA
# matthew.clark@sonoma.edu
# Version 10/23/2024

library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(ggpubr)

# input CSV with site information
pointsReduced <- read.csv("G:/Shared drives/BioSoundSCape/DAAC_distribution/biosoundscape_sites_daac_240802.csv")

# output graph .png file
outGraphLandcoverElev<- "G:/Shared drives/BioSoundSCape/DAAC_distribution/biosoundscape_sites_landcover_elevation_fire_graph_240802.png"
outGraphRecordings<- "G:/Shared drives/BioSoundSCape/DAAC_distribution/biosoundscape_sites_recordings_graph_240802.png"

pal <- RColorBrewer::brewer.pal(5, "Set2")

# Landcover graph
d <- drop_na(pointsReduced, LandCoverClass)
d$LandCoverClass <- factor(d$LandCoverClass, levels=c(sort(unique(d$LandCoverClass), decreasing=F)))

barGraphLandcover <- ggplot(d, aes(x = LandCoverClass)) + 
  geom_bar(fill = pal[3]) +
  theme_bw() + labs(x="",y="Count of sites") +
  guides(fill=guide_legend(title="Land-cover types")) +
  scale_x_discrete(drop=TRUE) +
  theme(
    axis.text.x = element_text(size = 10, angle=90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  )
#dev.new();print(barGraphLandcover)


# Elevation graph
d <- drop_na(pointsReduced, ElevationClass)

barGraphElevation <- d %>%
  mutate(ElevationClassOrdered = fct_relevel(ElevationClass, 
                                             "Low: 0-500 m", "Medium: 500-1000 m", "High: >1000 m")) %>%
  ggplot(aes(x = ElevationClassOrdered)) + 
  geom_bar(fill = pal[3]) +
  theme_bw() + labs(x="",y="Count of sites") +
  guides(fill=guide_legend(title="Elevation ranges")) +
  theme(
    axis.text.x = element_text(size = 10, angle=90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_blank(),
    legend.position = "none"
  )
#dev.new();print(barGraphElevation) 

# Time since fire graph
d <- drop_na(pointsReduced, FireClass)

barGraphFire <- d %>%
  mutate(FireClassOrdered = fct_relevel(FireClass,
                                        "1-to-6 years","6-to-12 years","12-to-17 years","17-to-25 years",
                                        "25+ years","No data or No Fire")) %>%
  ggplot(aes(x = FireClassOrdered)) + 
  geom_bar(fill = pal[3]) +
  theme_bw() + labs(x="",y="Count of sites") +
  guides(fill=guide_legend(title="Elevation ranges")) +
  theme(
    axis.text.x = element_text(size = 10, angle=90),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_blank(),
    legend.position = "none"
  )
#dev.new();print(barGraphFire) 

combined <- ggarrange(barGraphLandcover,barGraphElevation,barGraphFire,
                       labels = c("A", "B","C"),
                       ncol = 3, nrow = 1,
                       align = "hv")
dev.new();print(combined) 

ggsave(outGraphLandcoverElev, width = 7, height = 5, units = "in", dpi=300)


# number of recordings by month and campaign
d <- pointsReduced %>%
  group_by(Month) %>%
  summarize(total = sum(RecordingNum))
d$campaign <- ifelse(d$Month < 9,"Wet season",ifelse(d$Month > 9, "Dry season","Transition"))
d$monthtext <- c("Jun","Jul","Aug","Sep","Oct","Nov","Dec")
d$monthtext <- factor(d$monthtext, levels=c("Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

barGraphRecordings <- 
  ggplot(d, aes(x = monthtext, y = total, fill = campaign)) + 
  geom_bar(stat = "identity") +
  theme_bw() + labs(x="",y="Count of recordings") +
  scale_fill_brewer(palette = "Set2", na.value="grey") +
  guides(fill=guide_legend(title="Campaign")) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.position = "right"
  )
dev.new();print(barGraphRecordings) 

ggsave(outGraphRecordings, width = 7, height = 5, units = "in", dpi=300)