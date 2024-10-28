# Takes a file geodatabase from a Survey123 download and renames photos to include site ID
# Photos must be extracted from the file geodatabase first using the python script Export Attachments
# from within an ArcGIS Pro toolbox

# Matthew L. Clark, Ph.D.
# Department of Geography, Environment, and Planning
# Sonoma State University, California USA
# matthew.clark@sonoma.edu
# Version 08/13/2023

library(sf)
library(dplyr)
library(stringr)


# file geodatabase with AVIRIS and LVIS vector data
fgdb <- "E:/active/project/biosoundscape/Survey123 locations/BioSoundSCape_v2.0_FGDB_230922.gdb"

# working directory with photos
#workingDir <- "E:/active/project/biosoundscape/Survey123 locations/s123_photos/"
workingDir <- "E:/temp/photos/"
setwd(workingDir)

# get files list
files <- data.frame(filenames = list.files(pattern = ".jpg"))

# link to Survey 123 feature class in ArcGIS Online.
s123 <- st_read(fgdb, layer = "survey")
s123 <- as.data.frame(s123) %>% select(-SHAPE)

# loop through GlobalIDs
for (g in unique(s123$globalid)) {
  g1 <- gsub("\\{|\\}", "", g)
  f <- files %>% filter(str_detect(filenames,g1))
  if (dim(f)[1] > 0){
    s <- s123 %>% filter(globalid == g)
    d <- s$X_date
    site <- paste0(tolower(s$recorder_id),"_",substring(d,3,4),substring(d,6,7),substring(d,9,10))
    f1 <- str_split_fixed(gsub("\\{|\\}", "", f$filenames), "_", 4)
    for (i in 1:dim(f)[1]){
      infile <- f[i,1]
      of <- str_split_fixed(f1[i,4],"-",3)[1]
      of <- gsub("north_ie_view_north_rec", "north", of)
      of <- gsub("view_of_mounted_sound_r", "mounted", of)
      of <- gsub("upward_view_forest_only", "upward", of)
      outfile <- paste0(site,"_",of,".jpg")
      if (file.exists(infile) == T & file.exists(outfile) == F) file.rename(infile, outfile)
    }
  }
}

# extra fixes
file.rename("choice_14_230823_east.jpg", "s2lam003_230823_east.jpg")
file.rename("choice_14_230823_west.jpg", "s2lam003_230823_west.jpg")
file.rename("choice_14_230823_north.jpg", "s2lam003_230823_north.jpg")
file.rename("choice_14_230823_south.jpg", "s2lam003_230823_south.jpg")
file.rename("choice_14_230823_mounted.jpg", "s2lam003_230823_mounted.jpg")

file.rename("GlobalID_{A8142560-82EC-47BA-B7C6-FAA7C9ED2A0F}_picture_east-20230813-061247.jpg", "s2lam161_230813_east.jpg")
file.rename("GlobalID_{A8142560-82EC-47BA-B7C6-FAA7C9ED2A0F}_picture_west-20230813-061303.jpg", "s2lam161_230813_west.jpg")
file.rename("GlobalID_{A8142560-82EC-47BA-B7C6-FAA7C9ED2A0F}_picture_north_ie_view_north_rec-20230813-061239.jpg", "s2lam161_230813_north.jpg")
file.rename("GlobalID_{A8142560-82EC-47BA-B7C6-FAA7C9ED2A0F}_picture_south-20230813-061255.jpg", "s2lam161_230813_south.jpg")
file.rename("GlobalID_{A8142560-82EC-47BA-B7C6-FAA7C9ED2A0F}_picture_view_of_mounted_sound_r-20230813-061231.jpg", "s2lam161_230813_mounted.jpg")

