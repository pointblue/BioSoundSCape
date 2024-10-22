
## This script takes as input the following:
# The path to the directory where the folders with sites to be renamed 
# If an hour offset is needed, this can be set with offsetHours (in hours)
# Number of files at the start of survey to skip
# Number of files at the end of survey to skip
# The path to the folder where folders with renamed files will be placed

## This script then renames the files as needed and provides the following outputs:
# A folder within the output directory, named: unitname_deploydate, unitname is s2l[am/lg]###, and deploydate is yymmdd
# Within that folder, all files renamed as: unitname_deploydate_yyyy-mm-dd_hh-mm.WAV

# Original functions and script for a single folder rename by Leo Salas
# Updated to loop through folders and just perform an hour offset, if need; Matthew Clark Sept 24, 2023

########################
# INPUTS
########################

inDir <- "E:/archive/project/biosoundscape/to_clean" # input folder with subdirectories with sites
outDir <- "E:/archive/project/biosoundscape/AudioDataCampaign1UTCRenamed" # output folder for subdirectories of sites with renamed files
offsetHours <- 2 # for South Africa time from UTC, use 2
numToSkipStart<-1	#Number of files at the start of survey to skip renaming 
numToSkipEnd<-1 	#Number of files at the tail end of survey to skip renaming 


########################
# DEPENDENCIES
########################
library(plyr)

checkInputPaths<-function(impPath,type){
	if(substr(impPath,nchar(impPath),nchar(impPath))!="/"){
		impPath<-paste0(impPath,"/")
	}
	if(!dir.exists(impPath) & type %in% c("goog")){
		stop(paste("The directory",impPath,"cannot be reached or does not exist. Please make sure you have mapped and can access the Google drive from the File Explorer.",
						"\n If the folder does not exist in the Google drive, create this directory and try again."))
	}
	if(!dir.exists(impPath) & type %in% c("local")){
		stop(paste("The directory",impPath,"does not exist. Please create it and try again."))
	}
	if(!dir.exists(impPath) & type=="src"){
		stop(paste("The directory",impPath,"does not exist. Please provide the correct data directory and try again."))
	}
	if(type=="src" & NROW(list.files(impPath))<50){
		stop(paste0("WARNING: there are fewer than 50 files in ", impPath,". Is this source directory correct?"))
	}
	return(impPath)
}

checkARUname<-function(amn){
	if(tolower(substr(amn,1,3))!="s2l"){
		stop("Incorrect ARU name - must begin with 's2l'")
	}
	if(!tolower(substr(amn,4,5)) %in% c("am","lg")){
		stop("Incorrect ARU name - must have unit type: lg or am")
	}
	if(nchar(amn)!=8){
		stop("Incorrect ARU name - more than 8 characters long. Expect: 8.")
	}
	if(!is.numeric(as.numeric(substr(amn,6,8)))){
		stop("Incorrect ARU name - last 3 characters must be a number")
	}
	return(tolower(amn))
}

getNewNames<-function(wavNames,sourcePath,mothUnitName,deployDate,localDestFolder,offsetHours){
	
	if(offsetHours != 0){
		firstFile<-sort(wavNames)[1]
		firstDate<-substr(firstFile,1,nchar(firstFile)-4)
		start<-as.POSIXlt(firstDate,format="%Y%m%d_%H%M%S")
		end<-start + (offsetHours * 3600) # offsets number of hours
		diff<-end - start # finds time difference
	}else{
		diff<-0
	}
	
	#loop through the files, apply offset, get new name
	resdf<-ldply(wavNames,function(nn,sourcePath,mothUnitName,deployDate,localDestFolder,diff){
				currDate<-as.POSIXlt(substr(nn,1,nchar(nn)-4),format="%Y%m%d_%H%M%S")
				newDate<-currDate + diff
				oldFilePath<-paste0(sourcePath,nn)
				newFileLocalPath<-paste0(localDestFolder,mothUnitName,"_",deployDate,"_",format(newDate,"%Y-%m-%d_%H-%M"),".WAV")
				tdf<-data.frame(srcFile=oldFilePath,destFileLocal=newFileLocalPath)
				return(tdf)
			},sourcePath=sourcePath,mothUnitName=mothUnitName,deployDate=deployDate,localDestFolder=localDestFolder,diff=diff)
	resdf$srcFile<-as.character(resdf$srcFile)
	resdf$destFileLocal<-as.character(resdf$destFileLocal)
	return(resdf)
}


########################
# MAIN PROCESS
########################

setwd(inDir)
dirs <- list.dirs(full.names = F, recursive = F)

# dirs <- dirs[(which(dirs=="s2lam119_230703")+1):length(dirs)]

for (d in dirs){
  
  sourcePath<-checkInputPaths(d,type="src") # input directory
  ARUname <- substr(d,1,8) # get unit name
  mothUnitName<-checkARUname(ARUname) # check the unit name
  localDestPath<-checkInputPaths(outDir,type="local") # Mandatory: you need a local copy of renamed data to upload to Arbimon. 
  
  #check the first 6 files in sourcePath - see if it is correct.
  #head(list.files(sourcePath))
  
  ## generate essentials from the above
  # get the file names
  wavNames<-list.files(sourcePath,pattern=".WAV")
  # get the deploy date
  firstFile<-sort(wavNames)[1]
  deployDate<-substr(firstFile,3,8)
  
  ########################
  # Create local target directories
  ########################
  # create the local target directory 
  localDestFolder<-paste0(localDestPath,"/",mothUnitName,"_",deployDate,"/")
  unlink(localDestFolder, recursive=TRUE)
  dir.create(localDestFolder)
  
  ########################
  # RENAME & COPY output
  ########################
  newNamesdf<-getNewNames(wavNames,sourcePath=sourcePath,mothUnitName=mothUnitName,deployDate=deployDate,localDestFolder=localDestFolder,offsetHours=offsetHours)
  newNamesdf<-newNamesdf[order(newNamesdf$srcFile),]
  sks<-1;if(numToSkipStart>0){sks<-numToSkipStart+1}
  skd<-nrow(newNamesdf);if(numToSkipEnd>0){skd<-skd - numToSkipEnd}
  if(numToSkipStart>0 | numToSkipEnd>0){
    newNamesdf<-newNamesdf[c(sks:skd),]
  }
  
  l_ply(1:nrow(newNamesdf),function(rr,newNamesdf){
    src<-newNamesdf[rr,"srcFile"]
    destloc<-newNamesdf[rr,"destFileLocal"]
    file.copy(from=src,to=destloc)
  },newNamesdf=newNamesdf)
  
  print(paste0("Renamed: ",d))
  
  
}


