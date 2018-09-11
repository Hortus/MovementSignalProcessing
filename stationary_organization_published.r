#Script to analyze the bandpassed color signal data from stationary videos

#each plot is an individual file with frequency and corresponding area under the curve for that frequency.
#Plots are individual text files within a subdirectory that corresponds to videodate.  Need to get the planting date, video date info from the directory.  Need to get the cultivar, crop, rep, and position info from individual files within subdirectory.  Files in subdirectory are alphanumeric order, so position vector will need to correspond to the alphanumeric order of the files.

#set working directory to the signals directory
setwd("D:/.../signals")
path2main <- getwd()

#get all subdirectory names. first entry is a period
subdir.names <- list.dirs(path = ".",full.names=TRUE,recursive = TRUE)

#loop through the subdirectories to get videodate, planting date

#Create the list of dataframes.  Each dataframe represents 1 video date, planting date combination. It will not vary by number of rows.  In the end, each dataframe in this list of dataframes will be concatenated, representing the dataframe for a given planting date across all video dates.  
df.allvideoslist <- replicate(length(subdir.names), data.frame())

#length is minus 1, since the first entry is just a period.  Skip this first entry
for(i in 1:(length(subdir.names)-1)){
  subdir <- subdir.names[i+1] #ith plus 1 directory name
  #set the ith+1 directory as the new working directory (first directory is the root- not holding any files)
  subdir2 <- gsub("^.*\\/","",subdir) #remove everything before /
  path2sub <- paste(path2main,subdir2,sep="/")
  setwd(path2sub)
  
  #get video date for this subdirectory
  intVD <- sub('.*Y', '',subdir) #intermediate video date string (everythign after Y) of the subdir name
  VideoDate <- gsub("^([^_]*_[^_]*_[^_]*)_.*$", "\\1", intVD) #everything before the third underscore
  
  #get all files in the subdirectory
  file.names <- dir(path2sub)
  #create a dataframe for the ith video date. Each file within this video date subdirectory will be summarized in a single line within this dataframe.  This amounts to one line per one file in the subdirectory. dataframe has 27 columns
  df.ithvideodate <- data.frame(matrix( nrow=0, ncol=30))
  #colnames of the ith video date dataframe
  colnames.ithvideodate <- c("Line", "Crop", "PlantingDate", "Rep", "VideoDate","MeanFreq","MedianFreq","SdFreq", "Hz0.5", "Hz0.7", "Hz0.9", "Hz1.1", "Hz1.3", "Hz1.5", "Hz1.7", "Hz1.9", "'Hz2.1", "Hz2.3", "Hz2.5", "Hz2.7", "Hz2.9", "Hz3.1", "Hz3.3", "Hz3.5", "Hz3.7", "Hz3.9", "Hz4.1", "Hz4.3", "Hz4.5", "Position")
  colnames(df.ithvideodate) <- colnames.ithvideodate
  
  for(j in 1:(length(file.names))){
    filename <- file.names[j]
    file <- read.table(filename,header=FALSE)
    
    #get the mean frequency, median frequency, and frequency sd for the file (subseted for the pass band frequencies). These will be 
    pass <- subset(file, file$V1 >= 0.5 & file$V1 <4.9)
    meanFreq <- mean(pass$V1)
    medianFreq <- median(pass$V1)
    sdFreq <- sd(pass$V1)
    
    #subset the rows within each file based on 0.2 HZ increments in the first field. Start at 0.5, end at 4.7 (pass band frequencies)
    Hz0.5<- subset(file, file$V1 >= 0.5 & file$V1 <0.7)
    Hz0.7<- subset(file, file$V1 >= 0.7 & file$V1 <0.9)
    Hz0.9<- subset(file, file$V1 >= 0.9 & file$V1 <1.1)
    Hz1.1<- subset(file, file$V1 >= 1.1 & file$V1 <1.3)
    Hz1.3<- subset(file, file$V1 >= 1.3 & file$V1 <1.5)
    Hz1.5<- subset(file, file$V1 >= 1.5 & file$V1 <1.7)
    Hz1.7<- subset(file, file$V1 >= 1.7 & file$V1 <1.9)
    Hz1.9<- subset(file, file$V1 >= 1.9 & file$V1 <2.1)
    Hz2.1<- subset(file, file$V1 >= 2.1 & file$V1 <2.3)
    Hz2.3<- subset(file, file$V1 >= 2.3 & file$V1 <2.5)
    Hz2.5<- subset(file, file$V1 >= 2.5 & file$V1 <2.7)
    Hz2.7<- subset(file, file$V1 >= 2.7 & file$V1 <2.9)
    Hz2.9<- subset(file, file$V1 >= 2.9 & file$V1 <3.1)
    Hz3.1<- subset(file, file$V1 >= 3.1 & file$V1 <3.3)
    Hz3.3<- subset(file, file$V1 >= 3.3 & file$V1 <3.5)
    Hz3.5<- subset(file, file$V1 >= 3.5 & file$V1 <3.7)
    Hz3.7<- subset(file, file$V1 >= 3.7 & file$V1 <3.9)
    Hz3.9<- subset(file, file$V1 >= 3.9 & file$V1 <4.1)
    Hz4.1<- subset(file, file$V1 >= 4.1 & file$V1 <4.3)
    Hz4.3<- subset(file, file$V1 >= 4.3 & file$V1 <4.5)
    Hz4.5<- subset(file, file$V1 >= 4.5 & file$V1 <4.7)
    #Use lapply to get the sum of red change for each frequency bin
    Hz0.5 <- sum(Hz0.5$V2)
    Hz0.7 <- sum(Hz0.7$V2)
    Hz0.9 <- sum(Hz0.9$V2)
    Hz1.1 <- sum(Hz1.1$V2)
    Hz1.3 <- sum(Hz1.3$V2)
    Hz1.5 <- sum(Hz1.5$V2)
    Hz1.7 <- sum(Hz1.7$V2)
    Hz1.9 <- sum(Hz1.9$V2)
    Hz2.1 <- sum(Hz2.1$V2)
    Hz2.3 <- sum(Hz2.3$V2)
    Hz2.5 <- sum(Hz2.5$V2)
    Hz2.7 <- sum(Hz2.7$V2)
    Hz2.9 <- sum(Hz2.9$V2)
    Hz3.1 <- sum(Hz3.1$V2)
    Hz3.3 <- sum(Hz3.3$V2)
    Hz3.5 <- sum(Hz3.5$V2)
    Hz3.7 <- sum(Hz3.7$V2)
    Hz3.9 <- sum(Hz3.9$V2)
    Hz4.1 <- sum(Hz4.1$V2)
    Hz4.3 <- sum(Hz4.3$V2)
    Hz4.5 <- sum(Hz4.5$V2)

    
    #Plot info from filename
    #pull out line name from the jth filename in file.names
    #NOTE change all headings in the outputFLY files.  AC_Metcalf_2row to ACMetcalf_2row and ND_Genesis_2row to NDGenesis_2row.
    Line <- gsub("_.*$", "", file.names[j])
    LineCropStr <- gsub("^([^_]*_[^_]*)_.*$", "\\1", file.names[j]) #returns the objects before the second _ (line+crop)
    Crop <- gsub(".*_","",LineCropStr)
    PDRStr <- gsub(".*_","",file.names[j]) #returns string after the last _ (Planting Date + rep)
    PDRStr <- gsub("\\..*","",PDRStr) #returns string before .txt in filename
    Rep <- gsub("\\d","",PDRStr) #returns the rep (ABCD) from planting date + rep
    PlantingDate <- gsub("\\D","",PDRStr) #returns the planting date from planting date + rep
    Position <- NA #NA for now

    #append all values to jth file dataframe as a row, in the following order
    #Line, Crop, PlantingDate, Rep, VideoDate, Hz0.5, Hz0.7, Hz0.9, Hz1.1, Hz1.3, Hz1.5, Hz1.7, Hz1.9, Hz2.1, Hz2.3, Hz2.5, Hz2.7, Hz2.9, Hz3.1, Hz3.3, Hz3.5, Hz3.7, Hz3.9, Hz4.1, Hz4.3, Hz4.5, Position.  
    df.ithvideodate[j,]<- c(Line, Crop, PlantingDate, Rep, VideoDate, meanFreq, medianFreq, sdFreq, Hz0.5, Hz0.7, Hz0.9, Hz1.1, Hz1.3, Hz1.5, Hz1.7, Hz1.9, Hz2.1, Hz2.3, Hz2.5, Hz2.7, Hz2.9, Hz3.1, Hz3.3, Hz3.5, Hz3.7, Hz3.9, Hz4.1, Hz4.3, Hz4.5, Position)
    #Position will be added after looping through each file in the ith subdirectory
    
    
  }
  
  #Add position column
  #create a position vector.  2 if adjacent to camera track, 1 if not adjacent
  #Based on planting date.  Here these correspond to alphanumeric order of files in the subdirectories
  #Position <- c(2,1,2,1,2,2,1,1,2,1,1,2,1,2,2,1,2,1,1,2,1,2,1) #p1
  Position <- c(1,2,1,2,1,1,1,2,2,2,1,2,1,1,2,1,1,2,1,2,2) #p2
  #Position <- c(2,2,1,1,1,1,2,1,1,2,2,2,2,1,1,2,2,1,2,2,1,1,2,1,1,2,1) #p3
  #Position <- c(2,1,1,2,1,2,2,1,1,1,2,2,1,2,1,1,1,2,2,1,2,2,1,2,1,2,1) #p4
  
  df.ithvideodate$Position <- Position
  
  #append dataframe to df.allvideoslist
  df.allvideoslist[[i]] <- df.ithvideodate
  
}


#Rbind the dataframes in df.allvideoslist
finaldf.p2 <- do.call("rbind",df.allvideoslist)

#rbind finaldfs after running the script for all panels (only p2 present in this signal directory). Comment out until all finaldfn dataframes are in memory
#finaldf <- rbind(finaldf.p1,finaldf.p2,finaldf.p3,finaldf.p4)

#write finaldf to txt file
#write.table(finaldf, "D:/PhD/Videos/stationary_output/stationaryfinal.txt", sep="\t")

#finaldf containing the frequency, amplitude, and plot data for all four panels is provided in the signals directory.


