#Tool for the Definition and Analysis of Functional Economic Market Areas (DAFEMA).
#written by Carlos Padron carlos.florez.16@ucl.ac.uk;padron.ca@gmail.com.

#This tool has been written as final coursework for the assignature BENVGSA3: 
#Geographic Information Systems and Science (2016-2017).

################################################################################
#INSTRUCTIONS
################################################################################
#Please note that the script expects the data to be stored in the same folder as
#the script. 
#After sourcing the script, the user only needs to run the following command to 
#start:
#
#   dafemaStart(region, readData) 
#
#example 
#   dafemaStart("north east", readData = TRUE)
#   dafemaStart("north east")
#
#Please take in consideration the amount of msoa per region as it affects the
#computing time
# 340;"North East"                dafemaStart("north east")
# 573;"East Midlands"             dafemaStart("East Midlands")
# 692;"Yorkshire and The Humber"  dafemaStart("Yorkshire and The Humber")
# 700;"South West"                dafemaStart("South West")
# 735;"West Midlands"             dafemaStart("West Midlands")
# 736;"East of England"           dafemaStart("East of England")
# 924;"North West"                dafemaStart("North West")
# 983;"London"                    dafemaStart("London")
# 1108;"South East"               dafemaStart("South East")
#
#The region value can be one or many regions using a string, a string vector or 
#a defined variable i.e. 'london', c('london', 'south east'), definedVariable etc.
#
#This tool was tested with all the regions in england. They should work except
#South West which doesn't to work for the moment.
#
#The tool works for the region of London but this region is so intrinsically
#connected that shows only one big fema even for small values.
#
#The read data argument is optional and by default FALSE. In order to save time,
#dafema reads the data only once so if it is used again it will skip this step. 
#Anyhow, the user might modify or replace the data. In this case the user can 
#force dafema to read the data with the argument readData = TRUE.
#
#Once it is finished, the following commands can also be used:
#
#   dafemaShowMap(closureTarget)
#   View(dafemaFemaResults)
#
#dafemaShowMap shows a map where the user can visualise the msoa forming
#the fema per closure target.
#example:
#
#     dafemaShowMap(.40)
#
#View(dafemaFemaResults) shows a table with all closure values for every FEMA.

################################################################################
#This bit of sql code was used to merge the commuting data from output area to 
#middle super output area
#
# WITH msoa AS (SELECT
#               A."MSOA11CD" AS residence,
#               B."MSOA11CD" AS workplace,
#               sum(flow."Persons")
#               FROM
#               ucl.flow,
#               ucl.oalookup AS A,
#               ucl.oalookup AS B
#               WHERE
#               flow."Area.of.usual.residence" = A."OA11CD" AND
#               flow."Area.of.workplace" = B."OA11CD"
#               GROUP BY
#               A."MSOA11CD",
#               B."MSOA11CD")
# 
# SELECT
# A.residence,
# A.workplace,
# A.sum,
# ST_Touches(B.geom, C.geom)
# FROM
# msoa AS A,
# ucl."Middle_Layer_Super_Output_Areas_December_2011_Full_Extent_Bound" AS B,
# ucl."Middle_Layer_Super_Output_Areas_December_2011_Full_Extent_Bound" AS C
# WHERE
# A.residence=B.msoa11cd
# AND A.workplace=C.msoa11cd


################################################################################

#Initiation module: It runs and manages all the modules. It loads the 
#libraries and stores the global variables to be used by following modules.

library(rgdal)
library(RColorBrewer)


#main function. It only runs other functions.
dafemaStart <- function(region, readData = FALSE) {
  #check that data exists
  if (!file.exists("flow_data.csv")) {
    stop("Error: Data file doesn't exist. 
         Please store file flow_data.csv in the script folder")
  }
  if (!file.exists('regions.csv')
      || !file.exists("MSOA11_BUASD11_BUA11_LAD11_RGN11_EW_LU.csv")
      ) {
    stop("Error: Data file doesn't exist. 
         Please store file regions.csv,\n
         and MSOA11_BUASD11_BUA11_LAD11_RGN11_EW_LU.csv,\n
         in the script folder")
  }
  if (!file.exists("Middle_Layer_Super_Output_Areas_December_2011_Full_Extent_Boundaries_in_England_and_Wales.shp"))
  {
    stop("Error: Data file doesn't exist. 
         Please store file Middle_Layer_Super_Output_Areas_December_2011_Full_Extent_Boundaries_in_England_and_Wales.shp
         in the script folder")
  }
  #check region value
  regionList <- read.csv("regions.csv", header = TRUE)
  for (i in region) { 
    if (!(tolower(i) %in% tolower(regionList$RGN11NM))) {
      regions <- paste(regionList$RGN11NM, collapse = ", ")
      stop(paste("Error: region ",i," not found. Use any of: ",regions))
    }
  }
  #subsets the data based on region 
  dafemaSubsetRegion(region, readData)
  
  #calculates all FEMAs
  dafemaCalculateAllFema()

}
################################################################################
#subsets the data based on region 
dafemaSubsetRegion <- function(region, readData) {
  #if data already exist it skips. If user request to override data it will do so.
  if (readData==TRUE 
      || !exists("dafemaRawData") 
      || !exists("dafemaLookUp") 
      || !exists("dafemaRawMap") 
      ) {
    #reads data
    cat("reading data\n")
    #lookup table that links the oa to the regions
    dafemaLookUp <- read.csv("MSOA11_BUASD11_BUA11_LAD11_RGN11_EW_LU.csv", 
                                   header = TRUE)

    #commuting data
    dafemaRawData <- read.csv("flow_data.csv", header = TRUE, sep = ";")
    
    #msoa map
    dafemaMap <- readOGR(dsn = ".", 
 layer ="Middle_Layer_Super_Output_Areas_December_2011_Full_Extent_Boundaries_in_England_and_Wales")
    
    #stores values in global environment for future use    
    assign("dafemaRawData", dafemaRawData, envir = .GlobalEnv)
    assign("dafemaLookUp", dafemaLookUp, envir = .GlobalEnv)
    assign("dafemaRawMap", dafemaMap, envir = .GlobalEnv)
  }
  #subset data for only the required regions
  cat("subsetting data\n")
  dafemaMSOAList <- subset(dafemaLookUp, 
                     tolower(dafemaLookUp$RGN11NM) %in% tolower(region))
  
  #stores values in global environment for future use
  assign("dafemaMSOAList", dafemaMSOAList, envir = .GlobalEnv)
  
  dafemaData <- subset(dafemaRawData, 
                       dafemaRawData$residence %in% dafemaMSOAList$MSOA11CD &
                         dafemaRawData$workplace %in% dafemaMSOAList$MSOA11CD)
  #subsets the map
  dafemaMap <- subset(dafemaRawMap, 
                      dafemaRawMap$msoa11cd %in% dafemaMSOAList$MSOA11CD)
  
  #clean copy to be stored for any future use
  assign("dafemaData", dafemaData, envir = .GlobalEnv)
  assign("dafemaRegion", region, envir = .GlobalEnv)
  assign("dafemaMap", dafemaMap, envir = .GlobalEnv)
}
################################################################################
#calculate a set of FEMA for the closure values: 40, 50, 60, 70, 80, and 90.

dafemaCalculateAllFema <- function() {
  #set the closure value to 40 which is an arbitrary value. 
  #This variable increases by 10 with every transaction.
  #it also stores all closure values to be used by other values.
  closureTarget <- c(0.40, 0.50, 0.60, 0.70, 0.80, 0.90)
  #step record
  step <- 1
  
  #for every closure value calculate a set of fema
  for (i in closureTarget) {
    closurevalue <- closureTarget[step]
    dafemaCalculateFema(closurevalue)
    step <- step+1
    #copy all fema to new closure target
    cat("Copying fema to new step\n")
    dafemaFemaNext(closurevalue)
    
    cat("New step\n")
  }
  #joins all resulting tables into one
  dafemaResults()
  #it produces a table for the final maps
  dafemaMapData()
  #prepares data for map.
  cat("Complete! Run dafemaShowMap(closureTarget) or View(dafemaFemaResults) \n")
}
################################################################################
#calculate a set of FEMA for the given closure values.
#All calculations are based on Pratt, M., Wright, J.A., Cockings, S., Sterland, 
#I., 2013. Defining Retail Conurbations: Using a Rules-based Algorithm 
#and Coombes, M. and S. Bond, 2007. Travel-to-Work Areas: the 2007 review.
#Centre for Urban & Regional Development Studies in Newcastle University) and 
#the (Office for National Statistics.

dafemaCalculateFema <- function(closurevalue) {
  #it creates a dataframe to contain all femas and populates it with all msoa
  #in the selected region.
  cat("Creating proto ttwa\n")
  dafemaFema <- dafemaFemaFirst(closurevalue)
  
  #for containment target, the code will run until all areas
  #reach the target.
  cat("Calculating Fema for target ",closurevalue, "\n")
  while (min(dafemaFema$selfContainment) < closurevalue) {
    #obtains and sorts a list of all fema that doesn't reach the target.
    
    femaToMerge <- subset(dafemaFema,
                          dafemaFema$closureTarget==closurevalue
                          & dafemaFema$selfContainment < closurevalue
    )
    femaToMerge <- femaToMerge[order(femaToMerge$selfContainment, 
                                     decreasing = FALSE),]
    
    #Merges every member in the non-complying list with the adjacent fema
    #with more relationship. This is an alteration of the algorithm found 
    #in literature as it only considers adjacent areas. This approach was
    #taken to avoid having non-contiguous FEMA.
    
    #for low self contained fema merge find its neighbours
    dafemaNeighbourMsoa <- dafemaMsoaNeighbours(femaToMerge$femaMSOAMembers[1])
    
    #find femas for this step containing the msoa neighbours
    dafemaNeighbourFema <- dafemaFemaNeighbours(dafemaNeighbourMsoa,
                                                dafemaFema,
                                                closurevalue)
    
    #find fema neighbour with highest relation
    HigherRelatedFemaNeighbour <- dafemaHigherRelatedFemaNeighbour(
      femaToMerge[1,],
      dafemaNeighbourFema)
    
    #merges fema with lowest self-containment with the contiguous fema with
    #highest relationship
    dafemaTableLastNumber <- nrow(dafemaFema)+1
    dafemaFemaLastNumber <- max(dafemaFema$femaId)+1
    dafemaNewFemaMembers <- list(c(
      unlist(HigherRelatedFemaNeighbour$femaMSOAMembers[1]),
      unlist(femaToMerge$femaMSOAMembers[1])))
    
    dafemaFema[dafemaTableLastNumber,1] <- dafemaFemaLastNumber
    dafemaFema[dafemaTableLastNumber,2] <- closurevalue
    dafemaFema[dafemaTableLastNumber,3][[1]] <- dafemaNewFemaMembers
    dafemaFema[dafemaTableLastNumber,4] <- dafemaSelfContainment(dafemaNewFemaMembers)
    
    #removes from the list of femas the old femas that were merged
    dafemaFema <- subset(dafemaFema, 
                         dafemaFema$femaId != HigherRelatedFemaNeighbour$femaId[1]
                         & dafemaFema$femaId != femaToMerge[1,1])
    
    #end while (merging all low self contained until reach target)  
  }
  ###############stores snapshot
  cat("Saving snapshot\n")
  if (closurevalue==.40) {
    assign("dafemaData40",dafemaFema, envir = .GlobalEnv)
  }
  if (closurevalue==.50) {
    assign("dafemaData50",dafemaFema, envir = .GlobalEnv)
  }
  if (closurevalue==.60) {
    assign("dafemaData60",dafemaFema, envir = .GlobalEnv)
  }
  if (closurevalue==.70) {
    assign("dafemaData70",dafemaFema, envir = .GlobalEnv)
  }
  if (closurevalue==.80) {
    assign("dafemaData80",dafemaFema, envir = .GlobalEnv)
  }
  if (closurevalue==.90) {
    assign("dafemaData90",dafemaFema, envir = .GlobalEnv)
  }
  if (closurevalue==1.00) {
    assign("dafemaData100",dafemaFema, envir = .GlobalEnv)
  }
#end function
}
################################################################################
#it creates a dataframe to contain all femas and populates it with all msoa
#in the selected region.

dafemaFemaFirst <- function(closurevalue) {
  #checks if closure value is .40. If true, it creates a new dataframe
  #else, it copies from the previous step.
  if (closurevalue==.40){
    #It creates an unique id to the first set of OA
    firstFema <- 1:nrow(dafemaMSOAList)
    
    #creates a dataframe where all fema are going to be stored.
    dafemaFema <- data.frame(firstFema, 
                             closurevalue)
    colnames(dafemaFema) <- c("femaId", 
                              "closureTarget")
    
    #loads every msoa as a list and self containment.
    #This is to be able to store multiple msoa in the column
    dafemaFema$femaMSOAMembers <- list(NULL)
    for (i in 1:length(dafemaMSOAList$MSOA11CD)) {
      dafemaFema$selfContainment[i] <- dafemaSelfContainment(
        dafemaMSOAList$MSOA11CD[i])
      dafemaFema$femaMSOAMembers[[i]] <- as.list(as.vector(dafemaMSOAList[i,1]))
    }
    return(dafemaFema)
  }
  if (closurevalue==.50) {
    return(dafemaData50)
  }
  if (closurevalue==.60) {
    return(dafemaData60)
  }
  if (closurevalue==.70) {
    return(dafemaData70)
  }
  if (closurevalue==.80) {
    return(dafemaData80)
  }
  if (closurevalue==.90) {
    return(dafemaData90)
  }
}
################################################################################
#find the msoa neighbours of the given msoa 
dafemaMsoaNeighbours <- function(femaMSOAMembers) {
  #subset the data obtain list of relationships 
  #or flows for the selected fema
  dafemaFlows <- subset(dafemaData,
                        #exclude msoa that are part of the fema (residence
                        #and workplace are both part of the fema)
                        !(dafemaData$residence 
                          %in% unlist(femaMSOAMembers)
                          & dafemaData$workplace 
                          %in% unlist(femaMSOAMembers))
                        #include those msoa that are related to the fema.
                        #residence OR workplace are in the fema
                        & (dafemaData$residence 
                           %in% unlist(femaMSOAMembers)
                           | dafemaData$workplace 
                           %in% unlist(femaMSOAMembers))
                        #include only msoa that are contiguous
                        & dafemaData$st_touches=="t")
  
  #from the subset obtain only the msoa that are not part of the fema
  dafemaNeighbourMsoa <- c(as.vector(dafemaFlows$residence), 
                           as.vector(dafemaFlows$workplace))
  dafemaNeighbourMsoa <- unique(subset(dafemaNeighbourMsoa, 
                                       !(dafemaNeighbourMsoa 
                                         %in% 
                                           unlist(femaMSOAMembers))))
  return(dafemaNeighbourMsoa)
}
################################################################################
#calculates the self-containment of every FEMA as described in 
#Pratt et al. (2013)

dafemaSelfContainment <- function(femaMSOAMembers) {
  #null vectors that will contain the variables to calculate
  #self containment
  internalFlow <- NULL
  supplyFlow <- NULL
  demandFlow <- NULL
  supplyContainment <- NULL
  demandContainment <- NULL
  selfContainment <- NULL
  
  #for the fema in the input list the code calculates every variable
  #and self containment. All calculations are based on Pratt et al. (2013)
  #and Coombes and Bond (2007).
  
  #flow that starts and end in the same fema
  internalFlow <- sum(subset(dafemaData$sum,
                       dafemaData$residence %in% unlist(femaMSOAMembers)
                       & dafemaData$workplace %in% unlist(femaMSOAMembers)
                       ))
  #flow that starts in the fema and ends in any other fema
  supplyFlow <- sum(subset(dafemaData$sum,
                              dafemaData$residence %in% unlist(femaMSOAMembers)
                              & !(dafemaData$workplace %in% 
                                    unlist(femaMSOAMembers)) 
                              ))
  #flow that start outside the fema and ends in the fema
  demandFlow <- sum(subset(dafemaData$sum,
                           !(dafemaData$residence %in% unlist(femaMSOAMembers))
                           & dafemaData$workplace %in% unlist(femaMSOAMembers)
                           ))
  #suply and demand containment
  supplyContainment <- internalFlow/(supplyFlow+0.01)
  demandContainment <- internalFlow/(demandFlow+0.01)
  
  #self containment is the minimum of the two containment values 
  #(Pratt et al., 2013) and (Coombes 2007)
  selfContainment <- internalFlow/(supplyFlow+demandFlow+internalFlow)

  return(selfContainment)
}
################################################################################
#find FEMA in the step containing  a set of msoa

dafemaFemaNeighbours <- function(dafemaNeighbourMsoa, 
                               dafemaFema,
                               closureTarget) {
  #substract the set of fema only for this step
  dafemaNeighbourFemaId <- NULL
  dafemaFemaId <- subset(dafemaFema$femaId,
                         dafemaFema$closureTarget==closureTarget)
  #checks every fema in the step looking for 
  #the one containing the neighbours msoa
  for (i in 1:length(dafemaFemaId)) {
    #substract only one fema at the time and takes it if it contains the msoa.
    dafemaFemaTemp <- subset(dafemaFema,
                             dafemaFema$femaId==dafemaFemaId[i])
    if (any(dafemaNeighbourMsoa %in% unlist(
      dafemaFemaTemp$femaMSOAMembers))) {
      newRow <- length(dafemaNeighbourFemaId)+1
      dafemaNeighbourFemaId[newRow] <- dafemaFemaTemp$femaId
    }
  }
  dafemaNeighbourFema <- subset(dafemaFema,
                                dafemaFema$femaId %in%
                                  dafemaNeighbourFemaId)
  return(dafemaNeighbourFema)
  
}
################################################################################
dafemaHigherRelatedFemaNeighbour <- function(femaToMerge, dafemaNeighbourFema) {
  #for every neighbour fema obtain amount of flows from and to the 
  #non-complying fema. Only the biggest relationship is to be considered.
  dafemaNeighbourFema$flow <-NULL
  for (i in 1:nrow(dafemaNeighbourFema)){
    dafemaNeighbourFlowIncoming <- sum(subset(dafemaData$sum, 
                                              dafemaData$residence %in% 
                                                unlist(dafemaNeighbourFema$femaMSOAMembers[i])
                                              & dafemaData$workplace %in% 
                                                unlist(femaToMerge$femaMSOAMembers)
    ))
    dafemaNeighbourFlowOutgoing <- sum(subset(dafemaData$sum, 
                                              dafemaData$workplace %in% 
                                                unlist(dafemaNeighbourFema$femaMSOAMembers[i])
                                              & dafemaData$residence %in% 
                                                unlist(femaToMerge$femaMSOAMembers)
    ))      
    if (dafemaNeighbourFlowIncoming>=dafemaNeighbourFlowOutgoing){
      dafemaNeighbourFema[i,5] <- dafemaNeighbourFlowIncoming
    }
    else {
      dafemaNeighbourFema[i,5] <- dafemaNeighbourFlowOutgoing
    }
  }
  
  #sorts list of neighbour FEMA by the maximun number of 
  #person flow incoming or outgoing by descending order
  
  dafemaNeighbourFema <- dafemaNeighbourFema[
    order(dafemaNeighbourFema$V5, decreasing = TRUE),]
  return(dafemaNeighbourFema)
}
################################################################################
#produces result table
dafemaResults <- function(){
  if (exists("dafemaData40")){
    dafemaFemaResults <- dafemaData40
  }
  if (exists("dafemaData50")){
    for (i in 1:nrow(dafemaData50)){ 
      dafemaTableLastNumber <- nrow(dafemaFemaResults)+1
      dafemaFemaResults[dafemaTableLastNumber,1] <- dafemaData50[i,1]
      dafemaFemaResults[dafemaTableLastNumber,2] <- dafemaData50[i,2]
      dafemaFemaResults[dafemaTableLastNumber,3][[1]] <- dafemaData50[i,3]
      dafemaFemaResults[dafemaTableLastNumber,4] <- dafemaData50[i,4]
    }
  }
  if (exists("dafemaData60")){
    for (i in 1:nrow(dafemaData60)){ 
      dafemaTableLastNumber <- nrow(dafemaFemaResults)+1
      dafemaFemaResults[dafemaTableLastNumber,1] <- dafemaData60[i,1]
      dafemaFemaResults[dafemaTableLastNumber,2] <- dafemaData60[i,2]
      dafemaFemaResults[dafemaTableLastNumber,3][[1]] <- dafemaData60[i,3]
      dafemaFemaResults[dafemaTableLastNumber,4] <- dafemaData60[i,4]
    }
  }
  if (exists("dafemaData70")){
    for (i in 1:nrow(dafemaData70)){ 
      
      dafemaTableLastNumber <- nrow(dafemaFemaResults)+1
      dafemaFemaResults[dafemaTableLastNumber,1] <- dafemaData70[i,1]
      dafemaFemaResults[dafemaTableLastNumber,2] <- dafemaData70[i,2]
      dafemaFemaResults[dafemaTableLastNumber,3][[1]] <- dafemaData70[i,3]
      dafemaFemaResults[dafemaTableLastNumber,4] <- dafemaData70[i,4]
    }
  }
  if (exists("dafemaData80")){
    for (i in 1:nrow(dafemaData80)){ 
      dafemaTableLastNumber <- nrow(dafemaFemaResults)+1
      dafemaFemaResults[dafemaTableLastNumber,1] <- dafemaData80[i,1]
      dafemaFemaResults[dafemaTableLastNumber,2] <- dafemaData80[i,2]
      dafemaFemaResults[dafemaTableLastNumber,3][[1]] <- dafemaData80[i,3]
      dafemaFemaResults[dafemaTableLastNumber,4] <- dafemaData80[i,4]
    }
  }
  if (exists("dafemaData90")){
    for (i in 1:nrow(dafemaData90)){ 
      dafemaTableLastNumber <- nrow(dafemaFemaResults)+1
      dafemaFemaResults[dafemaTableLastNumber,1] <- dafemaData90[i,1]
      dafemaFemaResults[dafemaTableLastNumber,2] <- dafemaData90[i,2]
      dafemaFemaResults[dafemaTableLastNumber,3][[1]] <- dafemaData90[i,3]
      dafemaFemaResults[dafemaTableLastNumber,4] <- dafemaData90[i,4]
    }
  }
  assign("dafemaFemaResults",dafemaFemaResults, envir = .GlobalEnv)
}
################################################################################
#it produces a table for the final maps
dafemaMapData <- function(){
  dafemaFemaMapTable <- data.frame(as.character(NULL),
                                   as.numeric(NULL),
                                   as.numeric(NULL),
                                   as.character(NULL),
                                   stringsAsFactors=FALSE)
  colnames(dafemaFemaMapTable) <- c("msoa",
                                    "closure target",
                                    "fema id",
                                    "colour")
  if (exists("dafemaData40")){
    for (i in 1:nrow(dafemaData40)){
      msoaList <- unlist(dafemaData40$femaMSOAMembers[i])
      femaId <- dafemaData40$femaId[i]
      for (t in 1:length(msoaList)){
        newRow <- nrow(dafemaFemaMapTable)+1
        dafemaFemaMapTable[newRow,1] <- msoaList[t]
        dafemaFemaMapTable[newRow,2] <- .40
        dafemaFemaMapTable[newRow,3] <- femaId
      }
    }
  }
  if (exists("dafemaData50")){
    for (i in 1:nrow(dafemaData50)){
      msoaList <- unlist(dafemaData50$femaMSOAMembers[i])
      femaId <- dafemaData50$femaId[i]
      for (t in 1:length(msoaList)){
        newRow <- nrow(dafemaFemaMapTable)+1
        dafemaFemaMapTable[newRow,1] <- msoaList[t]
        dafemaFemaMapTable[newRow,2] <- .50
        dafemaFemaMapTable[newRow,3] <- femaId
      }
    }
  }
  if (exists("dafemaData60")){
    for (i in 1:nrow(dafemaData60)){
      msoaList <- unlist(dafemaData60$femaMSOAMembers[i])
      femaId <- dafemaData60$femaId[i]
      for (t in 1:length(msoaList)){
        newRow <- nrow(dafemaFemaMapTable)+1
        dafemaFemaMapTable[newRow,1] <- msoaList[t]
        dafemaFemaMapTable[newRow,2] <- .60
        dafemaFemaMapTable[newRow,3] <- femaId
      }
    }
  }
  if (exists("dafemaData70")){
    for (i in 1:nrow(dafemaData70)){
      msoaList <- unlist(dafemaData70$femaMSOAMembers[i])
      femaId <- dafemaData70$femaId[i]
      for (t in 1:length(msoaList)){
        newRow <- nrow(dafemaFemaMapTable)+1
        dafemaFemaMapTable[newRow,1] <- msoaList[t]
        dafemaFemaMapTable[newRow,2] <- .70
        dafemaFemaMapTable[newRow,3] <- femaId
      }
    }
  }
  if (exists("dafemaData80")){
    for (i in 1:nrow(dafemaData80)){
      msoaList <- unlist(dafemaData80$femaMSOAMembers[i])
      femaId <- dafemaData80$femaId[i]
      for (t in 1:length(msoaList)){
        newRow <- nrow(dafemaFemaMapTable)+1
        dafemaFemaMapTable[newRow,1] <- msoaList[t]
        dafemaFemaMapTable[newRow,2] <- .80
        dafemaFemaMapTable[newRow,3] <- femaId
      }
    }
  }
  if (exists("dafemaData90")){
    for (i in 1:nrow(dafemaData90)){
      msoaList <- unlist(dafemaData90$femaMSOAMembers[i])
      femaId <- dafemaData90$femaId[i]
      for (t in 1:length(msoaList)){
        newRow <- nrow(dafemaFemaMapTable)+1
        dafemaFemaMapTable[newRow,1] <- msoaList[t]
        dafemaFemaMapTable[newRow,2] <- .90
        dafemaFemaMapTable[newRow,3] <- femaId
      }
    }
  }
  if (exists("dafemaData100")){
    for (i in 1:nrow(dafemaData100)){
      msoaList <- unlist(dafemaData100$femaMSOAMembers[i])
      femaId <- dafemaData100$femaId[i]
      for (t in 1:length(msoaList)){
        newRow <- nrow(dafemaFemaMapTable)+1
        dafemaFemaMapTable[newRow,1] <- msoaList[t]
        dafemaFemaMapTable[newRow,2] <- 1.0
        dafemaFemaMapTable[newRow,3] <- femaId
      }
    }
  }
  cols <- brewer.pal(12,"Set3")
  uniqueFema <- unique(dafemaFemaMapTable$`fema id`)
  colourFema <- suppressWarnings(cbind(uniqueFema,cols))
  for (i in 1:nrow(dafemaFemaMapTable)){
    dafemaFemaMapTable$colour[i] <- subset(colourFema[,2], 
                                           dafemaFemaMapTable$`fema id`[i]==
                                           colourFema[,1])
  }
  assign("dafemaFemaMapTable",dafemaFemaMapTable, envir = .GlobalEnv)
}
################################################################################
#it shows a map for every closure target
dafemaShowMap <- function(closureTarget){
  #merge data with closure target
  table <- subset(dafemaFemaMapTable,
                  dafemaFemaMapTable$`closure target`==closureTarget)
  dafemaMap <- merge(dafemaMap, 
                     table,
                     by.x = "msoa11cd",
                     by.y = "msoa")
  plot(dafemaMap, col=dafemaMap$colour, axes=TRUE)
  title <- paste("FEMA for region ", dafemaRegion, " and closure ",closureTarget)
  title(main = title)
  uniqueFema <- unique(dafemaFemaMapTable$`fema id`)
  
  colourTable <- unique(table[c("fema id","colour")])
  # legend(legend = colourTable[,1], 
  #        fill =colourTable[,2] , 
  #        "right",
  #        title = "FEMA ID",
  #        cex = 0.65)
}
################################################################################
#produces a table for next interaction
dafemaFemaNext <- function(closurevalue) {
  if (closurevalue==.40) {
    dafemaData50 <- dafemaData40
    dafemaData50$closureTarget <- .50
    assign("dafemaData50",dafemaData50, envir = .GlobalEnv)
  }
  if (closurevalue==.50) {
    dafemaData60 <- dafemaData50
    dafemaData60$closureTarget <- .60
    assign("dafemaData60",dafemaData60, envir = .GlobalEnv)
  }
  if (closurevalue==.60) {
    dafemaData70 <- dafemaData60
    dafemaData70$closureTarget <- .70
    assign("dafemaData70",dafemaData70, envir = .GlobalEnv)
  }
  if (closurevalue==.70) {
    dafemaData80 <- dafemaData70
    dafemaData80$closureTarget <- .80
    assign("dafemaData80",dafemaData80, envir = .GlobalEnv)
  }
  if (closurevalue==.80) {
    dafemaData90 <- dafemaData80
    dafemaData90$closureTarget <- .90
    assign("dafemaData90",dafemaData90, envir = .GlobalEnv)
  }
  if (closurevalue==.90) {
    dafemaData100 <- dafemaData90
    dafemaData100$closureTarget <- 1.0
    assign("dafemaData100",dafemaData100, envir = .GlobalEnv)
  }
}
################################################################################

#rm(list=ls())
#dafemaStart("north east")
#dafemaStart("East Midlands")
#dafemaStart("Yorkshire and The Humber")
#dafemaStart("South West")
#dafemaStart("West Midlands")
#dafemaStart("East of England")
#dafemaStart("North West")
#dafemaStart("London")
#dafemaStart("South East")
#View(dafemaFemaResults)
#dafemaShowMap(0.4)
#dafemaShowMap(0.5)
#dafemaShowMap(0.6)
#dafemaShowMap(0.7)
#dafemaShowMap(0.8)
#dafemaShowMap(0.9)