
#Project 3 Data Frame Matthew Sullivan

#Create Empty Final Data Frame
finaldf=data.frame("season"=numeric(),"ID"=numeric(),"teams"=factor(),"wins"=numeric(),"losses"=numeric(),"opponents"=numeric())

#Create Empty Data Frame of Cumulative Raw Data
finalseason.v=data.frame("season"=integer(),"awayteam"=factor(),"awayscore"=integer(),"hometeam"=factor(),"homescore"=integer(),"wins"=numeric(),"losses"=numeric(),"games"=numeric())

#Define Vector of Widths of Online file
width.v=c(11,28,2,2,28,2)

start.year=1960
end.year=2010

#Create List of Data Frames
season.vtest=as.list(1:(end.year-start.year+1))

temp=as.list(1:(end.year-start.year+1))

season.v=(start.year:end.year)


#Start of Loop
###########################################################################
###########################################################################
###########################################################################

for(i in 1:length(season.v)){
  season=season.v[i]
  
  #Read in Raw Data Frame for given year
  read.fwf(paste("http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf",season.v[i],"gms.txt",sep=""),width.v,header=F)
  #Define raw data frame  for given year
  season.vtest[[i]]=(read.fwf(paste("http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf",season.v[i],"gms.txt",sep=""),width.v,header=F))
  season.vtest
  season.vtest[[i]]
  
  season.vtest[[i]]$V2
  print(season.vtest[[i]])
  season.vtest[[i]]$V2=gsub(" ","",season.vtest[[i]]$V2)
  season.vtest[[i]]$V2=gsub("-","",season.vtest[[i]]$V2)
  season.vtest[[i]]$V2=gsub("\\(","",season.vtest[[i]]$V2)
  season.vtest[[i]]$V2=gsub("\\)","",season.vtest[[i]]$V2)
  
  season.vtest[[i]]$V5=gsub(" ","",season.vtest[[i]]$V5)
  season.vtest[[i]]$V5=gsub("-","",season.vtest[[i]]$V5)
  season.vtest[[i]]$V5=gsub("\\(","",season.vtest[[i]]$V5)
  season.vtest[[i]]$V5=gsub("\\)","",season.vtest[[i]]$V5)
  
  
  #Clean Data Frame
  season.vtest[[i]]=data.frame("season"=season.v[i],"awayteam"=gsub(" ","",season.vtest[[i]]$V2),"awayscore"=season.vtest[[i]]$V3,"hometeam"=gsub(" ","",season.vtest[[i]]$V5),"homescore"=season.vtest[[i]]$V6)
  
  
  
  
  ###########################################################################
  #Define a Flipped Data Frame
  data.frame("season"=season.v[i],"awayteam"=season.vtest[[i]]$hometeam,"awayscore"=season.vtest[[i]]$homescore,"hometeam"=season.vtest[[i]]$awayteam,"homescore"=season.vtest[[i]]$awayscore)
  #Row Bind the Data Frame with the Flipped Data Frame
  season.vtest[[i]]=rbind(season.vtest[[i]],data.frame("season"=season.v[i],"awayteam"=season.vtest[[i]]$hometeam,"awayscore"=season.vtest[[i]]$homescore,"hometeam"=season.vtest[[i]]$awayteam,"homescore"=season.vtest[[i]]$awayscore))
  season.vtest[[i]]
  
  
  #Define Binary Wins Column
  ifelse(season.vtest[[i]]$awayscore>season.vtest[[i]]$homescore,1,0)
  season.vtest[[i]]$wins=ifelse(season.vtest[[i]]$awayscore>season.vtest[[i]]$homescore,1,0)
  #Define Binary Losses Column
  ifelse(season.vtest[[i]]$awayscore<season.vtest[[i]]$homescore,1,0)
  season.vtest[[i]]$losses=ifelse(season.vtest[[i]]$awayscore<season.vtest[[i]]$homescore,1,0)
  #Define Total Games Column
  season.vtest[[i]]$games=(season.vtest[[i]]$wins) + (season.vtest[[i]]$losses)
  season.vtest[[i]][1,2]
  season.vtest[[i]]
  
  ##Drop 1-AA Teams
  #Drop from First Half
  season.vtest[[i]]=season.vtest[[i]][-(which(aggregate(season.vtest[[i]]$games,by=list(season.vtest[[i]]$awayteam),FUN=sum)[season.vtest[[i]][,2],2]<6)),]
  #Drop from Second Half
  season.vtest[[i]]=season.vtest[[i]][-(which(aggregate(season.vtest[[i]]$games,by=list(season.vtest[[i]]$hometeam),FUN=sum)[season.vtest[[i]][,4],2]<6)),]
  #Check on Number of Rows
  nrow(season.vtest[[i]])
  season.vtest[[i]]$awayteam=factor(season.vtest[[i]]$awayteam)
  season.vtest[[i]]$hometeam=factor(season.vtest[[i]]$hometeam)
  
  ##################################################################################
  ##################################################################################
  ##################################################################################
  ###Dealing with Ties
  ##Find Ties
  
  #Indices of Ties
  which(season.vtest[[i]]$awayscore==season.vtest[[i]]$homescore)
  #How Many Ties
  length(which(season.vtest[[i]]$awayscore==season.vtest[[i]]$homescore))
  ##Remove Ties
  #Length Before Ties
  nrow(season.vtest[[i]])
  season.vtest[[i]]=season.vtest[[i]][(season.vtest[[i]]$awayscore!=season.vtest[[i]]$homescore),]
  #Length After Ties
  nrow(season.vtest[[i]])
  season.vtest[[i]]$awayteam=factor(season.vtest[[i]]$awayteam)
  season.vtest[[i]]$hometeam=factor(season.vtest[[i]]$hometeam)
  
  ######Map to New Data Frame
  #Start Data Frame with Team ID, Teams, Wins and losses
  data.frame("season"=season.v[i],"ID"=1:length(unique(season.vtest[[i]]$awayteam)),"teams"=aggregate(season.vtest[[i]]$wins,by=list(season.vtest[[i]]$awayteam),FUN=sum)[,1],"wins"=aggregate(season.vtest[[i]]$wins,by=list(season.vtest[[i]]$awayteam),FUN=sum)[,2],"losses"=aggregate(season.vtest[[i]]$losses,by=list(season.vtest[[i]]$awayteam),FUN=sum)[,2])
  #Create temporary data frame to be binded to final data frame of all seasons
  #The intuition here is to keep the raw data frame in order to vectorize opponents with a loop
  ##until I can create a way to do it sans looping
  temp[[i]]=data.frame("season"=season.v[i],"ID"=1:length(unique(season.vtest[[i]]$awayteam)),"teams"=aggregate(season.vtest[[i]]$wins,by=list(season.vtest[[i]]$awayteam),FUN=sum)[,1],"wins"=aggregate(season.vtest[[i]]$wins,by=list(season.vtest[[i]]$awayteam),FUN=sum)[,2],"losses"=aggregate(season.vtest[[i]]$losses,by=list(season.vtest[[i]]$awayteam),FUN=sum)[,2])
  
  #########################################################Map to new Data frame in Opponents Loop?
  #####################Vector of Opponents For Loop
  temp[[i]]$opponents=as.list(0)
  rownames(temp[[i]])=levels(temp[[i]]$teams)
  
  
  #Row Bind Data Frames Down Here
  finaldf=rbind(finaldf,temp[[i]][,-2])
  finalseason.v=rbind(finalseason.v,season.vtest[[i]])
  
}
finaldf

####################################Opponents Loop##################################################

##################################################################################################
##################################################################################################
##################################################################################################

for(j in 1:nrow(finaldf)){
  n=finaldf$wins[j]+finaldf$losses[j]
  finaldf$opponents[[j]][1:n]=temp[[which(season.v==finaldf[j,1])]][as.character(subset(finalseason.v,finalseason.v[,1]==finaldf[j,1])$hometeam[which(subset(finalseason.v,finalseason.v[,1]==finaldf[j,1])$awayteam==finaldf[j,2])][1:n]),2]
  finaldf$opponents[[j]]=as.numeric(finaldf$opponents[[j]])
}

##################################################################################################
##################################################################################################
##################################################################################################


save(finaldf,file="SullivanCorrected.rdata")
