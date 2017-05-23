#Project 3 Colley Function           Matthew Sullivan


load("SullivanCorrected.Rdata")
ls()
str(finaldf)


myenv=new.env()
start.year=1960
end.year=2010

season.v=(start.year:end.year)

for(i in 1:length(season.v)){
  myenv$dfa[[i]]=data.frame(subset(finaldf,finaldf[,1]==season.v[i]))
}

#Colley: function that takes year as an input and returns sorted data frame of rankings
#season: input of season year for which you want rankings e.g. 2010

Colley=function(season){
  
  #Create a Matrix with no data that spans the number of teams in the NCAA
  ColleyMatrix <- matrix(0,nrow=nrow(myenv$dfa[[season-1959]]), ncol=nrow(myenv$dfa[[season-1959]]))
  
  
  #For each row and for each column, assign values based on position: product of two indices
  
  for(i in 1:nrow(myenv$dfa[[season-1959]])){
    for(j in myenv$dfa[[season-1959]]$opponents[[i]]){
      ColleyMatrix[i,j] = ColleyMatrix[i,j] -1
      
    }
  }
  #Putting total number of games plus 2 on the diagonals
  diag(ColleyMatrix) = myenv$dfa[[season-1959]]$wins + myenv$dfa[[season-1959]]$losses +2
  
  
  #Create a vector of win/loss differential divided by 2 plus 1
  WinLossVector <- 1 + (myenv$dfa[[season-1959]]$wins - myenv$dfa[[season-1959]]$losses)/2       
  
  #Solve the system of equations
  Ranking <-solve(ColleyMatrix,WinLossVector)
  
  #Return unordered data frame
  RawSolution <- data.frame("Team Name"=myenv$dfa[[season-1959]]$teams,"Ranking"=Ranking)
  
  #Return data frame sorted by Ranking
  Solution <- RawSolution[order(RawSolution$Ranking,decreasing=TRUE),]
  return(Solution)
}

Colley(2010)

