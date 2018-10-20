record <- read.csv("TeamRecord.csv",header = TRUE)
record

played <- list(record$Matches)   #list of number of matches played by each player
played

won <- list(record$Won)          #list of number of matches won by each player
won

lost <- list(record$Lost)        #list of number of matches lost by each player
lost

fitness <- list()                #list of fitness value of each player
for(i in 1:30){
  a<- lost[[1]][i]
  b<- played[[1]][i]
  c<- won[[1]][i]
  fitness[[i]] <- ((1-(a/b))+c)/(1+c)  #------FITNESS FUNCTION------
}
fitness

plot(1:30,unlist(fitness),type = 'l')   #line plot of fitness value v/s player ID

population <- list()      #list which holds the initial population of 10 chromosomes           
for(i in seq(1,10,by=1)){
  population[[i]] <- list(c(sample.int(9,7,replace = F),sample.int(2,1,replace = F)+9,sample.int(6,2,replace = F)+11,sample.int(13,5,replace = F)+17))
}
population

mostFitted <- 0
for(i in 1:10){     #this loop indicates the number of generations/iteration in order to get best fitted chromosome
  mfc = -1
  lfc = -1
  max = 0
  min = 100
  for(j in 1:10){
    summ = 0
    for(k in 1:15){
      x <-population[[j]][[1]][[k]]
      summ = summ + fitness[[x]][1]
      #print(x)
    }
    if(summ>max){
      max = summ
      mfc = j
    }
    if(summ<min){
      min = summ
      lfc = j
    }
  }
  # ---------CROSSOVER-------
  for(l in 1:15){
    x <-population[[mfc]][[1]][[l]]
    y <-population[[lfc]][[1]][[l]]
    if(fitness[[x]][1] > fitness[[y]][1] & is.element(x,population[[lfc]][[1]])==FALSE){
      population[[lfc]][[1]][[l]] <- x
    }
  }
  mostFitted <- mfc
  print(mfc)
}
population[[mostFitted]][1]

record[population[[mostFitted]][[1]],c(2,6)]



       