best <- function(state,outcome) {
        data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        if (any(data[,7]==state)==FALSE)
            stop("invalid state")
        signal<-0
        if ("heart attack"==outcome)
                signal<-11
        else if ("heart failure"==outcome)
                signal<-17
        else if ("pneumonia"==outcome)
                signal<-23
        if (signal==0)
                stop("invalid outcome")
        data1<-data[data[,7]==state,] # find the useful state
        data1[,signal]<-as.numeric(data1[,signal])
        bestvalue<-min(data1[,signal],na.rm=TRUE)# find the useful outcome's min value 
      
        data2<-data1[data1[,signal] == bestvalue,]  
        data2<-data2[!is.na(data2[,signal]),]
 min(data2[,2])
}
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
## rate