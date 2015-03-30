rankall <- function(outcome, num = "best") 
        {
        ## Read outcome data
        data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        
        signal<-0
        
        if ("heart attack"==outcome)
                signal<-11
        else if ("heart failure"==outcome)
                signal<-17
        else if ("pneumonia"==outcome)
                signal<-23
        if (signal==0)
                stop("invalid outcome")
        
        data<-data[,c(2,7,signal)] # take the wanted data.
     #   state<-levels(data[,2])
      #  print(state)
     colnames(data) <- c("hospital","state","rate")
     data[,3]<-as.numeric(data[,3])
     
     data<-data[!is.na(data[,3]),] # get the cleared data without NA
     data<-split(data,data$state)
   #  leng<-length(data$state)
 #   data<-lapply(split(data,data$state),function(x) min(x[,3]))
       #print(seq_along(data))
        data<- lapply(data, function(x) x[order(x$rate,x$hospital),]) 
      # lapply(data, function(ele) ele) 
        if (num=="best")       
               { data<- lapply(data, function(x) x[1,c(1,2)]) 
          
                result<-do.call(rbind,data)}
         else if (num == "worst")
               { data<- lapply(data, function(x) x[nrow(x),c(1,2)]) 
                          
                         result<-do.call(rbind,data)
                        }
 else { 
 { data<- lapply(data, function(x) x[num,c(1,2)]) 
   
   result<-do.call(rbind,data)
 }
 
 result[,2]<-row.names(result)
 
 print(result)
 
 }
                
               
 

 #for (name in attributes(data))
 #       {
 #        print(name)
  #       tempe<-data$name
   #      #print(class(name))   
  #       print(tempe)  }
 #tempe<-data$AZ
    #  print(tempe[order(tempe$rate,tempe$hospital),])
    #  tempe<-tempe[order(tempe$rate,tempe$hospital),]
  #    print(rbind(tempe[1,],tempe[2,]))
     #print(order(data$AZ$rate,data$AZ$hopsital))
           
}
#     lprint(state)
     #   colnames(data) <- c("hospital","state","rate")
      #  data[,3]<-as.numeric(data[,3])
       
      #    data<-data[!is.na(data[,3]),] # get the cleared data without NA
     #   g<-data$state
       # if (num=="best")
               # data <- unsplit(lapply(split(data,data$state),function(x) min(x)),data$state)
         # index <- unsplit(tapply(split(data,data$state),function(x) order(x$rate)),data$state)
              #       data1<- data[index==1,]
            #                data1
     #   else if (num=="worst")
                           
                           
                         
      #  else if (num=="worst")
      #   index <- data$state(lapply(split(data,data$state),function(x) x[order(x$rate,x$hospital)==ncol(x),]),data$state)
     #   else 
            #    {
           #     result <- lapply(split(data,g),function(x) x[order(x$rate,x$hospital)==num,])
             
       # }
      #  data
      # unsplit(data,data$state)
        
#      result1<-data.frame(hospital <- attributes(result),state <- attributes(result))
      # colnames(result1) <- c("hospital","state")
      # print(result1$hospital[1])
      # result1$hospital[1]<-"fadsf"
       #print(result1$hospital[1])
     #  result1
       # for (letter in result){
                
              # result1$state<-letter
               # print(letter$state)
          #      print(result1[result1[,2]==letter$state,2])
                #print(result1[result1[,2]==letter$state,2])
               # print("sss")
              #  if (length(letter)==0)
              #      { result1[result1[,2]==letter$state,2] <-"<NA>"
               # print(letter)
               
             #  print("sss")
             #   }
            #  print(letter$hospital)
     #          else 
        #   result1[result1[,2]==letter$state,2] <-letter$hospital
       #        else
     #                   result1[1,letter]=result$letter[1,1]
       #}
     #result1
         #       result1
                
       # data<-unsplit(lapply(split(data,data$state),function(x) x[order(x$rate,x$hospital)==num,]), data$state)
       # data[index,c(1,2)]
      #result<-lapply(split(data,data$state),getindex(x,num))
    #  data
     #   data(index,f)
      
      
       #result2<-unsplit(lapply(split(data,data$state),function(x) x[order(x$rate,x$hospital)==nrow(x),]),data$state)
    # result2<-unsplit(result2,)
      # result2
        
        
        



## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name