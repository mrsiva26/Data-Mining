Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

dataframe_to_matrix<-function(source_dataframe)
{
  #Dropping education column and adding Transid instead
  #So ncol remains the same but name but colnames differ
  target_matrix = matrix(0,nrow=nrow(source_dataframe),ncol=(ncol(source_dataframe)))
  names = c("transid","age","workclass","fnlwgt","education_cat","marital_status","occupation","relationship","race","gender","capital_gain","captial_loss","hour_per_week","native_country","class")
  colnames(target_matrix) = names
  
  transid = rownames(source_dataframe)
  transid<-as.numeric(transid)
  target_matrix[,1] = cbind(transid)
  rm(transid)
  
  age = as.integer(factor(source_dataframe$age))
  target_matrix[,2]<-cbind(age)
  rm(age)
  
  workclass = as.integer(factor(source_dataframe$workclass))
  target_matrix[,3]<-cbind(workclass)
  rm(workclass)
  
  target_matrix[,4]<-cbind(source_dataframe$fnlwgt)
  
  target_matrix[,5]<-cbind(source_dataframe$education_cat)
  
  marital_status = as.integer(factor(source_dataframe$marital_status))
  target_matrix[,6]<-cbind(marital_status)
  rm(marital_status)
  
  occupation = as.integer(factor(source_dataframe$occupation))
  target_matrix[,7]<-cbind(occupation)
  rm(occupation)
  
  relationship = as.integer(factor(source_dataframe$relationship))
  target_matrix[,8]<-cbind(relationship)
  rm(relationship)
  
  race = as.integer(factor(source_dataframe$race))
  target_matrix[,9]<-cbind(race)
  rm(race)
  
  gender=as.integer(factor(source_dataframe$gender))
  target_matrix[,10]<-cbind(gender)
  rm(gender)
  
  target_matrix[,11]<-cbind(source_dataframe$capital_gain)
  target_matrix[,12]<-cbind(source_dataframe$capital_loss)
  
  target_matrix[,13]<-cbind(source_dataframe$hour_per_week)
  
  native_country = as.integer(factor(source_dataframe$native_country))
  target_matrix[,14]<-cbind(native_country)
  rm(native_country)
  
  cl = as.integer(factor(source_dataframe$class))
  target_matrix[,15]<-cbind(cl)
  rm(cl)
  
  return(target_matrix)
}

normalize = function(column) {
  column = as.matrix(column)
  meanval = as.numeric(mean(column))
  sdval = as.numeric(sd(column))
  minval = meanval - 2*sdval
  maxval = meanval + 2*sdval
  if(minval< min(column))minval = min(column)
  if(maxval> max(column))maxval = max(column)
  rangeval = maxval - minval
  #cat(sprintf("MeanVal: %f\nSDVal: %f\nActual MinVal: %f\nActual MaxVal: %f\nNew MinVal: %f\nNew MaxVal: %f", meanval, sdval, min(column),max(column), minval, maxval))
  
  for(i in 1:nrow(column))
  {
    if(column[i] > maxval){
      column[i] = 1
    }else if (column[i] < minval){
      column[i] = 0
    }else{
      column[i] = (column[i] - minval)/rangeval
    }
  }
  return(column)
}

preprocess = function(source_matrix)
{
  #Changing all NAs with Mode of that column
  for(i in 1:nrow(source_matrix))
  {
    #Changing all NAs with Mode of that column
    for( j in 2:(ncol(source_matrix)-1) )
    {
      if(is.na(source_matrix[i,j]) || source_matrix[i,j]==" ?")
        source_matrix[i,j] = Mode(source_matrix[,j])
    }
  }
  
  source_matrix[,2] = normalize(source_matrix[,2])
  source_matrix[,4] = normalize(source_matrix[,4])
  source_matrix[,5] = normalize(source_matrix[,5])
  source_matrix[,11] = normalize(source_matrix[,11])
  source_matrix[,12] = normalize(source_matrix[,12])
  source_matrix[,13] = normalize(source_matrix[,13])
  
  return (source_matrix)
}

implementincomeknn =function(testset,trainingset,no_of_neighbors)
{
  cat(sprintf("\n\n--------Implementing knn for k = %d-----------",no_of_neighbors))
  #declare a Matrix for storing results
  resultset = matrix(0, nrow=nrow(testset), ncol=4)
  #declare a distance dataframe
  distancematrix = matrix(0, nrow=nrow(trainingset), ncol=3)
  
  row12=matrix(0,nrow=2, ncol=15) 
  
  #outer loop for iterating through no. of rows
  for( i in 1:nrow(testset))
  {
    row12[1,] = testset[i,]
    
    #inner loop for comparing i-th row with all other rows
    for(j in 1:nrow(trainingset) )
    {
      dist=0
      row12[2,] = trainingset[j,]
      
      #Calculate Euclidean Distance
      if(row12[1,3] == row12[2,3]) workclassdiff = 0
      else workclassdiff = 1
      if(row12[1,15] == row12[2,15]) marital_statusdiff = 0
      else marital_statusdiff = 1
      if(row12[1,7] == row12[2,7]) occupationdiff = 0
      else occupationdiff = 1
      if(row12[1,8] == row12[2,8]) relationshipdiff = 0
      else relationshipdiff = 1
      if(row12[1,9] == row12[2,9]) racediff = 0
      else racediff = 1
      if(row12[1,10] == row12[2,10]) genderdiff = 0
      else genderdiff = 1
      if(row12[1,14] == row12[2,14]) native_countrydiff = 0
      else native_countrydiff = 1
      
      dist = 
        workclassdiff^2 + 
        occupationdiff +
        relationshipdiff +
        racediff +
        genderdiff +
        native_countrydiff +
        (row12[1,2]-row12[2,2])^2 + 
        (row12[1,5]-row12[2,5])^2 + 
        (row12[1,11]-row12[2,11])^2 +
        (row12[1,12]-row12[2,12])^2 +
        (row12[1,13]-row12[2,13])^2 
      dist=sqrt(dist)
      dist=as.numeric(dist)
      
      #assign the distance to distanmatrix
      distancematrix[as.numeric(j),1]=as.numeric(j)
      distancematrix[as.numeric(j),2]=dist
      distancematrix[as.numeric(j),3]=trainingset[j,15]
      #take next training row and compare it with i-th testing row
    }
    
    #Sort the Distance Matrix
    distancematrix <- distancematrix[order(distancematrix[,2]),];
    #print(distancematrix)
    
    poscount=negcount=0
    posid=negid=0
    #Choose k neighbors
    for(k in 1:no_of_neighbors)
    {
      if(distancematrix[k,3]==1)      
      {poscount = poscount+1; posid=distancematrix[k,1]}
      else
      {negcount = negcount+1; negid=distancematrix[k,1]}
    }
    
    resultset[i,1] = i #transid
    resultset[i,2] = testset[i,15] #actual class
    #cat(sprintf("\nNeighbors for row - %d: ",i))
    if(poscount>negcount)
    {
      #cat(sprintf("Class Positive count %d", poscount))
      resultset[i,3] = trainingset[posid,15]  
      resultset[i,4] = poscount/k
    }
    else if(negcount>poscount)
    {
      #cat(sprintf("Class Negative count %d", negcount))
      resultset[i,3] = trainingset[negid,15]  
      resultset[i,4] = negcount/k
    }
    else
    {
      #cat(sprintf("Class Positive,Negative count %d,%d", poscount,negcount))
      resultset[i,3] = distancematrix[1,3]  
      resultset[i,4] = 0.5
    }
    
    #goto next row in the table and repeat
  }
  return (resultset)
}


#############################################################################


#Read income.csv as a dataframe
income_new_dataframe = read.csv("income_NEW.csv", TRUE)
colnames(income_new_dataframe) = c("age","workclass","fnlwgt","education","education_cat","marital_status","occupation","relationship","race","gender","capital_gain","capital_loss","hour_per_week","native_country","class")
#Convert dataframe into matrix
income_new = dataframe_to_matrix(income_new_dataframe)
#Remove NA and normalize
income_new  = preprocess(income_new)

#Read income.csv as a dataframe
income_train_dataframe = read.csv("income_TRAIN_FINAL.csv", TRUE)
colnames(income_train_dataframe) = c("age","workclass","fnlwgt","education","education_cat","marital_status","occupation","relationship","race","gender","capital_gain","capital_loss","hour_per_week","native_country","class")
#Convert dataframe into matrix
incometraining = dataframe_to_matrix(income_train_dataframe)
#Remove NA and normalize
incometraining  = preprocess(incometraining)

#Read income.csv as a dataframe
income_test_dataframe = read.csv("income_TEST_FINAL.csv", TRUE)
colnames(income_test_dataframe) = c("age","workclass","fnlwgt","education","education_cat","marital_status","occupation","relationship","race","gender","capital_gain","capital_loss","hour_per_week","native_country","class")
#Convert dataframe into matrix
incometesting = dataframe_to_matrix(income_test_dataframe)
#Remove NA and normalize
incometesting  = preprocess(incometesting)

#declare a Matrix for storing results
incomeresult = matrix(0, nrow=nrow(incometesting), ncol=4)
colnames(incomeresult) = c("transID","actual","predicted", "prob")
#declare confusion matrices
confusionmatrix = matrix(0,ncol=2,nrow=2)
rownames(confusionmatrix) = c("Postitive <=50k","Negative >50k")
colnames(confusionmatrix) = c("Postitive <=50k","Negative >50k")
confusionmatrix2 = matrix(0,ncol=2,nrow=2)
rownames(confusionmatrix2) = c("Postitive <=50k","Negative >50k")
colnames(confusionmatrix2) = c("Postitive <=50k","Negative >50k")

#declare tpr,tnr,fpr,fnr arrays
tprarray = matrix(0,nrow = 15, ncol = 2)
tnrarray = matrix(0,nrow = 15, ncol = 2)
fprarray = matrix(0,nrow = 15, ncol = 2)
fnrarray = matrix(0,nrow = 15, ncol = 2)

for(num in 1:15)
{
  incomeresult=implementincomeknn(incometesting,incometraining,num)
  colnames(incomeresult) = c("transID","actual","predicted", "prob")
  
  tpr=tnr=fpr=fnr=0
  for(i in 1:nrow(incomeresult))
  {
    if(incomeresult[i,2] == 1 && incomeresult[i,3] == 1)
      tpr= tpr+1
    else if(incomeresult[i,2] == 2 && incomeresult[i,3] == 2)
      tnr= tnr+1
    else if(incomeresult[i,2] == 1 && incomeresult[i,3] == 2)
      fnr= fnr+1
    else if(incomeresult[i,2] == 2 && incomeresult[i,3] == 1)
      fpr= fpr+1
  }
  confusionmatrix[1,1]=tpr
  confusionmatrix[1,2]=fnr
  confusionmatrix[2,1]=fpr
  confusionmatrix[2,2]=tnr
  cat(sprintf("\nConfusion Matrix\n"))
  print(confusionmatrix)
  #cat(sprintf("\n\nTpr :%f\nTnr :%f\nFpr :%f\nFnr :%f", tpr,tnr,fpr,fnr))
  
  confusionmatrix2[1,1]=tpr/(tpr+fnr)
  confusionmatrix2[1,2]=fnr/(tpr+fnr)
  confusionmatrix2[2,1]=fpr/(fpr+tnr)
  confusionmatrix2[2,2]=tnr/(tnr+fpr)
  cat(sprintf("\nTPR, FNR,FPR, TNR Rates\n"))
  print(confusionmatrix2)
  #cat(sprintf("\n\nTpr :%f\nTnr :%f\nFpr :%f\nFnr :%f", tpr,tnr,fpr,fnr))
  
  incomeresult_dataframe = as.data.frame(incomeresult)
  colnames(incomeresult_dataframe) = c("transID","actual","predicted", "prob")
  for(i in 1:nrow(incomeresult_dataframe))
  {
    if(incomeresult[i,2] == 1)
      {incomeresult_dataframe[i,2] = "<=50k Positive"}
    else 
      {incomeresult_dataframe[i,2] = ">50k Negative"}
    
    if(incomeresult[i,3] == 1) 
      {incomeresult_dataframe[i,3] = "<=50k Positive"}
    else 
      {incomeresult_dataframe[i,3] = ">50k Negative"}
  }
  #print(incomeresult_dataframe)
  
  tprarray[num,1]=num; tprarray[num,2]=tpr/(tpr+fnr)
  tnrarray[num,1]=num; tnrarray[num,2]=tnr/(tnr+fpr)
  fprarray[num,1]=num; fprarray[num,2]=fpr/(fpr+tnr)
  fnrarray[num,1]=num; fnrarray[num,2]=fnr/(tpr+fnr)
  
  cat(sprintf("\nAccuracy rate: %f",(tpr+tnr)/(nrow(incometesting))))
  cat(sprintf("\nError rate: %f",(fpr+fnr)/(nrow(incometesting))))
  cat(sprintf("\nPrecision: %f",tpr/(tpr+fpr)))
  cat(sprintf("\nRecall: %f",tpr/(tpr+fnr)))
  cat(sprintf("\nF measure: %f",tpr/(2*tpr+fpr+fnr)))
  }

print("Values on ROC curve:")

for(i in 1:nrow(tprarray))
{
cat(sprintf("\nk=%d TPR: %f\tFPR: %f",i,tprarray[i,2],fprarray[i,2]))
}
plot(c(0,fprarray[,2],1), c(0,tprarray[,2],1), pch=12,type="l",main="ROC Curve", ylab= "True Positive", xlab= "False Positive", xlim=c(0, 1), ylim=c(0,1))
abline(0,1)