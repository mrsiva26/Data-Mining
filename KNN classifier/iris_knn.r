dataframe_to_matrix<-function(source_dataframe)
{
  target_matrix = matrix(0,nrow=nrow(source_dataframe),ncol=(ncol(source_dataframe)+1))
  
  transid = rownames(source_dataframe)
  transid = as.numeric(transid)
  target_matrix[,1] = cbind(transid)
  rm(transid)
  
  target_matrix[,2] = as.numeric(cbind(source_dataframe[,1]))
  target_matrix[,3] = as.numeric(cbind(source_dataframe[,2]))
  target_matrix[,4] = as.numeric(cbind(source_dataframe[,3]))
  target_matrix[,5] = as.numeric(cbind(source_dataframe[,4]))
  target_matrix[,6] = as.integer(factor(source_dataframe[,5]))
  class = as.integer(factor(source_dataframe$class))
  
  rm(class)
  
  return(target_matrix)
}

implementirisknn = function(testset,trainingset,no_of_neighbors)
{
  cat(sprintf("\n\n--------Implementing knn for k = %d-----------",no_of_neighbors))
  #declare a Matrix for storing results
  resultset = matrix(0, nrow=nrow(testset), ncol=4)
  
  #declare a distance matrix 
  distancematrix = matrix(0, nrow=nrow(trainingset), ncol=3)
  
  row12=matrix(0,nrow=2, ncol=6)
  
  #outer loop for iterating through testing rows
  for( i in 1:nrow(testset))
  {
    row12[1,] = as.matrix(testset[i,])
    #inner loop for comparing testing row with all training rows
    for(j in 1:nrow(trainingset) )
    {
      row12[2,] = as.matrix(trainingset[j,])
      #Calculate Euclidean Distance
      dist=0
      for(k in 2:5)
      {
        dist = dist + (row12[1,k] - row12[2,k]) * (row12[1,k] - row12[2,k])
      }
      dist=sqrt(dist)
      dist=as.numeric(dist)
      #assign the distance to distanmatrix
      distancematrix[as.numeric(j),1]=as.numeric(j)
      distancematrix[as.numeric(j),2]=dist
      distancematrix[as.numeric(j),3]=trainingset[j,6]
      #take next training row and compare it with i-th testing row
    }
    
    
    #Sort the Distance Matrix
    distancematrix <- distancematrix[order(distancematrix[,2]),];
    #print(distancematrix)
    
    class1count=class2count=class3count=0
    class1id=class2id=class3id=0
    #Choose k neighbors
    for(k in 1:no_of_neighbors)
    {
      if(distancematrix[k,3]==1)      {class1count = class1count+1; class1id=distancematrix[k,1]}
      else if(distancematrix[k,3]==2) {class2count = class2count+1; class2id=distancematrix[k,1]}
      else if(distancematrix[k,3]==3) {class3count = class3count+1; class3id=distancematrix[k,1]}
    }
    
    resultset[i,1] = i #transid
    resultset[i,2] = testset[i,6] #actual class
    #cat(sprintf("\nNeighbors for row - %d: ",i))
    if(class1count>class2count && class1count>class3count)
    {
      #cat(sprintf("Class 1 count %d", class1count))
      resultset[i,3] = trainingset[class1id,6]
      resultset[i,4] = class1count/k
    }
    else if(class2count>class1count && class2count>class3count)
    {
      #cat(sprintf("Class 2 count %d", class2count))
      resultset[i,3] = trainingset[class2id,6]  
      resultset[i,4] = class2count/k
    }
    else if(class3count>class1count && class3count>class2count)
    {
      #cat(sprintf("Class 3 count %d", class3count))
      resultset[i,3] = trainingset[class3id,6]  
      resultset[i,4] = class3count/k
    }else
    {
      #cat(sprintf("Class 1,2,3 count %d,%d,%d", class1count,class2count,class3count))
      resultset[i,3] = distancematrix[1,3]
      resultset[i,4] = 0.33
    }
    
    #goto next row in the table and repeat
  }
  return (resultset)
}

#Raed from csv and convert into matrix
iris = read.csv("Iris.csv",TRUE)
iristraining = dataframe_to_matrix(iris)
#print(iristraining)

#Raed from csv and convert into matrix
iris = read.csv("Iris_Test.csv",TRUE)
iristesting = dataframe_to_matrix(iris)


#declare a Matrix for storing results
irisresult = matrix(0, nrow=nrow(iristesting), ncol=4)
colnames(irisresult) = c("transID","actual","predicted", "prob")


for(num in 1:20)
{
  irisresult = implementirisknn(iristesting,iristraining,num)
  colnames(irisresult) = c("transID","actual","predicted", "prob")
  #declare confusion matrices
  confusionmatrix = matrix(0,ncol=3,nrow=3)
  rownames(confusionmatrix) = c("Setosa","Versicolor","Virginica")
  colnames(confusionmatrix) = c("Setosa","Versicolor","Virginica")
  for(i in 1:nrow(irisresult))
  {
    if(irisresult[i,2] == 1 && irisresult[i,3] == 1)
      confusionmatrix[1,1] = confusionmatrix[1,1] + 1
    else if(irisresult[i,2] == 1 && irisresult[i,3] == 2)
      confusionmatrix[1,2] = confusionmatrix[1,2] + 1
    else if(irisresult[i,2] == 1 && irisresult[i,3] == 3)
      confusionmatrix[1,3] = confusionmatrix[1,3] + 1
    
    else if(irisresult[i,2] == 2 && irisresult[i,3] == 1)
      confusionmatrix[2,1] = confusionmatrix[2,1] + 1
    else if(irisresult[i,2] == 2 && irisresult[i,3] == 2)
      confusionmatrix[2,2] = confusionmatrix[2,2] + 1
    else if(irisresult[i,2] == 2 && irisresult[i,3] == 3)
      confusionmatrix[2,3] = confusionmatrix[2,3] + 1
    
    else if(irisresult[i,2] == 3 && irisresult[i,3] == 1)
      confusionmatrix[3,1] = confusionmatrix[3,1] + 1
    else if(irisresult[i,2] == 3 && irisresult[i,3] == 2)
      confusionmatrix[3,2] = confusionmatrix[3,2] + 1
    else if(irisresult[i,2] == 3 && irisresult[i,3] == 3)
      confusionmatrix[3,3] = confusionmatrix[3,3] + 1
  }
  cat(sprintf("\nConfusion Matrix\n"))
  print(confusionmatrix)
  
  irisresult_dataframe = as.data.frame(irisresult)
  colnames(irisresult_dataframe) = c("transID","actual","predicted", "prob")
  for(i in 1:nrow(irisresult_dataframe))
  {
    if(irisresult[i,2] == 1) 
    {irisresult_dataframe[i,2] = "Setosa"}
    else if(irisresult[i,2] == 2)
    {irisresult_dataframe[i,2] = "Versicolor"}
    else 
    {irisresult_dataframe[i,2] = "Virginica"}
    
    if(irisresult[i,3] == 1) 
    {irisresult_dataframe[i,3] = "Setosa"}
    else if(irisresult[i,3] == 2)
    {irisresult_dataframe[i,3] = "Versicolor"}
    else 
    {irisresult_dataframe[i,3] = "Virginica"}
    
  }
  print(irisresult_dataframe)
  
  cat(sprintf("\nAccuracy rate: %f",(confusionmatrix[1,1]+confusionmatrix[2,2]+confusionmatrix[3,3])/(nrow(iristesting))))
  cat(sprintf("\nError rate: %f",1-(confusionmatrix[1,1]+confusionmatrix[2,2]+confusionmatrix[3,3])/(nrow(iristesting))))
}
