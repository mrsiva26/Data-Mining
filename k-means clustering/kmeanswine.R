library(fpc)
################################# FUNCTIONS ########################################

plotkmeans<-function(datapoints,clusterids,noofclusters){
  plot(datapoints[clusterids$cluster==1,], col=1, pch="|",
       xlim=c(min(datapoints[,1]),max(datapoints[,1])),
       ylim=c(min(datapoints[,2]),max(datapoints[,2]))
  )
  for(i in 2:noofclusters){
    text(datapoints[clusterids$cluster==i,], col=i*25+1,labels =i)
  }
}

calculateSSEandSSB = function(datapointswithclusterids,meansofclusters){
  totalsse=0
  tempmatrix = matrix(0,nrow=length(unique(datapointswithclusterids$cluster)),ncol=2)
  for(i in 1:nrow(tempmatrix)){
    tempmatrix[i,1] = i
    tempmatrix[i,2] = 0
  }
  #Loop thru all rows in the given data to find totalsse
  for(i in 1:nrow(datapointswithclusterids))
  {
    sseforthisrow=0
    #Loop to iterate thru all columns of each row 
    #Sums up distance of each column with mean of that cluster
    for(column in 1:ncol(meansofclusters))
    {
      sseforthisrow = sseforthisrow + (meansofclusters[datapointswithclusterids[i,12],column]-datapointswithclusterids[i,column])^2
    }
    tempmatrix[datapointswithclusterids[i,12],2] = tempmatrix[datapointswithclusterids[i,12],2]+sseforthisrow
    #print(sseforthisrow)
    totalsse = sseforthisrow + totalsse
  }
  cat(sprintf("\n"))
  colnames(tempmatrix) = c("Cluster id","SSE for this cluster")
  print(tempmatrix)
  totalssb=0
  #Find SSB for mean of each cluster with overall mean
  for(i in 1:nrow(meansofclusters))
  {
    ssbforthiscluster=0
    #Loop to iterate thru all columns of each row 
    #Sums up distance of each column with mean of all cluster
    for(column in 1:ncol(meansofclusters))
    {
      ssbforthiscluster = ssbforthiscluster + sum(datapointswithclusterids$cluster==i)*(mean(datapointswithclusterids[,column])-meansofclusters[i,column])^2
    }
    #print(ssbforthiscluster)
    totalssb = ssbforthiscluster + totalssb
  }
  cat(sprintf("\nSSE: %f",totalsse))
  cat(sprintf("\nSSB: %f",totalssb))
  cat(sprintf("\nSSE+SSB: %f\n",totalssb+totalsse))
  return(totalsse)
}

computeAverageSilhouetteWidth = function(datapointswithclusterids){
  #Matrix to store average silhouette widths
  silhouette = matrix(0,nrow=length(unique(datapointswithclusterids$cluster)),ncol=3)
  colnames(silhouette) = c("Cluster id","Cluster size","Average silhouette width")
  #bmatrix is used to choose the smallest dissimilarity cluster 
  #(ie) min(average distance of i to points in another cluster)
  bmatrix = matrix(0,nrow=length(unique(datapointswithclusterids$cluster)),ncol=3)
  for(i in 1:nrow(silhouette)){
    #Cluster i
    silhouette[i,1] = i
    bmatrix[i,1] = i
    #Count of number of points in cluster i and set it as cluster size for faster compuation
    silhouette[i,2] = sum(datapointswithclusterids$cluster==i)
    bmatrix[i,2] = sum(datapointswithclusterids$cluster==i)
    #Intializing it to zero
    silhouette[i,3] = 0
    bmatrix[i,3] = 0
  }
  
  #Convert matrix to dataframe for faster execution
  matrixfromdf = as.matrix(datapointswithclusterids)
  for(i in 1:nrow(matrixfromdf))
  {
    #Resetting it every time to find 
    #b = min (average distance of i to points in another cluster)
    bmatrix[,3] = 0
    for(j in 1:nrow(matrixfromdf))
    {
      #Calculating distance of i th row to every other point
      bmatrix[matrixfromdf[j,12],3] = bmatrix[matrixfromdf[j,12],3] + sum(abs(matrixfromdf[i,] - matrixfromdf[j,]))
    }
    
    #Calculating average distance to all points within cluster
    bmatrix[,3] = bmatrix[,3] / bmatrix[,2]
    ai = bmatrix[matrixfromdf[i,12],3]
    
    #Sort the Distance Matrix
    bmatrixsorted <- bmatrix[order(bmatrix[,3]),];
    
    #Choose the cluster with minimum average distance
    if(bmatrixsorted[1,1] == matrixfromdf[i,12]){
      bi = bmatrixsorted[2,3]
    }
    else{
      bi = bmatrixsorted[1,3]
    }
    
    if(bi > ai){
      silhouette[matrixfromdf[i,12],3] = silhouette[matrixfromdf[i,12],3] + 1 - (ai/bi)
    }else if(bi < ai){
      silhouette[matrixfromdf[i,12],3] = silhouette[matrixfromdf[i,12],3] + (bi/ai) -1
    }else{
    }
    #cat(sprintf("\n Row:%d || Cluster:%d  || ai:%f || bi:%f || s:%f", i,matrixfromdf[i,12],ai,bi,(1 - (ai/bi))))
  }
  #Divide by size of cluster to get average silhouette width
  silhouette[,3] = silhouette[,3] / silhouette[,2]
  print(silhouette)
  cat(sprintf("Average silhouette coefficient of all clusters: %f",mean(silhouette[,3])))
  #return(mean(silhouette[,3]))
}
################################ ACTUAL CODE ############################################

#Read in dataframe
winedataframe = read.csv("wine.csv", TRUE);

#Removing column id
winedataframe = winedataframe[-c(1,13,14)]
colnames(winedataframe) = c("fixed_acidity","volatile_acidity","citric_acid","residual_sugar","chlorides","free_sulphur_dioxide","total_sulphur_dioxide","density","pH","sulphates","alcohol")
#Change dataframe to matrix for faster calculation
winematrix = as.matrix(winedataframe)

#plot(winematrix[,1], winematrix[,2])
#inbuiltkmeans = kmeans(winematrix,centers = 3)
#plotkmeans(winematrix,inbuiltkmeans,3)
#inbuiltkmeans$centers
#inbuiltkmeans$cluster

#Speficy k
k= as.numeric(readline(prompt="Enter value for k: "));

#UNCOMMENT TO GET ELBOW GRAPH
#To calculate Elbow matrix
#elbowmatrix = matrix(0,nrow=15,ncol=2)
#for(kforelbow in 1:15)
#{
#elbowmatrix[kforelbow,1] = kforelbow
#k=kforelbow
#############################
  #Creating two dataframes to compare convergence
  wineprevoutput = data.frame(1:1599,nrow = nrow(winematrix),ncol=12)
  wineprevoutput = as.data.frame(winematrix)
  #Initially assigning all clusterids as -1
  wineprevoutput[,12] = -1
  winecurroutput = data.frame(1:1599,nrow = nrow(winematrix),ncol=12)
  winecurroutput = as.data.frame(winematrix)
  #Initially assigning all clusterids as 0
  winecurroutput[,12] = 0
  names = c("fixed_acidity","volatile_acidity","citric_acid","residual_sugar","chlorides","free_sulphur_dioxide","total_sulphur_dioxide","density","pH","sulphates","alcohol","cluster")
  colnames(wineprevoutput) = names
  colnames(winecurroutput) = names
  
  #Choose k initial means by sampling without replacement
  sampledids = sample(1:nrow(winematrix), k, replace=F)
  means = matrix(0,ncol=11,nrow=k)
  for(counttok in 1:k){
    means[counttok,]=winematrix[sampledids[counttok], -c(12)]
  }
  print("Rows used as initial means")
  print(sampledids)
  print("Inital means")
  print(means)
  
  #Max no.of.iterations = 100
  for(noofiteration in 1:100)
  {
    #Assign change=0 to detect convergance
    change=0
    #Loop thru all rows in the given data
    for(i in 1:nrow(winematrix))
    {
      prevdist = 1000000000
      cluster = 0
      #Assign cluster based on distance from mean
      #Loop thru all means to find out distance form each mean
      for(j in 1:nrow(means))
      {
        currdist=0
        #Loop to iterate thru all columns of each row (Sums up distance of each column)
        for(column in 1:ncol(means))
        {
          currdist = currdist + (means[j,column]-winematrix[i,column])^2
        }
        currdist=sqrt(currdist)
        currdist=as.numeric(currdist)
        if(currdist<prevdist)
        {
          prevdist = currdist
          cluster = j
        }
      }
      
      #Checking if its any different from previous iterations clustering
      winecurroutput[i,] = wineprevoutput[i,]
      if(wineprevoutput[i,12]!=cluster)
      {
        winecurroutput[i,12] = cluster
        change = change+1
      }
    }
    
    #No change in clusters
    if(change==0)
      break
    #Else go ahead for next iteration
    else{
      #Save this output cluster ids as previous for next iteration
      wineprevoutput = winecurroutput
      #Recompute means for next iteration
      for(i in 1:nrow(means))
      {
        #Loop to iterate thru all columns of each row
        for(column in 1:ncol(means))
        {
          means[i,column] = mean.default(winecurroutput[winecurroutput$cluster==i,column])
        }
      }
    }
  }
  print(winecurroutput[,12])
  cat(sprintf("\nConverged at %d",noofiteration))
  #Print Converged means
  for(i in 1:nrow(means))
  {
    cat(sprintf("\nNo of points in cluster %d : %d", i, sum(winecurroutput$cluster==i)))
    #cat(sprintf("\nMean of cluster %d :",i))
    #print(means[i,])
  }
  calculateSSEandSSB(winecurroutput,means)
  #elbowmatrix[k,2] = calculateSSEandSSB(winecurroutput,means)
  if(k>1)
  computeAverageSilhouetteWidth(winecurroutput)
  plotcluster(winematrix, winecurroutput$cluster)
  #UNCOMMENT TO GET ELBOW GRAPH
#}
#plot(elbowmatrix,lwd=2, xlim=c(1,15),ylim=c(0,max(elbowmatrix[,2])),type="l", col="red")
