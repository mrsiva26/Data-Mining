#Import CSV to dataframe Iris
income_mini = read.csv("C:/Users/Siva/Desktop/Data Mining/Homework 1/Iris.csv", TRUE)

#declare a dataframe for storing results
result_df <- data.frame(
                 "Transaction-ID"=integer(),
                 "1st"=integer(), 
                 "1st-dist"=double(), 
                 "2nd"=integer(),
                 "2nd-dist"=double(), 
                 "3rd"=integer(),
                 "3rd-dist"=double(), 
                 "4th"=integer(),
                 "4th-dist"=double(), 
                 "5th"=integer(),
                 "5th-dist"=double(),
                 stringsAsFactors=FALSE) 

#outer loop for iterating through no. of rows
for( i in 1:nrow(iris))
{
  #declare a distance dataframe
  distance_df = data.frame("Transaction-ID"=integer(), "distance"=double())
  
  row1 = iris[i,]
  
  #inner loop for comparing i-th row with all other rows
  for(j in 1:nrow(iris) )
  {
    #prevent comparison of i-th row with itself
    if(i!=j)
    {
      row2 = iris[j,]
      
      #Calculate Euclidean Distance
      dist=0
      for(k in 1:(ncol(row1)-1))
      {
        dist = dist + (row1[k] - row2[k]) * (row1[k] - row2[k])
      }
      dist=sqrt(dist)
      dist=as.numeric(dist)
      
      #assign the distance to distance_df
      jrow = data.frame("Transaction-ID"=j, "distance"=dist)
      distance_df = rbind(distance_df,jrow)
    }
  }
  #Sort the Distance Matrix
  distance_df = distance_df[order(distance_df$distance),]

  #Choose k neighnors and add a row in the result table
  temp <- data.frame("Transaction-ID"=i,
                     "1st"=distance_df[1,1], 
                     "1st-dist"=distance_df[1,2], 
                     "2nd"=distance_df[2,1],
                     "2nd-dist"=distance_df[2,2], 
                     "3rd"=distance_df[3,1],
                     "3rd-dist"=distance_df[3,2], 
                     "4th"=distance_df[4,1],
                     "4th-dist"=distance_df[4,2],
                     "5th"=distance_df[5,1],
                     "5th-dist"=distance_df[5,2])
  result_df=rbind(result_df,temp)
  cat(sprintf("\nEuclidean neighbors calculated for row - %d\n",i))
  print(result_df[i,])
  #goto next row in the table and repeat
}
cat(sprintf("\n"))
print(result_df)
write.csv(result_df, file = "C:/Users/Siva/Desktop/Data Mining/Homework 1/Iris_euclid_output.csv", row.names = TRUE)