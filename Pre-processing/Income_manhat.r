Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Read income.csv as a dataframe
income_mini = read.csv("C:/Users/Siva/Desktop/Data Mining/Homework 1/Income_NEW.csv", TRUE)
names(income_mini) = c("Trans_id","age","workclass","fnlwgt","education","education_cat","marital_status","occupation","relationship","race","gender","capital_gain","capital_loss","hour_per_week","native_country","class")
#print(income_mini)

#Changing all NAs with Mean/Mode of that column
for(i in 1:nrow(income_mini))
{
  for( j in 2:(ncol(income_mini)-1) )
  {
    if(is.na(income_mini[i,j]) || income_mini[i,j]==" ?"){
      
      if(class(income_mini[,j]) == "integer"){
        #cat(sprintf("\nI am an integer NA %d-%d",i,j))
        income_mini[i,j] = mean(income_mini[,j])
        
      }else{
        #cat(sprintf("\nI am a non integer NA %d-%d",i,j))
        income_mini[i,j] = as.character( Mode(income_mini[,j]) )
      }
      
    }
  }
}

#declare a dataframe for storing results
result_income <- data.frame(
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

maxage = as.numeric(max(income_mini$age))
minage = as.numeric(min(income_mini$age))
diffage = maxage - minage
print(maxage)
print(minage)
print(diffage)

maxfnlwgt = as.numeric(max(income_mini$fnlwgt))
minfnlwgt = as.numeric(min(income_mini$fnlwgt))
difffnlwgt = maxfnlwgt - minfnlwgt
print(maxfnlwgt)
print(minfnlwgt)
print(difffnlwgt)

maxedu = as.numeric(max(income_mini$education_cat))
minedu = as.numeric(min(income_mini$education_cat))
diffedu = maxedu - minedu
print(maxedu)
print(minedu)
print(diffedu)

maxgain = as.numeric(max(income_mini$capital_gain))
mingain = as.numeric(min(income_mini$capital_gain))
diffgain = maxgain - mingain 
print(maxgain)
print(mingain)
print(diffgain)

maxloss = as.numeric(max(income_mini$capital_loss))
minloss = as.numeric(min(income_mini$capital_loss))
diffloss = maxloss - minloss 
print(maxloss)
print(minloss)
print(diffloss)

maxhour = as.numeric(max(income_mini$hour_per_week))
minhour = as.numeric(min(income_mini$hour_per_week))
diffhour = maxhour - minhour 
print(maxhour)
print(minhour)
print(diffhour)

for(i in 1:nrow(income_mini))
{
  #cat(sprintf("\nAge before norm %f",income_mini[i,"age"]))
  income_mini[i,"age"] = (income_mini[i,"age"] - minage) / diffage
  #cat(sprintf("\nAge after norm %f",income_mini[i,"age"]))
  
  #cat(sprintf("\nFnlwgt before norm %f",income_mini[i,"fnlwgt"]))
  income_mini[i,"fnlwgt"] = (income_mini[i,"fnlwgt"] -minfnlwgt) / difffnlwgt
  #cat(sprintf("\nFnlwgt after norm %f",income_mini[i,"fnlwgt"]))
  
  #cat(sprintf("\nEdu before norm %f",income_mini[i,"education_cat"]))
  income_mini[i,"education_cat"] = (income_mini[i,"education_cat"] -minedu) / diffedu
  #cat(sprintf("\nEdu after norm %f",income_mini[i,"education_cat"]))
  
  #cat(sprintf("\nGain before norm %f",income_mini[i,"capital_gain"]))
  income_mini[i,"capital_gain"] = (income_mini[i,"capital_gain"] -mingain) / diffgain
  #cat(sprintf("\nGain after norm %f",income_mini[i,"capital_gain"]))
  
  #cat(sprintf("\nLoss before norm %f",income_mini[i,"capital_loss"]))
  income_mini[i,"capital_loss"] = (income_mini[i,"capital_loss"] - minloss) / diffloss
  #cat(sprintf("\nLoss after norm %f",income_mini[i,"capital_loss"]))
  
  #cat(sprintf("\nHour before norm %f",income_mini[i,"hour_per_week"]))
  income_mini[i,"hour_per_week"] = (income_mini[i,"hour_per_week"] -minhour) / diffhour
  #cat(sprintf("\nHour after norm %f",income_mini[i,"hour_per_week"]))
}

#outer loop for iterating through no. of rows
for( i in 1:nrow(income_mini))
{
  #declare a distance dataframe
  distance_df = data.frame("Transaction-ID"=integer(), "distance"=double())
  
  row1 = income_mini[i,]
  #print(row1)
  
  #inner loop for comparing i-th row with all other rows
  for(j in 1:nrow(income_mini) )
  {
    #prevent comparison of i-th row with itself
    if(i!=j)
    {
      row2 = income_mini[j,]
      #print(row2)
      
      #Calculate Manhattan Distance
      #cat(sprintf("\nAgediff %f", row1$age-row2$age))
      
      if(row1$workclass == row2$workclass) workclassdiff = 0
      else workclassdiff = 1
      #cat(sprintf("\nWorkclassdiff %d", workclassdiff))
      
      #cat(sprintf("\nFnlwgtdiff %f", row1$fnlwgt-row2$fnlwgt))
      
      #cat(sprintf("\nEducatdiff %f", row1$education_cat-row2$education_cat))
      
      if(row1$marital_status == row2$marital_status) marital_statusdiff = 0
      else marital_statusdiff = 1
      #cat(sprintf("\nMarital_statusdiff %d", marital_statusdiff))
      
      if(row1$occupation == row2$occupation) occupationdiff = 0
      else occupationdiff = 1
      #cat(sprintf("\nOccupationdiff %d", occupationdiff))
      
      if(row1$relationship == row2$relationship) relationshipdiff = 0
      else relationshipdiff = 1
      #cat(sprintf("\nRelationshipdiff %d", relationshipdiff))
      
      if(row1$race == row2$race) racediff = 0
      else racediff = 1
      #cat(sprintf("\nRacediff %d", racediff))
      
      if(row1$gender == row2$gender) genderdiff = 0
      else genderdiff = 1
      #cat(sprintf("\nGenderdiff %d", genderdiff))
      
      #cat(sprintf("\nGaindiff %f", row1$capital_gain-row2$capital_gain))
      
      #cat(sprintf("\nLossdiff %f", row1$capital_loss-row2$capital_loss))
      
      #cat(sprintf("\nHourdiff %f", row1$hour_per_week-row2$hour_per_week))
      
      if(row1$native_country == row2$native_country) native_countrydiff = 0
      else native_countrydiff = 1
      #cat(sprintf("\nNative_countrydiff %d", native_countrydiff))
      
      dist = 
        as.numeric(abs(row1$age-row2$age)) + 
        workclassdiff + 
        as.numeric(abs(row1$fnlwgt-row2$fnlwgt)) + 
        as.numeric(abs(row1$education_cat-row2$education_cat)) + 
        occupationdiff +
        relationshipdiff +
        racediff +
        genderdiff +
        as.numeric(abs(row1$capital_gain-row2$capital_gain)) +
        as.numeric(abs(row1$capital_loss-row2$capital_loss)) +
        as.numeric(abs(row1$hour_per_week-row2$hour_per_week)) +
        native_countrydiff
      
      dist=sqrt(dist)
      dist=as.numeric(dist)
      
      #assign the distance to distance_df
      jrow = data.frame("Transaction-ID"=row2$Trans_id, "distance"=dist)
      distance_df = rbind(distance_df,jrow)
    }
  }
  #Sort the Distance Matrix
  distance_df = distance_df[order(distance_df$distance),]
  #Choose k neighnors and add a row in the result table
  temp <- data.frame("Transaction-ID"=row1$Trans_id,
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
  result_income=rbind(result_income,temp)
  cat(sprintf("\nManhattan neighbors calculated for row - %d\n",i))
  print(result_income[i,])
  #take next row and compare it with i-th row
}
print(result_income)
write.csv(result_income, file = "C:/Users/Siva/Desktop/Data Mining/Homework 1/Income_manhat_output.csv", row.names = TRUE)