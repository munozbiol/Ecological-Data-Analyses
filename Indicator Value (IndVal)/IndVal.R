
# Indicator Value 

# The dummy dataset was created from the original dataset with the function
# randomizeMatrix() from the picante package.
# Once the project is finished and data available, the original dataset will be uploaded


#Load libraries 

library(indicspecies)
library(picante)


#cleaning the data and creating a the dummy dataset.

#we read the original data
originaldata <- read.csv("IndVal.csv", header = T )

#we observed duplicated rows
duplicated(originaldata$Embalse)

#removing duplicates
noduplicates <- originaldata[!duplicated(originaldata$Embalse),]

#Do we have NAs? 
which(is.na(noduplicates))


#Randomizing the original dataset

#Below we use two different randomize models to see their difference, 
#however, will use for further analysis the "richness" model. 

#We only selected the species abundance columns, since the function only can 
#deal with numeric values.

# richnnes changes the abundance among species
dummydata <- randomizeMatrix(noduplicates[,3:54], null.model = "richness")

#Frequency changes the abundances among sites
dummy_dataset <- randomizeMatrix(noduplicates[,3:54], null.model = "frequency")

#we turned them into Data Frames
dummydata <- as.data.frame(dummydata)
dummy_dataset <- as.data.frame(dummy_dataset)


#Creating a new mixed dataset

# We need the extract only the reservoir and trophic state columns from original.
extractcols <- noduplicates[,1:2]

# paste the previous columns to obtain new datasets

# by "richness" or mixed among species
dummy <- cbind(extractcols, dummydata) 

# by frequency or mixed among sites
dummydataset <- cbind(extractcols, dummy_dataset) 

# Save the new CSV file

write.csv(dummy, file = "dummy.csv")
write.csv(dummydataset, file = "dummyfrequencies")


# Performing the Ind Val 

#reading the dummy file 
speciesdummy <- read.csv("dummy.csv", header = T, row.names = 1)

#To run the function we need to split the data in two

# 1) data with only species abundances 

abundances <- speciesdummy[,-c(1:2)]

# 2 ) extract the information of the group or cluster interest in this case we use 
# the trophic status but also can be done for any other purpose. 

trophicstatus <- speciesdummy$Estado.Trofico

#Runinng the test

ind.val <- multipatt(abundances,
                     trophicstatus,
                     func = "IndVal.g",
                     control = how(nperm = 9999))

# If your computational power is not enough, consider reduce the number of permutations


#Seeing the results 

summary(ind.val)


# The species with stats above 0.5 and p value less than 0.05
# can be used as indicator or associated to something (trophic status).
# Here we obtained that seven species are related to Hipereutrophic, 