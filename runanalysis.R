#Please read the ReadME and associated Code Book for details
#on decisions made for this assignment

setwd("/Users/Kurt/Documents/Coursera/Code/Class03-all/UCI HAR Dataset")

# #Read in all data: loop through each file location, pull file names,
# #then pull each file in individually, naming it the same as its filename

paths = c("./test/","./train/")
for (i in 1:2){
  files = list.files(path=paths[i],".txt$")

  for (j in 1:length(files)) {
    assign(files[j],
           read.table(paste(paths[i],files[j],sep=""),
             head=FALSE))
  }
}

features = read.csv("./features.txt",sep=" ",
                  head=FALSE)
activity_labels= read.csv("./activity_labels.txt",sep=" ",
                   head=FALSE)

#apply correct names to each dataset
names(X_test.txt)=as.character(features[,2])
names(X_train.txt)=as.character(features[,2])

#Determine which columns are mean and standard deviation data.
#These are kept and stored in tidy with names, others discarded.
#Parentheses in metric labels are removed for ease of analysis.
keepcols = grepl("std|mean",features[,2])

tidy = rbind(X_test.txt,X_train.txt)
tidy = tidy[,keepcols]
names(tidy)=sapply(names(tidy),gsub,pattern="\\()",replacement="")

labels_vec = rbind(y_test.txt,y_train.txt)
subjects = rbind(subject_test.txt,subject_train.txt)

#rename the activities from numbers (example: 1 becomes "walking")
#using a join from the sqldf package
labels_vec = sqldf("select labels_vec.V1, activity_labels.V2
                         from labels_vec
                         left join activity_labels
                         on labels_vec.V1=activity_labels.V1")[,2]

names(labels_vec) = "labels"
names(subjects) = "subjects"

#finish creating the first tidy dataset including all means, standard deviations,
#labels, and subjects
subjects=as.factor(unlist(subjects))
tidy = cbind(labels_vec,subjects,tidy)

#begin creating second tidy data set with averages for each subject
#and activity.  Start by defining the matrix, then loop through each metric.

tidy2=NULL

for (column in 3:ncol(tidy)){
  # take the mean of each variable
  newmatrix=tapply(tidy[,column],
                        list(tidy$subjects,tidy$labels_vec),
                        mean)
  #bind the means together with the subjects and metric name
  #(which is the same for each row)
  newmatrix = data.frame("subject" = as.numeric(rownames(newmatrix)),
                         "metric"=rep(names(tidy)[column],nrow(newmatrix)),
                         newmatrix)
  #bind the matrix generated in this iteration to the matrix made in 
  #previous iterations
  tidy2=rbind(tidy2,newmatrix)

}
names(tidy2)[1:2]=c("subject","metric")

#output the file
setwd("/Users/Kurt/Documents/Coursera/Code/Class03-all/")
write.table(tidy2,
            file = "runanalysis.csv",
            sep=",",
            row.names = FALSE)
