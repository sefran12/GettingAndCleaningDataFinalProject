#### REQUIRED LIBRARIES ####

library(tidyverse)

#### DOWNLOADING DATA ####

# creating a directory for our final project

if (!dir.exists("FinalProject")) {
  dir.create("FinalProject")
}
setwd("FinalProject/")

# downloading relevant data

fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

if (!file.exists("UCI_HAR_Dataset.zip")) {
  download.file(url = fileUrl,
                destfile = "UCI_HAR_Dataset.zip")
}

# unzipping

unzip("UCI_HAR_Dataset.zip", exdir = "./Data")

#### LOADING DATA ####

# If you see the resulting directories, there are multitude of .txt files.
# I did the work of going one by one to see what each one was about

# First there is label data.

activity_labels <- read.table("Data/UCI HAR Dataset/activity_labels.txt",
                     header = FALSE, sep = "",
                     col.names = c("Factor", "Label"))

# there are many, many features, so a "by-hand" variable naming scheme
# is probably not a good idea. I guess what we need to do is programmatically
# cook some (more) descriptive names for the variables. We're lucky the data is
# regular enough for simple tricks to be enough

feature_names <- read.table("Data/UCI HAR Dataset/features.txt",
                            header = FALSE, sep = " ",
                            col.names = c("Index", "Feature_name"))

# Then the data itself

# Testing set

X_test <- read.table("Data/UCI HAR Dataset/test/X_test.txt",
                     header = FALSE)
y_test <- read.table("Data/UCI HAR Dataset/test/y_test.txt",
                     header = FALSE)

# Training set

X_train <- read.table("Data/UCI HAR Dataset/train/X_train.txt",
                     header = FALSE)
y_train <- read.table("Data/UCI HAR Dataset/train/y_train.txt",
                     header = FALSE)

# with its associated subjects

subject_test <- read.table("Data/UCI HAR Dataset/test/subject_test.txt",
                           header = FALSE)
subject_train <- read.table("Data/UCI HAR Dataset/train/subject_train.txt",
                            header = FALSE)

# NOTE: The folders on inertial signals will be discarded later by the exercise
# itself so we don't concern ourselves with them.

#### DATABASE CONSTRUCTION ####

# First, let's assign variable names according to our source material. I prefer
# doing it all two times (before the merge) to lessen the chance that
# I overlook something and the merge ends up with some subtle error.
# Better to tidyly merge two tidy datasets than tidying a dirty merged dataset.

colnames(X_train) <- feature_names[,2]
colnames(X_test) <- feature_names[,2]
colnames(subject_train) <- "subject"
colnames(subject_test) <- "subject"

# Now, let's convert our activity data into factors with the corresponding
# labels according to 'activity_labels'. Casting it directly with 'factor'
# leaves us with a factor vector. I force a data.frame and name the variable.

y_test <- data.frame(
  activity = factor(y_test$V1, levels = activity_labels[,1], labels = activity_labels[,2])
)
y_train <- data.frame(
  activity = factor(y_train$V1, levels = activity_labels[,1], labels = activity_labels[,2])
)

# Now we can peacefully merge subject, X, and y

train_data <- cbind(subject_train, X_train, y_train)
test_data <- cbind(subject_test, X_test, y_test)

# Then, I consider important remembering from where (of the crossvalidation step)
# does the data come from (in general, although in this exercise it does not matter):

train_data$crossvalidation <- "train"
test_data$crossvalidation <- "test"

#### MERGING DATASETS ####

# We 'merge' (that is, append the records of one to the other) the data sets to end up
# with the final data set.

human_activity_recognition_data <- rbind(train_data, test_data) # long name, but descriptive

# We can now touch up some little details we left out in the data

# Tidying up we got with the suprise that there are duplicated column names
# that come from the very source material 'feature_names.txt':

colnames(human_activity_recognition_data)[duplicated(colnames(human_activity_recognition_data))]

# We fix this with repair_names(). No measures with mean or std are affected so there's no
# cause for confusion.

human_activity_recognition_data <- human_activity_recognition_data %>% 
  repair_names() %>% 
  mutate(subject = as.factor(subject))

# We select

# OPTION 1: Everything that has 'mean' or 'std' in its name is selected

human_activity_recognition_data %>% 
  select(matches('*mean*|*std*'))

# OPTION 2: Only variables with calculated mean() and std() in their names are selected

final_HAR_data <- human_activity_recognition_data %>% 
  select(matches('*mean\\(\\)|*std\\(\\)'), subject, activity) 

# This feels more consistent, as it seems conceptually more correct to think
# that this:
# "Extracts only the measurements on the mean and standard deviation for each measurement."
# meant all .mean() and std() (derived measures) than selecting
# simply those which happen to have mean or std in their names.

#### DERIVED DATASETS ####

summary_data <- final_HAR_data %>% 
  group_by(subject, activity) %>% 
  summarise_all(mean)

### DESCRIPTIVE NAMES:
# Finally, given that the feature names are relatively expressive, we can
# clarify that t is time domain, acc is acceleration, Gyro is angular acceleration (gyroscope)
# f is frequency domain and Mag is magnitude. We do this with regular expressions.
# We also standarize the colnames as per usual R conventions. I know the prof. uses sometimes
# dots in the names (as in Google's R style guide) and discourages "_" in colnames but the
# R style guide itself separating with _ in names is encouraged, so I replace camelCase with
# underscore_separation and replace '-' with '_'

colnames(final_HAR_data) <- colnames(final_HAR_data) %>% 
  str_replace_all("[t]{1}(?=[:upper:])", "time_") %>% 
  str_replace_all("[f]{1}(?=[:upper:])", "frequency_") %>% 
  str_replace_all("(?<=[:lower:])[A][c][c]{1}(?=.)", "_acceleration") %>% 
  str_replace_all("(?<=[:lower:])[M][a][g]{1}(?=.)", "_magnitude") %>% 
  str_replace_all("-", "_")

colnames(summary_data) <- colnames(summary_data) %>% 
  str_replace_all("[t]{1}(?=[:upper:])", "time_") %>% 
  str_replace_all("[f]{1}(?=[:upper:])", "frequency_") %>% 
  str_replace_all("(?<=[:lower:])[A][c][c]{1}(?=.)", "_acceleration") %>% 
  str_replace_all("(?<=[:lower:])[M][a][g]{1}(?=.)", "_magnitude") %>% 
  str_replace_all("-", "_")

# Exporting data:
# It is not clear which data set we must upload to coursera so we save both.
# We write to .csv because parsing is usually easier and there is no
# possibility of mistaking the comma with some comma in the data.

write.csv(final_HAR_data, file = "tidy_dataset.csv")
write.csv(summary_data, file = "tidy_summary_dataset.csv")

# As the summary_data is the result of more transformations
# than the final_HAR_data, I assume that this is what we
# should upload to Coursera to show our work.