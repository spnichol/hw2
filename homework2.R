setwd("/home/spnichol/Documents/school/TAD")
options(scipen=999)
#Problem 1(a)
#Estimate the liklihood that the following e-mail was sent from Marine Le Pen or Emmanuel Macron. 

email <- c("immigration", "voter", "culture", "help", "neighborhood")

lepen1 <- c("immigration", "women", "assimilate", "afraid", "win")
lepen2 <- c("culture", "voter", "economy", "president", "help")
macron1 <- c("voter", "women", "help", "reform", "education")
macron2 <- c("union", "economy", "hope", "immigration", "neighborhood")
macron3 <- c("win", "union", "europe", "elect", "president")
lepen3 <- c("economy", "voter", "immigration", "president", "culture")
macron4 <- c("success", "german", "culture", "help", "french")

#calculate estimated probability of a given class from training data by dividing number of ocurrences of class by
#total number of documents
pr_c_lepen <- 3/7
pr_c_macron <- 4/7

#calculate estimated probability of a term given a class from training data by dividing number of term occurrences in 
#respective class (including multiple occurrences by total number of all terms in the training documents 


pr_immigration_lepen <- 2/15
pr_voter_lepen <- 2/15
pr_culture_lepen <- 2/15
pr_help_lepen <- 1/15
pr_neighborhood_lepen <- 0/15

pr_email_lepen <- (2/15) * (2/15) * (2/15) * (1/15) * (0/15) * (3/7)
pr_email_lepen

pr_immigration_macron <- 1/20
pr_voter_macron <- 1/20
pr_culture_macron <- 1/20
pr_help_macron <- 2/20
pr_neighborhood_macron <- 1/20

pr_email_macron <- (1/20) * (1/20) * (1/20) * (2/20) * (1/20) * (4/7)
pr_email_macron

#Problem 1(a) - continued 

# I don't completely trust these findings. Because LePen never uses the word "neighborhood" in the training documents,
# the value for the maximum likelihood estimation for this term is 0. This sparsity cancels out the other MLEs, making the 
# class probability estimate 0, even if the MLEs for the other tems are high. 


#Problem 1(b)
#Perform LaPlace smoothing by adding 1 to each value 

#class probability does not change
pr_c_lepen <- 3/7
pr_c_macron <- 4/7

#add one to each value, including total term count 


pr_immigration_lepen <- 3/16
pr_voter_lepen <- 3/16
pr_culture_lepen <- 3/16
pr_help_lepen <- 2/16
pr_neighborhood_lepen <- 1/16

pr_email_lepen <- (3/16) * (3/16) * (3/16) * (2/16) * (1/16) * (3/7)
pr_email_lepen

pr_immigration_macron <- 2/21
pr_voter_macron <- 2/21
pr_culture_macron <- 2/21
pr_help_macron <- 3/21
pr_neighborhood_macron <- 2/21

pr_email_macron <- (2/21) * (2/21) * (2/21) * (3/21) * (2/21) * (4/7)
pr_email_macron

#Problem 1(b) - continued

#Because we added 1 to each term, thus blunting the effect of the sparse terms, LePen is now the more likely candidate
#to have written the e-mail. 


rm(list = ls())
setwd("/home/spnichol/Documents/school/TAD")

#Problem 2(a)

#Preprocessing: Because we are simply creating a new variable, there is not much preprocessing of the text neccesary for this particular step. 
amzn <- read.csv("amazon_reviews.csv", stringsAsFactors = FALSE)
names(amzn)[1] <- "ID"
smed_score <- median(amzn$Score)

amzn$Class <- ifelse(amzn$Score <= 3, 0, 1)
amzn$Anchor <- ifelse(amzn$Score == 5 | amzn$Score == 1, 1, 0)
amzn$Anchor_Neg <- ifelse(amzn$Score ==1, -1, 0)

for (i in 1:nrow(amzn)) {
  if (amzn$Score[i] == 5) {
    amzn$Anchor_Combo[i] <- 1
  }
  else if (amzn$Score[i] == 1)
  {
    amzn$Anchor_Combo[i] <- -1
  }
  
  else {
    amzn$Anchor_Combo[i] <- 0
  }
}

#Problem 2(b)

#Preprocessing: As we are actually dealing with the text, I will perform various preprocessing tasks. First, I will remove the encoding 
#errors, as they will show up as words when tokenized. Then, I'll remove stopwords and punctuation, in addition to converting to lower case and stemming to account for 
#for different tenses and spelling errors. 

#Read in sentiment dictionary by Hu & Liu 
library(quanteda)
#read files and convert to vectors
neg_sent <- read.csv("negative-words.txt", sep=" ", header=FALSE, stringsAsFactors = FALSE)
pos_sent <- read.csv("positive-words.txt", sep=" ", header=FALSE, stringsAsFactors = FALSE)
pos_sent <- as.vector(pos_sent$V1)
neg_sent <- as.vector(neg_sent$V1)
#create sentiment dictionary with positive and negative values 
sent_dict <- dictionary(positive=pos_sent, negative=neg_sent)

#remove escape characters and unicode
gsub("[&#][1-9]*", "", amzn$Text, fixed=TRUE)
#create corpus with appropriate variables
amzn.corp <- corpus(amzn$Text, docnames=amzn$ID, docvars=data.frame(class=amzn$Class, anchor_combo = amzn$Anchor_Combo, ID=amzn$ID))
#create dfm with dictionary 
amzn.dfm <- dfm(tokenize(amzn.corp, removePunct=TRUE), dictionary=sent_dict, stem=TRUE, tolower=TRUE)

#convert to dfm and add postive and negative counts to df
amzn.df <- as.data.frame(amzn.dfm)
amzn <- cbind(amzn, amzn.df)
#calculate sentiment score
amzn$Sent_Score <- amzn$posit - amzn$negat
#create class for new sentiment score 
amzn$Sent_Class <- ifelse(amzn$Sent_Score >= 0, 1, 0)

#create histogram vizualizing distribution of positive versus negative words
score_min <- min(amzn$Sent_Score)
score_max <- max(amzn$Sent_Score)
hist(amzn$Sent_Score, breaks="Sturges", freq=TRUE, xlab="Sentiment Score", main="Distribution of Sentiment Scores", plot=TRUE, xlim=c(score_min, score_max))

#calculate percent positive 
pct_pos <- (nrow(amzn[amzn$Sent_Class == 1 ,]) / nrow(amzn)) *100
paste(pct_pos, "% positive", sep="")

#create confusion matrix
confu_mat <- table(amzn$Sent_Class, amzn$Class)
colnames(confu_mat) <- c("New_Score: Negative", "New_Score: Positive")
rownames(confu_mat) <- c("Original_Score: Negative", "Original_Score: Positive")
confu_mat

#calculate accuracy 
acc <- sum(confu_mat[c(1, 4)])/sum(confu_mat) * 100
paste("Accuracy:", acc, "%", sep="")

#calculate precision 
prec <- round(sum(confu_mat[4])/sum(confu_mat[c(4, 3)]) * 100, 2)
paste("Precision:", prec, "%", sep="")

#calculate recall 
recall <- round(sum(confu_mat[4])/sum(confu_mat[c(4, 2)]) * 100, 2)
paste("Recall:", recall, "%", sep="")

#rank by new score so that lower values come first
amzn <- amzn[with(amzn, order(amzn$Sent_Score, decreasing=TRUE)) ,]
#add rank from 1 to 10,000
amzn$Sent_Rank <- seq(1:10000)

#rank by original score so that lower values come first
amzn <- amzn[with(amzn, order(amzn$Score, decreasing=TRUE)) ,]
#add rank from 1 to 10,000
amzn$Act_Rank <- seq(1:10000)
#compute abs difference between two ranks
rank_sum <- sum(abs(amzn$Sent_Rank - amzn$Act_Rank))
rank_sum


#Problem 2(c)

#Train Naive Bayes classifier to predict if review is positive or negative 
amzn$Class <- ifelse(amzn$Class == 1, 1, -1)

#Create new dfm without dictionary for comparative purposes 
amzn.corp <- corpus(amzn$Text, docvars=data.frame(ID= amzn$ID, class=amzn$Class, score = amzn$Score, anchor=amzn$Anchor, anchor_neg = amzn$Anchor_Neg))
#create training and test sets
samp <- floor(0.20 * nrow(amzn))
set.seed(100)
train_set <- sample(seq_len(nrow(amzn)), size=samp)
train.df <- amzn[train_set,]
test.df <- amzn[-train_set,]
amzn$Split <- ifelse(amzn$ID %in% train.df$ID,  "train", "test")
#create dfm 
all.dfm <- dfm(tokenize(amzn.corp, removePunct=TRUE), stem=TRUE, tolower=TRUE, groups=amzn$ID, remove=stopwords("english"))

#create test and training corpus 
corp.train <- corpus_subset(amzn.corp, subset= amzn.corp$documents$ID %in% train.df$ID, select=c(class, ID))
corp.test <- corpus_subset(amzn.corp, subset= amzn.corp$documents$ID %in% test.df$ID, select=c(class, ID))

#create dfm with training set and save class labels
train.dfm <- dfm(tokenize(corp.train, removePunct=TRUE), stem=TRUE, tolower=TRUE)
train.dfm <- dfm_select(train.dfm, features=all.dfm)
train.dfm@docvars
labels <- as.vector(train.df$Class)
# train NB model with "uniforms" as prior 
nb <- textmodel_NB(x=train.dfm, y=labels, data=NULL,
                        smooth=1, prior="uniform") # Smooth gives values of 1 for new words; NB wouldn't work very well
corp.test$documents$ID
#create dfm for test to calculate accuracy, precision and recall 
test.dfm <- dfm(tokenize(corp.test, removePunct=TRUE), groups=corp.test$documents$ID, stem=TRUE, tolower=TRUE)
test.dfm <- dfm_select(test.dfm, features=all.dfm)
test.pr <- predict(nb, newdata=test.dfm)

#convert to df and add original class labels 
pred.df <- as.data.frame(test.pr[1:4])
pred.df$ID <-as.integer(rownames(pred.df))
pred.df <- merge(x=pred.df, y=test.df, by="ID")
#fix class labels
pred.df$nb.predicted <- ifelse(pred.df$nb.predicted == "1", 0, 1)

#create confusion matrix and add labels
confu_mat2 <- confusionMatrix(pred.df$nb.predicted, pred.df$Class)
confu_mat2 <- confu_mat2$table

confu_mat2
#calculate accuracy 
acc <- sum(confu_mat2[c(1, 4)])/sum(confu_mat2) * 100
paste("Accuracy:", acc, "%", sep="")

#calculate precision 
prec <- round(sum(confu_mat2[4])/sum(confu_mat2[c(4, 3)]) * 100, 2)
paste("Precision:", prec, "%", sep="")

#calculate recall 
recall <- round(sum(confu_mat2[4])/sum(confu_mat2[c(4, 2)]) * 100, 2)
paste("Recall:", recall, "%", sep="")



# Retry with priors changed to docfreq


 





#now try with just dictionary words 


#Create new dfm without dictionary for comparative purposes 
amzn.corp <- corpus(amzn$Text, docvars=data.frame(ID= amzn$ID, class=amzn$Class, score = amzn$Score, anchor=amzn$Anchor, anchor_neg = amzn$Anchor_Neg))
#create training and test sets
samp <- floor(0.20 * nrow(amzn))
set.seed(100)
train_set <- sample(seq_len(nrow(amzn)), size=samp)
train.df <- amzn[train_set,]
amzn$Split <- ifelse(amzn$ID %in% train.df$ID,  "train", "test")
#create dfm 
all.dfm <- dfm(tokenize(amzn.corp, removePunct=TRUE), stem=TRUE, tolower=TRUE, groups=amzn$ID, remove=stopwords("english"))
#create test and training corpus 
corp.train <- corpus_subset(amzn.corp, subset= amzn.corp$documents$ID %in% train.df$ID, select=c(class, ID))
corp.test <- corpus_subset(amzn.corp, subset= amzn.corp$documents$ID %in% test.df$ID, select=c(class, ID))

#create dfm with training set and save class labels
train.dfm <- dfm(tokenize(corp.train, removePunct=TRUE), stem=TRUE, tolower=TRUE)
train.dfm <- dfm_select(train.dfm, features=all.dfm)
train.dfm@docvars
labels <- as.vector(train.df$Class)



# train NB model with "uniforms" as prior 
nb <- textmodel_NB(x=train.dfm, y=labels, data=NULL,
                   smooth=1, prior="uniform") # Smooth gives values of 1 for new words; NB wouldn't work very well
corp.test$documents$ID
#create dfm for test to calculate accuracy, precision and recall 
test.dfm <- dfm(tokenize(corp.test, removePunct=TRUE), groups=corp.test$documents$ID, stem=TRUE, tolower=TRUE)
test.dfm <- dfm_select(test.dfm, features=all.dfm)
test.pr <- predict(nb, newdata=test.dfm)

#convert to df and add original class labels 
pred.df <- as.data.frame(test.pr[1:4])
pred.df$ID <-as.integer(rownames(pred.df))
pred.df <- merge(x=pred.df, y=test.df, by="ID")
#fix class labels
pred.df$nb.predicted <- ifelse(pred.df$nb.predicted == "1", 0, 1)

#create confusion matrix and add labels
confu_mat2 <- confusionMatrix(pred.df$nb.predicted, pred.df$Class)
confu_mat2 <- confu_mat2$table

confu_mat2


#calculate accuracy 
acc <- sum(confu_mat2[c(1, 4)])/sum(confu_mat2) * 100
paste("Accuracy:", acc, "%", sep="")

#calculate precision 
prec <- round(sum(confu_mat2[4])/sum(confu_mat2[c(4, 3)]) * 100, 2)
paste("Precision:", prec, "%", sep="")

#calculate recall 
recall <- round(sum(confu_mat2[4])/sum(confu_mat2[c(4, 2)]) * 100, 2)
paste("Recall:", recall, "%", sep="")






# Retry with just dictionary words*



#Create new dfm without dictionary for comparative purposes 
amzn.corp <- corpus(amzn$Text, docvars=data.frame(ID= amzn$ID, class=amzn$Class, score = amzn$Score, anchor=amzn$Anchor, anchor_neg = amzn$Anchor_Neg))
#create training and test sets
samp <- floor(0.20 * nrow(amzn))
set.seed(100)
train_set <- sample(seq_len(nrow(amzn)), size=samp)
train.df <- amzn[train_set,]
test.df <- amzn[-train_set,]
amzn$Split <- ifelse(amzn$ID %in% train.df$ID,  "train", "test")

#create test and training corpus 
corp.train <- corpus_subset(amzn.corp, subset= amzn.corp$documents$ID %in% train.df$ID, select=c(class, ID))
corp.test <- corpus_subset(amzn.corp, subset= amzn.corp$documents$ID %in% test.df$ID, select=c(class, ID))

#create dfm with training set and save class labels
train.dfm <- dfm(tokenize(corp.train, removePunct=TRUE),dictionary=sent_dict, groups=corp.train$documents$ID, stem=TRUE, tolower=TRUE)
labelids <- as.data.frame(train.dfm@Dimnames$docs)
names(labelids) <- "ID"
library(plyr)
labeldf <- join(labelids, train.df)
labels <- labeldf$Class


# train NB model with "uniforms" as prior 
nb.dict <- textmodel_NB(x=train.dfm, y=labels, data=NULL,
                   smooth=1, prior="docfreq") 

#create dfm for test to calculate accuracy, precision and recall 
test.dfm <- dfm(tokenize(corp.test, removePunct=TRUE),dictionary=sent_dict, groups=corp.test$documents$ID, stem=TRUE, tolower=TRUE)
test.pr1 <- predict(nb.dict, newdata=test.dfm)

#convert to df and add original class labels 
pred.df <- as.data.frame(test.pr1[1:4])
pred.df$ID <-as.integer(rownames(pred.df))
pred.df <- merge(x=pred.df, y=test.df, by="ID")

#create confusion matrix and add labels
confu_mat2 <- confusionMatrix(pred.df$nb.predicted, pred.df$Class)
confu_mat2 <- confu_mat2$table

confu_mat2

#calculate accuracy 
acc <- sum(confu_mat2[c(1, 4)])/sum(confu_mat2) * 100
paste("Accuracy:", acc, "%", sep="")

#calculate precision 
prec <- round(sum(confu_mat2[4])/sum(confu_mat2[c(4, 3)]) * 100, 2)
paste("Precision:", prec, "%", sep="")

#calculate recall 
recall <- round(sum(confu_mat2[4])/sum(confu_mat2[c(4, 2)]) * 100, 2)
paste("Recall:", recall, "%", sep="")






#--------------------------OLD------------------------------------#

#wordscores 
# anchor.dfm <- dfm(tokenize(corpus_subset(amzn.corp, subset= amzn.corp$documents$anchor == 1), removePunct=TRUE), stem=TRUE, tolower=TRUE, remove=stopwords("english"))
# anchor.dfm <- dfm_select(anchor.dfm, features=all.dfm, selection="keep")
# anchor.dfm <- dfm_smooth(anchor.dfm, smoothing=1)
# neg_anchor.dfm <- dfm(tokenize(corpus_subset(amzn.corp, subset= amzn.corp$documents$anchor_neg == -1), removePunct=TRUE), stem=TRUE, tolower=TRUE, remove=stopwords("english"))
# neg_anchor.dfm <- dfm_select(neg_anchor.dfm , features=all.dfm, selection="keep")
# neg_anchor.dfm <- dfm_smooth(neg_anchor.dfm, smoothing=1)
# possums <- colSums(anchor.dfm)/length(ntoken(anchor.dfm))
# negsums <- colSums(neg_anchor.dfm)/length(ntoken(neg_anchor.dfm))
# 
# pi_pos <- possums/possums+negsums
# pi_neg <- negsums/possums+negsums
# weights <- as.data.frame(pi_pos - pi_neg)
# weights2 <- cbind(weights, rownames(weights))
# weights <- cbind(weights, rownames(weights))
# 
# weights <- weights[with(weights, order(weights$`pi_pos - pi_neg`, decreasing=TRUE)) ,]
# head(weights, 20)
# tail(weights, 20)

# Apply these wordscores to the reviews, and calculate the RankSum statistic (de-
# scribed in Equation 1) of the reviews as scored by wordscores in the same way as
# you did for the dictionaries. AgCain, compute the absolute value of the sum of all of
# the differences in rank for each review. By this metric, which did better: dictionaries
# or wordscores?


#Calculate wordsums per document

# all.dfm_smooth <- dfm_smooth(all.dfm, smoothing=1)
# all.df <- as.data.frame(all.dfm)

#--------------------------OLD------------------------------------#


#Problem 2(d)
#wordscores 

#subset original df to just include extreme anchors
amzn_anch <- amzn[amzn$Anchor_Combo %in% c(1, -1) ,]

#create corpus 
anch.corp <- corpus(amzn_anch$Text, docnames=amzn_anch$ID, docvars=data.frame(class=amzn_anch$Class, anchor_combo = amzn_anch$Anchor_Combo, ID=amzn_anch$ID))

#create dfm 
pos_neg_dfm <- dfm(tokenize(anch.corp, removePunct=TRUE), stem=TRUE, tolower=TRUE, remove=stopwords("english"))

#create vector with labels 
labels <- amzn_anch$Anchor_Combo

#run wordscore model with smoothing factor of 1
ws_smooth<-textmodel(pos_neg_dfm, 
                     y = labels, 
                     model="wordscores", smooth=1)


#create vector of terms and their respective weights
feats<- sort(ws_smooth@Sw, decreasing=TRUE)

#positive features
head(feats, 10) 

#negative features
tail(feats, 10)

amzn.dfm_smooth <- dfm_smooth(amzn.dfm, smoothing=1)
#create wordscore prediction model 
ws_prediction <- predict(ws_smooth, newdata = amzn.dfm_smooth,
        rescaling = "none", level = 0.95, verbose = TRUE) 

#extract desired features 
test <- as.data.frame(cbind(ws_prediction@newdata@Dimnames$docs, ws_prediction@textscores$textscore_raw))
names(test) <- c("ID", "Wordscore")

#merge with original data 
amzn <- merge(x=amzn, y=test, by.x="ID", by.y="ID")
amzn$Wordscore <- as.numeric(as.character(amzn$Wordscore))

#create rank by wordscore 
amzn <- amzn[with(amzn, order(amzn$Wordscore, decreasing=TRUE)) ,]
amzn$Word_Rank <- seq(1:10000)

#compute abs difference between two ranks
rank_sum <- sum(abs(amzn$Word_Rank - amzn$Act_Rank))
rank_sum

#SV is the mean of the scores of the words in V weighted by their term frequency


ws_nosmooth<-textmodel(pos_neg_dfm, 
                       y = labels, 
                       model="wordscores")

#create vector of terms and their respective weights
feats<- sort(ws_smooth@Sw, decreasing=TRUE)

#positive features
head(feats, 10) 

#negative features
tail(feats, 10)


ws_prediction_sm <- predict(ws_nosmooth, newdata = amzn.dfm,
                         rescaling = "none", level = 0.95, verbose = TRUE) 

test <- as.data.frame(cbind(ws_prediction_sm@newdata@Dimnames$docs, ws_prediction_sm@textscores$textscore_raw))
names(test) <- c("ID", "Wordscore_No_Smoothing")


#merge with original data 
amzn <- merge(x=amzn, y=test, by.x="ID", by.y="ID")
amzn$Wordscore_No_Smoothing <- as.numeric(as.character(amzn$Wordscore_No_Smoothing))

#create rank by wordscore 
amzn <- amzn[with(amzn, order(amzn$Wordscore_No_Smoothing, decreasing=TRUE)) ,]
amzn$Word_Rank_No_Smoothing <- seq(1:10000)

#compute abs difference between two ranks
rank_sum <- sum(abs(amzn$Word_Rank_No_Smoothing - amzn$Act_Rank))
rank_sum










#Problem 2(e)

#An advantage of SVMs or Naive Bayes relative to the dictionary or wordscores approach is that you can be more specific with your classifications.
#Whereas the dictionary or wordscores approaches require you to dichotomize everything into "positive" and "negative", you miss picking up on language
#that might fall out of that spectrum, but is equally important to studty. With SVMs, you can train your model to also identify "middle of the road"
#examples and then study their use of language. 

library(NLP)
library(tm)
library(RTextTools)
library(wordcloud)


#create function to use for testing different training sizes 
splt_data <- function(x, kerntype) {
  
  #subset data to just first 1,000 entries for purposes of time 
  amzn_svm <- amzn[1:1000, ]

  #subset data for appropriate training size 
  train.df <- amzn_svm[1:x,]
  
  #create weighted TfIdf matrix with training text 
  train.dtm <- create_matrix(train.df$Text, language="english", stemWords = FALSE,
                             weighting = weightTfIdf, removePunctuation = FALSE)
  
  #create container with document term matrix and appropriate classes for training data 
  container <- create_container(train.dtm, t(train.df$Score), trainSize=1:length(train.df$Score),
                                virgin=FALSE)
  #perform cross validation with 5 folds and the appropriate kernel type 
  cv.svm <- cross_validate(container, nfold=5, algorithm = 'SVM', kernel = kerntype)
  
  #return average accuracy 
  return (cv.svm$meanAccuracy)
  

}


#try with different ratios of test/training

#10/90
train_size <- seq(1:9)
train_size <- as.data.frame(train_size)
kerntype <- 'linear'
lin_time <- system.time(for (i in 1:nrow(train_size)) {
  split <- train_size$train_size[i]*100
  try <- splt_data(split, kerntype)
  train_size$acc_linear[i] <- try })



kerntype <- 'radial'

rad_time <- system.time(for (i in 1:nrow(train_size)) {
  split <- train_size$train_size[i]*100
  try2 <- splt_data(split, kerntype)
  train_size$acc_radial[i] <- try2 })


train_size$train_size <- train_size$train_size * 100 
train_size
lin_time
rad_time 





#Question 3
#First bullet: Is there any nationality that is likely to give statistically significant higher than average ratings? Yes, probably. 
#Because an effort was made to collect responses from a wide variety of nationalities, and the dataset is very small (250) compared to the 
#number of countries that exist, it is very likely that the small sample size per group would lead to some groups giving average scores that 
#differ statistically from the mean score. 

#Second bullet: Yes, an analysis of variance shows that there are statistically significant differences in the average ratings of each
#demographic group. 

#Third bullet: There is only a statistical difference if you choose an alpha of > .05, which I would not in this case, because of the 
#sensitive nature of the null hypothesis. 

#Fourth bullet: I would probably ask them to specify their race - as it would be helpful to know if there is some effect of "rater bias" as far as 
#the race of the rater is concerned. With that information, we might be able to control for that issue. 
#I would also ask them to specify their trust of politicians as a whole. In the case that the sample of politician race is not equally 
#represented, it would be helpful to know what proportion of trustworthiness corresponds to "race" and what proportion is due to that rater's 
#inherent distrust of politicians in general. 


#read in data

hit <- read.csv("CF_rate_trustworthiness.csv")

hit$demo_group <- as.factor(gsub("[0-9]", "", hit$image_name))

#create ANOVA model for checking differences between groups
model <- lm(rating ~ demo_group, data=hit)
demo_anova <- anova(model)
summary(demo_anova)

#create ANOVA model for just women and men 

hit$gender <- gsub("black", "", hit$demo_group)
hit$gender <- gsub("white", "", hit$gender)


model_gen <- lm(rating ~ gender, data=hit)
gen_anova <- anova(model_gen)
summary(gen_anova)
gen_anova



