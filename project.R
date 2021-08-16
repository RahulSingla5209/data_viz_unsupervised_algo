# r exploratory script for into to ml part 2
library(dplyr)
library(ggplot2)
library(mosaic)
library(quantmod)
library(foreach)
library(LICORS) 
library(cluster)
library(tm) 
library(tidyverse)
library(slam)
library(proxy)
library(e1071)
library(arules)
library(arulesViz)

##################################################
################ Problem 1: Green buildings ######
##################################################

green.buildings = read.csv('greenbuildings.csv')
summary(green.buildings)
dim(green.buildings)
# removing occupancy less than 10% to be inline with the analysis done 
green.buildings = subset(green.buildings, leasing_rate >= 10)
dim(green.buildings)

rent_agg = green.buildings %>% group_by(green_rating) %>% summarise(m = median(Rent))
colnames(rent_agg) = c('green_rating', 'median_rent')
rent_agg$green_rating = as.character(rent_agg$green_rating)

ggplot(data=rent_agg, aes(x=green_rating, y=median_rent, fill=green_rating)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('Median rent of green and non-green buildings')

# trying to look at the rent of a green building with its cluster median rent
# since we dont know the cluster it might be good to look at how economical well of
# a green building is w.r.t cluster
# 
# green.buildings_green = subset(green.buildings, green_rating==1,
#                                select=c(cluster, Rent, class_a, class_b, age, stories, leasing_rate, cluster_rent))
# green.buildings_nongreen = subset(green.buildings, green_rating==0,
#                                   select=c(cluster, Rent, class_a, class_b, age, stories, leasing_rate, cluster_rent))
# 
# summary(lm(Rent ~ cluster_rent - 1, green.buildings_green))


# initially just look at all the rows

# what i want to see - as age progress how does that affects the price of not-renovated buidling 
non_renovated = subset(green.buildings, renovated==0,
                       select=c(Rent, age, cluster, green_rating))
non_renovated['age_buckets'] = ifelse(non_renovated$age <= 5, '0-5', 
                                      ifelse(non_renovated$age <= 10, '05-10',
                                             ifelse(non_renovated$age <= 20, '10-20',
                                                    ifelse(non_renovated$age <= 30, '20-30', '30+'))))
non_renovated_sum = non_renovated %>% group_by(age_buckets, green_rating) %>% summarize(median(Rent))
colnames(non_renovated_sum) = c('age_bucket', 'green_rating', 'median_rent')
non_renovated_sum$green_rating = as.character(non_renovated_sum$green_rating)

ggplot(data=non_renovated_sum, aes(x=age_bucket, y=median_rent, fill=green_rating)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('Median rent of non-renovated green and non-green buildings by age')

# what i want to see - as age progress how does that affects the price of not-renovated building 
# along with class a and with amenities
non_renovated_a_am = subset(green.buildings, (renovated==0 & class_a==1 & amenities==1),
                       select=c(Rent, age, cluster, green_rating))

non_renovated_a_am['age_buckets'] = ifelse(non_renovated_a_am$age <= 5, '0-5', 
                                      ifelse(non_renovated_a_am$age <= 10, '05-10',
                                             ifelse(non_renovated_a_am$age <= 20, '10-20',
                                                    ifelse(non_renovated_a_am$age <= 30, '20-30', '30+'))))
non_renovated_a_am_sum = non_renovated_a_am %>% group_by(age_buckets, green_rating) %>% summarize(median(Rent))
colnames(non_renovated_a_am_sum) = c('age_bucket', 'green_rating', 'median_rent')
non_renovated_a_am_sum$green_rating = as.character(non_renovated_a_am_sum$green_rating)

ggplot(data=non_renovated_a_am_sum, aes(x=age_bucket, y=median_rent, fill=green_rating)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('Median rent of non-renovated, class A and with amenities green and non-green buildings by age')


non_renovated_a_am_cnt = non_renovated_a_am %>% group_by(green_rating) %>% count(age_buckets) 

ggplot(data=non_renovated_a_am_cnt, aes(x=age_buckets, y=n, fill=green_rating)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('Count rent of non-renovated, class A and with amenities green and non-green buildings by age')


# Break even calculation
area = 250000
investment = 100000000
green.premium = 0.05

# getting median non-green and green rent for 30 year
non_green_rent = median(subset(green.buildings, (green_rating == 0 & age <= 30 & renovated==0 & class_a==1 & amenities==1), 
                          select = Rent)[, 1])
green_rent = median(subset(green.buildings, (green_rating == 1 & age <= 30 & renovated==0 & class_a==1 & amenities==1), 
                          select = Rent)[, 1])

paste0('Non-green break even years - ', round(investment/area/non_green_rent, 2))
paste0('Green break even years - ', round(investment*(1 + green.premium)/area/green_rent, 2))

# getting extra rent for green buildings incremental by age group
stacked = base::merge(subset(non_renovated_a_am_sum, green_rating == 0), 
      subset(non_renovated_a_am_sum, green_rating == 1),
      by = c('age_bucket'))
stacked['non_green_rent'] = area*stacked['median_rent.x']
stacked['green_rent'] = area*stacked['median_rent.y']
stacked = subset(stacked, age_bucket != '30+')
stacked['rent_diff'] = stacked['green_rent'] - stacked['non_green_rent']

ggplot(data=stacked, aes(x=age_bucket, y=rent_diff,)) +
  geom_bar(stat="identity") +
  ggtitle('Different in rent of green vs non green buildings by differnet')

# over the lifetime of the building, assumed to be 30 years, green building
# won't be able to break even before the non-green building, as overall
# expected rent diff (based on the median rent of the subset) 
# is `r round(sum(stacked['rent_diff'])/1000000, 2)` negative 


##################################################
################ Problem 2: Airport data #########
##################################################

airport = read.csv('ABIA.csv')
head(airport)
dim(airport)
summary(airport)

# arrival and departure delays are available for inbound and outbound
# flights from austin

# creating variables total time - schd dept to actual arrival
airport$actual.journey.time = airport$CRSElapsedTime + airport$ArrDelay

# creating variable AUS in/out- inbound/outbound
airport$AUS = ifelse(airport$Origin == 'AUS', 'Outbound', 'Inbound')


# looking at Month vs delay
month_delay = airport %>% group_by(Month) %>% select(ArrDelay) %>% summarize(mean_delay = mean(ArrDelay, na.rm = TRUE))
ggplot(month_delay, aes(x=Month, y=mean_delay)) + geom_bar(stat='identity')

# looking at Month vs delay
carrier_delay = airport %>% group_by(UniqueCarrier) %>% select(ArrDelay) %>% summarize(mean_delay = mean(ArrDelay, na.rm = TRUE))
ggplot(carrier_delay, aes(x=UniqueCarrier, y=mean_delay)) + geom_bar(stat='identity')

# Inbound vs Outbound
inb_outb = airport %>% group_by(AUS) %>% select(ArrDelay) %>% summarize(mean_delay = mean(ArrDelay, na.rm = TRUE))
ggplot(inb_outb, aes(x=AUS, y=mean_delay)) + geom_bar(stat='identity')

# Visualization description - what i want to see
# month of the year vs arrival and departure, 
# with austin
# what is the average/distribution delay per 1000 mile
# by airline


grouped = airport %>% group_by(Month, UniqueCarrier, AUS) %>% select(ArrDelay) %>% summarize(mean_delay = mean(ArrDelay, na.rm = TRUE), count=length(ArrDelay))
# grouped = subset(grouped, count > 10)

grouped$Month = as.factor(grouped$Month)
ggplot(grouped, aes(x=Month, y=mean_delay, fill=AUS)) + 
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(. ~ UniqueCarrier)  

##################################################
################ Problem 3: Portfolio ############
##################################################
initial.capital = 100000

# Import a ETF data
etf = c('VOE', 'VLUE', 'IJS', 'ARKK')
getSymbols(etf)

# Adjust for splits and dividends
VOEa = adjustOHLC(VOE)
VLUEa = adjustOHLC(VLUE)
IJSa = adjustOHLC(IJS)
ARKKa = adjustOHLC(ARKK)

# Look at close-to-close changes
# Combine close to close changes in a single matrix
all_returns = cbind(ClCl(VOEa),ClCl(VLUEa),ClCl(IJSa), ClCl(ARKKa))
head(all_returns)

# taking last 5 years data
history = 5
end_date = Sys.Date()
start_date = end_date - history*365
date.range = seq(from=start_date, to=end_date, by='day')

# sub-setting the data based on history
all_returns = as.matrix(all_returns[date.range])

# returns plots
pairs(data.frame(all_returns))

# different portfolio strategy
# 1. equal weight
# 2. mean returns ration weighted
# 3. average volume weighted


# portfolio weights for 1.
portfolio1 = rep(1/length(etf), length(etf))


# portfolio weights for 2.
avg_return = apply(all_returns, 2, mean)
portfolio2 = avg_return/sum(avg_return)

# portfolio weights for 3.
avg_volume = c(mean(VOEa$VOE.Volume[date.range], na.rm=TRUE), 
               mean(VLUEa$VLUE.Volume[date.range], na.rm=TRUE),
               mean(IJSa$IJS.Volume[date.range], na.rm=TRUE),
               mean(ARKKa$ARKK.Volume[date.range], na.rm=TRUE))
portfolio3 = avg_volume/sum(avg_volume)

# bootstrapping for portfolio 1
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
  total.capital = initial.capital
  n_days = 20
  wealthtracker1 = rep(0, n_days)

  for(today in 1:n_days) {
    holdings1 = portfolio1 * total.capital
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    
    holdings1 = holdings1 + holdings1*return.today
    
    total.capital = sum(holdings1)
    wealthtracker1[today] = total.capital
  }
  wealthtracker1
}


# bootstrapping for portfolio 2
sim2 = foreach(i=1:5000, .combine='rbind') %do% {
  total.capital = initial.capital
  n_days = 20
  wealthtracker2 = rep(0, n_days)
  
  for(today in 1:n_days) {
    holdings2 = portfolio2 * total.capital
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    
    holdings2 = holdings2 + holdings2*return.today
    
    total.capital = sum(holdings2)
    wealthtracker2[today] = total.capital
  }
  wealthtracker2
}


# bootstrapping for portfolio 3
sim3 = foreach(i=1:5000, .combine='rbind') %do% {
  total.capital = initial.capital
  n_days = 20
  wealthtracker3 = rep(0, n_days)
  
  for(today in 1:n_days) {
    holdings3 = portfolio3 * total.capital
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    
    holdings3 = holdings3 + holdings3*return.today
    
    total.capital = sum(holdings3)
    wealthtracker3[today] = total.capital
  }
  wealthtracker3
}

# histogram of 20 day returns
hist(sim1[,n_days]- initial.capital, breaks=30)
hist(sim2[,n_days]- initial.capital, breaks=30)
hist(sim3[,n_days]- initial.capital, breaks=30)

# mean returns after 20 days
avg_return1 = mean(sim1[, n_days] - initial.capital)
avg_return2 = mean(sim2[, n_days] - initial.capital)
avg_return3 = mean(sim3[, n_days] - initial.capital)

# 5% value at risk:
var1 = quantile(sim1[,n_days]- initial.capital, prob=0.05)
var2 = quantile(sim2[,n_days]- initial.capital, prob=0.05)
var3 = quantile(sim3[,n_days]- initial.capital, prob=0.05)

print(c(avg_return1, var1))
print(c(avg_return2, var2))
print(c(avg_return3, var3))

# second strategy - high reward, medium risk


##################################################
############ Problem 4: Market segmentation ######
##################################################
segment.data = read.csv('social_marketing.csv')
head(segment.data)
summary(segment.data)

# removing variables not required
segment.data = subset(segment.data, select = -c(spam, chatter, uncategorized))

# PCA
pc = prcomp(segment.data[1:nrow(segment.data),2:ncol(segment.data)], scale=TRUE, rank=20)
loadings = pc$rotation
scores = pc$x

summary(pc)

# data is varies in X need atleast 16 PC to cover 80% variance hence using the columns as it is

# not using hclust since its segmentation, and each cluster should be exclusive

# Center and scale the data
# X = scale(segment.data[, -1], center=TRUE, scale=TRUE)
X = segment.data[, -1]

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")

# Using kmeans++ initialization for elbow curve
k_grid = seq(2, 30, by=1)
SSE_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = LICORS::kmeanspp(X, k, nstart=25)
  cluster_k$tot.withinss
}

plot(k_grid, SSE_grid)

# calculating CH index
N = nrow(X)
CH_grid = foreach(k = k_grid, .combine='c') %do% {
  cluster_k = LICORS::kmeanspp(X, k, nstart=25)
  W = cluster_k$tot.withinss
  B = cluster_k$betweenss
  CH = (B/W)*((N-k)/(k-1))
  CH
}

plot(k_grid, CH_grid)
# 6 clusters seem like a good idea

# calculating gap statistics
# gap_stat = clusGap(x=X, FUNcluster=kmeanspp, K.max=10, B=50, nstart=25)
set.seed(123)
final_cluster = LICORS::kmeanspp(X, 6, nstart=25)

cluster_mean = t(data.frame(final_cluster$centers))
cluster_mean

# 20% cutoff  is taken 
cluster1 = cluster_mean[abs(cluster_mean[,1]) >=  0.2*max(abs(cluster_mean[,1])), 1]
barplot(cluster1)
cluster2 = cluster_mean[abs(cluster_mean[,2]) >=  0.2*max(abs(cluster_mean[,2])), 2]
barplot(cluster2)
cluster3 = cluster_mean[abs(cluster_mean[,3]) >=  0.2*max(abs(cluster_mean[,3])), 3]
barplot(cluster3)
cluster4 = cluster_mean[abs(cluster_mean[,4]) >=  0.2*max(abs(cluster_mean[,4])), 4]
barplot(cluster4)
cluster5 = cluster_mean[abs(cluster_mean[,5]) >=  0.2*max(abs(cluster_mean[,5])), 5]
barplot(cluster5)
cluster6 = cluster_mean[abs(cluster_mean[,6]) >=  0.2*max(abs(cluster_mean[,6])),  6]
barplot(cluster6)

hist(final_cluster$cluster)


##################################################
############ Problem 5: Author Attribution #######
##################################################

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en')}

## apply to all of Simon Cowell's articles
## (probably not THE Simon Cowell: https://twitter.com/simoncowell)
## "globbing" = expanding wild cards in filename paths
file_list = Sys.glob('ReutersC50/C50train/*/*.txt')
file_read = lapply(file_list, readerPlain) 

file_read

# Clean up the file names
# no doubt the stringr library would be nicer here.
# this is just what I hacked together
mynames = file_list %>%
  { strsplit(., '/', fixed=TRUE) } %>%
  { lapply(., tail, n=2) } %>%
  { lapply(., paste0, collapse = '') } %>%
  unlist

# Rename the articles
mynames
names(file_read) = mynames

## once you have documents in a vector, you 
## create a text mining 'corpus' with: 
documents_raw = Corpus(VectorSource(file_read))

## Some pre-processing/tokenization steps.
## tm_map just maps some function to every document in the corpus
my_documents = documents_raw %>%
  tm_map(content_transformer(tolower))  %>%             # make everything lowercase
  tm_map(content_transformer(removeNumbers)) %>%        # remove numbers
  tm_map(content_transformer(removePunctuation)) %>%    # remove punctuation
  tm_map(content_transformer(stripWhitespace))          # remove excess white-space

## Remove stopwords.  Always be careful with this: one person's trash is another one's treasure.
# 2 example built-in sets of stop words
stopwords("en")
stopwords("SMART")
# let's just use the "basic English" stop words
my_documents = tm_map(my_documents, content_transformer(removeWords), stopwords("en"))


## create a doc-term-matrix from the corpus
dtm = DocumentTermMatrix(my_documents)
dtm # some basic summary statistics
inspect(dtm[1:10,1:20])


dtm = removeSparseTerms(dtm, 0.98)
dtm

# construct TF IDF weights -- might be useful if we wanted to use these
# as features in a predictive model
# tfidf_dtm = weightTfIdf(dtm)

# creating test set
X = data.frame(as.matrix(dtm))
y = rep(1:50,each=50)
dim(X)

# building a naive bayes model

# AP's multinomial probability vector
# Notice the smoothing factor
# Why?
smooth_count = 1/nrow(X)
nb_model = naiveBayes(x=X, y=y, laplace = smooth_count)
train_predict = predict(nb_model, X)

sum(train_predict == y)/length(y)

# test set
file_list = Sys.glob('ReutersC50/C50test/*/*.txt')
file_read = lapply(file_list, readerPlain) 

mynames = file_list %>%
  { strsplit(., '/', fixed=TRUE) } %>%
  { lapply(., tail, n=2) } %>%
  { lapply(., paste0, collapse = '') } %>%
  unlist

names(file_read) = mynames

documents_raw = Corpus(VectorSource(file_read))

my_documents = documents_raw %>%
  tm_map(content_transformer(tolower))  %>%             # make everything lowercase
  tm_map(content_transformer(removeNumbers)) %>%        # remove numbers
  tm_map(content_transformer(removePunctuation)) %>%    # remove punctuation
  tm_map(content_transformer(stripWhitespace))          # remove excess white-space

stopwords("en")
stopwords("SMART")

my_documents = tm_map(my_documents, content_transformer(removeWords), stopwords("en"))

dtm = DocumentTermMatrix(my_documents)
dtm # some basic summary statistics
inspect(dtm[1:10,1:20])

X_test = data.frame(as.matrix(dtm))
y_test = rep(1:50,each=50)
dim(X_test)

X_test = X[, colnames(X)]
dim(X_test)

nb_model = naiveBayes(x=X_test, y=y_test, laplace = smooth_count)
test_predict = predict(nb_model, X_test)
accuracy = sum(test_predict == y_test)/length(y_test)
accuracy


##################################################
############ Problem 6: Associations Rule ########
##################################################
groceries = read.transactions('groceries.txt', sep = ',')
groceries
x = summary(groceries)
barchart(x@itemSummary[1:5])

grocery_rules = apriori(groceries, 
                     parameter=list(support=.005, confidence=.1, maxlen=5))

plot(grocery_rules, measure = c("support", "lift"), shading = "confidence")

grocery_rules_subset = inspect(subset(grocery_rules, subset=lift > 2 & support < 0.2))
  
# plot(grocery_rules, method='two-key plot')
summary(grocery_rules_subset)
plot(grocery_rules, method = 'graph')

saveAsGraph(grocery_rules_subset, file = "grocery.graphml")

