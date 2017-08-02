
setwd("~/Documents/MIDS/W241/scantronpatterns")
wd = getwd()
d = read.csv(paste(wd,"scantron_scores.csv",sep="/"), header=T)
head(d)


################
### CLEANING ###
################

sapply(d, class)
d$Group = as.factor(d$Group)

# renaming questions with Q prefix
names(d)
names(d)[19:38] = paste("Q",1:20,sep="")
names(d)

# get just first letter of answers & make factors
d[,19:38] = sapply(d[,19:38], function(x) print(substr(x,1,1)))
for (i in 19:38) {d[,i] = as.factor(d[,i])}

################
###    EDA   ###
################

# plot distribution of scores by group
plot(d$Group, d$Final_Score)
# group 1 has the smallest variance - people taking it with
# no patterns ends up with more similar scores?

# group 2 and 3 have higher averages but larger variances -
# are some people catching onto the pattern and some people are being
# thrown off of it? would potentially explain such a high variance?


# CACE


