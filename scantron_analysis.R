
# set working directory to the github folder
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
names(d)[22:41] = paste("Q",1:20,sep="")
names(d)

# get just first letter of answers & make factors
d[,22:41] = sapply(d[,22:41], function(x) print(substr(x,1,1)))
for (i in 22:41) {d[,i] = as.factor(d[,i])}

################
###    EDA   ###
################

# number of responses in each group
table(d$Group)

# plot distribution of scores by group
plot(d$Group, d$final_score)
# group 1 has the smallest variance - people taking it with
# no patterns ends up with more similar scores?

# group 2 and 3 have higher averages but larger variances -
# are some people catching onto the pattern and some people are being
# thrown off of it? would potentially explain such a high variance?

# print means
for (i in c(1,2,3)) {
  print(mean(d[d$Group==i,]$final_score))
}


###################################
# EXPLORE PATTERNS IN DEMOGRAPHICS
###################################

# GENDER
d$GroupNumeric = as.numeric(as.character(d$Group))

m1 = lm(GroupNumeric ~ G1, data = d)
summary(m1)
# gender is not related to the group

for (i in c("Male","Female")) {
  print(mean(d[d$G1==i,]$final_score))
}



# GROUP
lm_group = lm(final_score ~ Group, data = d)
summary(lm_group)





