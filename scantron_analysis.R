##########################################################
#              Scantron Pattern Analysis                 #
#                 W241 Final Project                     #
#                Brian, Nikki, Priya                     #
##########################################################

# set working directory to the github folder
setwd("~/Documents/MIDS/W241/scantronpatterns")

wd = getwd()
d = read.csv(paste(wd,"scantron_scores.csv",sep="/"), header=T)
head(d)


#==============================#
#           CLEANING           #
#==============================#

sapply(d, class)
d$Group = as.factor(d$Group)

# renaming questions with Q prefix
names(d)
names(d)[22:41] = paste("Q",1:20,sep="")
names(d)

# get just first letter of answers & make factors
d[,22:41] = sapply(d[,22:41], function(x) print(substr(x,1,1)))
for (i in 22:41) {d[,i] = as.factor(d[,i])}


#==============================#
#              EDA             #
#==============================#

# number of responses in each group
table(d$Group)

# plot distribution of scores by group (Figure 4.2)
boxplot(final_score ~ Group, data=d, main="Test Scores by Treatment Group", 
        xlab="Treatment Group", ylab="# Answers Correct Out of 20",
        col = brewer.pal(3, 'Set2'))
text(1,9.5, round(mean(d[d$Group==1,]$final_score),2))
text(2,8.5, round(mean(d[d$Group==2,]$final_score),2))
text(3,9.5, round(mean(d[d$Group==3,]$final_score),2))

# group 1 has the smallest variance - people taking it with
# no patterns ends up with more similar scores?

# group 2 and 3 have higher averages but larger variances -
# are some people catching onto the pattern and some people are being
# thrown off of it? would potentially explain such a high variance?

# print means
for (i in c(1,2,3)) {
  print(mean(d[d$Group==i,]$final_score))
}


#==============================#
#     RANDOMIZATION CHECKS     #
#==============================#

# IS GENDER RANDOMLY DISTRIBUTED ACROSS GROUPS? (Figure 5.1) 
d$GroupNumeric = as.numeric(as.character(d$Group))
m1 = lm(GroupNumeric ~ G1, data = d)
summary(m1)
# gender is not related to the group = random!

for (i in c("Male","Female")) {
  print(mean(d[d$G1==i,]$final_score))
}

# IS EDUCATION RANDOMLY DISTRIBUTED ACROSS GROUPS? (Figure 5.2a)
m_ed = lm(GroupNumeric ~ G2, data = d)
summary(m_ed)
# looks like master's degree might be somewhat related
# professional/masters seem very similar though

# combine some of the masters-like education levels together
# Figure 5.2b
d$G2b = d$G2
levels(d$G2b) = c(levels(d$G2b), "Masters/Professional")
d$G2b[d$G2b %in% c("Master's degree","Professional degree (JD, MD)")] = "Masters/Professional"
m_ed2 = lm(GroupNumeric ~ G2b, data=d)
summary(m_ed2)

# IS AGE RANDOMLY DISTRIBUTED ACROSS GROUPS? (Figure 5.3)
m_age = lm(GroupNumeric ~ G3, data = d)
summary(m_age)


#==============================#
#            RESULTS           #
#==============================#

# IS THE FINAL SCORE RELATED TO GROUP
m2 = lm(final_score ~ Group + G1 + G2b + G3, data=d)
summary(m2)
# no significance of treatment group except education

# look at education level on final score
d$G2b = factor(d$G2b, levels=levels(d$G2b)[c(4,7,1,2,8,3)])
levels(d$G2b) = c("High school","Some college","Associate college (2 years)","Bachelor's college (4 years)","Masters/Professional (JD,MD)","Doctoral")
par(mai=c(3,1,.5,0.3))
plot(d$final_score ~ d$G2b, data=d, main="Test Scores by Education Level", 
     xlab="Education Level", ylab="# Answers Correct Out of 20",
     col = brewer.pal(6, 'PuBuGn'), cex.axis=.8)

# averages
sapply(levels(d$G2b), function(x) mean(d$final_score[d$G2b==x]))



# T-TESTS TO COMPARE TREATMENT 2 & 3 WITH THE CONTROL GROUP
# BONFERRONI TO CONTROL FOR MULTIPLE T-TESTS (2)
# new p-value needed for significance = .05/2 = .025
t.test(final_score ~ Group, data=d, subset = (d$Group %in% c(1,2)))
t.test(final_score ~ Group, data=d, subset = (d$Group %in% c(1,3)))
# doesn't matter - it's so not significant at p-values of ~0.26 and ~0.95
