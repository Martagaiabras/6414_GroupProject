set.seed(1)
split = sample.split(dat.reduced_2$`Churn Value`, SplitRatio = 0.75)
train = subset(dat.reduced_2, split == TRUE)
test = subset(dat.reduced_2, split == FALSE)