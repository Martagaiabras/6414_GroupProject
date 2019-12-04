library("dplyr")
library("tidyr")

#type in the confusion table values

hm <- readr::read_delim("y actual_false actual_true
pred_false 1220 71 
pred_true 284 183
", delim=" ")

hm <- hm %>% gather(x, value, actual_false:actual_true)

ggplot(hm, aes(x=x, y=y, fill=value)) + geom_tile() +geom_text(aes(label=value), color="white") 
