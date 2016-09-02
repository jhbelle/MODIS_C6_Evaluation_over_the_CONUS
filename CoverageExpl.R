## ------------
## Name: CoverageExpl.R
## Program version: R 3.1.0
## Dependencies: NA
## Purpose: Look into predictors of high/low coverage
## ------------
Grid <- read.csv("H:/Rotation_Yang/XueFei_grid/GridAgg.csv")[,3:12]
Winter <- read.csv("H:/Rotation_Yang/AggDatDecade12.csv")
Winter <- merge(Grid, Winter, by.x="Input_FID", by.y="ID")
Winter$Pop <- as.numeric(Winter$Pop)*1
Spring <- read.csv("H:/Rotation_Yang/AggDatDecade3.csv")
Spring <- merge(Grid, Spring, by.x="Input_FID", by.y="ID")
Spring$Pop <- as.numeric(Spring$Pop)*1
Summer <- read.csv("H:/Rotation_Yang/AggDatDecade6.csv")
Summer <- merge(Grid, Summer, by.x="Input_FID", by.y="ID")
Summer$Pop <- as.numeric(Summer$Pop)*1
Fall <- read.csv("H:/Rotation_Yang/AggDatDecade9.csv")
Fall <- merge(Grid, Fall, by.x="Input_FID", by.y="ID")
Fall$Pop <- as.numeric(Fall$Pop)*1
#Spring$MeanElev <- as.numeric(Spring$MeanElev)*1
#Spring$STDElev <- as.numeric(Spring$STDElev)*1
#Spring$MeanSlope <- as.numeric(Spring$MeanSlope)*1

Vals <- c(sum(Winter$Pop*Winter$PCovDT123)/sum(Winter$Pop), sum(Winter$Pop*Winter$PCovDB123)/sum(Winter$Pop), sum(Winter$Pop*Winter$PCovB123)/sum(Winter$Pop), sum(Winter$Pop*Winter$PCovDT3)/sum(Winter$Pop), sum(Winter$Pop*Winter$PCovDB3)/sum(Winter$Pop), sum(Winter$Pop*Winter$PCovB3)/sum(Winter$Pop), sum(Spring$Pop*Spring$PCovDT123)/sum(Spring$Pop), sum(Spring$Pop*Spring$PCovDB123)/sum(Spring$Pop), sum(Spring$Pop*Spring$PCovB123)/sum(Spring$Pop), sum(Spring$Pop*Spring$PCovDT3)/sum(Spring$Pop), sum(Spring$Pop*Spring$PCovDB3)/sum(Spring$Pop), sum(Spring$Pop*Spring$PCovB3)/sum(Spring$Pop), sum(Summer$Pop*Summer$PCovDT123)/sum(Summer$Pop), sum(Summer$Pop*Summer$PCovDB123)/sum(Summer$Pop), sum(Summer$Pop*Summer$PCovB123)/sum(Summer$Pop), sum(Summer$Pop*Summer$PCovDT3)/sum(Summer$Pop), sum(Summer$Pop*Summer$PCovDB3)/sum(Summer$Pop), sum(Summer$Pop*Summer$PCovB3)/sum(Summer$Pop), sum(Fall$Pop*Fall$PCovDT123)/sum(Fall$Pop), sum(Fall$Pop*Fall$PCovDB123)/sum(Fall$Pop), sum(Fall$Pop*Fall$PCovB123)/sum(Fall$Pop), sum(Fall$Pop*Fall$PCovDT3)/sum(Fall$Pop), sum(Fall$Pop*Fall$PCovDB3)/sum(Fall$Pop), sum(Fall$Pop*Fall$PCovB3)/sum(Fall$Pop))
Seasons <- c(rep("Winter", 6), rep("Spring", 6), rep("Summer", 6), rep("Fall", 6))
Seasons2 <- c(rep(4, 6), rep(1, 6), rep(2, 6), rep(3, 6))
Type <- rep(c(rep("123", 3), rep("3", 3)), 4)
Alg <- rep(c("DT", "DB", "C"), 8)
Dat <- cbind.data.frame(Vals, Seasons2, Type, Alg)
library(ggplot2)
scale_x_discrete("Season", limits=c("Spring", "Summer", "Fall", "Winter")) 
ggplot(Dat, aes(x=Seasons2, y=Vals*100, linetype=Alg)) + geom_line() + ylim(c(0, 100)) + ylab("Population-weighted coverage (%)") + scale_x_discrete("Season", labels=c("Spring", "Summer", "Fall", "Winter")) + facet_grid(.~Type) + theme_classic()
