# Load libraries and functions

library(ggplot2)
library(reshape2)

# This'multiplot' lets plots' layout to be a matrix
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Load and preprocess data
setwd("E:/Project/MHLP")
Data <- read.csv("DATA_TE1557.csv",sep=',')

# Transform 'DATE' into date type
Data$DATE <- as.Date(Data$DATE) 

# Delete samples by running time
Data <- Data[which(Data$ASU.RUNNING.TIME > 23.99),]

# Delete useless attributes
myvars <- names(Data) %in% c("X","ASU.RUNNING.TIME") 
Data_1 <- Data[!myvars]

# Creat graphics
# Bulit a dataframe for plotting
dP1 <- melt(Data_1, id.vars=c("DATE"), 
           measure.vars=c("A","B","C","D","E","F","G","H"), 
           variable.name="TUNNEL", 
           value.name="TEMPERATURE")

# First plot - line 
p1 <- ggplot(dP1, aes(x=DATE, y=TEMPERATURE, colour=TUNNEL, group=TUNNEL)) + 
      geom_line() + ggtitle("Line curve")

# Second plot - fitted
p2 <- ggplot(dP1, aes(x=DATE, y=TEMPERATURE, colour=TUNNEL)) + 
      geom_point(alpha=.3) + 
      geom_smooth(alpha=.2, size=1) + 
      ggtitle("Fitted curve")

# Third plot - density
p3 <- ggplot(dP1, aes(x=TEMPERATURE, colour=TUNNEL)) +
      geom_density() + 
      ggtitle("Density curve")

# Fourth plot - histogram
p4 <- ggplot(dP1, aes(x=TEMPERATURE, fill=TUNNEL)) +
      geom_histogram(colour="black", binwidth=2) +
      facet_grid(TUNNEL ~ .) +
      ggtitle("Histogram") +
      theme(legend.position="none")

# Plot a matrix of four graphics
multiplot(p1, p2,p3,p4,cols=2)

# Correlation analysis for whole data
Cor_Data_1 <- cor(Data_1[-1])

# Add a column of the gap between maximum and minimum temperature
Cache <- data.frame(DATE=Data_1$DATE,GAP=apply(Data_1[,2:9],1,function(x) max(x)-min(x)))
Data_2 <- merge(Data_1,Cache,by="DATE")

dP2 <- melt(with(Data_2,data.frame(DATE,LPGAN_T=LPGAN.TEMPERATURE,GAP)),
            id.vars=c("DATE"), 
            measure.vars=c("LPGAN_T","GAP"), 
            variable.name="TYPE", 
            value.name="TEMPERATURE")

# Fiveth plot
p5 <- ggplot(dP2, aes(x=DATE, y=TEMPERATURE, colour=TYPE)) + 
      geom_point(alpha=.3) + 
      geom_smooth(alpha=.2, size=1) + 
      ggtitle("Fitted curve")

p6 <- ggplot(dP2, aes(x=TEMPERATURE, fill=TYPE)) +
      geom_histogram(colour="black", binwidth=2) +
      facet_grid(TYPE ~ .) +
      ggtitle("Histogram") +
      theme(legend.position="none")

multiplot(p5,p6,cols=2)

Cor_Data_2 <- cor(with(Data_2,data.frame(GAP,TOTAL.AIR,LPGAN.TEMPERATURE)))

# Built a dataframe contains of date, year(factor),month(factor), 
# unit's loading(facrot), mean and gap.
attach(Data_1)
Data_3 <- data.frame(DATE=DATE,
                     YEAR=sapply(DATE,
                                 function(x) factor(data.frame(Y=strsplit(as.character.Date(x),split ="-"))[1,1])),
                     MONTH=sapply(DATE,
                                 function(x) factor(months(x))),
                     
                     # Loading is calculated  divied into 20 levels aginst to design.
                     LOADING=sapply(TOTAL.AIR,
                                 function(x) factor(round(x*10*2/315625))),
                     MEAN=apply(Data_1[,2:9],1,mean),
                     GAP=apply(Data_1[,2:9],1,
                               function(x) max(x)-min(x)),
                     Data_1[,2:9])
detach(Data_1)

# ANOVA with one, two and three factors separately
attach(Data_3)
fitGtoY <- aov(GAP~YEAR)
fitMtoY <- aov(MEAN~YEAR)
fitGtoML <- aov(GAP~MONTH*LOADING)
fitMtoML <- aov(MEAN~MONTH*LOADING)
fitGtoMLY <- aov(GAP~MONTH*LOADING*YEAR)
fitMtoMLY <- aov(MEAN~MONTH*LOADING*YEAR)
detach(Data_3)

# Print final results
print("Gap ~ Year")
print(summary(fitGtoY))
print("Mean ~ Year")
print(summary(fitMtoY))
print("Gap ~ Month and Loading ")
print(summary(fitGtoML))
print("Mean ~ Month and Loading ")
print(summary(fitMtoML))
print("Gap ~ Month and Loading and Year ")
print(summary(fitGtoMLY))
print("Mean ~ Month and Loading and Year ")
print(summary(fitMtoMLY))