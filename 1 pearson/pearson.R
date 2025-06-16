install.packages("corrplot")
library(corrplot)


data <- read.csv("pearson.csv", header = TRUE)


data[,13] <- as.numeric(data[,13])


names(data) <- c("enP","lsd", "asd", "Vm", "d", "m", "GII", "Xz",  "iec", "r", "blm", "blsd","Vi", "pd", "t", "tt", "u", "tu", "ttu", "n", "nn", "pm", "pi", "ppv", "sm","ssd","com","cosd","deB", "covd")


p.xg <- cor(data)


write.csv(p.xg,"pccresult.csv")


pcc_threshold = 0.8
m <- which(p.xg > pcc_threshold & lower.tri(p.xg), arr.ind = TRUE)
write.csv(m, file = paste("pcc.csv", pcc_threshold, ".csv", sep = ''))


corrplot(p.xg, type = "upper",
         tl.cex = 1.2, 
         cl.cex = 1.3, cl.align.text = "l")

cex.before <- par("cex") 
par(cex = 0.4) 
p <- corrplot(corr=p.xg, type = "full", method = "color")

corrplot(p.xg, type = "upper",
         tl.cex = 1.2, 
         cl.cex = 1.3, cl.align.text = "l")

ls()
dsamp <- diamonds[sample(nrow(diamonds), 2000), ]
qplot(carat, price, data=dsamp, colour=clarity)
savePlot(filename = "Rplot",
         type ="wmf",
         restoreConsole = TRUE)



# help(corrplot)


corrplot.mixed(p.xg)  
corrplot.mixed(p.xg, lower = "color", upper = "ellipse",outline = TRUE, mar = c(1, 1, 1, 1),
               tl.cex = 0.95, number.font = 1,
               cl.cex = 1.5,cl.align.text = "l")

