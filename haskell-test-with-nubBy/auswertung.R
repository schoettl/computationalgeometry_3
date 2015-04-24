library(ggplot2)

data = read.csv("haskell-times-sec.csv", sep = ";", header = FALSE)
names(data) = c("numberOfLines", "time", "timeInSeconds")

qplot(numberOfLines, timeInSeconds, data = data, geom = c("point", "line"), xlab = "Anzahl der Strecken", ylab = "Laufzeit in Sekunden")
ggsave("timeOverLineNumber.png", width = 10, height = 10, unit = "cm")

qplot(numberOfLines^4, timeInSeconds, data = data, geom = c("point", "line"), xlab = "(Anzahl der Strecken)^4", ylab = "Laufzeit in Sekunden")
#+ scale_y_sqrt()
ggsave("timeOverLineNumber4.png", width = 10, height = 10, unit = "cm")
