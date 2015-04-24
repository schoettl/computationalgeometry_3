library(ggplot2)
library(reshape2)
data = read.table("data/test.dat", sep = " ", col.names = c("p.x", "p.y", "q.x", "q.y"))
data$id=factor(1:nrow(data))
datam = melt(data, id = "id")
datam = cbind(datam, colsplit(datam$variable, "\\.", names = c("point", "coord"))) # sollte eigentlich funktionieren
datac = dcast(datam, id + point ~ coord)
head(datam)
head(datac)
ggplot(data, aes(x = p.x, y = p.y, color = id)) +
  geom_segment(aes(xend = q.x, yend = q.y)) +
  geom_point(size = 4) + geom_point(aes(x = q.x, y = q.y), size = 4)
