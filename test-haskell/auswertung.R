# Auswertung der Laufzeiten des Haskellprogramms:
# ../haskell/Main.hs (commit 7c9b269eb6beebe98db111f07964a78290670bed)

library(ggplot2)

data = read.table("test-1000-50000.dat", sep = " ", col.names = c("lines", "intersects", "time"))
qplot(lines, time, data = data, geom = "point", xlab = "Anzahl Strecken", ylab = "Programmlaufzeit in Sekunden") + stat_smooth(method = "loess")
ggsave("timeOverLines.png")
qplot(lines^2, time, data = data, geom = "point", xlab = "(Anzahl Strecken)^2", ylab = "Programmlaufzeit in Sekunden") + stat_smooth(method = "loess") 
ggsave("timeOverLines2.png")
