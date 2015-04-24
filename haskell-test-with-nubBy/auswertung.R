# Zum Auswerten der Laufzeiten des Haskellprogramms (mit nubBy) bei
# steigender Anzahl an Strecken. Zugehöriges Programm liegt in
# ../haskell/Main.hs beim Git tag haskell-with-nubBy.
# Daten wurden automatisiert mit time erzeugt und dann mit awk
# bearbeitet und zusammenhehängt.

library(ggplot2)

data = read.csv("haskell-times-sec.csv", sep = ";", header = FALSE)
names(data) = c("numberOfLines", "time", "timeInSeconds")

qplot(numberOfLines, timeInSeconds, data = data, geom = c("point", "line"), xlab = "Anzahl der Strecken", ylab = "Laufzeit in Sekunden")
ggsave("timeOverLineNumber.png", width = 10, height = 10, unit = "cm")

qplot(numberOfLines^4, timeInSeconds, data = data, geom = c("point", "line"), xlab = "(Anzahl der Strecken)^4", ylab = "Laufzeit in Sekunden")
#+ scale_y_sqrt()
ggsave("timeOverLineNumber4.png", width = 10, height = 10, unit = "cm")

# Ergebnis: Laufzeit dieses Haskell-Programms ist nicht wie erwartet O(n^2),
# sondern O(n^4). Siehe beiliegende Plots.
