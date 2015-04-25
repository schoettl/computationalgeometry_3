library(ggplot2)
library(reshape2)

loadData = function(lang) {
    read.table(paste0("run-intersect-", lang, ".dat"), col.names = c("id", "variable", "value"))
}

datafiles = c("c", "h")
for (lang in datafiles) {
    datam = loadData(lang)
    data = dcast(datam, id ~ ...)
    data = transform(data,
            real = as.numeric(real),
            sys  = as.numeric(sys),
            user = as.numeric(user),
            lines = as.integer(lines),
            intersects = as.integer(intersects))
    data = melt(data, measure.vars = c("real", "sys", "user"))
    qplot(lines, value, data = data, color = variable,
          xlab = "Anzahl der Strecken",
          ylab = "Programmlaufzeit in Sekunden") +
          scale_x_log10() + scale_y_log10() + geom_line()
    ggsave(paste0("timeOverLines_", lang, ".png"))
}
