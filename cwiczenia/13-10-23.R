d1 <- read.csv("imodyfikatory.csv", encoding = "UTF-8")


table(umyslnie$Wykształcenie)
table(d1$Wykształcenie)

round((table(umyslnie$Wykształcenie) / 137) * 100, 2)

xtab <- prop.table(table(umyslnie$Odpowiedź, umyslnie$Grupa), 2)

barplot(xtab, legend.text = T, col = c("hotpink", "chartreuse"))

colors()


