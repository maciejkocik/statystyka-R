walsh <- read.csv("dane/walsh.csv")
unahborger <- read.csv("dane/unahborger.csv")
head(unahborger)

tab <- table(unahborger$DefendantsRace, unahborger$DeathSentence)

total <- margin.table(tab)

rows <- margin.table(tab, 1)

cols <- margin.table(tab, 2)

(rows[1]/total * cols[1]/total) * total
(rows[2]/total * cols[1]/total) * total

oczekiwane <- rbind(cols, cols) * cbind(rows, rows) / total

chi_kw <- sum((tab-oczekiwane)**2 / oczekiwane)
chi_kw
qchisq(0.95,1)
pchisq(chi_kw, 1, lower.tail = F)
chisq.test(tab, correct = F)

walsh
chisq.test(table(walsh$Treatment, walsh$Outcome))

install.packages("gmodels")

library(gmodels)

CrossTable(walsh$Treatment, walsh$Outcome)

jankowski <- read.csv("dane/jankowski.csv")

tab <- table(jankowski$Number.of.CAC.checked, jankowski$Abused.as.adult)
chisq.test(tab)

install.packages("vcd")
library(vcd)

tab <- table(jankowski$Number.of.CAC.checked, jankowski$Abused.as.adult, 
             dnn = c("Number of CAC checked", "Abused as adult"))

tab
assocstats(tab)
mosaicplot(tab)

assoc(tab, shade = T)
mosaic(tab, shade = T)
