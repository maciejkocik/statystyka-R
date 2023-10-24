b1 <- beaver1[beaver1$day == 346, ]
b2 <- beaver2[beaver2$day == 307, ]

plot(b1$time, b1$temp, type="l")

ymin <- min(b1$temp, b2$temp)
ymax <- max(b1$temp, b2$temp)

plot(b1$time, b1$temp, 
     type = "l",
     xlab = "Czas pomiaru",
     ylab = "Temperatura (C)",
     main = "Zależność temperatury bobra od czasu pomiaru",
     sub = "Pomiary bobra nr 1",
     col = "tomato",
     ylim = c(ymin, ymax)
     )




points(b2$time, b2$temp,
       col = "darkblue",
       type = "b")

legend(x = "topleft", legend = c("Bóbr 1", "Bóbr 2"),
       col = c("tomato", "darkblue"), pch = c(3, 11))


boxplot(b1$temp)
boxplot(temp ~ factor(day), data = beaver1)

vioplot(temp ~ factor(day), data = beaver1, col="tomato", main="temperature")
