student <- read.csv2("funkcje/student-mat.csv")

student_oceny <- student[, c("G1", "G2", "G3")]

apply(student_oceny, 2, mean)

lapply(student_oceny, max)

sapply(student_oceny, max)

tapply(student$G3, student$sex, mean)
tapply(student$G3, student$sex, max)
tapply(student$G3, student$sex, min)

colnames(student)

tapply(student$G3, student$romantic, mean)

tapply(student$G3, student$Fjob, mean)
tapply(student$G3, student$Mjob, mean)

barplot(
  rbind(
  tapply(student$G3, student$Fjob, mean), 
  tapply(student$G3, student$Mjob, mean)
  ),
  col=c("hotpink", "lightblue"),
  beside=T)

student$FeduFct <- factor(student$Fedu,
                          levels = c(0,1,2,3,4),
                          labels = c("none", "primary1", "primary2", 
                                     "secondary", "higher")
                          )

barplot(
  rbind(
    tapply(student$G3, student$Fedu, mean), 
    tapply(student$G3, student$Medu, mean)
  ),
  col=c("hotpink", "lightblue"),
  beside=T)
