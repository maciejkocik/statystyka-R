library(dplyr)

mtcars
mtcars[mtcars$hp > 150, ]
filter(mtcars, hp > 150)

mtcars[, c("hp", "mpg")]
select(mtcars, c(hp, mpg))
select(mtcars, hp, mpg)

select(filter(mtcars, hp > 150), hp, mpg)

mean(mtcars$hp)

mtcars$hp %>% mean

mtcars %>% filter(hp > 150) %>% select(hp, mpg)

# mutate / transmute

mtcars$konnamikenagalon <- mtcars$hp / mtcars$mpg

mtcars %>% mutate(konnamilenagalon = hp / mpg)
mtcars %>% transmute(konnamilenagalon = hp / mpg)

mtcars %>% summarise(mean_hp = mean(hp), sd_hp = sd(hp))

mtcars %>% 
  group_by(cyl) %>% 
  summarise(mean_hp = mean(hp), sd_hp = sd(hp), n = n())

