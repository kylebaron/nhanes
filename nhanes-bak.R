library(dplyr)
library(tidyr)
library(purrr)
library(nhanesA)
library(data.table)

nhanesTables("DEMO", 2018)
nhanesTables("EXAM", 2018)
nhanesTables("LAB", 2018)
nhanesTables("LAB", 2018) %>% filter(grepl("ALB|BIO", Data.File.Name))


nhanesA:::.get_year_from_nh_table("BMX_J")

domains <- c("BMX", "DEMO")
years <- LETTERS[4:12]
to_get <- map(domains, ~ paste0(.x, "_", years)) %>% flatten_chr()


bmx <- nhanes("BMX_J")
fwrite(bmx, file = "data/bmx.csv")
demo <- nhanes("DEMO_J")
fwrite(demo, file = "data/demo.csv")
bio <- nhanes("BIOPRO_J")
fwrite(bio, file = "data/bio.csv")
albcr <- nhanes("ALB_CR_J")
fwrite(albcr, file = "data/albcr.csv")

size <- select(bmx, SEQN, WT = BMXWT, HT = BMXHT)
age <- select(demo, SEQN, AGE = RIDAGEYR, MO = RIDEXAGM) 
labs <- select(bio, SEQN, BUN = LBXSBU, SCR = LBXSCR, TG = LBXSTR)
sex <- select(demo, SEQN, SEX = RIAGENDR) %>% mutate(SEX = SEX-1)

zap_label <- function(x) {
  attr(x, "label") <- NULL
  x
}

size <- mutate(size, across(everything(), zap_label))
age <- mutate(age, across(everything(), zap_label))
labs <- mutate(labs, across(everything(), zap_label))
sex <- mutate(sex, across(everything(), zap_label))


data <- left_join(size,age) %>% 
  left_join(labs) %>% 
  left_join(sex) %>% 
  as_tibble() 

fwrite(data, file = "data/nhanes.csv")

data <- mutate(data, MO = ifelse(is.na(MO), 12*AGE, MO))

ggplot(data, aes(MO, WT)) + geom_point() + xlim(0, NA) + 
  geom_smooth()

ggplot(data, aes(MO/12, TG)) + geom_point() + xlim(0, NA) + 
  geom_smooth() + scale_y_log10()

ggplot(data, aes(MO/12, BUN)) + geom_point() + scale_y_log10() + 
  geom_smooth()
