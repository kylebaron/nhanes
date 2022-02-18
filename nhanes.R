library(dplyr)
library(tidyr)
library(purrr)
library(nhanesA)
library(data.table)
library(ggplot2)

#'  nhanes Tables
#'  Year: 2018
#'  Check out DEMO, EXAM, LAB
nhanesTables("DEMO", 2018) # DEMO_J
nhanesTables("EXAM", 2017) # BMX_J
nhanesTables("LAB", 2018) # ALB_CR_J
nhanesTables("LAB", 2014) # ALB_CR_J

nhanesA:::.get_year_from_nh_table("UAS_J")
nhanesA:::data_idx
#' nhanes 2018
#' - BMX - WT, HT, BMI
size <- nhanes("UAS_H") %>% as_tibble()
size <- select(size, SEQN, WT = BMXWT, HT  = BMXHT, BMI = BMXBMI)
#str(size)
#lapply(size, attr, "label")
#' - DEMO - AGE, AGE MONTHS, SEX
demo <- nhanes("DEMO_J")
demo <- select(demo, SEQN,  AGE = RIDAGEYR, MO = RIDEXAGM, SEX = RIAGENDR)
head(demo)
#' - BIOPRO - BUN, SCR, TG
bio <- nhanes("BIOPRO_J")
bio <- select(bio, SEQN, BUN = LBXSBU, SCR = LBXSCR, 
              TG = LBXSTR)
#lapply(bio, attr, "label")
#' - ALB_CR - NOTHING


#' Zap label
zap_label <- function(x) {
  attr(x, "label") <- NULL
  x
}
size[] <- lapply(size, zap_label)
demo[] <- lapply(demo, zap_label)
bio[] <- lapply(bio, zap_label)


#' Join
data <- left_join(size, demo) %>% left_join(bio)
data <- mutate(data, MO = ifelse(is.na(MO), AGE *12, MO))

pop <- sample_n(data, 1000)

ggplot(data, aes(MO/12, WT)) + geom_point()

ggplot(data, aes(MO/12, BUN)) + geom_point()
ggplot(data, aes(MO/12, TG)) + geom_point() + scale_y_log10() +
  geom_smooth()

#' Months is only up to 19

#' WT vs Age
#' TG vs Age
#' BUN vs Age


