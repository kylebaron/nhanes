library(dplyr)
library(tidyr)
library(purrr)
library(nhanesA)
library(data.table)
nhanesTables("DEMOGRAPHICS", 2018)

nhanesA:::.get_year_from_nh_table("BMX_B")

domains <- c("BMX", "DEMO")
years <- LETTERS[4:12]
to_get <- map(domains, ~ paste0(.x, "_", years)) %>% flatten_chr()


x <- nhanes("BMX_D")

a <- nhanesSearchTableNames("BMX")
b <- nhanesSearchTableNames("DEMO")
c <-  nhanesSearchTableNames("LAB")
d <- nhanesSearchTableNames("BIOPRO")
e <- nhanesSearchTableNames("ALB_CR")

xnhanes <- function(domain) {
  dl <- try(nhanes(domain))
  if(!is.data.frame(dl)) return(NULL)
  mutate(dl, domain = domain)
}
bmx <- map(a, xnhanes)
demo <- map(b, xnhanes)
lab <- map(c, xnhanes)
bio <- map(d, xnhanes)
alb <- map(e, xnhanes)

bmx <- lapply(bmx, as.data.frame) %>% rbindlist(fill = TRUE)
length(unique(bmx$SEQN))



x <- nhanes("BIOPRO_E")
sapply(x, attr, "label") 


