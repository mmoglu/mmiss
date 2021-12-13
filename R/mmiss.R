#' Tabulating missing values/percentages
#'
#' This function displays counts and percentages of missing values
#' for each variable as well as the entire dataset
#'
#' @examples
#' my_vec <- 1:5
#' my_vec2 <- c(1:5,NA)
#' v1 <- sample(my_vec, size = 10, replace = TRUE)
#' v2 <- sample(my_vec2, size = 10, replace = TRUE)
#' v3 <- sample(my_vec, size = 10, replace = TRUE)
#' v4 <- sample(my_vec2, size = 10, replace = TRUE)
#' df <- data.frame(v1,v2,v3,v4)
#' df
#' mmiss(df)
#' @export


mmiss <- function(df){
  tot.num.dp = length(df)*nrow(df)
  tot.num.miss.dp = sum(is.na(df))
  tot.pr.miss.dp = round(sum(is.na(df))/tot.num.dp*100, digits = 2)
  tot.num.comp.dp = sum(!is.na(df))
  tot.pr.comp.dp = round(tot.num.comp.dp/tot.num.dp*100, digits=2)

  vars.num.miss.dp = colSums(is.na(df))
  vars.pr.miss.dp = round(vars.num.miss.dp/nrow(df)*100, digits=2)

  vars.num.comp.dp = colSums(!is.na(df))
  vars.pr.comp.dp = round(vars.num.comp.dp/nrow(df)*100, digits=2)

  results = rbind(vars.num.miss.dp,
                  vars.pr.miss.dp,
                  vars.num.comp.dp,
                  vars.pr.comp.dp)

  totalc <- c(tot.num.miss.dp,tot.pr.miss.dp,
              tot.num.comp.dp, tot.pr.comp.dp)
  results2 <- cbind(results,totalc)
  totalr <- c(vars.num.miss.dp+ vars.num.comp.dp,
              tot.num.miss.dp+tot.num.comp.dp)
  results3 <- rbind(results2,totalr)
  d=data.frame(results3)
  class(d) <- c("miss.data.frame", "data.frame")
  d
}

print.miss.data.frame <- function(d, ...){
  d["vars.pr.miss.dp",]=sprintf("%.1f%%", d["vars.pr.miss.dp",])
  d["vars.pr.comp.dp",]=sprintf("%.1f%%", d["vars.pr.comp.dp",])
  print.data.frame(d, ...)
}


my_vec <- 1:5
my_vec2 <- c(1:5,NA)

v1 <- sample(my_vec, size = 10, replace = TRUE)
v2 <- sample(my_vec2, size = 10, replace = TRUE)
v3 <- sample(my_vec, size = 10, replace = TRUE)
v4 <- sample(my_vec2, size = 10, replace = TRUE)
df <- data.frame(v1,v2,v3,v4)
df

mmiss(df)  -> mdf
print(mdf)
print.data.frame(mdf)

print.miss.data.frame(mdf)




