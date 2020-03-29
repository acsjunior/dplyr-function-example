data(iris)
head(iris)

summary(iris$Petal.Length)

library(dplyr)

iris %>%
  group_by(Species) %>%
  summarise(min = min(Petal.Length),
            q1 = quantile(Petal.Length, 0.25),
            mediana = median(Petal.Length),
            media = mean(Petal.Length),
            q3 = quantile(Petal.Length, 0.75),
            max = max(Petal.Length),
            variancia = var(Petal.Length),
            d_padrao = sd(Petal.Length),
            cv = d_padrao / media)



custom_summary <- function(dados, variavel, categoria=FALSE) {
  out <- dados
  
  if(categoria != FALSE) {
    out <- out %>%
      group_by(!!sym(categoria))
  }
  
  out <- out %>%
    summarise(min = min(!!sym(variavel)),
              q1 = quantile(!!sym(variavel), 0.25),
              mediana = median(!!sym(variavel)),
              media = mean(!!sym(variavel)),
              q3 = quantile(!!sym(variavel), 0.75),
              max = max(!!sym(variavel)),
              variancia = var(!!sym(variavel)),
              d_padrao = sd(!!sym(variavel)),
              cv = d_padrao / media)
  
  return(out)
}

custom_summary(dados = iris, variavel = "Sepal.Width")

custom_summary(dados = iris, variavel = "Sepal.Width", categoria = "Species")
