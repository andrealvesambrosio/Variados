#  Semente
set.seed(131)

# Bibliotecas
require(tidyverse)
require(gridExtra)

# Gerando dados com comportamento semelhante à uma exponencial

# Ruídos
ruido_dobro  = c(6, 13, 19, 34, 44, 54, 66, 75, 88, 90) 
ruido_metade = c(2, 11, 18, 32, 59, 61, 69, 76, 92, 97)

# Valores de X
x = seq(4, 6, by = 0.02)

# Possíveis valores de Y
y0 = exp(x)
y1 = exp(x - 0.5) + (exp(x - 3))^2 
y2 = exp(x + 0.5) - (exp(x - 2)) + x^2


y = c()
# Populando valores de Y por sorteio e acrescentando ruido
for(i in 1:length(x)){
  
  y[i] = sample(c(y0[i], y1[i], y2[i]), size = 1)
  
  if(i %in% ruido_dobro){
    y[i] = 2* y[i]
  }
  if(i %in% ruido_metade){
    y[i] = 0.33* y[i]
  }
  
}

# Amostragem
index_sample = sample(1:101, size = 40)
x_amostra = x[index_sample]
y_amostra = y[index_sample]

# Dados restantes
x_fora = x[-index_sample]
y_fora = y[-index_sample]

# Populando dataframe com todos dados
df = data.frame(Tipo = c(rep(x = 'Não Amostrado', times = length(x_fora)),  
                    rep(x = 'Amostrado', times = length(x_amostra))
                    ),
                    
                    X = c(x_fora, x_amostra), 
                    Y = c(y_fora, y_amostra)) %>%
  as_tibble()

# Visualizando os dados
p1 = df %>%
  ggplot(aes(x = X, y = Y, color = Tipo)) +
  geom_point() +
  scale_color_manual(values=c("#E69F00", "#999999")) +
  labs(X = 'X',
       Y = 'Y',
       title = 'Distinguindo os dados')

p2 = df %>%
  dplyr::filter(Tipo == 'Amostrado') %>%
  ggplot(aes(x = X, y = Y)) +
  geom_point(color = '#E69F00') +
  labs(x = 'X',
       y = 'Y',
       title = 'Dados Amostrados')

p3 = df %>%
  ggplot(aes(x = X, y = Y)) +
  geom_point(color = '#999999') +
  labs(x = 'X',
       y = 'Y',
       title = 'Todos os dados')


p0 = grid.arrange(p3, p2, nrow = 1)

grid.arrange(p0, p1, nrow = 2)
  
# Modelagem com dados amostrados ----
dados_fit = df %>%
  dplyr::filter(Tipo == 'Amostrado')

fit = glm(Y ~ X, family = Gamma(link = 'log'), data = dados_fit)
fit_quadratico = lm(Y ~ poly(X, 2, raw = TRUE), data = dados_fit)

N = length(dados_fit$X)
rmse_fit  = sqrt((N^-1)*sum((fit$fitted.values - dados_fit$Y)^2))
rmse_quad = sqrt((N^-1)*sum((fit_quadratico$fitted.values - dados_fit$Y)^2))

# Comparando RMSE
rmse_fit
rmse_quad

# Curvas
x_fit = seq(4, 6, by = 0.01)
y_fit = predict(fit, list(X = x_fit), type = 'response')
y_fit_quadratico = predict(fit_quadratico, list(X = x_fit), type = 'response')

df_curva = data.frame(Curva = c(rep('Gamma', times = length(x_fit)),
                                rep('Quadratico', times = length(x_fit))),
                      Y = c(y_fit, y_fit_quadratico),
                      X = c(x_fit, x_fit))  %>%
  as_tibble()

df_curva %>%
  ggplot(aes(x = X, y = Y)) +
  geom_line(data = df_curva, aes(x = X, y = Y, color = Curva), size = 1) +
  geom_point(data = dados_fit, aes(x = X, y = Y)) +
  labs(x = 'X',
       y = 'Y',
       color = 'Modelo',
       title = 'Ajuste dos dados amostrados')

# Total (utilizando fit antigo)
dados_fit_resto = df %>%
  dplyr::filter(Tipo == 'Não Amostrado')

N = length(dados_fit_resto$X)
rmse_fit_resto  = sqrt((N^-1)*sum((predict(fit,            list(X = dados_fit_resto$X), type = 'response') - dados_fit_resto$Y)^2))
rmse_quad_resto = sqrt((N^-1)*sum((predict(fit_quadratico, list(X = dados_fit_resto$X), type = 'response') - dados_fit_resto$Y)^2))

# Comparando RMSE
rmse_fit_resto
rmse_quad_resto


# Curvas
df_curva %>%
  ggplot(aes(x = X, y = Y)) +
  geom_line(data = df_curva, aes(x = X, y = Y, color = Curva), size = 1) +
  geom_point(data = dados_fit_resto, aes(x = X, y = Y)) +
  labs(x = 'X',
       y = 'Y',
       color = 'Modelo',
       title = 'Dados que não foram modelados')


# Fim ----

# Testando para Polinômio de grau 4
fit_poly = lm(Y ~ poly(X, 4, raw = TRUE), data = dados_fit)

N = length(dados_fit$X)
rmse_fit  = sqrt((N^-1)*sum((fit$fitted.values - dados_fit$Y)^2))
rmse_poly = sqrt((N^-1)*sum((fit_poly$fitted.values - dados_fit$Y)^2))

rmse_fit
rmse_poly

y_fit_poly = predict(fit_poly, list(X = x_fit), type = 'response')

df_curva = data.frame(Curva = c(rep('Gamma', times = length(x_fit)),
                                rep('Grau 4', times = length(x_fit))),
                      Y = c(y_fit, y_fit_poly),
                      X = c(x_fit, x_fit))  %>%
  as_tibble()

q0 = df_curva %>%
  ggplot(aes(x = X, y = Y)) +
  geom_line(data = df_curva, aes(x = X, y = Y, color = Curva), size = 1) +
  geom_point(data = dados_fit, aes(x = X, y = Y)) +
  labs(x = 'X',
       y = 'Y',
       color = 'Modelo',
       title = 'Dados amostrados')

q1 = df_curva %>%
  ggplot(aes(x = X, y = Y)) +
  geom_line(data = df_curva, aes(x = X, y = Y, color = Curva), size = 1) +
  geom_point(data = dados_fit_resto, aes(x = X, y = Y)) +
  labs(x = 'X',
       y = 'Y',
       color = 'Modelo',
       title = 'Dados fora da Amostra')

grid.arrange(q0, q1)

N = length(dados_fit_resto$X)
rmse_fit_resto  = sqrt((N^-1)*sum((predict(fit,            list(X = dados_fit_resto$X), type = 'response') - dados_fit_resto$Y)^2))
rmse_poly_resto = sqrt((N^-1)*sum((predict(fit_poly, list(X = dados_fit_resto$X), type = 'response') - dados_fit_resto$Y)^2))

# Comparando RMSE
rmse_fit_resto
rmse_poly_resto
