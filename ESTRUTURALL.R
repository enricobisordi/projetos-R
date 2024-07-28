library(BETS)
library(ipeadatar)
library(dplyr)
library(janitor)
library(rbcb)
library(vars)
library(forecast)
library(lubridate)
library(mFilter)
library(rbcb)
library(tidyverse)
library(xts)
library(zoo)
library(readxl)
library(GetBCBData)
library(BETS)
library(lavaan)
library(tempdisagg)
library(lubridate)
library(semPlot)
library(stringr)
library(mFilter)
library(lavaan)
library(dynlm)
library(systemfit)
library(forecast)
library(bimets)
library(ipeadatar)
library(janitor)
library(vars)
library(forecast)

rm(list = ls())

acum_p <- function(data,n){
  factor <- (1+(data/100))
  prod <- RcppRoll::roll_prodr(factor, n=n)
  final <- (prod-1)*100
  
  return(final)
}

IPCA = BETSget(433, data.frame = TRUE) 
selic = ipeadata('GM366_TJOVER366', language = "br")
IBC = ipeadata('SGS12_IBCBRDESSAZ12', language = "br")
swap = ipeadata('BMF12_SWAPDI36012', language = "br")
ipca_exp <-
  get_monthly_market_expectations("IPCA") %>%
  clean_names() %>%
  dplyr::select(date, median) %>%
  arrange(date) %>%
  group_by(year = year(date), month = month(date)) %>%
  filter(date == first(date)) %>%
  ungroup() %>%
  distinct(date, .keep_all = TRUE) %>%
  mutate(date = floor_date(date, unit = "month")) %>%
  dplyr::select(date, median)

ipca_exp_12 <-
  get_market_expectations(
    type = "inflation-12-months",
    ind = "IPCA") %>%
  clean_names() %>%
  dplyr::select(date = data, ipca = mediana) %>%
  arrange(date) %>%
  group_by(year = year(date), month = month(date)) %>%
  filter(date == first(date)) %>%
  ungroup() %>%
  distinct(date, .keep_all = TRUE) %>%
  mutate(date = floor_date(date, unit = "month")) %>%
  dplyr::select(date, ipca)

selic <- selic %>%
  mutate(mes = format(date, "%Y-%m")) %>%
  group_by(mes) %>%
  summarise(selic_media = mean(value),
            date = first(date))

IBC <- subset(IBC, select = -tcode)
IBC <- subset(IBC, select = -uname)
IBC <- subset(IBC, select = -code)
swap <- subset(swap, select = -tcode)
swap <- subset(swap, select = -uname)
swap <- subset(swap, select = -code)
M1 <- subset(M1, select = -tcode)
M1 <- subset(M1, select = -uname)
M1 <- subset(M1, select = -code)
selic <- subset(selic, select = -mes)

IBC <- IBC %>%
  rename(IBC = value)
ipca_exp <- ipca_exp %>%
  rename(ipca_exp = median)
IPCA <- IPCA %>%
  rename(IPCA = value)
swap <- swap %>%
  rename(swap = value)
selic <- selic %>%
  rename(selic = selic_media)

IPCA <- IPCA %>%
  mutate(IPCA_lag = lag(IPCA, 1))

check_na <- function(df) {
  df %>%
    summarise_all(anyNA)
}


hp <- hpfilter(IBC$IBC, freq = 12)
IBC$tendencia <- hp$trend
IBC$hiato <- hp$cycle

rm(hp)

IPCA$ipcaanual <- acum_p(IPCA$IPCA,12)

IPCA <- IPCA %>%
  filter(date >= as.Date("2003-01-01") & date <= as.Date("2024-05-01"))

M1 <- M1 %>%
  filter(date >= as.Date("2003-01-01") & date <= as.Date("2024-05-01"))

IBC <- IBC %>%
  filter(date >= as.Date("2003-01-01") & date <= as.Date("2024-05-01"))

ipca_exp <- ipca_exp %>%
  filter(date >= as.Date("2003-01-01") & date <= as.Date("2024-05-01"))

ipca_exp_12 <- ipca_exp_12 %>%
  filter(date >= as.Date("2003-01-01") & date <= as.Date("2024-05-01"))

selic <- selic %>%
  filter(date >= as.Date("2003-01-01") & date <= as.Date("2024-05-10"))

swap <- swap %>%
  filter(date >= as.Date("2003-01-01") & date <= as.Date("2024-05-01"))


selic$juro_real = ((1 + (selic$selic/100))/(1 + (IPCA$ipcaanual/100)) - 1)*100
selic$juro_real_lag = lag(selic$juro_real)

IBC$hiato_lag = lag(IBC$hiato)


M1<- M1  %>%
  filter(date >= as.Date("2003-02-01") & date <= as.Date("2024-05-01"))

IPCA <- IPCA %>%
  filter(date >= as.Date("2003-02-01") & date <= as.Date("2024-05-01"))

IBC <- IBC %>%
  filter(date >= as.Date("2003-02-01") & date <= as.Date("2024-05-01"))

ipca_exp <- ipca_exp %>%
  filter(date >= as.Date("2003-02-01") & date <= as.Date("2024-05-01"))

ipca_exp_12 <- ipca_exp_12 %>%
  filter(date >= as.Date("2003-02-01") & date <= as.Date("2024-05-01"))

selic <- selic %>%
  filter(date >= as.Date("2003-02-01") & date <= as.Date("2024-05-10"))

swap <- swap %>%
  filter(date >= as.Date("2003-02-01") & date <= as.Date("2024-05-01"))

#rm(IPCA, IBC, ipca_exp, ipca_exp_12, selic, swap)

check_na(IBC)
check_na(IPCA)
check_na(ipca_exp)
check_na(swap)
check_na(ipca_exp_12)
check_na(selic)

merged <- merge(swap, IPCA, by = "date")
merged <- merge(merged, ipca_exp, by = "date")
merged <- merge(merged, ipca_exp_12, by = "date")
merged <- merged %>%
  mutate(month_year = format(date, "%Y-%m"))  # Criar coluna com formato ano-mês

selic <- selic %>%
  mutate(month_year = format(date, "%Y-%m"))

merged <- merge(merged, selic, by = "month_year")
IBC <- IBC %>%
  rename(date.x = date)
merged <- merge(merged, IBC, by = "date.x")

merged <- subset(merged, select = -date.y)
merged <- subset(merged, select = -tendencia)

M1 <- M1 %>%
  rename(date.x = date)
merged <- merge(merged, M1, by = "date.x")

write.csv(merged, file = "C:/Users/Enrico.bisordi/OneDrive - Rio Bravo/Desktop/Nova pasta/merged.csv", row.names = FALSE)


phillips <- IPCA ~ IPCA_lag + hiato_lag + ipca_exp
is <- hiato ~ hiato_lag + juro_real_lag
fischer <- juro_real ~ ipca + swap

phillips_model <- dynlm(phillips, data = merged)
is_model <- dynlm(is, data = merged)
fischer_model <- dynlm(fischer, data = merged)


#merged_estimar = data.frame(juro_real = c(5.922062), ipca_exp = c(0.33,0.1521,0.1,0.19,0.29,0.22,0.46,0.42,0.52,0.34,0.35,0.25), ipca = c(3.716006, 3.614721, 3.511313, 3.552656,3.619814,3.524780,3.710605,3.702756, 3.847287,3.681958, 3.733644, 3.681932), swap = c(10, 10,10,10,10,10,10,10,10,10,10,10))
merged_estimar = data.frame(IPCA_lag = c(0.38999), hiato_lag = c(-0.007446), ipca_exp = c(0.46), juro_real_lag = c(7.3677), ipca = c(3.7106), swap = c(10.8))


phillips_pred <- predict(phillips_model, newdata = merged_estimar)
is_pred <- predict(is_model, newdata = merged_estimar)
fischer_pred <- predict(fischer_model, newdata = merged_estimar)
phillips_pred
is_pred
fischer_pred




merged_estimar = data.frame(IPCA_lag = c(0.46,0.4507, 0.4095,0.3753994), hiato_lag = c(-0.115738657,-0.04709537,-0.02084687,-0.01106952), juro_real_lag = c(6.28675709,6.605395,6.807260,7.013356), ipca_exp = c(0.33,0.1521,0.1,0.29),  ipca = c(3.716006,3.614721,3.511313,3.552656), swap = c(10.4,10.4,10.4,10.4))
swap_futuro <- c(10.4, 10.4, 10.4, 10.4, 10.4, 10.4, 10.4, 10.4, 10.4, 10.4, 10.4, 10.4)
ipca_12_futuro <- c(3.716006, 3.614721, 3.511313, 3.552656, 3.619814, 3.524780, 3.710605, 3.702756, 3.847287, 3.681958, 3.733644, 3.681932)
ipca_exp_futuro <- c(0.33, 0.1521, 0.1, 0.19, 0.29, 0.22, 0.46, 0.42, 0.52, 0.34, 0.35, 0.25)

































previsoes_ipca = phillips_pred
plot_inflacao <- data.frame(Observado = merged$IPCA, Previsao = previsoes_ipca)
plot_inflacao <- mutate(plot_inflacao, Observado_shifted = lag(Observado))
plot_inflacao<- tail(plot_inflacao, -1)
y_limits <- range(plot_inflacao$Observado, na.rm = TRUE) 
plot(plot_inflacao$Observado_shifted, type = "l", col = "blue", lwd = 2,
     ylim = y_limits,
     main = "Observado vs. Previsão de IPCA",
     xlab = "Tempo", ylab = "IPCA")
lines(plot_inflacao$Previsao, col = "red", lty = 2, lwd = 2)
legend("topleft", legend = c("Observado", "Previsto"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

previsoes_hiato = is_pred
plot_hiato <- data.frame(Observado = merged$hiato, Previsao = previsoes_hiato)
plot_hiato <- mutate(plot_hiato, Observado_shifted = lag(Observado))
plot_hiato <- tail(plot_hiato, -1)
y_limits <- range(plot_hiato$Observado, na.rm = TRUE) 
y_limits <- range(plot_hiato$Observado_shifted, na.rm = TRUE) 
plot(plot_hiato$Observado_shifted, type = "l", col = "blue", lwd = 2,
     ylim = y_limits,
     main = "Observado vs. Previsão de hiato",
     xlab = "Tempo", ylab = "Hiato")
lines(plot_hiato$Previsao, col = "red", lty = 2, lwd = 2)
legend("topleft", legend = c("Observado", "Previsto"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

previsoes_real = fischer_pred
plot_real <- data.frame(Observado = merged$juro_real, Previsao = previsoes_real)
plot_real <- mutate(plot_real, Observado_shifted = lag(Observado))
plot_real <- mutate(plot_real, Previsao_shifted = lag(previsoes_real))
plot_real <- tail(plot_real, -1)
y_limits <- range(plot_real$Observado, na.rm = TRUE) 
y_limits <- range(plot_real$Observado, na.rm = TRUE) 
plot(plot_real$Observado, type = "l", col = "blue", lwd = 2,
     ylim = y_limits,
     main = "Observado vs. Previsão de juro real",
     xlab = "Tempo", ylab = "Selic")
lines(plot_real$Previsao_shifted, col = "red", lty = 2, lwd = 2)
legend("topleft", legend = c("Observado", "Previsto"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)








phillips_formula <- IPCA ~ IPCA_lag + hiato_lag + ipca_exp
is_formula <- hiato ~ hiato_lag + juro_real_lag
fischer_formula <- juro_real ~ ipca + swap

# Ajustar modelos aos dados atuais (merged)
phillips_model <- dynlm(phillips_formula, data = merged)
is_model <- dynlm(is_formula, data = merged)
fischer_model <- dynlm(fischer_formula, data = merged)

# Criar dataframe para previsões futuras
n_periodos <- 12
merged_futuro <- data.frame(
  IPCA_lag = rep(NA, n_periodos),
  hiato_lag = rep(NA, n_periodos),
  ipca_exp = rep(NA, n_periodos),
  juro_real_lag = rep(NA, n_periodos),
  IPCA = rep(NA, n_periodos),
  swap = rep(NA, n_periodos)
)

# Suponha que merged_estimar tem o valor para o primeiro período
merged_estimar <- data.frame(IPCA_lag = 0.46, hiato_lag = -0.115738657, ipca_exp = 0.33, juro_real_lag = 6.28675709, IPCA = 3.716006, swap = 10.4)















model <- 
    # Medição
    x1 =~ y1 + y2 + y3
    x2 =~ z1 + z2 + z3

    # Relações estruturais
    x1 ~ x2


# Ajustar o modelo aos dados
fit <- sem(model, data = seus_dados)

# Fazer previsões para os próximos 12 períodos
previsoes <- predict(fit, n.ahead = 12)





phillips_formula <- "
  IPCA ~ IPCA_lag + hiato_lag + ipca_exp
"

is_formula <- "
  hiato ~ hiato_lag + juro_real_lag
"

fischer_formula <- "
  juro_real ~ IPCA + swap
"



phillips_fit <- sem(phillips_formula, data = merged)

# Ajustar o modelo IS aos dados
is_fit <- sem(is_formula, data = merged)

# Ajustar o modelo de Fischer aos dados
fischer_fit <- sem(fischer_formula, data = merged)


phillips_pred <- predict(phillips_fit, newdata = merged_estimar)

# Fazer previsões para o modelo IS
is_pred <- predict(is_fit, newdata = merged_estimar)

# Fazer previsões para o modelo de Fischer
fischer_pred <- predict(fischer_fit, newdata = merged_estimar)

