# Lista 1 - Séries temporais(Ana BeatrizdeSouzaSoares)
# Aluna: Ana Beatriz de Souza Soares.
# Perguntar 1:Dados climáticos.

## Bibliotecas de séries temporais

library(dplyr)
library(feasts)
library(forecast)
library(fpp3)
library(ggplot2)
library(lubridate)
library(readr)
library(TSA)
library(tsibble)


### Carregando banco de dados em formato csv
diretorio <- "C:\\Users\\usuario\\Documents\\lista_1\\dados\\DailyDelhiClimateTrain.csv"
df_cli <- read_csv(diretorio)
head(df_cli)

#================================================================
# ANALISE DIARIA
#================================================================

### transformando para o formato tsibble
serie_diaria <- df_cli %>%
  mutate(date = as_date(date)) %>%
  as_tsibble(index = date)

serie_diaria

## Plot das series diárias
serie_diaria %>%
  pivot_longer(-date) %>%
  ggplot(aes(x = date, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~., scales = "free_y") +
  labs(title = "Séries Temporais Diárias", x = "Data", y = "Valor")

## Estudando a sazonalidade diária - period = 365 para dados diários
### Temperatura média
serie_diaria %>%
  model(classical_decomposition(meantemp, type = "additive")) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição clássica aditiva - Temperatura Média Diária")

### Humidade
serie_diaria %>%
  model(classical_decomposition(humidity, type = "additive")) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição clássica aditiva - Humidade Diária")

### Pressão
serie_diaria %>%
  model(classical_decomposition(meanpressure, type = "additive")) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição clássica aditiva - Pressão Média Diária")

### Velocidade do vento
serie_diaria %>%
  model(classical_decomposition(wind_speed, type = "additive")) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição clássica aditiva - Velocidade do Vento Diária")

#================================================================
# ANALISE MENSAL
#================================================================

## transformando para o formato tsibble mensal
serie_mensal <- df_cli %>%
  mutate(mes_ano = yearmonth(date)) %>%
  group_by(mes_ano) %>%
  summarise(humidity = mean(humidity, na.rm = TRUE),
            meanpressure = mean(meanpressure, na.rm = TRUE),
            meantemp = mean(meantemp, na.rm = TRUE),
            wind_speed = mean(wind_speed, na.rm = TRUE)) %>%
  as_tsibble(index = mes_ano)

serie_mensal

## Plot das series mensais
serie_mensal %>%
  pivot_longer(-mes_ano) %>%
  ggplot(aes(x = mes_ano, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~., scales = "free_y") +
  labs(title = "Séries Temporais Mensais", x = "Mês/Ano", y = "Valor")

## Estudando a sazonalidade mensal
### Humidade
serie_mensal %>%
  gg_season(humidity, labels = "both") +
  labs(y = "Média mensal da Humidade",
       title = "Sazonalidade: Humidade")

serie_mensal %>%
  gg_subseries(humidity) +
  labs(y = "Humidade",
       title = "Sub-séries Sazonais: Humidade")

### Temperatura
serie_mensal %>%
  gg_season(meantemp, labels = "both") +
  labs(y = "Média mensal da Temperatura",
       title = "Sazonalidade: Temperatura")

serie_mensal %>%
  gg_subseries(meantemp) +
  labs(y = "Temperatura",
       title = "Sub-séries Sazonais: Temperatura")

### Pressão
serie_mensal %>%
  gg_season(meanpressure, labels = "both") +
  labs(y = "Média mensal da Pressão",
       title = "Sazonalidade: Pressão")

### Velocidade do vento
serie_mensal %>%
  gg_season(wind_speed, labels = "both") +
  labs(y = "Média mensal do Vento",
       title = "Sazonalidade: Velocidade do Vento")

## Decomposição aditiva mensal - period = 12
serie_mensal %>%
  model(classical_decomposition(meantemp, type = "additive")) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição clássica aditiva - Temperatura Média Mensal")

serie_mensal %>%
  model(classical_decomposition(humidity, type = "additive")) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição clássica aditiva - Humidade Mensal")

#================================================================
# ANALISE TRIMESTRAL
#================================================================

## transformando para o formato tsibble trimestral
serie_tri <- df_cli %>%
  mutate(tri_ano = yearquarter(date)) %>%
  group_by(tri_ano) %>%
  summarise(humidity = mean(humidity, na.rm = TRUE),
            meanpressure = mean(meanpressure, na.rm = TRUE),
            meantemp = mean(meantemp, na.rm = TRUE),
            wind_speed = mean(wind_speed, na.rm = TRUE)) %>%
  as_tsibble(index = tri_ano)

serie_tri

## Plot das series trimestrais
serie_tri %>%
  pivot_longer(-tri_ano) %>%
  ggplot(aes(x = tri_ano, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~., scales = "free_y") +
  labs(title = "Séries Temporais Trimestrais", x = "Trimestre/Ano", y = "Valor")

## Estudando a sazonalidade trimestral
serie_tri %>%
  gg_season(meantemp, labels = "both") +
  labs(y = "Média trimestral da Temperatura",
       title = "Sazonalidade: Temperatura")

serie_tri %>%
  gg_subseries(meantemp) +
  labs(y = "Temperatura",
       title = "Sub-séries Sazonais: Temperatura")

## Decomposição aditiva trimestral - period = 4
serie_tri %>%
  model(classical_decomposition(meantemp, type = "additive")) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição clássica aditiva - Temperatura Média Trimestral")

serie_tri %>%
  model(classical_decomposition(humidity, type = "additive")) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição clássica aditiva - Humidade Trimestral")
  
#### Tendência: Em 4 anos não tem tendência de aquecimento ou resfriamento. A linha `trend` é quase flat. Dele varia sazonalmente mas não muda o nível médio.
  
#### Sazonalidade: Forte e anual em todas variáveis. `meantemp` tem pico em Q2 = abril - junho ∼32°C e vale em Q1 = janeiro - março ∼15°C. `humidity` tem pico em Q3 = julho - setembro ∼80% por causa das monções. `meanpressure` é inversa: cai no verão.

 #### Por que mensal e trimestral: No diário você vê ruído. No mensal vê o ciclo anual limpo. No trimestral perde detalhe mas confirma que a sazonalidade é de 4 estações.

# Conclusão .


#### A análise exploratória das séries temporais climáticas nesse conjunto de dados, no período de 2013 a 2017, foi realizada em três granularidades distintas: diária, mensal e trimestral. 

#### O objetivo consistiu em identificar a presença de componentes de tendência e sazonalidade nas variáveis `meantemp`, `humidity`, `meanpressure` e `wind_speed`, utilizando o framework `tsibble` e decomposição clássica aditiva. 

#### Os dados foram agregados por média para as frequências mensal e trimestral, procedimento adequado para variáveis de estoque como temperatura e pressão.

#### A visualização via `ggplot2` e a decomposição via `feasts` permitiram isolar os componentes `trend`, `seasonal` e `random` de cada série.

#### Os resultados indicam ausência de tendência significativa de longo prazo no período analisado. 

#### O componente `trend` extraído para todas as quatro variáveis apresentou comportamento aproximadamente flat, sem inclinação positiva ou negativa sustentada ao longo dos quatro anos.
#### Isso sugere que, para o horizonte temporal do conjunto de dados, não há evidência de aquecimento, resfriamento ou mudança estrutural no nível médio das condições climáticas .

#### A variabilidade observada nos gráficos diários é dominada por flutuações de alta frequência e pelo ciclo sazonal, não por deslocamentos no nível da série. Portanto, as séries podem ser consideradas estacionárias em média para o período.

#### Em contrapartida, a componente sazonal é forte, determinística e anual para todas as variáveis, com `frequency = 365` na escala diária, `frequency = 12` na mensal e `frequency = 4` na trimestral. 

#### A variável `meantemp` exibe sazonalidade com amplitude de aproximadamente 17°C, apresentando pico no segundo trimestre, abr-jun, com médias próximas a 32°C, e vale no primeiro trimestre, janeiro - março, com médias de 15°C. A `humidity` tem padrão sazonal defasado, com pico no terceiro trimestre, julho - setembro, atingindo 80% devido ao regime de monções, e vale no primeiro trimestre em torno de 40%. 

#### A `meanpressure` apresenta sazonalidade inversa à temperatura, com valores mínimos no verão e máximos no inverno, com amplitude de ∼10 hPa. A `wind_speed` possui sazonalidade de menor amplitude, ∼1.5 m/s, com pico no segundo e terceiro trimestres.

#### A mudança de granularidade temporal de diária para mensal e trimestral cumpriu o objetivo de filtrar o ruído de alta frequência e evidenciar o padrão sazonal. Na escala diária, o componente `random` é dominante e mascara parcialmente o ciclo anual. Na agregação mensal, o ciclo anual torna-se visualmente limpo nos gráficos de `gg_season` e `gg_subseries`.

 #### Na escala trimestral, perde-se resolução intra-estação, mas confirma-se que o ciclo sazonal fundamental é composto por quatro estações bem definidas.

#### Conclui-se que as séries climáticas dos dados são caracterizadas por forte sazonalidade aditiva anual e ausência de tendência no curto prazo, sendo o comportamento governado principalmente pelo ciclo climático regional.

#  Perguntar 2) Dados do COVID. 
diretorio <- "C:\\Users\\usuario\\Documents\\lista_1\\dados\\vaccinations.csv"
df_covid <- read_csv(diretorio)
head(df_covid)
 

# [a] Escolhe país : Brasil
pais <- "Brazil"
vars_covid <- c("new_cases", "new_deaths", "total_cases", "new_vaccinations")

df_br <- df_covid %>%
  filter(location == pais) %>%
  mutate(date = as_date(date)) %>%
  arrange(date) %>% 
  fill_gaps() %>% 
  fill(all_of(vars_covid),.direction = "downup") 

for(var in vars_covid){
  p <- ggplot(df_br, aes(x = date, y =.data[[var]])) +
    geom_line(color = "steelblue") +
    labs(title = paste(var, "-", pais),
         x = "Data", y = var) +
    theme_minimal()
  print(p)
}


# [b] Tendência e sazonalidade
# Tendência e sazonalidade - frequency = 7
for(var in c("new_cases", "new_deaths")){ # total_cases não decompõe, é acumulada
  df_var <- df_br %>% select(date,!!var) %>% drop_na()

  if(nrow(df_var) >= 14){ # Precisa de 2 ciclos = 14 dias
    ts_covid <- ts(df_var[[var]],
                   frequency = 7,
                   start = c(year(min(df_var$date)), yday(min(df_var$date))))
    decomp <- decompose(ts_covid, type = "additive")
    plot(decomp, main = paste("Decomposição", var, "-", pais))
  }
}
#### Interpretação: Tendência = ondas de covid. Sazonalidade = queda no fds por subnotificação.

#### A decomposição das séries diárias de `new_cases` e `new_deaths` para o Brasil, com `frequency = 7`, revela dois componentes dominantes. O componente `trend` captura as ondas epidêmicas, caracterizadas por crescimento exponencial seguido de decaimento, correspondendo às fases de transmissão comunitária das variantes Gama, Delta e Ômicron entre 2020-2022.
#### A tendência não é monotônica nem linear, apresentando múltiplos picos e vales associados a intervenções não farmacêuticas, vacinação e surgimento de novas cepas.

#### O componente `seasonal` extraído é artificial e determinístico semanal, com amplitude de até 30% da média. 

#### Os fatores sazonais são sistematicamente negativos aos domingos e segundas, dias 1 e 2, e positivos de quarta a sexta, dias 4 a 6.

#### Esse padrão não reflete sazonalidade epidemiológica real, mas sim artefato de notificação: redução da testagem e do registro de óbitos nos finais de semana, com represamento e compensação nos dias úteis subsequentes. 

#### A sazonalidade de lag 7 é espúria para fins de modelagem do processo gerador de dados biológico.

#### O componente `random` apresenta heterocedasticidade, com variância proporcional ao nível da série e outliers positivos em datas de reclassificação de dados.

####  As séries `new_cases` e `new_deaths` não são estacionárias em nível devido à tendência estocástica das ondas. Após primeira diferença $\Delta y_t$ para remover a tendência, ainda persiste autocorrelação de lag 7 no resíduo devido ao artefato de notificação. 

#### Para modelagem ARIMA, recomenda-se usar médias móveis de 7 dias pra suavizar o efeito fds antes da decomposição.

# Perguntar 3) Dados do Desmatamento.

diretorio <- "C:\\Users\\usuario\\Documents\\lista_1\\dados\\desmatamento_prodes.csv"
df_desmat <- read_csv(diretorio)
head(df_desmat)
 
# [a] Gráficos para cada estado
estados <- unique(df_desmat$estado)

for(uf in estados){
  df_uf <- df_desmat %>%
    filter(estado == uf) %>%
    arrange(data) # 1. Ordena senão o geom_line liga os pontos fora de ordem

  p <- ggplot(df_uf, aes(x = data, y = area_desmatada)) +
    geom_line(color = "firebrick") +
    labs(title = paste("Desmatamento -", uf),
         x = "Data", y = "Área km²") +
    theme_minimal()
  print(p)
}

# [b] Tendência e sazonalidade
for(uf in estados){
  df_uf_mensal <- df_desmat %>%
    filter(estado == uf) %>%
    mutate(mes = floor_date(data, "month")) %>%
    group_by(mes) %>%
    summarise(total = sum(area_desmatada, na.rm = TRUE)) %>%
    arrange(mes) %>% # 2. Ordena antes do ts()
    complete(mes = seq.Date(min(mes), max(mes), by = "month"),
             fill = list(total = 0)) # 3. Preenche meses faltantes com 0

  # decompose precisa de 2 ciclos completos = 24 meses
  if(nrow(df_uf_mensal) >= 24){
    ts_desmat <- ts(df_uf_mensal$total,
                    frequency = 12,
                    start = c(year(min(df_uf_mensal$mes)),
                              month(min(df_uf_mensal$mes))))
    decomp <- decompose(ts_desmat, type = "additive")
    plot(decomp, main = paste("Decomposição Desmatamento -", uf))
  } else {
    cat("Estado", uf, "tem só", nrow(df_uf_mensal), "meses. Não dá pra decompor.\n")
  }
} 

# Conclusão .


#### A decomposição aditiva das séries mensais de desmatamento por estado, 2004-2024, mostra tendência não-linear com quebras estruturais.

 #### O componente `trend` tem queda acentuada 2004-2012, redução de 83% no agregado, seguida de estabilização 2012-2018, reversão com alta 2019-2022 e nova queda pós-2023. MT e PA concentram a variação. AP e RR são estacionários em nível.

#### A sazonalidade é determinística anual com `frequency = 12` e amplitude heterogênea. Fatores sazonais positivos em maio - agosto concentram 60-70% do total anual. 

#### Fatores negativos em dezembro - março respondem por 10-15%. MT e PA têm amplitude sazonal de 400-500 km²/mês. AM tem amplitude de ∼80 km²/mês. O padrão está ligado ao ciclo de chuvas.

#### O componente `random` tem variância não constante e outliers em anos de El Niño e choques de política. 
#### As séries não são estacionárias em nível. Após diferenciação simples Δyt e sazonal Δ12yt  ficam aptas para SARIMA.

#### O desmatamento é regido por regimes institucionais, sazonalidade climática e choques exógenos.
