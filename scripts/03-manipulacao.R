a# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Base de dados -----------------------------------------------------------

sp <- read_rds("dados/sp_amostra.rds")
glimpse(sp)


# filter ------------------------------------------------------------------

# exemplo 1
sp %>% filter(capital_social > 1e6)

# exemplo 2
capital_social_maior <- capital_social_maior %>% filter(nota_imdb > 1e6)
capital_social_maior

# exemplo 3
piracicaba <- sp %>% filter(municipio == "PIRACICABA")
piracicaba

# exemplo 4 - Relembrando as comparações com o R

1 == 1
"a" == "b"
2 > 1
1 < 2
2 >= 3
4 <= 4

# exercício 1
# Criar um objeto `mei` com todas as empresas que optam pelo mei.


# exemplo 5
# operadores lógicos

sp %>% filter(opcao_pelo_mei == 1 & porte == "01")
sp %>% filter(opcao_pelo_simples == 1 & opcao_pelo_mei == 1)
sp %>% filter(opcao_pelo_simples == 1 | opcao_pelo_mei == 1)
sp %>% filter(!(opcao_pelo_simples == 1 | opcao_pelo_mei == 1))
  
sp %>% filter(!(municipio == "SAO PAULO"))
sp %>% filter(municipio != "SAO PAULO")

# exercício 2
# Criar um objeto com as empresas que tem endereço no seu bairro e município.

# exercício 3
# Existe alguma empresa no seu CEP?

# exercício 4
# Liste as empresas dos municípios: SAO PAULO, OSASCO e GUARULHOS

# exemplo 6
# %in%

sp %>% filter(porte %in% c("01", "02", "03"))
sp %>% filter(porte == "01" | porte == "02" | porte == "03")

# exercicio 6
# Refaça o exercício 5 usando o %in%.

# exemplo 7
# Relembrando as operações com NA

NA > 5

10 == NA

NA + 10

NA / 2

NA == NA

# Seja x a idade de Maria. Não sabemos a idade de Maria:
x <- NA

# Seja y a idade de João. Não sabemos a idade de João:
y <- NA

# Maria e João têm a mesma idade?
x == y
#> [1] NA
# Não sabemos.

is.na(x)

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

sp %>% filter(is.na(data_opcao_pelo_simples))

# exercício 7
# Identifique as empresas que possuem pelo menos um telefone cadastrado.

# exemplo 8
# str_detect

sp %>% filter(str_detect(razao_social, "DANIEL"))

# Regex

# [:alnum:]
# [:alpha:]
# [:digit:]
# [:lower:]
# [:punct:]
# [:space:]
# [:upper:]

# ? item precedente é opcional e será encontrado até uma vez.
# 
# * item precedente pode ser encontrado zerou ou mais vezes
# 
# + item precedente é encontrado 1 ou mais vezes
# 
# {n} item precedente é encontrado exatamente 1 `n` vezes
# 
# {n,} item precedente é encotrado `n` ou mais vezes
# 
# {n,m} ite precedente é encontrado de `n` a `m` vezes.

# ^ início da string
# $ fim da string

sp %>% filter(str_detect(razao_social, "DANIEL"))
sp %>% filter(str_detect(razao_social, "DANIEL "))
sp %>% filter(str_detect(razao_social, "^DANIEL "))
sp %>% filter(str_detect(razao_social, "[:digit:]"))
sp %>% filter(str_detect(razao_social, "[:digit:]$"))
sp %>% filter(str_detect(razao_social, "[:digit:]{2,5}"))
sp %>% filter(str_detect(razao_social, "[:punct:]"))

# exercício 8
# Descubra as empresas que possuem razão social que comece com 3 numéros.


# exemplo 9

sp %>% filter_at(vars(razao_social, nome_fantasia), ~str_detect(.x, "DANIEL "))
sp %>% filter_if(is.Date, ~.x > ymd("2018-01-01"))

# arrange -----------------------------------------------------------------

# exemplo 1

sp %>% arrange(capital_social)

# exemplo 2

sp %>% arrange(desc(capital_social))

# exemplo 3

imdb %>% arrange(capital_social, razao_social)

# exercício 1
# Ordene as empresas por ordem crescente de início da atividade.

# exemplo 4
# NA

df <- tibble(x = c(NA, 2, 1), y = c(1, 2, 3))

df %>% arrange(x)
df %>% arrange(!is.na(x), x)

# exemplo 5

sp %>% filter(municipio == "OSASCO") %>% arrange(desc(capital_social))

# exercício 2 
# Ordene por ordem de inicio de atividade as empresas de Piracicaba.

# select ------------------------------------------------------------------

# exemplo 1

sp %>% select(cnpj, razao_social, municipio)

# exemplo 2 

sp %>% select(starts_with("ddd"))

# starts_with(): Starts with a prefix.
# ends_with(): Ends with a suffix.
# contains(): Contains a literal string.
# matches(): Matches a regular expression.
# num_range(): Matches a numerical range like x01, x02, x03.
# one_of(): Matches variable names in a character vector.
# everything(): Matches all variables.
# last_col(): Select last variable, possibly with an offset.

# exemplo 3

sp %>% select(-starts_with("ddd"), -cnpj)

# exercício 1
# Crie uma tabela com apenas as colunas de `cnpj`, `cnae_fiscal` e `municipio`

# exercício 2
# Remova as colunas de telefone de 3 formas diferentes.

# exercício 3
# Crie uma tabela com apenas as empresas de piracicaba, com as colunas `cnpj`, 
# `razao_social` e `data_inicio_atividade` ordenada por data de inicio da 
# atividade.

# mutate ------------------------------------------------------------------

# exemplo 1

sp %>% mutate(capital_social = capital_social / 1000)

# exemplo 2

sp %>% mutate(capital_social_milhares = capital_social/1000)

# exercício 1
# Crie uma coluna com os 3 primeiros dígitos do CEP das empreas. Dica `str_sub`.

# exercicio 2
# Crie uma coluna com a idade da empresa em dias. Dica `difftime`.

# exercício 3
# Crie uma tabela com as 10 empresas mais antigas. Dica `row_number`.

# exemplo 3

sp %>% mutate(perc_rank = percent_rank(capital_social))

# row_number(): equivalent to rank(ties.method = "first")\
# min_rank(): equivalent to rank(ties.method = "min")
# dense_rank(): like min_rank(), but with no gaps between ranks
# percent_rank(): a number between 0 and 1 computed by rescaling min_rank to [0, 1]
# cume_dist(): a cumulative distribution function. Proportion of all values less than or equal to the current rank.
# ntile(): a rough rank, which breaks the input vector into n buckets.

# exemplo 5

sp %>% 
  mutate_if(is.Date, as.character)

sp %>% 
  mutate_at(
    vars(razao_social, nome_fantasia), 
    ~stringi::stri_trans_general(.x, "ascii")
    )

# summarise ---------------------------------------------------------------

# exemplo 1

sp %>% summarise(media_capital = mean(capital_social, na.rm=TRUE))

# exemplo 2

sp %>% summarise(
  media_capital = mean(capital_social, na.rm=TRUE),
  mediana_capital = median(capital_social, na.rm = TRUE)
)

# exemplo 3

sp %>% summarise(
  media_capital = mean(capital_social, na.rm=TRUE),
  mediana_capital = median(capital_social, na.rm = TRUE),
  qtd = n(),
  qtd_cnpj = n_distinct(cnpj)
)

# exemplo 4

sp %>%
  summarise(n_sp = sum(municipio == "SAO PAULO", na.rm = TRUE))

# exercício 1
# Use o `summarise` para calcular a quantidade de empresas que estão em Piracicaba.

# exercício 2
# Calcule o quantil 25 e o quantil 75 do capital social. dica: função `quantile`

# exercício 3
# Calcule o tempo médio que uma empresa fica no Simples.

# group_by + summarise ----------------------------------------------------

# exemplo 1

sp %>% group_by(municipio)

# exemplo 2

sp %>% 
  group_by(municipio) %>% 
  summarise(qtd_empresas = n())

# exemplo 3

sp %>% 
  group_by(municipio, bairro) %>% 
  summarise(qtd_empresas = n())

# exercício 1
# Crie uma tabela com os 10 municípios com mais empresas.

# exercício 2
# Crie uma tabela com o capital social médio e mediano por porte da empresa.

# exercício 3
# Crie uma tabela com os 10 bairros de São Paulo que possuem a maior média de 
# capital social das empresas. 

# exemplo 4

sp %>%
  mutate(cep2 = str_sub(cep, 1, 2)) %>% 
  filter(cep2 %in% c(10, 20, 30)) %>% 
  group_by(porte) %>%
  summarise(qtd_empresas = n()) %>%
  arrange(desc(qtd_empresas))

# exemplo 5

sp %>% 
  filter(municipio == "PIRACICABA") %>%
  group_by(descricao_tipo_logradouro) %>%
  summarise(
    qtd = n()
    )

# exemplo 6

sp %>% 
  group_by(descricao_tipo_logradouro) %>% 
  summarise_if(is.Date, list(min = min, max = max))

# joins -------------------------------------------------------------------

# exemplo 1

socios <- read_rds("dados/socios_sp_amostra.rds")

sp_empresas <- sp %>%
  left_join(socios, by = c("cnpj"))

# exemplo 2

# https://github.com/georgevbsantiago/qsacnpj
# remotes::install_github("georgevbsantiago/qsacnpj")

com_cnae <- sp %>% 
  mutate(cnae_fiscal = sprintf("%07d", cnae_fiscal)) %>%
  left_join(qsacnpj::tab_cnae, by = c("cnae_fiscal" = "cod_cnae"))

com_cnae %>% 
  filter(nm_cnae == "Cultivo de cana-de-açúcar") %>% 
  count(municipio, sort = TRUE)

# exemplo 3

sp %>% 
  mutate(situacao_cadastral = as.character(situacao_cadastral)) %>% 
  left_join(
    qsacnpj::tab_situacao_cadastral, 
    by = c("situacao_cadastral" = "cod_situacao_cadastral")
  ) %>% 
  count(nm_situacao_cadastral)

# exercicio 1
# Calcule a quantidade média de sócios por cnpj. 

# exercício 2
# Calcule a quantidade média e mediana de empresas por sócio.

# exercício 3
# Desafio: Quais empresas possuem sócios da mesma família? 
# (possuem o mesmo último sobrenome)

# outros joins ------------------------------------------------------------

inner_join
anti_join
right_join
full_join

# tidyr -------------------------------------------------------------------

tidyr::relig_income

relig_income %>% 
  pivot_longer(-religion,  names_to = "income", values_to = "n")

relig_income %>% 
  pivot_longer(contains("k"),  names_to = "income", values_to = "n")

tidyr::fish_encounters

fish_encounters %>% 
  pivot_wider(
    names_from = station, 
    values_from = seen, 
    values_fill = list(seen = 0)
  )


# Mais infos aqui: vignette("pivot")