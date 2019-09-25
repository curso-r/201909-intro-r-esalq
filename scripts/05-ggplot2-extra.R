library(tidyverse)
library(lubridate)

sp <- read_rds("dados/sp_amostra.rds")
socios <- read_rds("dados/socios_sp_amostra.rds")

natureza <- qsacnpj::tab_natureza_juridica %>% 
  mutate(
    codigo_natureza_juridica = as.numeric(cod_subclass_natureza_juridica)
  )

# 1. Natureza -------------------------------------------------------------

natureza_sp <- sp %>% 
  left_join(natureza, "codigo_natureza_juridica") %>% 
  replace_na(list(nm_natureza_juridica = "Vazio",
                  nm_subclass_natureza_juridica = "Vazio")) %>% 
  mutate(
    tipo = if_else(
      nm_natureza_juridica == "Administração Pública",
      "Administração Pública", 
      nm_subclass_natureza_juridica
    ),
    tipo = fct_lump(tipo, 10, other_level = "Outros")
  ) %>% 
  count(tipo, sort = TRUE) %>% 
  mutate(prop = scales::percent(n/sum(n)))

natureza_sp %>%
  mutate(tipo = fct_reorder(str_wrap(tipo, 40), n)) %>% 
  ggplot(aes(x = tipo, y = n / 1e3)) +
  geom_col(fill = "cyan", alpha = .9) +
  coord_flip() +
  geom_text(aes(label = prop), nudge_y = 150) +
  theme_minimal(14) +
  labs(x = "Natureza Jurídica", 
       y = "Quantidade de empresas (milhares)")

# 2. Data da abertura. -------------------------------------------

d_abertura <- sp %>% 
  filter(identificador_matriz_filial == 1) %>% 
  mutate(
    mes_atividade = floor_date(data_inicio_atividade, "quarter"),
    ano_atividade = floor_date(data_inicio_atividade, "year")
  ) %>% 
  filter(
    mes_atividade > "1950-01-01",
    mes_atividade < "2019-06-01"
  ) %>% 
  count(ano_atividade)

d_abertura %>% 
  mutate(mes_atividade = ano_atividade) %>% 
  filter(
    year(mes_atividade) > 1950,
    year(mes_atividade) < 2019
  ) %>% 
  ggplot(aes(x = mes_atividade, y = n/1000)) +
  geom_line(size = 1) +
  theme_minimal(14) +
  scale_x_date(breaks = scales::date_breaks("3 year"),
               labels = scales::date_format("%Y")) +
  scale_y_continuous(limits = c(0, 800)) +
  labs(x = "Ano de início das atividades",
       y = "Quantidade de empresas (milhares)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank())



# 3. Atividade econômica principal. ------------------------------

tab_cnae <- sp %>% 
  mutate(cod_cnae = str_pad(cnae_fiscal, 7, "left", "0")) %>% 
  left_join(qsacnpj::tab_cnae, "cod_cnae") %>% 
  replace_na(
    list(nm_secao = "Vazio", nm_divisao = "Vazio",
         nm_grupo = "Vazio", nm_classe = "Vazio",
         nm_cnae = "Vazio")
  ) %>% 
  count(nm_secao, nm_divisao, nm_grupo, nm_classe, nm_cnae)

d_cnae <- sp %>% 
  mutate(cod_cnae = str_pad(cnae_fiscal, 7, "left", "0")) %>% 
  left_join(qsacnpj::tab_cnae, "cod_cnae") %>% 
  replace_na(list(nm_secao = "Vazio", nm_divisao = "Vazio",
                  nm_grupo = "Vazio", nm_classe = "Vazio",
                  nm_cnae = "Vazio"))

d_cnae %>% 
  mutate(tipo = fct_lump(nm_secao, 14)) %>% 
  count(tipo) %>% 
  mutate(tipo = fct_reorder(str_wrap(tipo, 40), n)) %>% 
  mutate(prop = scales::percent(n/sum(n))) %>% 
  ggplot(aes(x = tipo, y = n / 1e3)) +
  geom_col(fill = "cyan", alpha = .9) +
  coord_flip() +
  geom_text(aes(label = prop), nudge_y = 100) +
  theme_minimal(12) +
  labs(x = "Atividade principal", 
       y = "Quantidade de empresas (milhares)")


# 4. Município da sede. -------------------------------------------

d_muni <- brazilmaps::get_brmap(
  "City", 
  geo.filter = list(State = 35)
) %>% 
  mutate(nome = abjutils::rm_accent(nome))

sp %>% 
  count(municipio) %>%
  mutate(nome = case_when(
    municipio == "BIRITIBA-MIRIM" ~ "BIRITIBA MIRIM",
    municipio == "MOGI-GUACU" ~ "MOGI GUACU",
    TRUE ~ municipio
  )) %>% 
  inner_join(d_muni, "nome") %>% 
  mutate(ncat = cut(n/1000, ceiling(c(0, 1e3, 2e3, 1e4, 1e5, 
                                      max(n))/1000),
                    dig.lab = 10)) %>% 
  st_as_sf() %>% 
  ggplot(aes(fill = ncat)) +
  geom_sf(colour = "black", size = .3) +
  scale_fill_viridis_d(option = "A", begin = 0.1, alpha = .9) +
  theme_void(14) +
  theme(legend.position = c(.85,.8)) +
  labs(fill = "Quantidade de\nempresas (milhares)")

# 5. Quantidade de sócios. ----------------------------------------

qtd_socios <- socios %>% 
  count(cnpj)

empresa_natureza <- sp %>% 
  left_join(natureza, "codigo_natureza_juridica") %>% 
  replace_na(list(nm_natureza_juridica = "Vazio",
                  nm_subclass_natureza_juridica = "Vazio")) %>% 
  mutate(
    tipo = if_else(
      nm_natureza_juridica == "Administração Pública",
      "Administração Pública", nm_subclass_natureza_juridica
    ),
    tipo = fct_lump(tipo, 10, other_level = "Outros")
  )

qtd_socios %>% 
  mutate(n = fct_lump(str_pad(n, 2, "left", 0), 10, 
                      other_level = "11 ou mais")) %>% 
  count(n) %>% 
  mutate(prop = percent(nn/sum(nn)), n = fct_rev(n)) %>% 
  ggplot(aes(x = n, y = nn / 1e3)) +
  geom_col(fill = vistrnv::trnv_colors()[1], alpha = .9) +
  coord_flip() +
  geom_text(aes(label = prop), nudge_y = 30) +
  theme_minimal(14) +
  labs(x = "Quantidade de sócios", 
       y = "Quantidade de empresas (milhares)")


d_plot_qtd_socios <- qtd_socios %>% 
  inner_join(select(empresa_natureza, cnpj, tipo), "cnpj") %>% 
  filter(str_detect(tipo, "Limitada|Anôn")) %>% 
  mutate(tipo = if_else(str_detect(tipo, "Limitada"), 
                        "Limitada", "SA")) %>% 
  mutate(n = fct_lump(str_pad(n, 2, "left", 0), 10, 
                      other_level = "11 ou mais")) %>% 
  count(tipo, n) %>% 
  group_by(tipo) %>% 
  mutate(prop = nn/sum(nn), n = fct_rev(n)) %>% 
  ungroup()

d_plot_qtd_socios %>% 
  ggplot(aes(x = n, y = prop, fill = tipo)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_viridis_d(begin = 0.3, end = 0.8) +
  theme_minimal(14) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Quantidade de sócios", 
       y = "Proporção das empresas",
       fill = "Tipo") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.position = c(.8, .2))


# 6 -----------------------------------------------------------------------

sp %>% 
  mutate(cod_cnae = str_pad(cnae_fiscal, 7, "left", "0")) %>% 
  left_join(qsacnpj::tab_cnae, "cod_cnae") %>% 
  replace_na(
    list(nm_secao = "Vazio", nm_divisao = "Vazio",
         nm_grupo = "Vazio", nm_classe = "Vazio",
         nm_cnae = "Vazio")
  ) %>% 
  filter(nm_grupo == "Pecuária") %>% 
  group_by(ano = year(data_inicio_atividade), nm_classe) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = ano, y = n, color = nm_classe)) +
  geom_line() +
  facet_wrap(~nm_classe, scales = "free")
  
  
  

# Exercícios --------------------------------------------------------------




