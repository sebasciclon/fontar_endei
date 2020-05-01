
# endei ii ------------------------------------------------------------------------------

# prueba g it
path = getwd()
source('scripts/libraries.r')


# read database -------------------------------------------------------------------------

file = glue(path, '/raw/ENDEI II_anonimizada_Junio_2019.sav')

endei = read.spss(file, to.data.frame=TRUE)

endei = endei %>% 
  as_tibble() %>% 
  setNames(tolower(colnames(endei)))

endei_colnames = colnames(endei) %>% as.data.frame() %>% 
  setNames('variable') %>% mutate(flag_db = 1) %>% 
  mutate(column = variable)


# pre procesamiento de variables ----------------------------------------------

# levanto el gsheet
p_gsheet = 'https://docs.google.com/spreadsheets/d/1Jbj3heTwCzAKYwZ_egHeicJs29gedl7_imyUhxKCU8c/edit#gid=0'
endei_vars = read_sheet(p_gsheet)

nrow(endei_vars %>% filter(modelo == 'si')) # 682
nrow(endei_vars %>% filter(tipo == 'categorica')) # 507
nrow(endei_vars %>% filter(tipo == 'numeric')) # 174

table(endei_vars$familia)

# analizo las variables que coinciden base y gsheet
endei_vars = endei_vars %>% 
  left_join(endei_colnames, by = "variable") %>% 
  mutate(flag_db = ifelse(is.na(flag_db), 0, 1)) %>% filter(flag_db==1)

# seteo las variables en el formato correcto
to_char = endei_vars %>% filter(tipo == 'categorica') %>%
  select(variable)

to_num = endei_vars %>% filter(tipo == 'numeric') %>%
  select(variable)

endei = endei %>% 
  mutate_at(to_char$variable, funs(as.character(.))) %>% 
  mutate_at(to_num$variable, funs(as.numeric(.))) %>% 
  select(endei_vars$variable)

# exploratorio
explora = skim_to_list(endei)
explora$numeric
explora$factor

# analizo las var del fontar

# p.8.1.5 - conoce fontar
table(endei$p.8.1.5)
# finan_fontar - financia AI con fontar
endei = endei %>% mutate(d_fontar=ifelse(finan_fontar!=0 & !is.na(finan_fontar), 1, 0))
table(endei$d_fontar)

# perfil
table(endei$perfil_inn, endei$d_fontar)

# i+d
table(endei$p.1.7)
sum(is.na(endei$p.1.8))
typeof(endei$p.1.8)

# p.4.1.1 - obtuvo nuevos productos
table(endei$p.4.1.1, endei$d_fontar)

table(endei$p.2.2)

table(endei$p.7.1.1.a)

typeof(endei$ingr_total_2016)
endei %>% 
  ggplot(aes(x=as.numeric(ingr_total_2016))) +
  geom_histogram()

typeof(endei$toma_decision)
table(endei$va14)
  table(endei$p.10.8.1)
    

# variables cluster
endei$cant_compus/compus_tra