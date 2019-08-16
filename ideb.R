library(readxl)
library(dplyr)
col_names <- c("UF", "CO_MUNICIPIO", "NO_MUNICIPIO", "CO_ESCOLA","NO_ESCOLA", "REDE",
               "APROVACAO_1A5ANO_2005", "APROVACAO_1ANO_2005","APROVACAO_2ANO_2005","APROVACAO_3ANO_2005","APROVACAO_4ANO_2005","APROVACAO_5ANO_2005", "APROVACAO_IND_RENDIMENTO_2005",
               "APROVACAO_1A5ANO_2007", "APROVACAO_1ANO_2007","APROVACAO_2ANO_2007","APROVACAO_3ANO_2007","APROVACAO_4ANO_2007","APROVACAO_5ANO_2007", "APROVACAO_IND_RENDIMENTO_2007",
               "APROVACAO_1A5ANO_2009", "APROVACAO_1ANO_2009","APROVACAO_2ANO_2009","APROVACAO_3ANO_2009","APROVACAO_4ANO_2009","APROVACAO_5ANO_2009", "APROVACAO_IND_RENDIMENTO_2009",
               "APROVACAO_1A5ANO_2011", "APROVACAO_1ANO_2011","APROVACAO_2ANO_2011","APROVACAO_3ANO_2011","APROVACAO_4ANO_2011","APROVACAO_5ANO_2011", "APROVACAO_IND_RENDIMENTO_2011",
               "APROVACAO_1A5ANO_2013", "APROVACAO_1ANO_2013","APROVACAO_2ANO_2013","APROVACAO_3ANO_2013","APROVACAO_4ANO_2013","APROVACAO_5ANO_2013", "APROVACAO_IND_RENDIMENTO_2013",
               "APROVACAO_1A5ANO_2015", "APROVACAO_1ANO_2015","APROVACAO_2ANO_2015","APROVACAO_3ANO_2015","APROVACAO_4ANO_2015","APROVACAO_5ANO_2015", "APROVACAO_IND_RENDIMENTO_2015",
               "APROVACAO_1A5ANO_2017", "APROVACAO_1ANO_2017","APROVACAO_2ANO_2017","APROVACAO_3ANO_2017","APROVACAO_4ANO_2017","APROVACAO_5ANO_2017", "APROVACAO_IND_RENDIMENTO_2017",
               "SAEB_MATEMATICA_2005", "SAEB_PORTUGUES_2005", "SAEB_MEDIA_2005",
               "SAEB_MATEMATICA_2007", "SAEB_PORTUGUES_2007", "SAEB_MEDIA_2007",
               "SAEB_MATEMATICA_2009", "SAEB_PORTUGUES_2009", "SAEB_MEDIA_2009",
               "SAEB_MATEMATICA_2011", "SAEB_PORTUGUES_2011", "SAEB_MEDIA_2011",
               "SAEB_MATEMATICA_2013", "SAEB_PORTUGUES_2013", "SAEB_MEDIA_2013",
               "SAEB_MATEMATICA_2015", "SAEB_PORTUGUES_2015", "SAEB_MEDIA_2015",
               "SAEB_MATEMATICA_2017", "SAEB_PORTUGUES_2017", "SAEB_MEDIA_2017",
               "IDEB_2005","IDEB_2007","IDEB_2009","IDEB_2011", "IDEB_2013", "IDEB_2015","IDEB_2017",
               "IDEB_PROJ_2007", "IDEB_PROJ_2009", "IDEB_PROJ_2011", "IDEB_PROJ_2013","IDEB_PROJ_2015", "IDEB_PROJ_2017", "IDEB_PROJ_2019", "IDEB_PROJ_2021")
corrige <- function(x){
    
}

download.file(
    url = "http://download.inep.gov.br/educacao_basica/portal_ideb/planilhas_para_download/2017/divulgacao_anos_iniciais-escolas-2017.xlsx",
    destfile = "data/ideb_ensfun_ini.xlsx", method = "wininet", mode = "wb")
ideb <- read_xlsx(path = "data/ideb_ensfun_ini.xlsx", skip = 7, col_names = col_names)
ideb[, c(7:ncol(ideb))] <- apply(
    X = ideb[, c(7:ncol(ideb))],
    MARGIN = 2,
    FUN = function(y){
        y <- as.numeric(y)
    })

saeb_media <- ideb %>% 
    filter(UF == "DF") %>% 
    select(NO_ESCOLA, REDE,
           SAEB_MEDIA_2011,SAEB_MEDIA_2013,SAEB_MEDIA_2015,SAEB_MEDIA_2017)
saeb_media$MEDIA_GERAL <- rowMeans(saeb_media[, c(3:ncol(saeb_media))], na.rm = TRUE)
saeb_media <- saeb_media %>% 
    arrange(desc(MEDIA_GERAL))
saeb_media <- head(saeb_media, 10)

ideb_media <- ideb %>% 
    filter(UF == "DF") %>% 
    select(NO_ESCOLA, REDE,
           IDEB_2011,IDEB_2013,IDEB_2015,IDEB_2017)
ideb_media$MEDIA_GERAL <- rowMeans(ideb_media[, c(3:ncol(ideb_media))], na.rm = TRUE)
ideb_media <- ideb_media %>% 
    arrange(desc(MEDIA_GERAL))
ideb_media <- head(ideb_media, 10)

