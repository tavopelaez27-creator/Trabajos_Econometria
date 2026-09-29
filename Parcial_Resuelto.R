#########################################################################
###############                                           ###############
##                       UNIVERSIDAD DEL QUINDIO                       ##
##                        PROGRAMA DE ECONOMIA                         ##
##                       PARCIAL 1 ECONOMETRIA 2                       ##
#########################################################################

# Gustavo Adolfo Pelaez Cifuentes
# gapelaezc@uqvirtual.edu.co
# +57 3027513970

install.packages("usethis")
library(readxl)
library(dplyr)
library(ggplot2)
library(lmtest) 
library(car)      
library(nortest)
library(moments) 

# ---- 1. IMPORTAR DATOS --------------------------------------------------
ruta <- "C:/Users/tavop/Downloads/Datos_Parcial.xlsx" 
datos <- read_excel(ruta, sheet = "Hoja1")
names(datos) <- c("Cod","Pais","POBREZA","PIB_PER","GASTO_EDU","ESTU_UNI",
                   "DESEMPLEO","INFORMALIDAD")

str(datos)
summary(datos)

# (a) ESPECIFICACION DEL MODELO

# Variable dependiente:
#   POBREZA = % de poblacion en pobreza extrema (Banco Mundial, SI.POV.DDAY)
#
# Variables explicativas y signo esperado (teoria economica):
#   PIB_PER (PIB per capita)      -> signo esperado NEGATIVO
#         A mayor ingreso por habitante, menor incidencia de pobreza
#         (hipotesis del "goteo"/crecimiento pro-pobre, Kuznets).
#         NOTA: la variable presenta escalas muy heterogeneas entre paises
#         (posibles unidades en moneda local, no en USD PPA), por lo que se
#         usa su logaritmo natural ln(PIB_PER) para estabilizar la varianza
#         y suavizar la fuerte asimetria (ver diagnostico de supuestos).
#
#   ESTU_UNI (tasa bruta de matricula en educacion terciaria, % )
#                                   -> signo esperado NEGATIVO
#         Mayor acumulacion de capital humano incrementa la empleabilidad y
#         los ingresos futuros, reduciendo la probabilidad de pobreza.
#
#   DESEMPLEO (tasa de desempleo, % de la fuerza laboral)
#                                   -> signo esperado POSITIVO
#         La perdida de ingreso laboral por desempleo eleva directamente la
#         probabilidad de que un hogar caiga en pobreza.
#

# (b) ANALISIS DESCRIPTIVO CON TEST ESTADISTICOS


datos <- datos %>% filter(Pais != "Sub-Saharan Africa")
datos$ESTU_UNI[datos$ESTU_UNI == 0] <- NA

base <- datos %>%
  filter(!is.na(POBREZA), !is.na(PIB_PER), !is.na(ESTU_UNI), !is.na(DESEMPLEO)) %>%
  mutate(lnPIB = log(PIB_PER))

cat("N observaciones utiles:", nrow(base), "\n")

# --- Estadisticos descriptivos ---

resumen <- base %>%
  summarise(across(c(POBREZA, PIB_PER, lnPIB, ESTU_UNI, DESEMPLEO),
                    list(media = mean, mediana = median, sd = sd,
                         min = min, max = max,
                         asimetria = ~moments::skewness(.x))))
print(resumen)

# --- Tabla de frecuencias de POBREZA por categorias ---

base$Cat_Pobreza <- cut(base$POBREZA,
                         breaks = c(-0.01, 0.5, 1.5, 3, 6, 100),
                         labels = c("Muy baja (0-0.5%)","Baja (0.5-1.5%)",
                                    "Media (1.5-3%)","Alta (3-6%)",
                                    "Muy alta (>6%)"))
tabla_frec <- table(base$Cat_Pobreza)
tabla_frec_rel <- prop.table(tabla_frec) * 100
tabla_frec_acum <- cumsum(tabla_frec)
print(cbind(Frecuencia = tabla_frec, Frec_Relativa = round(tabla_frec_rel,1),
            Frec_Acumulada = tabla_frec_acum))

# --- Diagrama de barras de la tabla de frecuencias ---

ggplot(base, aes(x = Cat_Pobreza)) +
  geom_bar(fill = "#2E5A88", color = "black") +
  labs(title = "Distribucion de frecuencias de POBREZA por pais",
       x = "Categoria de POBREZA", y = "Numero de paises") +
  theme_minimal()

# --- Diagramas de caja (deteccion visual de atipicos) ---

boxplot(base$POBREZA, main = "POBREZA (%)", col = "#8FAEDB")
boxplot(base$lnPIB, main = "ln(PIB per capita)", col = "#8FAEDB")
boxplot(base$ESTU_UNI, main = "Matricula universitaria (%)", col = "#8FAEDB")
boxplot(base$DESEMPLEO, main = "Desempleo (%)", col = "#8FAEDB")

# --- Deteccion y eliminacion de atipicos en POBREZA ---

Q1 <- quantile(base$POBREZA, 0.25)
Q3 <- quantile(base$POBREZA, 0.75)
IQR_v <- Q3 - Q1
lim_inf <- Q1 - 1.5 * IQR_v
lim_sup <- Q3 + 1.5 * IQR_v

atipicos <- base %>% filter(POBREZA < lim_inf | POBREZA > lim_sup)
print(atipicos[, c("Pais","POBREZA")])

base_limpia <- base %>% filter(POBREZA >= lim_inf, POBREZA <= lim_sup)
cat("N sin atipicos:", nrow(base_limpia), "\n")

boxplot(base$POBREZA, base_limpia$POBREZA,
        names = c("Con atipicos","Sin atipicos"),
        col = c("#DB8F8F","#8FDB9E"),
        main = "POBREZA antes/despues de remover atipicos")

# --- Prueba de medias (t de Welch): POBREZA segun nivel de desempleo ---

mediana_desempleo <- median(base_limpia$DESEMPLEO)
base_limpia$Grupo_Desempleo <- ifelse(base_limpia$DESEMPLEO > mediana_desempleo,
                                       "Alto desempleo","Bajo desempleo")
t.test(POBREZA ~ Grupo_Desempleo, data = base_limpia)   

# --- Prueba de normalidad de la variable dependiente ---

shapiro.test(base_limpia$POBREZA)


# (c) ESTIMACION DE LOS PARAMETROS

modelo <- lm(POBREZA ~ lnPIB + ESTU_UNI + DESEMPLEO, data = base_limpia)
summary(modelo)


# (d) TABLA ANOVA

anova(modelo)


# (e) PRUEBA DE SIGNIFICANCIA INDIVIDUAL (t)

summary(modelo)$coefficients

# H0: Bj = 0  vs  H1: Bj != 0   (t calculado y p-valor en la tabla anterior)


# (f) PRUEBA DE SIGNIFICANCIA GLOBAL (F)

summary(modelo)$fstatistic

# H0: B1=B2=B3=0  vs  H1: al menos un Bj != 0


# (g) VERIFICACION DE SUPUESTOS (Gauss-Markov)

plot(modelo, which = 1)

bptest(modelo)

shapiro.test(resid(modelo))
plot(modelo, which = 2)     # QQ-plot
hist(resid(modelo), main = "Histograma de residuos", col = "#8FAEDB")

dwtest(modelo)

vif(modelo)

plot(modelo, which = 4)     
plot(modelo, which = 5)

#COMPARACION DE LA POBREZA ENTRE CONTINENTES

base_cont <- datos

continente <- c(
  "Argentina"="America","Bolivia"="America","Brazil"="America","Colombia"="America",
  "Costa Rica"="America","Dominican Republic"="America","Ecuador"="America",
  "Guatemala"="America","Honduras"="America","Panama"="America","Peru"="America",
  "Paraguay"="America","El Salvador"="America","Uruguay"="America","United States"="America",
  "Austria"="Europa","Belgium"="Europa","Bulgaria"="Europa","Cyprus"="Europa","Czechia"="Europa",
  "Denmark"="Europa","Spain"="Europa","Estonia"="Europa","Finland"="Europa","France"="Europa",
  "Greece"="Europa","Croatia"="Europa","Ireland"="Europa","Italy"="Europa","Lithuania"="Europa",
  "Luxembourg"="Europa","Latvia"="Europa","Moldova"="Europa","Malta"="Europa","Norway"="Europa",
  "Poland"="Europa","Portugal"="Europa","Romania"="Europa","Russian Federation"="Europa",
  "Serbia"="Europa","Slovak Republic"="Europa","Slovenia"="Europa","Sweden"="Europa",
  "Armenia"="Asia","Georgia"="Asia","Indonesia"="Asia","Iran, Islamic Rep."="Asia","Iraq"="Asia",
  "Kyrgyz Republic"="Asia","Philippines"="Asia","Thailand"="Asia","Tajikistan"="Asia",
  "Turkiye"="Asia","Uzbekistan"="Asia",
  "Kiribati"="Oceania",
  "Rwanda"="Africa"
)
base_cont$Continente <- continente[base_cont$Pais]

# --- Estadisticos descriptivos por continente ---

tabla_continente <- base_cont %>%
  group_by(Continente) %>%
  summarise(n = n(), media = mean(POBREZA), mediana = median(POBREZA),
            sd = sd(POBREZA), min = min(POBREZA), max = max(POBREZA))
print(tabla_continente)

# --- Diagrama de barras: pobreza promedio por continente ---

ggplot(tabla_continente, aes(x = Continente, y = media, fill = Continente)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = media - sd, ymax = media + sd), width = 0.2, na.rm = TRUE) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5) +
  labs(title = "Pobreza promedio por continente", y = "POBREZA promedio (%)") +
  theme_minimal() + theme(legend.position = "none")

# --- Diagrama de cajas por continente (todas las regiones) ---

ggplot(base_cont, aes(x = Continente, y = POBREZA, fill = Continente)) +
  geom_boxplot() +
  labs(title = "Distribucion de POBREZA por continente") +
  theme_minimal() + theme(legend.position = "none")

# --- Diagrama de cajas solo America / Europa / Asia (grupos con n>=10) ---

base_cont3 <- base_cont %>% filter(Continente %in% c("America","Europa","Asia"))
ggplot(base_cont3, aes(x = Continente, y = POBREZA, fill = Continente)) +
  geom_boxplot() +
  labs(title = "POBREZA por continente (America, Europa, Asia)") +
  theme_minimal() + theme(legend.position = "none")

# --- Barras apiladas: categoria de pobreza por continente ---

base_cont$Cat_Pobreza <- cut(base_cont$POBREZA,
                              breaks = c(-0.01, 0.5, 1.5, 3, 6, 100),
                              labels = c("Muy baja (0-0.5%)","Baja (0.5-1.5%)",
                                         "Media (1.5-3%)","Alta (3-6%)","Muy alta (>6%)"))
tabla_cruzada <- table(base_cont$Continente, base_cont$Cat_Pobreza)
print(tabla_cruzada)

ggplot(base_cont, aes(x = Continente, fill = Cat_Pobreza)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Categoria de pobreza por continente", y = "Numero de paises") +
  theme_minimal()

# --- ANOVA de un factor: diferencia de POBREZA entre America, Europa y Asia ---

anova_continente <- aov(POBREZA ~ Continente, data = base_cont3)
summary(anova_continente)

# --- Prueba post-hoc de Tukey: diferencia entre cada par de continentes ---

TukeyHSD(anova_continente)

# --- Alternativa: comparaciones pareadas t de Welch con ajuste de Bonferroni ---

pairwise.t.test(base_cont3$POBREZA, base_cont3$Continente,
                 p.adjust.method = "bonferroni", pool.sd = FALSE)


# COMPARACION DE PAISES POR CONTINENTE

continente <- c(
  "Argentina"="America","Armenia"="Asia","Austria"="Europa","Belgium"="Europa",
  "Bulgaria"="Europa","Bolivia"="America","Brazil"="America","Colombia"="America",
  "Costa Rica"="America","Cyprus"="Europa","Czechia"="Europa","Denmark"="Europa",
  "Dominican Republic"="America","Ecuador"="America","Spain"="Europa","Estonia"="Europa",
  "Finland"="Europa","France"="Europa","Georgia"="Asia","Greece"="Europa",
  "Guatemala"="America","Honduras"="America","Croatia"="Europa","Indonesia"="Asia",
  "Ireland"="Europa","Iran, Islamic Rep."="Asia","Iraq"="Asia","Italy"="Europa",
  "Kyrgyz Republic"="Asia","Kiribati"="Oceania","Lithuania"="Europa","Luxembourg"="Europa",
  "Latvia"="Europa","Moldova"="Europa","Malta"="Europa","Norway"="Europa",
  "Panama"="America","Peru"="America","Philippines"="Asia","Poland"="Europa",
  "Portugal"="Europa","Paraguay"="America","Romania"="Europa","Russian Federation"="Europa",
  "Rwanda"="Africa","El Salvador"="America","Serbia"="Europa","Slovak Republic"="Europa",
  "Slovenia"="Europa","Sweden"="Europa","Thailand"="Asia","Tajikistan"="Asia",
  "Turkiye"="Asia","Uruguay"="America","United States"="America","Uzbekistan"="Asia"
)
base$Continente <- continente[base$Pais]
table(base$Continente)   # America=7, Asia=9, Europa=27, Africa=1

orden_cont <- c("Europa","America","Asia","Africa")
base$Continente <- factor(base$Continente, levels = orden_cont)

# --- Tabla resumen de POBREZA y regresores por continente ---

resumen_continente <- base %>%
  group_by(Continente) %>%
  summarise(N = n(),
            POBREZA_media = mean(POBREZA), POBREZA_mediana = median(POBREZA),
            PIB_PER_media = mean(PIB_PER), ESTU_UNI_media = mean(ESTU_UNI),
            DESEMPLEO_media = mean(DESEMPLEO)) %>%
  arrange(desc(POBREZA_media))
print(resumen_continente)

# --- Grafico de barras: POBREZA por pais, coloreado por continente ---

ggplot(base, aes(x = reorder(Pais, POBREZA), y = POBREZA, fill = Continente)) +
  geom_col() +
  coord_flip() +
  labs(title = "Nivel de POBREZA por pais, agrupado por continente",
       x = "", y = "POBREZA (% de poblacion)") +
  theme_minimal()

# --- Grafico de barras: POBREZA PROMEDIO por continente ---

ggplot(resumen_continente, aes(x = reorder(Continente, -POBREZA_media),
                                y = POBREZA_media, fill = Continente)) +
  geom_col() +
  geom_text(aes(label = round(POBREZA_media,2)), vjust = -0.4) +
  labs(title = "POBREZA promedio por continente", x = "", y = "POBREZA promedio (%)") +
  theme_minimal()

# --- Diagrama de caja: POBREZA por continente ---

ggplot(base, aes(x = Continente, y = POBREZA, fill = Continente)) +
  geom_boxplot() +
  geom_jitter(width = 0.08, alpha = 0.6) +
  labs(title = "Comparacion de POBREZA por continente", x = "", y = "POBREZA (%)") +
  theme_minimal()

# --- Diagramas de caja de las variables explicativas por continente ---

ggplot(base, aes(x = Continente, y = PIB_PER, fill = Continente)) +
  geom_boxplot() + scale_y_log10() +
  labs(title = "PIB per capita por continente (escala log)", x="", y="PIB per capita") +
  theme_minimal()

ggplot(base, aes(x = Continente, y = ESTU_UNI, fill = Continente)) +
  geom_boxplot() +
  labs(title = "Matricula universitaria por continente", x="", y="ESTU_UNI (%)") +
  theme_minimal()

ggplot(base, aes(x = Continente, y = DESEMPLEO, fill = Continente)) +
  geom_boxplot() +
  labs(title = "Desempleo por continente", x="", y="DESEMPLEO (%)") +
  theme_minimal()

# --- Prueba de diferencia de medias entre continentes ---
# ANOVA de un factor (incluye Africa, n=1, referencial)

anova_cont <- aov(POBREZA ~ Continente, data = base)
summary(anova_cont)
TukeyHSD(anova_cont)     

kruskal.test(POBREZA ~ Continente, data = base)

# Comparacion mas robusta: solo America, Europa y Asia (n >= 7 cada uno)

base3 <- base %>% filter(Continente %in% c("America","Europa","Asia")) %>%
  mutate(Continente = droplevels(Continente))
summary(aov(POBREZA ~ Continente, data = base3))
kruskal.test(POBREZA ~ Continente, data = base3)
pairwise.t.test(base3$POBREZA, base3$Continente, p.adjust.method = "bonferroni")


