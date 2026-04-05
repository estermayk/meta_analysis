library(dplyr)
library(effsize)
rm(list=ls())

#import data
srdata <- read.csv('data/snell_rood_data.csv')

#remove species found only in urban or only in rural
srdatafilt <- srdata %>%
  group_by(Latin.Name) %>%
  filter(all(c(0, 1) %in% Population.designation)) %>%
  ungroup()

list(unique(srdatafilt$Latin.Name))

#b_brevicauda moddel
b_brev <- srdatafilt[srdatafilt$Latin.Name=="BLARINA BREVICAUDA",]
b_brev_model <- lm(cranialvolume~bodylength+Population.designation-1, data = b_brev) 

#c_gapperi model
c_gap <- srdatafilt[srdatafilt$Latin.Name=='CLETHRIONOMYS GAPPERI',]
c_gap_model <- lm(cranialvolume~bodylength+Population.designation-1, data = c_gap) 

#e_fuscus model
e_fus <- srdatafilt[srdatafilt$Latin.Name=="EPTESICUS FUSCUS",]
e_fus_model <- lm(cranialvolume~bodylength+Population.designation-1, data = e_fus) 

#g_bur model
g_bur <- srdatafilt[srdatafilt$Latin.Name=="GEOMYS BURSARIUS",]
g_bur_model <- lm(cranialvolume~bodylength+Population.designation-1, data = g_bur) 

#m_pen model
m_pen <- srdatafilt[srdatafilt$Latin.Name=="MICROTUS PENNSYLVANICUS",]
m_pen_model <- lm(cranialvolume~bodylength+Population.designation-1, data = m_pen) 

#m_luc model
m_luc <- srdatafilt[srdatafilt$Latin.Name=="MYOTIS LUCIFUGUS",]
m_luc_model <- lm(cranialvolume~bodylength+Population.designation-1, data = m_luc) 

#p_leu model
p_leu <- srdatafilt[srdatafilt$Latin.Name=="PEROMYSCUS LEUCOPUS",]
p_leu_model <- lm(cranialvolume~bodylength+Population.designation-1, data = p_leu) 

#s_car model
s_car <- srdatafilt[srdatafilt$Latin.Name=="SCIURUS CAROLINENSIS",]
s_car_model <- lm(cranialvolume~bodylength+Population.designation-1, data = s_car) 

#s_cin model
s_cin <- srdatafilt[srdatafilt$Latin.Name=="SOREX CINEREUS",]
s_cin_model <- lm(cranialvolume~bodylength+Population.designation-1, data = s_cin)

#t_hud model
t_hud <- srdatafilt[srdatafilt$Latin.Name=="TAMIASCIURUS HUDSONICUS",]
t_hud_model <- lm(cranialvolume~bodylength+Population.designation-1, data = t_hud) 

summary(b_brev_model)
summary(c_gap_model)
summary(e_fus_model)
summary(g_bur_model)
summary(m_luc_model)
summary(m_pen_model)
summary(p_leu_model)
summary(s_car_model)
summary(s_cin_model)
summary(t_hud_model)


#---- calculating means and SE ----

#b_brev
rurmeanb_brev <- mean(b_brev$relativecc[b_brev$Population.designation == 0])
rursdb_brev <- sd(b_brev$relativecc[b_brev$Population.designation == 0])
urbmeanb_brev <- mean(b_brev$relativecc[b_brev$Population.designation == 1])
urbsdb_brev <- sd(b_brev$relativecc[b_brev$Population.designation == 1])

#c_gap
rurmeanc_gap <- mean(c_gap$relativecc[c_gap$Population.designation == 0])
rursdc_gap <- sd(c_gap$relativecc[c_gap$Population.designation == 0])
urbmeanc_gap <- mean(c_gap$relativecc[c_gap$Population.designation == 1])
urbsdc_gap <- sd(c_gap$relativecc[c_gap$Population.designation == 1])

#e_fus
rurmeane_fus <- mean(e_fus$relativecc[e_fus$Population.designation == 0])
rursde_fus <- sd(e_fus$relativecc[e_fus$Population.designation == 0])
urbmeane_fus <- mean(e_fus$relativecc[e_fus$Population.designation == 1])
urbsde_fus <- sd(e_fus$relativecc[e_fus$Population.designation == 1])

#g_bur
rurmeang_bur <- mean(g_bur$relativecc[g_bur$Population.designation == 0])
rursdg_bur <- sd(g_bur$relativecc[g_bur$Population.designation == 0])
urbmeang_bur <- mean(g_bur$relativecc[g_bur$Population.designation == 1])
urbsdg_bur <- sd(g_bur$relativecc[g_bur$Population.designation == 1])

#m_luc
rurmeanm_luc <- mean(m_luc$relativecc[m_luc$Population.designation == 0])
rursdm_luc <- sd(m_luc$relativecc[m_luc$Population.designation == 0])
urbmeanm_luc <- mean(m_luc$relativecc[m_luc$Population.designation == 1])
urbsdm_luc <- sd(m_luc$relativecc[m_luc$Population.designation == 1])

#m_pen
rurmeanm_pen <- mean(m_pen$relativecc[m_pen$Population.designation == 0])
rursdm_pen <- sd(m_pen$relativecc[m_pen$Population.designation == 0])
urbmeanm_pen <- mean(m_pen$relativecc[m_pen$Population.designation == 1])
urbsdm_pen <- sd(m_pen$relativecc[m_pen$Population.designation == 1])

#p_leu
rurmeanp_leu <- mean(p_leu$relativecc[p_leu$Population.designation == 0])
rursdp_leu <- sd(p_leu$relativecc[p_leu$Population.designation == 0])
urbmeanp_leu <- mean(p_leu$relativecc[p_leu$Population.designation == 1])
urbsdp_leu <- sd(p_leu$relativecc[p_leu$Population.designation == 1])

#s_car
rurmeans_car <- mean(s_car$relativecc[s_car$Population.designation == 0])
rursds_car <- sd(s_car$relativecc[s_car$Population.designation == 0])
urbmeans_car <- mean(s_car$relativecc[s_car$Population.designation == 1])
urbsds_car <- sd(s_car$relativecc[s_car$Population.designation == 1])

#s_cin
rurmeans_cin <- mean(s_cin$relativecc[s_cin$Population.designation == 0])
rursds_cin <- sd(s_cin$relativecc[s_cin$Population.designation == 0])
urbmeans_cin <- mean(s_cin$relativecc[s_cin$Population.designation == 1])
urbsds_cin <- sd(s_cin$relativecc[s_cin$Population.designation == 1])

#t_hud
rurmeant_hud <- mean(t_hud$relativecc[t_hud$Population.designation == 0])
rursdt_hud <- sd(t_hud$relativecc[t_hud$Population.designation == 0])
urbmeant_hud <- mean(t_hud$relativecc[t_hud$Population.designation == 1])
urbsdt_hud <- sd(t_hud$relativecc[t_hud$Population.designation == 1])


#---- hedge's d----

#b_brev
ruralb_brev <- c(b_brev$relativecc[b_brev$Population.designation == 0])
urbanb_brev <- c(b_brev$relativecc[b_brev$Population.designation == 1])
b_brev_effect_size <- cohen.d(ruralb_brev, urbanb_brev, hedges.correction = TRUE)
b_brev_effect_size

#c_gap
ruralc_gap <- c(c_gap$relativecc[c_gap$Population.designation == 0])
urbanc_gap <- c(c_gap$relativecc[c_gap$Population.designation == 1])
c_gap_effect_size <- cohen.d(ruralc_gap, urbanc_gap, hedges.correction = TRUE)
c_gap_effect_size

#e_fus
rurale_fus <- c(e_fus$relativecc[e_fus$Population.designation == 0])
urbane_fus <- c(e_fus$relativecc[e_fus$Population.designation == 1])
e_fus_effect_size <- cohen.d(rurale_fus, urbane_fus, hedges.correction = TRUE)
e_fus_effect_size

#g_bur
ruralg_bur <- c(g_bur$relativecc[g_bur$Population.designation == 0])
urbang_bur <- c(g_bur$relativecc[g_bur$Population.designation == 1])
g_bur_effect_size <- cohen.d(ruralg_bur, urbang_bur, hedges.correction = TRUE)
g_bur_effect_size

#m_luc
ruralm_luc <- c(m_luc$relativecc[m_luc$Population.designation == 0])
urbanm_luc <- c(m_luc$relativecc[m_luc$Population.designation == 1])
m_luc_effect_size <- cohen.d(ruralm_luc, urbanm_luc, hedges.correction = TRUE)
m_luc_effect_size

#m_pen
ruralm_pen <- c(m_pen$relativecc[m_pen$Population.designation == 0])
urbanm_pen <- c(m_pen$relativecc[m_pen$Population.designation == 1])
m_pen_effect_size <- cohen.d(ruralm_pen, urbanm_pen, hedges.correction = TRUE)
m_pen_effect_size

#p_leu
ruralp_leu <- c(p_leu$relativecc[p_leu$Population.designation == 0])
urbanp_leu <- c(p_leu$relativecc[p_leu$Population.designation == 1])
p_leu_effect_size <- cohen.d(ruralp_leu, urbanp_leu, hedges.correction = TRUE)
p_leu_effect_size

#s_car
rurals_car <- c(s_car$relativecc[s_car$Population.designation == 0])
urbans_car <- c(s_car$relativecc[s_car$Population.designation == 1])
s_car_effect_size <- cohen.d(rurals_car, urbans_car, hedges.correction = TRUE)
s_car_effect_size

#s_cin
rurals_cin <- c(s_cin$relativecc[s_cin$Population.designation == 0])
urbans_cin <- c(s_cin$relativecc[s_cin$Population.designation == 1])
s_cin_effect_size <- cohen.d(rurals_cin, urbans_cin, hedges.correction = TRUE)
s_cin_effect_size

#t_hud
ruralt_hud <- c(t_hud$relativecc[t_hud$Population.designation == 0])
urbant_hud <- c(t_hud$relativecc[t_hud$Population.designation == 1])
t_hud_effect_size <- cohen.d(ruralt_hud, urbant_hud, hedges.correction = TRUE)
t_hud_effect_size

urbssb_brev <- 
  
sum(museum$State == "California")
