depas <- read.csv('data/meta_analysis.csv')

#bla_b
bla_b_rumean <- 13.46
bla_b_ruse <- 0.4
bla_b_russ <- 9
bla_b_rusd <- bla_b_ruse * sqrt(bla_b_russ)
bla_b_urmean <- 13.96
bla_b_urse <- 2.35
bla_b_urss <- 13
bla_b_ursd <- bla_b_urse * sqrt(bla_b_urss)
bla_b_poolsd <- sqrt(((bla_b_urss - 1) * bla_b_ursd^2 + (bla_b_russ - 1) * bla_b_rusd^2) / (bla_b_urss + bla_b_russ - 2))
bla_b_cohen_d <- (bla_b_urmean - bla_b_rumean) / bla_b_poolsd
bla_b_correction_factor <- 1 - (3 / (4 * (bla_b_urss + bla_b_russ) - 9))
bla_b_hedges_g <- bla_b_cohen_d * bla_b_correction_factor
bla_b_se_g <- sqrt((bla_b_urss + bla_b_russ) / (bla_b_urss * bla_b_russ) + (bla_b_hedges_g^2 / (2 * (bla_b_urss + bla_b_russ))))
bla_b_t_crit <- qt(0.975, df = bla_b_urss + bla_b_russ - 2)
bla_b_ci_lower <- bla_b_hedges_g - bla_b_t_crit * bla_b_se_g
bla_b_ci_upper <- bla_b_hedges_g + bla_b_t_crit * bla_b_se_g

bla_b_hedges_g
c(bla_b_ci_lower, bla_b_ci_upper)

#ept_f
ept_f_rumean <- 16.53
ept_f_ruse <- 1.35
ept_f_russ <- 10
ept_f_rusd <- ept_f_ruse * sqrt(ept_f_russ)
ept_f_urmean <- 13.17
ept_f_urse <- 1.18
ept_f_urss <- 11
ept_f_ursd <- ept_f_urse * sqrt(ept_f_urss)
ept_f_poolsd <- sqrt(((ept_f_urss - 1) * ept_f_ursd^2 + (ept_f_russ - 1) * ept_f_rusd^2) / (ept_f_urss + ept_f_russ - 2))
ept_f_cohen_d <- (ept_f_urmean - ept_f_rumean) / ept_f_poolsd
ept_f_correction_factor <- 1 - (3 / (4 * (ept_f_urss + ept_f_russ) - 9))
ept_f_hedges_g <- ept_f_cohen_d * ept_f_correction_factor
ept_f_se_g <- sqrt((ept_f_urss + ept_f_russ) / (ept_f_urss * ept_f_russ) + (ept_f_hedges_g^2 / (2 * (ept_f_urss + ept_f_russ))))
ept_f_t_crit <- qt(0.975, df = ept_f_urss + ept_f_russ - 2)
ept_f_ci_lower <- ept_f_hedges_g - ept_f_t_crit * ept_f_se_g
ept_f_ci_upper <- ept_f_hedges_g + ept_f_t_crit * ept_f_se_g

ept_f_hedges_g
c(ept_f_ci_lower, ept_f_ci_upper)

#mic_p
mic_p_rumean <- 14.12
mic_p_ruse <- 1.04
mic_p_russ <- 12
mic_p_rusd <- mic_p_ruse * sqrt(mic_p_russ)
mic_p_urmean <- 26.09
mic_p_urse <- 3.84
mic_p_urss <- 15
mic_p_ursd <- mic_p_urse * sqrt(mic_p_urss)
mic_p_poolsd <- sqrt(((mic_p_urss - 1) * mic_p_ursd^2 + (mic_p_russ - 1) * mic_p_rusd^2) / (mic_p_urss + mic_p_russ - 2))
mic_p_cohen_d <- (mic_p_urmean - mic_p_rumean) / mic_p_poolsd
mic_p_correction_factor <- 1 - (3 / (4 * (mic_p_urss + mic_p_russ) - 9))
mic_p_hedges_g <- mic_p_cohen_d * mic_p_correction_factor
mic_p_se_g <- sqrt((mic_p_urss + mic_p_russ) / (mic_p_urss * mic_p_russ) + (mic_p_hedges_g^2 / (2 * (mic_p_urss + mic_p_russ))))
mic_p_t_crit <- qt(0.975, df = mic_p_urss + mic_p_russ - 2)
mic_p_ci_lower <- mic_p_hedges_g - mic_p_t_crit * mic_p_se_g
mic_p_ci_upper <- mic_p_hedges_g + mic_p_t_crit * mic_p_se_g

mic_p_hedges_g
c(mic_p_ci_lower, mic_p_ci_upper)


#per_l
per_l_rumean <- 15.84
per_l_ruse <- 0.68
per_l_russ <- 12
per_l_rusd <- per_l_ruse * sqrt(per_l_russ)
per_l_urmean <- 25.88
per_l_urse <- 4.82
per_l_urss <- 25
per_l_ursd <- per_l_urse * sqrt(per_l_urss)
per_l_poolsd <- sqrt(((per_l_urss - 1) * per_l_ursd^2 + (per_l_russ - 1) * per_l_rusd^2) / (per_l_urss + per_l_russ - 2))
per_l_cohen_d <- (per_l_urmean - per_l_rumean) / per_l_poolsd
per_l_correction_factor <- 1 - (3 / (4 * (per_l_urss + per_l_russ) - 9))
per_l_hedges_g <- per_l_cohen_d * per_l_correction_factor
per_l_se_g <- sqrt((per_l_urss + per_l_russ) / (per_l_urss * per_l_russ) + (per_l_hedges_g^2 / (2 * (per_l_urss + per_l_russ))))
per_l_t_crit <- qt(0.975, df = per_l_urss + per_l_russ - 2)
per_l_ci_lower <- per_l_hedges_g - per_l_t_crit * per_l_se_g
per_l_ci_upper <- per_l_hedges_g + per_l_t_crit * per_l_se_g

per_l_hedges_g
c(per_l_ci_lower, per_l_ci_upper)

