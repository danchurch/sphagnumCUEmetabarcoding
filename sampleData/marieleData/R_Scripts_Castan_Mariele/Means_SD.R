library(dplyr)



##############################---------------CUE-------------------########################

Daten %>%
  filter(depth == 1, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(cue, na.rm = TRUE),
    sd = sd(cue, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(cue, na.rm = TRUE),
    sd = sd(cue, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 3, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(cue, na.rm = TRUE),
    sd = sd(cue, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(cue, na.rm = TRUE),
    sd = sd(cue, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(cue, na.rm = TRUE),
    sd = sd(cue, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "P") %>%
  summarise(
    mean = mean(cue, na.rm = TRUE),
    sd = sd(cue, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "P") %>%
  summarise(
    mean = mean(cue, na.rm = TRUE),
    sd = sd(cue, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 0, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(cue, na.rm = TRUE),
    sd = sd(cue, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 1, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(cue, na.rm = TRUE),
    sd = sd(cue, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(cue, na.rm = TRUE),
    sd = sd(cue, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(cue, na.rm = TRUE),
    sd = sd(cue, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(cue, na.rm = TRUE),
    sd = sd(cue, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "T") %>%
  summarise(
    mean = mean(cue, na.rm = TRUE),
    sd = sd(cue, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "T") %>%
  summarise(
    mean = mean(cue, na.rm = TRUE),
    sd = sd(cue, na.rm = TRUE)
  )



##############################---------------MBC-------------------########################

Daten %>%
  filter(depth == 1, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(mbcug, na.rm = TRUE),
    sd = sd(mbcug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(mbcug, na.rm = TRUE),
    sd = sd(mbcug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 3, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(mbcug, na.rm = TRUE),
    sd = sd(mbcug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(mbcug, na.rm = TRUE),
    sd = sd(mbcug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(mbcug, na.rm = TRUE),
    sd = sd(mbcug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "P") %>%
  summarise(
    mean = mean(mbcug, na.rm = TRUE),
    sd = sd(mbcug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "P") %>%
  summarise(
    mean = mean(mbcug, na.rm = TRUE),
    sd = sd(mbcug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 0, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(mbcug, na.rm = TRUE),
    sd = sd(mbcug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 1, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(mbcug, na.rm = TRUE),
    sd = sd(mbcug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(mbcug, na.rm = TRUE),
    sd = sd(mbcug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(mbcug, na.rm = TRUE),
    sd = sd(mbcug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(mbcug, na.rm = TRUE),
    sd = sd(mbcug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "T") %>%
  summarise(
    mean = mean(mbcug, na.rm = TRUE),
    sd = sd(mbcug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "T") %>%
  summarise(
    mean = mean(mbcug, na.rm = TRUE),
    sd = sd(mbcug, na.rm = TRUE)
  )




##############################---------------Growth-------------------########################

Daten %>%
  filter(depth == 1, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(cprodug, na.rm = TRUE),
    sd = sd(cprodug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(cprodug, na.rm = TRUE),
    sd = sd(cprodug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 3, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(cprodug, na.rm = TRUE),
    sd = sd(cprodug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(cprodug, na.rm = TRUE),
    sd = sd(cprodug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(cprodug, na.rm = TRUE),
    sd = sd(cprodug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "P") %>%
  summarise(
    mean = mean(cprodug, na.rm = TRUE),
    sd = sd(cprodug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "P") %>%
  summarise(
    mean = mean(cprodug, na.rm = TRUE),
    sd = sd(cprodug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 0, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(cprodug, na.rm = TRUE),
    sd = sd(cprodug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 1, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(cprodug, na.rm = TRUE),
    sd = sd(cprodug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(cprodug, na.rm = TRUE),
    sd = sd(cprodug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(cprodug, na.rm = TRUE),
    sd = sd(cprodug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(cprodug, na.rm = TRUE),
    sd = sd(cprodug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "T") %>%
  summarise(
    mean = mean(cprodug, na.rm = TRUE),
    sd = sd(cprodug, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "T") %>%
  summarise(
    mean = mean(cprodug, na.rm = TRUE),
    sd = sd(cprodug, na.rm = TRUE)
  )


##############################---------------Respiration-------------------########################

Daten %>%
  filter(depth == 1, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(respirationa, na.rm = TRUE),
    sd = sd(respirationa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(respirationa, na.rm = TRUE),
    sd = sd(respirationa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 3, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(respirationa, na.rm = TRUE),
    sd = sd(respirationa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(respirationa, na.rm = TRUE),
    sd = sd(respirationa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(respirationa, na.rm = TRUE),
    sd = sd(respirationa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "P") %>%
  summarise(
    mean = mean(respirationa, na.rm = TRUE),
    sd = sd(respirationa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "P") %>%
  summarise(
    mean = mean(respirationa, na.rm = TRUE),
    sd = sd(respirationa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 0, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(respirationa, na.rm = TRUE),
    sd = sd(respirationa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 1, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(respirationa, na.rm = TRUE),
    sd = sd(respirationa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(respirationa, na.rm = TRUE),
    sd = sd(respirationa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(respirationa, na.rm = TRUE),
    sd = sd(respirationa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(respirationa, na.rm = TRUE),
    sd = sd(respirationa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "T") %>%
  summarise(
    mean = mean(respirationa, na.rm = TRUE),
    sd = sd(respirationa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "T") %>%
  summarise(
    mean = mean(respirationa, na.rm = TRUE),
    sd = sd(respirationa, na.rm = TRUE)
  )




##############################---------------Turnover-------------------########################

Daten %>%
  filter(depth == 1, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(turnover, na.rm = TRUE),
    sd = sd(turnover, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(turnover, na.rm = TRUE),
    sd = sd(turnover, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 3, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(turnover, na.rm = TRUE),
    sd = sd(turnover, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(turnover, na.rm = TRUE),
    sd = sd(turnover, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(turnover, na.rm = TRUE),
    sd = sd(turnover, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "P") %>%
  summarise(
    mean = mean(turnover, na.rm = TRUE),
    sd = sd(turnover, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "P") %>%
  summarise(
    mean = mean(turnover, na.rm = TRUE),
    sd = sd(turnover, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 0, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(turnover, na.rm = TRUE),
    sd = sd(turnover, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 1, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(turnover, na.rm = TRUE),
    sd = sd(turnover, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(turnover, na.rm = TRUE),
    sd = sd(turnover, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(turnover, na.rm = TRUE),
    sd = sd(turnover, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(turnover, na.rm = TRUE),
    sd = sd(turnover, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "T") %>%
  summarise(
    mean = mean(turnover, na.rm = TRUE),
    sd = sd(turnover, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "T") %>%
  summarise(
    mean = mean(turnover, na.rm = TRUE),
    sd = sd(turnover, na.rm = TRUE)
  )




##############################---------------C-------------------########################

Daten %>%
  filter(depth == 1, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Ca, na.rm = TRUE),
    sd = sd(Ca, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Ca, na.rm = TRUE),
    sd = sd(Ca, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 3, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Ca, na.rm = TRUE),
    sd = sd(Ca, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Ca, na.rm = TRUE),
    sd = sd(Ca, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Ca, na.rm = TRUE),
    sd = sd(Ca, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 0, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Ca, na.rm = TRUE),
    sd = sd(Ca, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 1, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Ca, na.rm = TRUE),
    sd = sd(Ca, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Ca, na.rm = TRUE),
    sd = sd(Ca, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Ca, na.rm = TRUE),
    sd = sd(Ca, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Ca, na.rm = TRUE),
    sd = sd(Ca, na.rm = TRUE)
  )


##############################---------------N-------------------########################

Daten %>%
  filter(depth == 1, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Na, na.rm = TRUE),
    sd = sd(Na, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Na, na.rm = TRUE),
    sd = sd(Na, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 3, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Na, na.rm = TRUE),
    sd = sd(Na, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Na, na.rm = TRUE),
    sd = sd(Na, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Na, na.rm = TRUE),
    sd = sd(Na, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 0, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Na, na.rm = TRUE),
    sd = sd(Na, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 1, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Na, na.rm = TRUE),
    sd = sd(Na, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Na, na.rm = TRUE),
    sd = sd(Na, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Na, na.rm = TRUE),
    sd = sd(Na, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Na, na.rm = TRUE),
    sd = sd(Na, na.rm = TRUE)
  )



##############################---------------O-------------------########################

Daten %>%
  filter(depth == 1, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Oa, na.rm = TRUE),
    sd = sd(Oa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Oa, na.rm = TRUE),
    sd = sd(Oa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 3, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Oa, na.rm = TRUE),
    sd = sd(Oa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Oa, na.rm = TRUE),
    sd = sd(Oa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Oa, na.rm = TRUE),
    sd = sd(Oa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 0, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Oa, na.rm = TRUE),
    sd = sd(Oa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 1, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Oa, na.rm = TRUE),
    sd = sd(Oa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Oa, na.rm = TRUE),
    sd = sd(Oa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Oa, na.rm = TRUE),
    sd = sd(Oa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Oa, na.rm = TRUE),
    sd = sd(Oa, na.rm = TRUE)
  )




##############################---------------H-------------------########################

Daten %>%
  filter(depth == 1, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Ha, na.rm = TRUE),
    sd = sd(Ha, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Ha, na.rm = TRUE),
    sd = sd(Ha, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 3, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Ha, na.rm = TRUE),
    sd = sd(Ha, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Ha, na.rm = TRUE),
    sd = sd(Ha, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(Ha, na.rm = TRUE),
    sd = sd(Ha, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 0, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Ha, na.rm = TRUE),
    sd = sd(Ha, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 1, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Ha, na.rm = TRUE),
    sd = sd(Ha, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Ha, na.rm = TRUE),
    sd = sd(Ha, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Ha, na.rm = TRUE),
    sd = sd(Ha, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(Ha, na.rm = TRUE),
    sd = sd(Ha, na.rm = TRUE)
  )




##############################---------------S-------------------########################

Daten %>%
  filter(depth == 1, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(S, na.rm = TRUE),
    sd = sd(S, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(S, na.rm = TRUE),
    sd = sd(S, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 3, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(S, na.rm = TRUE),
    sd = sd(S, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(S, na.rm = TRUE),
    sd = sd(S, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(S, na.rm = TRUE),
    sd = sd(S, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 0, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(S, na.rm = TRUE),
    sd = sd(S, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 1, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(S, na.rm = TRUE),
    sd = sd(S, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(S, na.rm = TRUE),
    sd = sd(S, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(S, na.rm = TRUE),
    sd = sd(S, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(S, na.rm = TRUE),
    sd = sd(S, na.rm = TRUE)
  )




##############################---------------CN-------------------########################

Daten %>%
  filter(depth == 1, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(CNa, na.rm = TRUE),
    sd = sd(CNa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(CNa, na.rm = TRUE),
    sd = sd(CNa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 3, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(CNa, na.rm = TRUE),
    sd = sd(CNa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(CNa, na.rm = TRUE),
    sd = sd(CNa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(CNa, na.rm = TRUE),
    sd = sd(CNa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 0, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(CNa, na.rm = TRUE),
    sd = sd(CNa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 1, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(CNa, na.rm = TRUE),
    sd = sd(CNa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(CNa, na.rm = TRUE),
    sd = sd(CNa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(CNa, na.rm = TRUE),
    sd = sd(CNa, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(CNa, na.rm = TRUE),
    sd = sd(CNa, na.rm = TRUE)
  )


##############################---------------H/C-------------------########################

Daten %>%
  filter(depth == 1, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(HC, na.rm = TRUE),
    sd = sd(HC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(HC, na.rm = TRUE),
    sd = sd(HC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 3, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(HC, na.rm = TRUE),
    sd = sd(HC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(HC, na.rm = TRUE),
    sd = sd(HC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(HC, na.rm = TRUE),
    sd = sd(HC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 0, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(HC, na.rm = TRUE),
    sd = sd(HC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 1, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(HC, na.rm = TRUE),
    sd = sd(HC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(HC, na.rm = TRUE),
    sd = sd(HC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(HC, na.rm = TRUE),
    sd = sd(HC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(HC, na.rm = TRUE),
    sd = sd(HC, na.rm = TRUE)
  )



##############################---------------O/C-------------------########################

Daten %>%
  filter(depth == 1, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(OC, na.rm = TRUE),
    sd = sd(OC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(OC, na.rm = TRUE),
    sd = sd(OC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 3, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(OC, na.rm = TRUE),
    sd = sd(OC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(OC, na.rm = TRUE),
    sd = sd(OC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "O2", site == "P") %>%
  summarise(
    mean = mean(OC, na.rm = TRUE),
    sd = sd(OC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 0, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(OC, na.rm = TRUE),
    sd = sd(OC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 1, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(OC, na.rm = TRUE),
    sd = sd(OC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 2, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(OC, na.rm = TRUE),
    sd = sd(OC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 4, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(OC, na.rm = TRUE),
    sd = sd(OC, na.rm = TRUE)
  )


Daten %>%
  filter(depth == 5, oxygen == "N2", site == "T") %>%
  summarise(
    mean = mean(OC, na.rm = TRUE),
    sd = sd(OC, na.rm = TRUE)
  )




