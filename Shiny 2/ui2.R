# Load data
library(tidyverse)
library(haven)
library(EdSurvey)

TIMSS2015_US <- readTIMSS(path = "~/Shiny 2/TIMSS/2015", countries = c("usa"), gradeLvl = "8")
TIMSS2015_TW <- readTIMSS(path = "~/Shiny 2/TIMSS/2015", countries = c("twn"), gradeLvl = "8")

US_Math <- getData(TIMSS2015_US, 
                   varnames = c("idcntry","idschool","idclass","idstud", "itsex", "bsdmlowp", # demographics
                                "bsmmat01", "bsmmat02", "bsmmat03", "bsmmat04", "bsmmat05", # plausible values (DV)
                                "bsmibm01", "bsmibm02", "bsmibm03", "bsmibm04", "bsmibm05", # international benchmark
                                "bsdgslm", "bsbm17a", "bsbm17b", "bsbm17c", "bsbm17d", "bsbm17e", "bsbm17f", "bsbm17g", "bsbm17h", "bsbm17i", # like math
                                "bsdgher","bsbg04", "bsbg06d", "bsbg06e", "bsbg07a", "bsbg07b", "bsdg06s", "bsdgedup", # home ed resources (1)
                                "bsdgssb","bsbg15a","bsbg15b","bsbg15c","bsbg15d","bsbg15e","bsbg15f","bsbg15g", # students belong (1)
                                "bsdgsb", "bsbg16a", "bsbg16b", "bsbg16c", "bsbg16d", "bsbg16e", "bsbg16f", "bsbg16g", "bsbg16h", "bsbg16i", # student bullying (1)
                                "bsdgeml", "bsbm18a", "bsbm18b", "bsbm18c", "bsbm18d", "bsbm18e", "bsbm18f", "bsbm18g", "bsbm18h", "bsbm18i", "bsbm18j", # engaging math teaching (1)
                                "bsdgscm", "bsbm19a", "bsbm19b", "bsbm19c", "bsbm19d", "bsbm19e", "bsbm19f", "bsbm19g", "bsbm19h", "bsbm19i", # math confidence (1)
                                "bsdgsvm", "bsbm20a", "bsbm20b", "bsbm20c", "bsbm20d", "bsbm20e", "bsbm20f", "bsbm20g", "bsbm20h", "bsbm20i", # value math (1)
                                "bsdmwkhw","bsbm25aa", "bsbm25ba", # weekly hwk time (1)
                                "btbg01.math","btbg02.math","btbg03.math","btbg12.math","btbg13.math", # level 2 demographics
                                "btdgeas.math","btbg06a.math","btbg06b.math","btbg06c.math","btbg06d.math","btbg06e.math","btbg06f.math","btbg06g.math","btbg06h.math","btbg06i.math","btbg06j.math","btbg06k.math","btbg06l.math","btbg06m.math","btbg06o.math", # school acad ach (2)
                                "btdgsos.math","btbg07a.math","btbg07b.math","btbg07c.math","btbg07d.math","btbg07e.math","btbg07f.math","btbg07g.math","btbg07h.math", # safe schools (2)
                                "btdgscr.math","btbg08a.math","btbg08b.math","btbg08c.math","btbg08d.math","btbg08e.math","btbg08f.math","btbg08g.math", # conditions/resources (2)
                                "btdgtjs.math","btbg10a.math","btbg10b.math","btbg10c.math","btbg10d.math","btbg10e.math","btbg10f.math","btbg10g.math", # job satisfaction (2)
                                "btdgcft.math","btbg11a.math","btbg11b.math","btbg11c.math","btbg11d.math","btbg11e.math","btbg11f.math","btbg11g.math","btbg11h.math", # challenges (2)
                                "btdglsn.math","btbg15a.math","btbg15b.math","btbg15c.math","btbg15d.math","btbg15e.math","btbg15f.math","btbg15g.math", # limited by students (2)
                                "btdm05.math", "btbg05a.math", "btbg05f.math", # major in math/math ed (2)
                                # these items don't have a composite scale but could be interesting
                                "btbm16.math", # time spent teaching math (min/wk) (2)
                                "btbm17a.math","btbm17b.math","btbm17c.math","btbm17d.math","btbm17e.math","btbm17f.math","btbm17g.math","btbm17h.math","btbm17i.math", # teacher confidence (2)
                                "btbm18a.math","btbm18b.math","btbm18c.math","btbm18d.math","btbm18e.math","btbm18f.math","btbm18g.math","btbm18h.math","btbm18i.math","btbm18j.math", # teacher pedagogy (2)
                                "btbm22a.math","btbm22b.math","btbm22ca.math","btbm22cb.math","btbm22cc.math","btbm22cd.math","btbm22ce.math", # homework (2)
                                "btbm23a.math","btbm23b.math","btbm23c.math", # testing emphasis (2)
                                "btbm25.math","btbm24a.math","btbm24b.math","btbm24c.math","btbm24d.math","btbm24e.math","btbm24f.math","btbm24g.math", # pd (2)
                                "bcbg03a","bcdg03","bcbg03b","bcbg04","bcbg05b","bcbg06a","bcbg06b","bcbg07a","bcbg07b","bcbg07c","bcbg08a","bcbg08b","bcbg09a","bcbg10","bcbg12","bcbg16a","bcbg17a","bcbg18a","bcbg18b","bcbg19","bcbg20", # level 3 demographics
                                "bcdgmrs","bcbg13aa","bcbg13ab","bcbg13ac","bcbg13ad","bcbg13ae","bcbg13af","bcbg13ag","bcbg13ah","bcbg13ai","bcbg13ba","bcbg13bb","bcbg13bc","bcbg13bd","bcbg13be", # instruction affected by math resources (3)
                                "bcdgeas","bcbg14a","bcbg14b","bcbg14c","bcbg14d","bcbg14e","bcbg14f","bcbg14g","bcbg14h","bcbg14i","bcbg14j","bcbg14k","bcbg14l","bcbg14m", # school acad suc (3)
                                "bcdgdas","bcbg15a","bcbg15b","bcbg15c","bcbg15d","bcbg15e","bcbg15f","bcbg15g","bcbg15h","bcbg15i","bcbg15j","bcbg15k"), # discipline (3)
                   omittedLevels = FALSE, addAttributes = TRUE)
TW_Math <- getData(TIMSS2015_TW, 
                   varnames = c("idcntry","idschool","idclass","idstud", "itsex", "bsdmlowp", # demographics
                                "bsmmat01", "bsmmat02", "bsmmat03", "bsmmat04", "bsmmat05", # plausible values (DV)
                                "bsmibm01", "bsmibm02", "bsmibm03", "bsmibm04", "bsmibm05", # international benchmark
                                "bsdgslm", "bsbm17a", "bsbm17b", "bsbm17c", "bsbm17d", "bsbm17e", "bsbm17f", "bsbm17g", "bsbm17h", "bsbm17i", # like math
                                "bsdgher","bsbg04", "bsbg06d", "bsbg06e", "bsbg07a", "bsbg07b", "bsdg06s", "bsdgedup", # home ed resources (1)
                                "bsdgssb","bsbg15a","bsbg15b","bsbg15c","bsbg15d","bsbg15e","bsbg15f","bsbg15g", # students belong (1)
                                "bsdgsb", "bsbg16a", "bsbg16b", "bsbg16c", "bsbg16d", "bsbg16e", "bsbg16f", "bsbg16g", "bsbg16h", "bsbg16i", # student bullying (1)
                                "bsdgeml", "bsbm18a", "bsbm18b", "bsbm18c", "bsbm18d", "bsbm18e", "bsbm18f", "bsbm18g", "bsbm18h", "bsbm18i", "bsbm18j", # engaging math teaching (1)
                                "bsdgscm", "bsbm19a", "bsbm19b", "bsbm19c", "bsbm19d", "bsbm19e", "bsbm19f", "bsbm19g", "bsbm19h", "bsbm19i", # math confidence (1)
                                "bsdgsvm", "bsbm20a", "bsbm20b", "bsbm20c", "bsbm20d", "bsbm20e", "bsbm20f", "bsbm20g", "bsbm20h", "bsbm20i", # value math (1)
                                "bsdmwkhw","bsbm25aa", "bsbm25ba", # weekly hwk time (1)
                                "btbg01.math","btbg02.math","btbg03.math","btbg12.math","btbg13.math", # level 2 demographics
                                "btdgeas.math","btbg06a.math","btbg06b.math","btbg06c.math","btbg06d.math","btbg06e.math","btbg06f.math","btbg06g.math","btbg06h.math","btbg06i.math","btbg06j.math","btbg06k.math","btbg06l.math","btbg06m.math","btbg06o.math", # school acad suc (2)
                                "btdgsos.math","btbg07a.math","btbg07b.math","btbg07c.math","btbg07d.math","btbg07e.math","btbg07f.math","btbg07g.math","btbg07h.math", # safe schools (2)
                                "btdgscr.math","btbg08a.math","btbg08b.math","btbg08c.math","btbg08d.math","btbg08e.math","btbg08f.math","btbg08g.math", # conditions/resources (2)
                                "btdgtjs.math","btbg10a.math","btbg10b.math","btbg10c.math","btbg10d.math","btbg10e.math","btbg10f.math","btbg10g.math", # job satisfaction (2)
                                "btdgcft.math","btbg11a.math","btbg11b.math","btbg11c.math","btbg11d.math","btbg11e.math","btbg11f.math","btbg11g.math","btbg11h.math", # challenges (2)
                                "btdglsn.math","btbg15a.math","btbg15b.math","btbg15c.math","btbg15d.math","btbg15e.math","btbg15f.math","btbg15g.math", # limited by students (2)
                                "btdm05.math", "btbg05a.math", "btbg05f.math", # major in math/math ed (2)
                                # these items don't have a composite scale but could be interesting
                                "btbm17a.math","btbm17b.math","btbm17c.math","btbm17d.math","btbm17e.math","btbm17f.math","btbm17g.math","btbm17h.math","btbm17i.math", # teacher confidence (2)
                                "btbm16.math","btbm18a.math","btbm18b.math","btbm18c.math","btbm18d.math","btbm18e.math","btbm18f.math","btbm18g.math","btbm18h.math","btbm18i.math","btbm18j.math", # teacher pedagogy (2)
                                "btbm22a.math","btbm22b.math","btbm22ca.math","btbm22cb.math","btbm22cc.math","btbm22cd.math","btbm22ce.math", # homework (2)
                                "btbm23a.math","btbm23b.math","btbm23c.math", # testing emphasis (2)
                                "btbm25.math","btbm24a.math","btbm24b.math","btbm24c.math","btbm24d.math","btbm24e.math","btbm24f.math","btbm24g.math", # pd (2)
                                "bcbg03a","bcdg03","bcbg03b","bcbg04","bcbg05b","bcbg06a","bcbg06b","bcbg07a","bcbg07b","bcbg07c","bcbg08a","bcbg08b","bcbg09a","bcbg10","bcbg12","bcbg16a","bcbg17a","bcbg18a","bcbg18b","bcbg19","bcbg20", # level 3 demographics
                                "bcdgmrs","bcbg13aa","bcbg13ab","bcbg13ac","bcbg13ad","bcbg13ae","bcbg13af","bcbg13ag","bcbg13ah","bcbg13ai","bcbg13ba","bcbg13bb","bcbg13bc","bcbg13bd","bcbg13be", # instruction affected by math resources (3)
                                "bcdgeas","bcbg14a","bcbg14b","bcbg14c","bcbg14d","bcbg14e","bcbg14f","bcbg14g","bcbg14h","bcbg14i","bcbg14j","bcbg14k","bcbg14l","bcbg14m", # school acad suc (3)
                                "bcdgdas","bcbg15a","bcbg15b","bcbg15c","bcbg15d","bcbg15e","bcbg15f","bcbg15g","bcbg15h","bcbg15i","bcbg15j","bcbg15k"), # discipline (3)
                   omittedLevels = FALSE, addAttributes = TRUE)
TIMSS2015 <- rbind(US_Math, TW_Math)

# Data Preprocessing

Benchmark1 <- TIMSS2015 %>%
  select(idcntry, bsmibm01)
Benchmark2 <- TIMSS2015 %>%
  select(idcntry, bsmibm02)
Benchmark3 <- TIMSS2015 %>%
  select(idcntry, bsmibm03)
Benchmark4 <- TIMSS2015 %>%
  select(idcntry, bsmibm04)
Benchmark5 <- TIMSS2015 %>%
  select(idcntry, bsmibm05)

TooLowAch <- TIMSS2015 %>%
  select(idcntry, bsdmlowp)

Like_Math <- TIMSS2015 %>%
  select(idcntry, bsdgslm:bsbm17i)

HomeEdResources <- TIMSS2015 %>%
  select(idcntry, bsdgher:bsdgedup)

Students_Belonging <- TIMSS2015 %>%
  select(idcntry, bsdgssb:bsbg15g)

Students_Bullying <- TIMSS2015 %>%
  select(idcntry, bsdgsb:bsbg16i)

Engaging_Teaching <- TIMSS2015 %>%
  select(idcntry, bsdgeml:bsbm18j)

Students_Confidence <- TIMSS2015 %>%
  select(idcntry, bsdgscm:bsbm19i)

Students_ValueMath <- TIMSS2015 %>%
  select(idcntry, bsdgsvm:bsbm20i)

Weekly_Hwk <- TIMSS2015 %>%
  select(idcntry, bsdmwkhw, bsbm25aa, bsbm25ba)

Teachers_Demographics <- TIMSS2015 %>%
  select(idcntry, btbg01.math:btbg13.math)

Teachers_AcadSuc <- TIMSS2015 %>%
  select(idcntry, btdgeas.math:btbg06o.math)

Teachers_SafeSchools <- TIMSS2015 %>%
  select(idcntry, btdgsos.math:btbg07h.math)

Teachers_CondRes <- TIMSS2015 %>%
  select(idcntry, btdgscr.math:btbg08g.math)

Teachers_Satisfied <- TIMSS2015 %>%
  select(idcntry, btdgtjs.math:btbg10g.math)

Teachers_Challenges <- TIMSS2015 %>%
  select(idcntry, btdgcft.math:btbg11h.math)

Teachers_StudentLimits <- TIMSS2015 %>%
  select(idcntry, btdglsn.math:btbg15g.math)

Teachers_MathMajors <- TIMSS2015 %>%
  select(idcntry, btdm05.math, btbg05a.math, btbg05f.math)

Teachers_Confidence <- TIMSS2015 %>%
  select(idcntry, btbm17a.math:btbm17i.math)

Teachers_Pedagogy <- TIMSS2015 %>%
  select(idcntry, btbm16.math:btbm18j.math)

Teachers_Hwk <- TIMSS2015 %>%
  select(idcntry, btbm22a.math:btbm22ce.math)

Teachers_Tests <- TIMSS2015 %>%
  select(idcntry, btbm23a.math, btbm23b.math, btbm23c.math)

Teachers_PD <- TIMSS2015 %>%
  select(idcntry, btbm25.math:btbm24g.math)

School_Demographics <- TIMSS2015 %>%
  select(idcntry, bcbg03a:bcbg20)

School_Resources <- TIMSS2015 %>%
  select(idcntry, bcdgmrs:bcbg13be)

School_AcadSuc <- TIMSS2015 %>%
  select(idcntry, bcdgeas:bcbg14m)

School_Discipline <- TIMSS2015 %>%
  select(idcntry, bcdgdas:bcbg15k)

# Histograms

Plausible_Value1 <- TIMSS2015 %>% 
  select(idcntry, bsmmat01)
Plausible_Value2 <- TIMSS2015 %>% 
  select(idcntry, bsmmat02)
Plausible_Value3 <- TIMSS2015 %>% 
  select(idcntry, bsmmat03)
Plausible_Value4 <- TIMSS2015 %>% 
  select(idcntry, bsmmat04)
Plausible_Value5 <- TIMSS2015 %>% 
  select(idcntry, bsmmat05)

# Panels

sidebar_content <- sidebarPanel(
  selectInput(
    "var",
    label = "Variable",
    choices = c("Plausible_Value1",
                "Plausible_Value2",
                "Plausible_Value3",
                "Plausible_Value4",
                "Plausible_Value5",
                "Benchmark1",
                "Benchmark2",
                "Benchmark3",
                "Benchmark4",
                "Benchmark5",
                "Like_Math",
                "TooLowAch",
                "HomeEdResources",
                "Students_Belonging",
                "Students_Bullying",
                "Engaging_Teaching",
                "Students_Confidence",
                "Students_ValueMath",
                "Weekly_Hwk",
                "Teachers_Demographics",
                "Teachers_AcadSuc",
                "Teachers_SafeSchools",
                "Teachers_CondRes",
                "Teachers_Satisfied",
                "Teachers_Challenges",
                "Teachers_StudentLimits",
                "Teachers_MathMajors",
                "Teachers_Confidence",
                "Teachers_Pedagogy",
                "Teachers_Hwk",
                "Teachers_Tests",
                "Teachers_PD",
                "School_Demographics",
                "School_Resources",
                "School_AcadSuc",
                "School_Discipline")
  ),
  conditionalPanel(
    condition = "input.var != 'Like_Math' & input.var != 'HomeEdResources' & input.var != 'Students_Belonging' & input.var != 'Students_Bullying' &
    input.var != 'Engaging_Teaching' & input.var != 'Students_Confidence' & input.var != 'Students_ValueMath' & input.var != 'Weekly_Hwk' &
    input.var != 'Teachers_Demographics' & input.var != 'Teachers_AcadSuc' & input.var != 'Teachers_SafeSchools' & input.var != 'Teachers_CondRes' &
    input.var != 'Teachers_Satisfied' & input.var != 'Teachers_Challenges' & input.var != 'Teachers_StudentLimits' & input.var != 'Teachers_MathMajors'
    & input.var != 'Teachers_Confidence' & input.var != 'Teachers_Pedagogy' & input.var != 'Teachers_Hwk' & input.var != 'Teachers_Tests' &
    input.var != 'Teachers_PD' & input.var != 'School_Demographics' & input.var != 'School_Resources' & input.var != 'School_AcadSuc' & input.var != 'School_Discipline'",
    selectInput(
      "item",
      label = "Item",
      choices = c("NA")
    ),
  ),
  conditionalPanel(
    condition = "input.var == 'School_Discipline'",
    selectInput(
      "item_disc",
      label = "Item",
      choices = c("NA",
                  "bcbg15a",
                  "bcbg15b",
                  "bcbg15c",
                  "bcbg15d",
                  "bcbg15e",
                  "bcbg15f",
                  "bcbg15g",
                  "bcbg15h",
                  "bcbg15i",
                  "bcbg15j",
                  "bcbg15k")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'School_AcadSuc'",
    selectInput(
      "item_sas",
      label = "Item",
      choices = c("NA",
                  "bcbg14a",
                  "bcbg14b",
                  "bcbg14c",
                  "bcbg14d",
                  "bcbg14e",
                  "bcbg14f",
                  "bcbg14g",
                  "bcbg14h",
                  "bcbg14i",
                  "bcbg14j",
                  "bcbg14k",
                  "bcbg14l",
                  "bcbg14m")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'School_Resources'",
    selectInput(
      "item_res",
      label = "Item",
      choices = c("NA",
                  "bcbg13aa",
                  "bcbg13ab",
                  "bcbg13ac",
                  "bcbg13ad",
                  "bcbg13ae",
                  "bcbg13af",
                  "bcbg13ag",
                  "bcbg13ah",
                  "bcbg13ai",
                  "bcbg13ba",
                  "bcbg13bb",
                  "bcbg13bc",
                  "bcbg13bd",
                  "bcbg13be")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'School_Demographics'",
    selectInput(
      "item_sd",
      label = "Item",
      choices = c("bcbg03a",
                  "bcdg03",
                  "bcbg03b",
                  "bcbg04",
                  "bcbg05b",
                  "bcbg06a",
                  "bcbg06b",
                  "bcbg07a",
                  "bcbg07b",
                  "bcbg07c",
                  "bcbg08a",
                  "bcbg08b",
                  "bcbg09a",
                  "bcbg10",
                  "bcbg12",
                  "bcbg16a",
                  "bcbg17a",
                  "bcbg18a",
                  "bcbg18b",
                  "bcbg19",
                  "bcbg20")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Teachers_PD'",
    selectInput(
      "item_pd",
      label = "Item",
      choices = c("btbm25.math",
                  "btbm24a.math",
                  "btbm24b.math",
                  "btbm24c.math",
                  "btbm24d.math",
                  "btbm24e.math",
                  "btbm24f.math",
                  "btbm24g.math")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Teachers_Tests'",
    selectInput(
      "item_test",
      label = "Item",
      choices = c("btbm23a.math",
                  "btbm23b.math",
                  "btbm23c.math")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Teachers_Hwk'",
    selectInput(
      "item_hwk",
      label = "Item",
      choices = c("btbm22a.math",
                  "btbm22b.math",
                  "btbm22ca.math",
                  "btbm22cb.math",
                  "btbm22cc.math",
                  "btbm22cd.math",
                  "btbm22ce.math")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Teachers_Pedagogy'",
    selectInput(
      "item_ped",
      label = "Item",
      choices = c("btbm16.math",
                  "btbm18a.math",
                  "btbm18b.math",
                  "btbm18c.math",
                  "btbm18d.math",
                  "btbm18e.math",
                  "btbm18f.math",
                  "btbm18g.math",
                  "btbm18h.math",
                  "btbm18i.math",
                  "btbm18j.math")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Teachers_Confidence'",
    selectInput(
      "item_conf",
      label = "Item",
      choices = c("btbm17a.math",
                  "btbm17b.math",
                  "btbm17c.math",
                  "btbm17d.math",
                  "btbm17e.math",
                  "btbm17f.math",
                  "btbm17g.math",
                  "btbm17h.math",
                  "btbm17i.math")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Teachers_MathMajors'",
    selectInput(
      "item_tmm",
      label = "Item",
      choices = c("btdm05.math",
                  "btbg05a.math",
                  "btbg05f.math")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Teachers_StudentLimits'",
    selectInput(
      "item_tsl",
      label = "Item",
      choices = c("NA",
                  "btbg15a.math",
                  "btbg15b.math",
                  "btbg15c.math",
                  "btbg15d.math",
                  "btbg15e.math",
                  "btbg15f.math",
                  "btbg15g.math")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Teachers_Challenges'",
    selectInput(
      "item_tch",
      label = "Item",
      choices = c("NA",
                  "btbg11a.math",
                  "btbg11b.math",
                  "btbg11c.math",
                  "btbg11d.math",
                  "btbg11e.math",
                  "btbg11f.math",
                  "btbg11g.math",
                  "btbg11h.math")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Teachers_Satisfied'",
    selectInput(
      "item_sat",
      label = "Item",
      choices = c("NA",
                  "btbg10a.math",
                  "btbg10b.math",
                  "btbg10c.math",
                  "btbg10d.math",
                  "btbg10e.math",
                  "btbg10f.math",
                  "btbg10g.math")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Teachers_CondRes'",
    selectInput(
      "item_tcr",
      label = "Item",
      choices = c("NA",
                  "btbg08a.math",
                  "btbg08b.math",
                  "btbg08c.math",
                  "btbg08d.math",
                  "btbg08e.math",
                  "btbg08f.math",
                  "btbg08g.math")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Teachers_SafeSchools'",
    selectInput(
      "item_tss",
      label = "Item",
      choices = c("NA",
                  "btbg07a.math",
                  "btbg07b.math",
                  "btbg07c.math",
                  "btbg07d.math",
                  "btbg07e.math",
                  "btbg07f.math",
                  "btbg07g.math",
                  "btbg07h.math")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Teachers_AcadSuc'",
    selectInput(
      "item_tac",
      label = "Item",
      choices = c("NA",
                  "btbg06a.math",
                  "btbg06b.math",
                  "btbg06c.math",
                  "btbg06d.math",
                  "btbg06e.math",
                  "btbg06f.math",
                  "btbg06g.math",
                  "btbg06h.math",
                  "btbg06i.math",
                  "btbg06j.math",
                  "btbg06k.math",
                  "btbg06l.math",
                  "btbg06m.math",
                  "btbg06o.math")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Teachers_Demographics'",
    selectInput(
      "item_td",
      label = "Item",
      choices = c("btbg01.math",
                  "btbg02.math",
                  "btbg03.math",
                  "btbg12.math",
                  "btbg13.math")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Weekly_Hwk'",
    selectInput(
      "item_wh",
      label = "Item",
      choices = c("bsdmwkhw",
                  "bsbm25aa",
                  "bsbm25ba")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Students_ValueMath'",
    selectInput(
      "item_vm",
      label = "Item",
      choices = c("NA",
                  "bsbm20a",
                  "bsbm20b",
                  "bsbm20c",
                  "bsbm20d",
                  "bsbm20e",
                  "bsbm20f",
                  "bsbm20g",
                  "bsbm20h",
                  "bsbm20i")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Students_Confidence'",
    selectInput(
      "item_sc",
      label = "Item",
      choices = c("NA",
                  "bsbm19a",
                  "bsbm19b",
                  "bsbm19c",
                  "bsbm19d",
                  "bsbm19e",
                  "bsbm19f",
                  "bsbm19g",
                  "bsbm19h",
                  "bsbm19i")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Engaging_Teaching'",
    selectInput(
      "item_et",
      label = "Item",
      choices = c("NA",
                  "bsbm18a",
                  "bsbm18b",
                  "bsbm18c",
                  "bsbm18d",
                  "bsbm18e",
                  "bsbm18f",
                  "bsbm18g",
                  "bsbm18h",
                  "bsbm18i",
                  "bsbm18j")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Students_Bullying'",
    selectInput(
      "item_bull",
      label = "Item",
      choices = c("NA",
                  "bsbg16a",
                  "bsbg16b",
                  "bsbg16c",
                  "bsbg16d",
                  "bsbg16e",
                  "bsbg16f",
                  "bsbg16g",
                  "bsbg16h",
                  "bsbg16i")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Students_Belonging'",
    selectInput(
      "item_sb",
      label = "Item",
      choices = c("NA",
                  "bsbg15a",
                  "bsbg15b",
                  "bsbg15c",
                  "bsbg15d",
                  "bsbg15e",
                  "bsbg15f",
                  "bsbg15g")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'HomeEdResources'",
    selectInput(
      "item_her",
      label = "Item",
      choices = c("NA",
                  "bsbg04",
                  "bsbg06d",
                  "bsbg06e",
                  "bsbg07a",
                  "bsbg07b",
                  "bsdg06s",
                  "bsdgedup")
    )
  ),
  conditionalPanel(
    condition = "input.var == 'Like_Math'",
    selectInput(
      "item_lm",
      label = "Item",
      choices = c("NA",
                  "bsbm17a",
                  "bsbm17b",
                  "bsbm17c",
                  "bsbm17d",
                  "bsbm17e",
                  "bsbm17f",
                  "bsbm17g",
                  "bsbm17h",
                  "bsbm17i")
    )
  ),
  selectInput(
    "group",
    label = "Group By",
    choices = c("NA",
                "idcntry")
  ),
  selectInput(
    "display",
    label = "Display",
    choices = c("Count",
                "Proportion")
  )
  )

main_content <- mainPanel(
  plotOutput("plot")
)

second_panel <- tabPanel(
  "Visualization",
  titlePanel("Grade 8 Trends in International Mathematics: U.S. vs. Taiwan"),
  p("Use the selector input below to choose which variable you would like to see."),
  sidebarLayout(
    sidebar_content, main_content
  )
)

# UI
ui2 <- navbarPage(
  "By: Christopher Meng",
  second_panel
)