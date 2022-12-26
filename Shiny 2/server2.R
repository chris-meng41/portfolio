# Load data and libraries
library(tidyverse)
library(ggplot2)
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

Engaging_Teaching <- TIMSS2015 %>%
  select(idcntry, bsdgeml:bsbm18j)

Students_Confidence <- TIMSS2015 %>%
  select(idcntry, bsdgscm:bsbm19i)

Students_ValueMath <- TIMSS2015 %>%
  select(idcntry, bsdgsvm:bsbm20i)

WeeklyHwk <- TIMSS2015 %>%
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

# Create server
server2 <- function(input, output) {
  output$plot <- renderPlot({
    req(input$var, input$group, input$display)
    if (input$display == "Count") {
      if (input$group == "NA") {
        if (input$var == "School_Discipline") {
          if (input$item_disc == "NA") {
            ggplot(data = School_Discipline, aes(bcdgdas)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15a") {
            ggplot(data = School_Discipline, aes(bcbg15a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15b") {
            ggplot(data = School_Discipline, aes(bcbg15b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15c") {
            ggplot(data = School_Discipline, aes(bcbg15c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15d") {
            ggplot(data = School_Discipline, aes(bcbg15d)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15e") {
            ggplot(data = School_Discipline, aes(bcbg15e)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15f") {
            ggplot(data = School_Discipline, aes(bcbg15f)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15g") {
            ggplot(data = School_Discipline, aes(bcbg15g)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15h") {
            ggplot(data = School_Discipline, aes(bcbg15h)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15i") {
            ggplot(data = School_Discipline, aes(bcbg15i)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15j") {
            ggplot(data = School_Discipline, aes(bcbg15j)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15k") {
            ggplot(data = School_Discipline, aes(bcbg15k)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } 
        } else if (input$var == "School_AcadSuc") {
          if (input$item_sas == "NA") {
            ggplot(data = School_AcadSuc, aes(bcdgeas)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14a") {
            ggplot(data = School_AcadSuc, aes(bcbg14a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14b") {
            ggplot(data = School_AcadSuc, aes(bcbg14b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14c") {
            ggplot(data = School_AcadSuc, aes(bcbg14c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14d") {
            ggplot(data = School_AcadSuc, aes(bcbg14d)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14e") {
            ggplot(data = School_AcadSuc, aes(bcbg14e)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14f") {
            ggplot(data = School_AcadSuc, aes(bcbg14f)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14g") {
            ggplot(data = School_AcadSuc, aes(bcbg14g)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14h") {
            ggplot(data = School_AcadSuc, aes(bcbg14h)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14i") {
            ggplot(data = School_AcadSuc, aes(bcbg14i)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14j") {
            ggplot(data = School_AcadSuc, aes(bcbg14j)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14k") {
            ggplot(data = School_AcadSuc, aes(bcbg14k)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14l") {
            ggplot(data = School_AcadSuc, aes(bcbg14l)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14m") {
            ggplot(data = School_AcadSuc, aes(bcbg14m)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } 
        } else if (input$var == "School_Resources") {
          if (input$item_res == "NA") {
            ggplot(data = School_Resources, aes(bcdgmrs)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_res == "bcbg13aa") {
            ggplot(data = School_Resources, aes(bcbg13aa)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13ab") {
            ggplot(data = School_Resources, aes(bcbg13ab)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13ac") {
            ggplot(data = School_Resources, aes(bcbg13ac)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13ad") {
            ggplot(data = School_Resources, aes(bcbg13ad)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13ae") {
            ggplot(data = School_Resources, aes(bcbg13ae)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13af") {
            ggplot(data = School_Resources, aes(bcbg13af)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13ag") {
            ggplot(data = School_Resources, aes(bcbg13ag)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13ah") {
            ggplot(data = School_Resources, aes(bcbg13ah)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13ai") {
            ggplot(data = School_Resources, aes(bcbg13ai)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13ba") {
            ggplot(data = School_Resources, aes(bcbg13ba)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13bb") {
            ggplot(data = School_Resources, aes(bcbg13bb)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13bc") {
            ggplot(data = School_Resources, aes(bcbg13bc)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13bd") {
            ggplot(data = School_Resources, aes(bcbg13bd)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13be") {
            ggplot(data = School_Resources, aes(bcbg13be)) +
              geom_bar(stat = "count")
          } 
        } else if (input$var == "School_Demographics") {
          if (input$item_sd == "bcbg03a") {
            ggplot(data = School_Demographics, aes(bcbg03a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sd == "bcdg03") {
            ggplot(data = School_Demographics, aes(bcdg03)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sd == "bcbg03b") {
            ggplot(data = School_Demographics, aes(bcbg03b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sd == "bcbg04") {
            ggplot(data = School_Demographics, aes(bcbg04)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sd == "bcbg05b") {
            ggplot(data = School_Demographics, aes(bcbg05b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sd == "bcbg06a") {
            ggplot(data = School_Demographics, aes(bcbg06a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sd == "bcbg06b") {
            ggplot(data = School_Demographics, aes(bcbg06b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sd == "bcbg07a") {
            ggplot(data = School_Demographics, aes(bcbg07a)) +
              geom_histogram(bins = 16)
          } else if (input$item_sd == "bcbg07b") {
            ggplot(data = School_Demographics, aes(bcbg07b)) +
              geom_histogram(bins = 16)
          } else if (input$item_sd == "bcbg07c") {
            ggplot(data = School_Demographics, aes(bcbg07c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sd == "bcbg08a") {
            ggplot(data = School_Demographics, aes(bcbg08a)) +
              geom_bar(stat = "count")
          } else if (input$item_sd == "bcbg08b") {
            ggplot(data = School_Demographics, aes(bcbg08b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sd == "bcbg09a") {
            ggplot(data = School_Demographics, aes(bcbg09a)) +
              geom_bar(stat = "count")
          } else if (input$item_sd == "bcbg10") {
            ggplot(data = School_Demographics, aes(bcbg10)) +
              geom_histogram(bins = 22)
          } else if (input$item_sd == "bcbg12") {
            ggplot(data = School_Demographics, aes(bcbg12)) +
              geom_bar(stat = "count")
          } else if (input$item_sd == "bcbg16a") {
            ggplot(data = School_Demographics, aes(bcbg16a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sd == "bcbg17a") {
            ggplot(data = School_Demographics, aes(bcbg17a)) +
              geom_bar(stat = "count")
          } else if (input$item_sd == "bcbg18a") {
            ggplot(data = School_Demographics, aes(bcbg18a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sd == "bcbg18b") {
            ggplot(data = School_Demographics, aes(bcbg18b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sd == "bcbg19") {
            ggplot(data = School_Demographics, aes(bcbg19)) +
              geom_histogram(bins = 20)
          } else if (input$item_sd == "bcbg20") {
            ggplot(data = School_Demographics, aes(bcbg20)) +
              geom_histogram(bins = 30)
          } 
        } else if (input$var == "Teachers_PD") {
          if (input$item_pd == "btbm25.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm25.math)), aes(btbm25.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_pd == "btbm24a.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24a.math)), aes(btbm24a.math)) +
              geom_bar(stat = "count")
          } else if (input$item_pd == "btbm24b.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24b.math)), aes(btbm24b.math)) +
              geom_bar(stat = "count")
          } else if (input$item_pd == "btbm24c.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24c.math)), aes(btbm24c.math)) +
              geom_bar(stat = "count")
          } else if (input$item_pd == "btbm24d.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24d.math)), aes(btbm24d.math)) +
              geom_bar(stat = "count")
          } else if (input$item_pd == "btbm24e.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24e.math)), aes(btbm24e.math)) +
              geom_bar(stat = "count")
          } else if (input$item_pd == "btbm24f.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24f.math)), aes(btbm24f.math)) +
              geom_bar(stat = "count")
          } else if (input$item_pd == "btbm24g.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24g.math)), aes(btbm24g.math)) +
              geom_bar(stat = "count")
          } 
        } else if (input$var == "Teachers_Tests") {
          if (input$item_test == "btbm23a.math") {
            ggplot(data = subset(Teachers_Tests, !is.na(btbm23a.math)), aes(btbm23a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_test == "btbm23b.math") {
            ggplot(data = subset(Teachers_Tests, !is.na(btbm23b.math)), aes(btbm23b.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_test == "btbm23c.math") {
            ggplot(data = subset(Teachers_Tests, !is.na(btbm23c.math)), aes(btbm23c.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Teachers_Hwk") {
          if (input$item_hwk == "btbm22a.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22a.math)), aes(btbm22a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_hwk == "btbm22b.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22b.math)), aes(btbm22b.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_hwk == "btbm22ca.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22ca.math)), aes(btbm22ca.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_hwk == "btbm22cb.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22cb.math)), aes(btbm22cb.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_hwk == "btbm22cc.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22cc.math)), aes(btbm22cc.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_hwk == "btbm22cd.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22cd.math)), aes(btbm22cd.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_hwk == "btbm22ce.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22ce.math)), aes(btbm22ce.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } 
        } else if (input$var == "Teachers_Pedagogy") {
          if (input$item_ped == "btbm16.math") {
            ggplot(data = Teachers_Pedagogy, aes(btbm16.math)) +
              geom_histogram(bins = 18)
          } else if (input$item_ped == "btbm18a.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18a.math)), aes(btbm18a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18b.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18b.math)), aes(btbm18b.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18c.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18c.math)), aes(btbm18c.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18d.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18d.math)), aes(btbm18d.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18e.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18e.math)), aes(btbm18e.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18f.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18f.math)), aes(btbm18f.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18g.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18g.math)), aes(btbm18g.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18h.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18h.math)), aes(btbm18h.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18i.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18i.math)), aes(btbm18i.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18j.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18j.math)), aes(btbm18j.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } 
        } else if (input$var == "Teachers_Confidence") {
          if (input$item_conf == "btbm17a.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17a.math)), aes(btbm17a.math)) +
              geom_bar(stat = "count")
          } else if (input$item_conf == "btbm17b.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17b.math)), aes(btbm17b.math)) +
              geom_bar(stat = "count")
          } else if (input$item_conf == "btbm17c.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17c.math)), aes(btbm17c.math)) +
              geom_bar(stat = "count")
          } else if (input$item_conf == "btbm17d.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17d.math)), aes(btbm17d.math)) +
              geom_bar(stat = "count")
          } else if (input$item_conf == "btbm17e.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17e.math)), aes(btbm17e.math)) +
              geom_bar(stat = "count")
          } else if (input$item_conf == "btbm17f.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17f.math)), aes(btbm17f.math)) +
              geom_bar(stat = "count")
          } else if (input$item_conf == "btbm17g.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17g.math)), aes(btbm17g.math)) +
              geom_bar(stat = "count")
          } else if (input$item_conf == "btbm17h.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17h.math)), aes(btbm17h.math)) +
              geom_bar(stat = "count")
          } else if (input$item_conf == "btbm17i.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17i.math)), aes(btbm17i.math)) +
              geom_bar(stat = "count")
          } 
        } else if (input$var == "Teachers_MathMajors") {
          if (input$item_tmm == "btdm05.math") {
            ggplot(data = subset(Teachers_MathMajors, !is.na(btdm05.math)), aes(btdm05.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tmm == "btbg05a.math") {
            ggplot(data = subset(Teachers_MathMajors, !is.na(btbg05a.math)), aes(btbg05a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tmm == "btbg05f.math") {
            ggplot(data = subset(Teachers_MathMajors, !is.na(btbg05f.math)), aes(btbg05f.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Teachers_StudentLimits") {
          if (input$item_tsl == "NA") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btdglsn.math)), aes(btdglsn.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tsl == "btbg15a.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15a.math)), aes(btbg15a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tsl == "btbg15b.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15b.math)), aes(btbg15b.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tsl == "btbg15c.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15c.math)), aes(btbg15c.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tsl == "btbg15d.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15d.math)), aes(btbg15d.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tsl == "btbg15e.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15e.math)), aes(btbg15e.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tsl == "btbg15f.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15f.math)), aes(btbg15f.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tsl == "btbg15g.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15g.math)), aes(btbg15g.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Teachers_Challenges") {
          if (input$item_tch == "NA") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btdgcft.math)), aes(btdgcft.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tch == "btbg11a.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11a.math)), aes(btbg11a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tch == "btbg11b.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11b.math)), aes(btbg11b.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tch == "btbg11c.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11c.math)), aes(btbg11c.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tch == "btbg11d.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11d.math)), aes(btbg11d.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tch == "btbg11e.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11e.math)), aes(btbg11e.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tch == "btbg11f.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11f.math)), aes(btbg11f.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tch == "btbg11g.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11g.math)), aes(btbg11g.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tch == "btbg11h.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11h.math)), aes(btbg11h.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Teachers_Satisfied") {
          if (input$item_sat == "NA") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btdgtjs.math)), aes(btdgtjs.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sat == "btbg10a.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10a.math)), aes(btbg10a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sat == "btbg10b.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10b.math)), aes(btbg10b.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sat == "btbg10c.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10c.math)), aes(btbg10c.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sat == "btbg10d.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10d.math)), aes(btbg10d.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sat == "btbg10e.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10e.math)), aes(btbg10e.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sat == "btbg10f.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10f.math)), aes(btbg10f.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sat == "btbg10g.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10g.math)), aes(btbg10g.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Teachers_CondRes") {
          if (input$item_tcr == "NA") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btdgscr.math)), aes(btdgscr.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tcr == "btbg08a.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08a.math)), aes(btbg08a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tcr == "btbg08b.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08b.math)), aes(btbg08b.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tcr == "btbg08c.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08c.math)), aes(btbg08c.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tcr == "btbg08d.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08d.math)), aes(btbg08d.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tcr == "btbg08e.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08e.math)), aes(btbg08e.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tcr == "btbg08f.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08f.math)), aes(btbg08f.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tcr == "btbg08g.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08g.math)), aes(btbg08g.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Teachers_SafeSchools") {
          if (input$item_tss == "NA") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btdgsos.math)), aes(btdgsos.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tss == "btbg07a.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07a.math)), aes(btbg07a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tss == "btbg07b.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07b.math)), aes(btbg07b.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tss == "btbg07c.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07c.math)), aes(btbg07c.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tss == "btbg07d.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07d.math)), aes(btbg07d.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tss == "btbg07e.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07e.math)), aes(btbg07e.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tss == "btbg07f.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07f.math)), aes(btbg07f.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tss == "btbg07g.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07g.math)), aes(btbg07g.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tss == "btbg07h.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07h.math)), aes(btbg07h.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Teachers_AcadSuc") {
          if (input$item_tac == "NA") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btdgeas.math)), aes(btdgeas.math)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06a.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06a.math)), aes(btbg06a.math)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06b.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06b.math)), aes(btbg06b.math)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06c.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06c.math)), aes(btbg06c.math)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06d.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06d.math)), aes(btbg06d.math)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06e.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06e.math)), aes(btbg06e.math)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06f.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06f.math)), aes(btbg06f.math)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06g.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06g.math)), aes(btbg06g.math)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06h.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06h.math)), aes(btbg06h.math)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06i.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06i.math)), aes(btbg06i.math)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06j.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06j.math)), aes(btbg06j.math)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06k.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06k.math)), aes(btbg06k.math)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06l.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06l.math)), aes(btbg06l.math)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06m.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06m.math)), aes(btbg06m.math)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06o.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06o.math)), aes(btbg06o.math)) +
              geom_bar(stat = "count")
          }
        } else if (input$var == "Teachers_Demographics") {
          if (input$item_td == "btbg01.math") {
            ggplot(data = Teachers_Demographics, aes(btbg01.math)) +
              geom_histogram(na.rm = TRUE, bins = 22)
          } else if (input$item_td == "btbg02.math") {
            ggplot(data = subset(Teachers_Demographics, !is.na(btbg02.math)), aes(btbg02.math)) +
              geom_bar(stat = "count")
          } else if (input$item_td == "btbg03.math") {
            ggplot(data = subset(Teachers_Demographics, !is.na(btbg03.math)), aes(btbg03.math)) +
              geom_bar(stat = "count", na.rm = TRUE) +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_td == "btbg12.math") {
            ggplot(data = Teachers_Demographics, aes(btbg12.math)) +
              geom_histogram(bins = 14)
          } else if (input$item_td == "btbg13.math") {
            ggplot(data = Teachers_Demographics, aes(btbg13.math)) +
              geom_histogram(bins = 18)
          }
        } else if (input$var == "Weekly_Hwk") {
          if (input$item_wh == "bsdmwkhw") {
            ggplot(data = Weekly_Hwk, aes(bsdmwkhw)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_wh == "bsbm25aa") {
            ggplot(data = Weekly_Hwk, aes(bsbm25aa)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_wh == "bsbm25ba") {
            ggplot(data = Weekly_Hwk, aes(bsbm25ba)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Students_ValueMath") {
          if (input$item_vm == "NA") {
            ggplot(data = Students_ValueMath, aes(bsdgsvm)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20a") {
            ggplot(data = Students_ValueMath, aes(bsbm20a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20b") {
            ggplot(data = Students_ValueMath, aes(bsbm20b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20c") {
            ggplot(data = Students_ValueMath, aes(bsbm20c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20d") {
            ggplot(data = Students_ValueMath, aes(bsbm20d)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20e") {
            ggplot(data = Students_ValueMath, aes(bsbm20e)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20f") {
            ggplot(data = Students_ValueMath, aes(bsbm20f)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20g") {
            ggplot(data = Students_ValueMath, aes(bsbm20g)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20h") {
            ggplot(data = Students_ValueMath, aes(bsbm20h)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20i") {
            ggplot(data = Students_ValueMath, aes(bsbm20i)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Students_Confidence") {
          if (input$item_sc == "NA") {
            ggplot(data = Students_Confidence, aes(bsdgscm)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19a") {
            ggplot(data = Students_Confidence, aes(bsbm19a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19b") {
            ggplot(data = Students_Confidence, aes(bsbm19b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19c") {
            ggplot(data = Students_Confidence, aes(bsbm19c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19d") {
            ggplot(data = Students_Confidence, aes(bsbm19d)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19e") {
            ggplot(data = Students_Confidence, aes(bsbm19e)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19f") {
            ggplot(data = Students_Confidence, aes(bsbm19f)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19g") {
            ggplot(data = Students_Confidence, aes(bsbm19g)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19h") {
            ggplot(data = Students_Confidence, aes(bsbm19h)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19i") {
            ggplot(data = Students_Confidence, aes(bsbm19i)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Engaging_Teaching") {
          if (input$item_et == "NA") {
            ggplot(data = Engaging_Teaching, aes(bsdgeml)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18a") {
            ggplot(data = Engaging_Teaching, aes(bsbm18a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18b") {
            ggplot(data = Engaging_Teaching, aes(bsbm18b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18c") {
            ggplot(data = Engaging_Teaching, aes(bsbm18c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18d") {
            ggplot(data = Engaging_Teaching, aes(bsbm18d)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18e") {
            ggplot(data = Engaging_Teaching, aes(bsbm18e)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18f") {
            ggplot(data = Engaging_Teaching, aes(bsbm18f)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18g") {
            ggplot(data = Engaging_Teaching, aes(bsbm18g)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18h") {
            ggplot(data = Engaging_Teaching, aes(bsbm18h)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18i") {
            ggplot(data = Engaging_Teaching, aes(bsbm18i)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18j") {
            ggplot(data = Engaging_Teaching, aes(bsbm18j)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Students_Bullying") {
          if (input$item_bull == "NA") {
            ggplot(data = Students_Bullying, aes(bsdgsb)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16a") {
            ggplot(data = Students_Bullying, aes(bsbg16a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16b") {
            ggplot(data = Students_Bullying, aes(bsbg16b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16c") {
            ggplot(data = Students_Bullying, aes(bsbg16c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16d") {
            ggplot(data = Students_Bullying, aes(bsbg16d)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16e") {
            ggplot(data = Students_Bullying, aes(bsbg16e)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16f") {
            ggplot(data = Students_Bullying, aes(bsbg16f)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16g") {
            ggplot(data = Students_Bullying, aes(bsbg16g)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16h") {
            ggplot(data = Students_Bullying, aes(bsbg16h)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16i") {
            ggplot(data = Students_Bullying, aes(bsbg16i)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Students_Belonging") {
          if (input$item_sb == "NA") {
            ggplot(data = Students_Belonging, aes(bsdgssb)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sb == "bsbg15a") {
            ggplot(data = Students_Belonging, aes(bsbg15a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sb == "bsbg15b") {
            ggplot(data = Students_Belonging, aes(bsbg15b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sb == "bsbg15c") {
            ggplot(data = Students_Belonging, aes(bsbg15c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sb == "bsbg15d") {
            ggplot(data = Students_Belonging, aes(bsbg15d)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sb == "bsbg15e") {
            ggplot(data = Students_Belonging, aes(bsbg15e)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sb == "bsbg15f") {
            ggplot(data = Students_Belonging, aes(bsbg15f)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sb == "bsbg15g") {
            ggplot(data = Students_Belonging, aes(bsbg15g)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Like_Math") {
          if (input$item_lm == "NA") {
            ggplot(data = Like_Math, aes(bsdgslm)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17a") {
            ggplot(data = Like_Math, aes(bsbm17a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17b") {
            ggplot(data = Like_Math, aes(bsbm17b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17c") {
            ggplot(data = Like_Math, aes(bsbm17c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17d") {
            ggplot(data = Like_Math, aes(bsbm17d)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17e") {
            ggplot(data = Like_Math, aes(bsbm17e)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17f") {
            ggplot(data = Like_Math, aes(bsbm17f)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17g") {
            ggplot(data = Like_Math, aes(bsbm17g)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17h") {
            ggplot(data = Like_Math, aes(bsbm17h)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17i") {
            ggplot(data = Like_Math, aes(bsbm17i)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Plausible_Value1") {
          ggplot(data = Plausible_Value1, aes(bsmmat01)) +
            geom_histogram()
        } else if (input$var == "Plausible_Value2") {
          ggplot(data = Plausible_Value2, aes(bsmmat02)) +
            geom_histogram()
        } else if (input$var == "Plausible_Value3") {
          ggplot(data = Plausible_Value3, aes(bsmmat03)) +
            geom_histogram()
        } else if (input$var == "Plausible_Value4") {
          ggplot(data = Plausible_Value4, aes(bsmmat04)) +
            geom_histogram()
        } else if (input$var == "Plausible_Value5") {
          ggplot(data = Plausible_Value5, aes(bsmmat05)) +
            geom_histogram()
        } else if (input$var == "Benchmark1") {
          ggplot(data = Benchmark1, aes(bsmibm01)) +
            geom_bar(stat = "count")
        } else if (input$var == "Benchmark2") {
          ggplot(data = Benchmark2, aes(bsmibm02)) +
            geom_bar(stat = "count")
        } else if (input$var == "Benchmark3") {
          ggplot(data = Benchmark3, aes(bsmibm03)) +
            geom_bar(stat = "count")
        } else if (input$var == "Benchmark4") {
          ggplot(data = Benchmark4, aes(bsmibm04)) +
            geom_bar(stat = "count")
        } else if (input$var == "Benchmark5") {
          ggplot(data = Benchmark5, aes(bsmibm05)) +
            geom_bar(stat = "count")
        } else if (input$var == "TooLowAch") {
          ggplot(data = TooLowAch, aes(bsdmlowp)) +
            geom_bar(stat = "count")
        } else if (input$var == "HomeEdResources") {
          if (input$item_her == "NA") {
            ggplot(data = HomeEdResources, aes(bsdgher)) +
              geom_bar(stat = "count")
          } else if (input$item_her == "bsbg04") {
            ggplot(data = HomeEdResources, aes(bsbg04)) +
              geom_bar(stat = "count")
          } else if (input$item_her == "bsbg06d") {
            ggplot(data = HomeEdResources, aes(bsbg06d)) +
              geom_bar(stat = "count")
          } else if (input$item_her == "bsbg06e") {
            ggplot(data = HomeEdResources, aes(bsbg06e)) +
              geom_bar(stat = "count")
          } else if (input$item_her == "bsbg07a") {
            ggplot(data = HomeEdResources, aes(bsbg07a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_her == "bsbg07b") {
            ggplot(data = HomeEdResources, aes(bsbg07b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_her == "bsdg06s") {
            ggplot(data = HomeEdResources, aes(bsdg06s)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_her == "bsdgedup") {
            ggplot(data = HomeEdResources, aes(bsdgedup)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        }
      }
      else { # group = idcntry
        if (input$var == "School_Discipline") {
          if (input$item_disc == "NA") {
            ggplot(data = School_Discipline, aes(bcdgdas)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15a") {
            ggplot(data = School_Discipline, aes(bcbg15a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15b") {
            ggplot(data = School_Discipline, aes(bcbg15b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15c") {
            ggplot(data = School_Discipline, aes(bcbg15c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15d") {
            ggplot(data = School_Discipline, aes(bcbg15d)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15e") {
            ggplot(data = School_Discipline, aes(bcbg15e)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15f") {
            ggplot(data = School_Discipline, aes(bcbg15f)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15g") {
            ggplot(data = School_Discipline, aes(bcbg15g)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15h") {
            ggplot(data = School_Discipline, aes(bcbg15h)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15i") {
            ggplot(data = School_Discipline, aes(bcbg15i)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15j") {
            ggplot(data = School_Discipline, aes(bcbg15j)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15k") {
            ggplot(data = School_Discipline, aes(bcbg15k)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "School_AcadSuc") {
          if (input$item_sas == "NA") {
            ggplot(data = School_AcadSuc, aes(bcdgeas)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14a") {
            ggplot(data = School_AcadSuc, aes(bcbg14a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14b") {
            ggplot(data = School_AcadSuc, aes(bcbg14b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14c") {
            ggplot(data = School_AcadSuc, aes(bcbg14c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14d") {
            ggplot(data = School_AcadSuc, aes(bcbg14d)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14e") {
            ggplot(data = School_AcadSuc, aes(bcbg14e)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14f") {
            ggplot(data = School_AcadSuc, aes(bcbg14f)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14g") {
            ggplot(data = School_AcadSuc, aes(bcbg14g)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14h") {
            ggplot(data = School_AcadSuc, aes(bcbg14h)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14i") {
            ggplot(data = School_AcadSuc, aes(bcbg14i)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14j") {
            ggplot(data = School_AcadSuc, aes(bcbg14j)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14k") {
            ggplot(data = School_AcadSuc, aes(bcbg14k)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14l") {
            ggplot(data = School_AcadSuc, aes(bcbg14l)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14m") {
            ggplot(data = School_AcadSuc, aes(bcbg14m)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "School_Resources") {
          if (input$item_res == "NA") {
            ggplot(data = School_Resources, aes(bcdgmrs)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13aa") {
            ggplot(data = School_Resources, aes(bcbg13aa)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13ab") {
            ggplot(data = School_Resources, aes(bcbg13ab)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13ac") {
            ggplot(data = School_Resources, aes(bcbg13ac)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13ad") {
            ggplot(data = School_Resources, aes(bcbg13ad)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13ae") {
            ggplot(data = School_Resources, aes(bcbg13ae)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13af") {
            ggplot(data = School_Resources, aes(bcbg13af)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13ag") {
            ggplot(data = School_Resources, aes(bcbg13ag)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13ah") {
            ggplot(data = School_Resources, aes(bcbg13ah)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13ai") {
            ggplot(data = School_Resources, aes(bcbg13ai)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13ba") {
            ggplot(data = School_Resources, aes(bcbg13ba)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13bb") {
            ggplot(data = School_Resources, aes(bcbg13bb)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13bc") {
            ggplot(data = School_Resources, aes(bcbg13bc)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13bd") {
            ggplot(data = School_Resources, aes(bcbg13bd)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13be") {
            ggplot(data = School_Resources, aes(bcbg13be)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "School_Demographics") {
          if (input$item_sd == "bcbg03a") {
            ggplot(data = School_Demographics, aes(bcbg03a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcdg03") {
            ggplot(data = School_Demographics, aes(bcdg03)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg03b") {
            ggplot(data = School_Demographics, aes(bcbg03b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg04") {
            ggplot(data = School_Demographics, aes(bcbg04)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg05b") {
            ggplot(data = School_Demographics, aes(bcbg05b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg06a") {
            ggplot(data = School_Demographics, aes(bcbg06a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg06b") {
            ggplot(data = School_Demographics, aes(bcbg06b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg07a") {
            ggplot(data = School_Demographics, aes(bcbg07a)) +
              geom_histogram(bins = 16) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg07b") {
            ggplot(data = School_Demographics, aes(bcbg07b)) +
              geom_histogram(bins = 16) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg07c") {
            ggplot(data = School_Demographics, aes(bcbg07c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg08a") {
            ggplot(data = School_Demographics, aes(bcbg08a)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg08b") {
            ggplot(data = School_Demographics, aes(bcbg08b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg09a") {
            ggplot(data = School_Demographics, aes(bcbg09a)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg10") {
            ggplot(data = School_Demographics, aes(bcbg10)) +
              geom_histogram(bins = 22) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg12") {
            ggplot(data = School_Demographics, aes(bcbg12)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg16a") {
            ggplot(data = School_Demographics, aes(bcbg16a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg17a") {
            ggplot(data = School_Demographics, aes(bcbg17a)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg18a") {
            ggplot(data = School_Demographics, aes(bcbg18a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg18b") {
            ggplot(data = School_Demographics, aes(bcbg18b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg19") {
            ggplot(data = School_Demographics, aes(bcbg19)) +
              geom_histogram(bins = 20) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg20") {
            ggplot(data = School_Demographics, aes(bcbg20)) +
              geom_histogram(bins = 30) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "Teachers_PD") {
          if (input$item_pd == "btbm25.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm25.math)), aes(btbm25.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_pd == "btbm24a.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24a.math)), aes(btbm24a.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_pd == "btbm24b.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24b.math)), aes(btbm24b.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_pd == "btbm24c.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24c.math)), aes(btbm24c.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_pd == "btbm24d.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24d.math)), aes(btbm24d.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_pd == "btbm24e.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24e.math)), aes(btbm24e.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_pd == "btbm24f.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24f.math)), aes(btbm24f.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_pd == "btbm24g.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24g.math)), aes(btbm24g.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "Teachers_Tests") {
          if (input$item_test == "btbm23a.math") {
            ggplot(data = subset(Teachers_Tests, !is.na(btbm23a.math)), aes(btbm23a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_test == "btbm23b.math") {
            ggplot(data = subset(Teachers_Tests, !is.na(btbm23b.math)), aes(btbm23b.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_test == "btbm23c.math") {
            ggplot(data = subset(Teachers_Tests, !is.na(btbm23c.math)), aes(btbm23c.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Teachers_Hwk") {
          if (input$item_hwk == "btbm22a.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22a.math)), aes(btbm22a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_hwk == "btbm22b.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22b.math)), aes(btbm22b.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_hwk == "btbm22ca.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22ca.math)), aes(btbm22ca.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_hwk == "btbm22cb.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22cb.math)), aes(btbm22cb.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide == guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_hwk == "btbm22cc.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22cc.math)), aes(btbm22cc.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_hwk == "btbm22cd.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22cd.math)), aes(btbm22cd.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_hwk == "btbm22ce.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22ce.math)), aes(btbm22ce.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "Teachers_Pedagogy") {
          if (input$item_ped == "btbm16.math") {
            ggplot(data = Teachers_Pedagogy, aes(btbm16.math)) +
              geom_histogram(bins = 18) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18a.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18a.math)), aes(btbm18a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18b.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18b.math)), aes(btbm18b.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18c.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18c.math)), aes(btbm18c.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18d.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18d.math)), aes(btbm18d.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18e.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18e.math)), aes(btbm18e.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18f.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18f.math)), aes(btbm18f.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18g.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18g.math)), aes(btbm18g.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18h.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18h.math)), aes(btbm18h.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18i.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18i.math)), aes(btbm18i.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18j.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18j.math)), aes(btbm18j.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "Teachers_Confidence") {
          if (input$item_conf == "btbm17a.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17a.math)), aes(btbm17a.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_conf == "btbm17b.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17b.math)), aes(btbm17b.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_conf == "btbm17c.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17c.math)), aes(btbm17c.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_conf == "btbm17d.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17d.math)), aes(btbm17d.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_conf == "btbm17e.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17e.math)), aes(btbm17e.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_conf == "btbm17f.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17f.math)), aes(btbm17f.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_conf == "btbm17g.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17g.math)), aes(btbm17g.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_conf == "btbm17h.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17h.math)), aes(btbm17h.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_conf == "btbm17i.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17i.math)), aes(btbm17i.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "Teachers_MathMajors") {
          if (input$item_tmm == "btdm05.math") {
            ggplot(data = subset(Teachers_MathMajors, !is.na(btdm05.math)), aes(btdm05.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tmm == "btbg05a.math") {
            ggplot(data = subset(Teachers_MathMajors, !is.na(btbg05a.math)), aes(btbg05a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tmm == "btbg05f.math") {
            ggplot(data = subset(Teachers_MathMajors, !is.na(btbg05f.math)), aes(btbg05f.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Teachers_StudentLimits") {
          if (input$item_tsl == "NA") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btdglsn.math)), aes(btdglsn.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tsl == "btbg15a.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15a.math)), aes(btbg15a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tsl == "btbg15b.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15b.math)), aes(btbg15b.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tsl == "btbg15c.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15c.math)), aes(btbg15c.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tsl == "btbg15d.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15d.math)), aes(btbg15d.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tsl == "btbg15e.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15e.math)), aes(btbg15e.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tsl == "btbg15f.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15f.math)), aes(btbg15f.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tsl == "btbg15g.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15g.math)), aes(btbg15g.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Teachers_Challenges") {
          if (input$item_tch == "NA") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btdgcft.math)), aes(btdgcft.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tch == "btbg11a.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11a.math)), aes(btbg11a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tch == "btbg11b.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11b.math)), aes(btbg11b.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tch == "btbg11c.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11c.math)), aes(btbg11c.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tch == "btbg11d.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11d.math)), aes(btbg11d.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tch == "btbg11e.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11e.math)), aes(btbg11e.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tch == "btbg11f.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11f.math)), aes(btbg11f.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tch == "btbg11g.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11g.math)), aes(btbg11g.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tch == "btbg11h.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11h.math)), aes(btbg11h.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Teachers_Satisfied") {
          if (input$item_sat == "NA") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btdgtjs.math)), aes(btdgtjs.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sat == "btbg10a.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10a.math)), aes(btbg10a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sat == "btbg10b.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10b.math)), aes(btbg10b.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sat == "btbg10c.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10c.math)), aes(btbg10c.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sat == "btbg10d.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10d.math)), aes(btbg10d.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sat == "btbg10e.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10e.math)), aes(btbg10e.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sat == "btbg10f.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10f.math)), aes(btbg10f.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sat == "btbg10g.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10g.math)), aes(btbg10g.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Teachers_CondRes") {
          if (input$item_tcr == "NA") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btdgscr.math)), aes(btdgscr.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tcr == "btbg08a.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08a.math)), aes(btbg08a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tcr == "btbg08b.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08b.math)), aes(btbg08b.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tcr == "btbg08c.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08c.math)), aes(btbg08c.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tcr == "btbg08d.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08d.math)), aes(btbg08d.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tcr == "btbg08e.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08e.math)), aes(btbg08e.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tcr == "btbg08f.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08f.math)), aes(btbg08f.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tcr == "btbg08g.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08g.math)), aes(btbg08g.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Teachers_SafeSchools") {
          if (input$item_tss == "NA") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btdgsos.math)), aes(btdgsos.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tss == "btbg07a.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07a.math)), aes(btbg07a.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tss == "btbg07b.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07b.math)), aes(btbg07b.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tss == "btbg07c.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07c.math)), aes(btbg07c.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tss == "btbg07d.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07d.math)), aes(btbg07d.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tss == "btbg07e.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07e.math)), aes(btbg07e.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tss == "btbg07f.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07f.math)), aes(btbg07f.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tss == "btbg07g.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07g.math)), aes(btbg07g.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tss == "btbg07h.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07h.math)), aes(btbg07h.math)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Teachers_AcadSuc") {
          if (input$item_tac == "NA") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btdgeas.math)), aes(btdgeas.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06a.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06a.math)), aes(btbg06a.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06b.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06b.math)), aes(btbg06b.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06c.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06c.math)), aes(btbg06c.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06d.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06d.math)), aes(btbg06d.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06e.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06e.math)), aes(btbg06e.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06f.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06f.math)), aes(btbg06f.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06g.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06g.math)), aes(btbg06g.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06h.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06h.math)), aes(btbg06h.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06i.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06i.math)), aes(btbg06i.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06j.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06j.math)), aes(btbg06j.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06k.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06k.math)), aes(btbg06k.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06l.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06l.math)), aes(btbg06l.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06m.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06m.math)), aes(btbg06m.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06o.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06o.math)), aes(btbg06o.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Teachers_Demographics") {
          if (input$item_td == "btbg01.math") {
            ggplot(data = Teachers_Demographics, aes(btbg01.math)) +
              geom_histogram(na.rm = TRUE, bins = 22) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_td == "btbg02.math") {
            ggplot(data = subset(Teachers_Demographics, !is.na(btbg02.math)), aes(btbg02.math)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_td == "btbg03.math") {
            ggplot(data = subset(Teachers_Demographics, !is.na(btbg03.math)), aes(btbg03.math)) +
              geom_bar(stat = "count", na.rm = TRUE) +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_td == "btbg12.math") {
            ggplot(data = Teachers_Demographics, aes(btbg12.math)) +
              geom_histogram(bins = 14) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_td == "btbg13.math") {
            ggplot(data = Teachers_Demographics, aes(btbg13.math)) +
              geom_histogram(bins = 18) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Weekly_Hwk") {
          if (input$item_wh == "bsdmwkhw") {
            ggplot(data = Weekly_Hwk, aes(bsdmwkhw)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_wh == "bsbm25aa") {
            ggplot(data = Weekly_Hwk, aes(bsbm25aa)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_wh == "bsbm25ba") {
            ggplot(data = Weekly_Hwk, aes(bsbm25ba)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Students_ValueMath") {
          if (input$item_vm == "NA") {
            ggplot(data = Students_ValueMath, aes(bsdgsvm)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20a") {
            ggplot(data = Students_ValueMath, aes(bsbm20a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20b") {
            ggplot(data = Students_ValueMath, aes(bsbm20b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20c") {
            ggplot(data = Students_ValueMath, aes(bsbm20c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20d") {
            ggplot(data = Students_ValueMath, aes(bsbm20d)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20e") {
            ggplot(data = Students_ValueMath, aes(bsbm20e)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20f") {
            ggplot(data = Students_ValueMath, aes(bsbm20f)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20g") {
            ggplot(data = Students_ValueMath, aes(bsbm20g)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20h") {
            ggplot(data = Students_ValueMath, aes(bsbm20h)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20i") {
            ggplot(data = Students_ValueMath, aes(bsbm20i)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Students_Confidence") {
          if (input$item_sc == "NA") {
            ggplot(data = Students_Confidence, aes(bsdgscm)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19a") {
            ggplot(data = Students_Confidence, aes(bsbm19a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19b") {
            ggplot(data = Students_Confidence, aes(bsbm19b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19c") {
            ggplot(data = Students_Confidence, aes(bsbm19c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19d") {
            ggplot(data = Students_Confidence, aes(bsbm19d)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19e") {
            ggplot(data = Students_Confidence, aes(bsbm19e)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19f") {
            ggplot(data = Students_Confidence, aes(bsbm19f)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19g") {
            ggplot(data = Students_Confidence, aes(bsbm19g)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19h") {
            ggplot(data = Students_Confidence, aes(bsbm19h)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19i") {
            ggplot(data = Students_Confidence, aes(bsbm19i)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Engaging_Teaching") {
          if (input$item_et == "NA") {
            ggplot(data = Engaging_Teaching, aes(bsdgeml)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18a") {
            ggplot(data = Engaging_Teaching, aes(bsbm18a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18b") {
            ggplot(data = Engaging_Teaching, aes(bsbm18b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18c") {
            ggplot(data = Engaging_Teaching, aes(bsbm18c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18d") {
            ggplot(data = Engaging_Teaching, aes(bsbm18d)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18e") {
            ggplot(data = Engaging_Teaching, aes(bsbm18e)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18f") {
            ggplot(data = Engaging_Teaching, aes(bsbm18f)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18g") {
            ggplot(data = Engaging_Teaching, aes(bsbm18g)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18h") {
            ggplot(data = Engaging_Teaching, aes(bsbm18h)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18i") {
            ggplot(data = Engaging_Teaching, aes(bsbm18i)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18j") {
            ggplot(data = Engaging_Teaching, aes(bsbm18j)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Students_Bullying") {
          if (input$item_bull == "NA") {
            ggplot(data = Students_Bullying, aes(bsdgsb)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16a") {
            ggplot(data = Students_Bullying, aes(bsbg16a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16b") {
            ggplot(data = Students_Bullying, aes(bsbg16b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16c") {
            ggplot(data = Students_Bullying, aes(bsbg16c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16d") {
            ggplot(data = Students_Bullying, aes(bsbg16d)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16e") {
            ggplot(data = Students_Bullying, aes(bsbg16e)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16f") {
            ggplot(data = Students_Bullying, aes(bsbg16f)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16g") {
            ggplot(data = Students_Bullying, aes(bsbg16g)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16h") {
            ggplot(data = Students_Bullying, aes(bsbg16h)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16i") {
            ggplot(data = Students_Bullying, aes(bsbg16i)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Students_Belonging") {
          if (input$item_sb == "NA") {
            ggplot(data = Students_Belonging, aes(bsdgssb)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sb == "bsbg15a") {
            ggplot(data = Students_Belonging, aes(bsbg15a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sb == "bsbg15b") {
            ggplot(data = Students_Belonging, aes(bsbg15b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sb == "bsbg15c") {
            ggplot(data = Students_Belonging, aes(bsbg15c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sb == "bsbg15d") {
            ggplot(data = Students_Belonging, aes(bsbg15d)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sb == "bsbg15e") {
            ggplot(data = Students_Belonging, aes(bsbg15e)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sb == "bsbg15f") {
            ggplot(data = Students_Belonging, aes(bsbg15f)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sb == "bsbg15g") {
            ggplot(data = Students_Belonging, aes(bsbg15g)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Like_Math") {
          if (input$item_lm == "NA") {
            ggplot(data = Like_Math, aes(bsdgslm)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17a") {
            ggplot(data = Like_Math, aes(bsbm17a)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17b") {
            ggplot(data = Like_Math, aes(bsbm17b)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17c") {
            ggplot(data = Like_Math, aes(bsbm17c)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17d") {
            ggplot(data = Like_Math, aes(bsbm17d)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17e") {
            ggplot(data = Like_Math, aes(bsbm17e)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17f") {
            ggplot(data = Like_Math, aes(bsbm17f)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17g") {
            ggplot(data = Like_Math, aes(bsbm17g)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17h") {
            ggplot(data = Like_Math, aes(bsbm17h)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17i") {
            ggplot(data = Like_Math, aes(bsbm17i)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Plausible_Value1") {
          ggplot(data = Plausible_Value1, aes(bsmmat01)) +
            geom_histogram() +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Plausible_Value2") {
          ggplot(data = Plausible_Value2, aes(bsmmat02)) +
            geom_histogram() +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Plausible_Value3") {
          ggplot(data = Plausible_Value3, aes(bsmmat03)) +
            geom_histogram() +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Plausible_Value4") {
          ggplot(data = Plausible_Value4, aes(bsmmat04)) +
            geom_histogram() +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Plausible_Value5") {
          ggplot(data = Plausible_Value5, aes(bsmmat05)) +
            geom_histogram() +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Benchmark1") {
          ggplot(data = Benchmark1, aes(bsmibm01)) +
            geom_bar(stat = "count") +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Benchmark2") {
          ggplot(data = Benchmark2, aes(bsmibm02)) +
            geom_bar(stat = "count") +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Benchmark3") {
          ggplot(data = Benchmark3, aes(bsmibm03)) +
            geom_bar(stat = "count") +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Benchmark4") {
          ggplot(data = Benchmark4, aes(bsmibm04)) +
            geom_bar(stat = "count") +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Benchmark5") {
          ggplot(data = Benchmark5, aes(bsmibm05)) +
            geom_bar(stat = "count") +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "TooLowAch") {
          ggplot(data = TooLowAch, aes(bsdmlowp)) +
            geom_bar(stat = "count") +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "HomeEdResources") {
          if (input$item_her == "NA") {
            ggplot(data = HomeEdResources, aes(bsdgher)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_her == "bsbg04") {
            ggplot(data = HomeEdResources, aes(bsbg04)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_her == "bsbg06d") {
            ggplot(data = HomeEdResources, aes(bsbg06d)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_her == "bsbg06e") {
            ggplot(data = HomeEdResources, aes(bsbg06e)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_her == "bsbg07a") {
            ggplot(data = HomeEdResources, aes(bsbg07a)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group))) +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_her == "bsbg07b") {
            ggplot(data = HomeEdResources, aes(bsbg07b)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group))) +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_her == "bsdg06s") {
            ggplot(data = HomeEdResources, aes(bsdg06s)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group))) +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_her == "bsdgedup") {
            ggplot(data = HomeEdResources, aes(bsdgedup)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group))) +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        }
      }
    }
    else if (input$display == "Proportion") {
      if (input$group == "NA") {
        if (input$var == "School_Discipline") {
          if (input$item_disc == "NA") {
            ggplot(data = School_Discipline, aes_string("bcdgdas", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15a") {
            ggplot(data = School_Discipline, aes_string("bcbg15a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15b") {
            ggplot(data = School_Discipline, aes_string("bcbg15b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15c") {
            ggplot(data = School_Discipline, aes_string("bcbg15c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15d") {
            ggplot(data = School_Discipline, aes_string("bcbg15d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15e") {
            ggplot(data = School_Discipline, aes_string("bcbg15e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15f") {
            ggplot(data = School_Discipline, aes_string("bcbg15f", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15g") {
            ggplot(data = School_Discipline, aes_string("bcbg15g", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15h") {
            ggplot(data = School_Discipline, aes_string("bcbg15h", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15i") {
            ggplot(data = School_Discipline, aes_string("bcbg15i", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15j") {
            ggplot(data = School_Discipline, aes_string("bcbg15j", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_disc == "bcbg15k") {
            ggplot(data = School_Discipline, aes_string("bcbg15k", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } 
        } else if (input$var == "School_AcadSuc") {
          if (input$item_sas == "NA") {
            ggplot(data = School_AcadSuc, aes_string("bcdgeas", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14a") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14b") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14c") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14d") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) 
          } else if (input$item_sas == "bcbg14e") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14f") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14f", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) 
          } else if (input$item_sas == "bcbg14g") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14g", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14h") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14h", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14i") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14i", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14j") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14j", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14k") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14k", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14l") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14l", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sas == "bcbg14m") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14m", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } 
        } else if (input$var == "School_Resources") {
          if (input$item_res == "NA") {
            ggplot(data = School_Resources, aes_string("bcdgmrs", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_res == "bcbg13aa") {
            ggplot(data = School_Resources, aes_string("bcbg13aa", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13ab") {
            ggplot(data = School_Resources, aes_string("bcbg13ab", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13ac") {
            ggplot(data = School_Resources, aes_string("bcbg13ac", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13ad") {
            ggplot(data = School_Resources, aes_string("bcbg13ad", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13ae") {
            ggplot(data = School_Resources, aes_string("bcbg13ae", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13af") {
            ggplot(data = School_Resources, aes_string("bcbg13af", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13ag") {
            ggplot(data = School_Resources, aes_string("bcbg13ag", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") 
          } else if (input$item_res == "bcbg13ah") {
            ggplot(data = School_Resources, aes_string("bcbg13ah", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13ai") {
            ggplot(data = School_Resources, aes_string("bcbg13ai", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13ba") {
            ggplot(data = School_Resources, aes_string("bcbg13ba", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13bb") {
            ggplot(data = School_Resources, aes_string("bcbg13bb", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13bc") {
            ggplot(data = School_Resources, aes_string("bcbg13bc", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13bd") {
            ggplot(data = School_Resources, aes_string("bcbg13bd", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_res == "bcbg13be") {
            ggplot(data = School_Resources, aes_string("bcbg13be", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } 
        } else if (input$var == "School_Demographics") {
          if (input$item_sd == "bcbg03a") {
            ggplot(data = School_Demographics, aes_string("bcbg03a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcdg03") {
            ggplot(data = School_Demographics, aes_string("bcdg03", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg03b") {
            ggplot(data = School_Demographics, aes_string("bcbg03b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg04") {
            ggplot(data = School_Demographics, aes_string("bcbg04", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg05b") {
            ggplot(data = School_Demographics, aes_string("bcbg05b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg06a") {
            ggplot(data = School_Demographics, aes_string("bcbg06a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg06b") {
            ggplot(data = School_Demographics, aes_string("bcbg06b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg07a") {
            ggplot(data = School_Demographics, aes_string("bcbg07a", y = "..prop..", group = 1)) +
              geom_histogram(bins = 16) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg07b") {
            ggplot(data = School_Demographics, aes_string("bcbg07b", y = "..prop..", group = 1)) +
              geom_histogram(bins = 16) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg07c") {
            ggplot(data = School_Demographics, aes_string("bcbg07c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg08a") {
            ggplot(data = School_Demographics, aes_string("bcbg08a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg08b") {
            ggplot(data = School_Demographics, aes_string("bcbg08b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg09a") {
            ggplot(data = School_Demographics, aes_string("bcbg09a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg10") {
            ggplot(data = School_Demographics, aes_string("bcbg10", y = "..prop..", group = 1)) +
              geom_histogram(bins = 22) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg12") {
            ggplot(data = School_Demographics, aes_string("bcbg12", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg16a") {
            ggplot(data = School_Demographics, aes_string("bcbg16a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg17a") {
            ggplot(data = School_Demographics, aes_string("bcbg17a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg18a") {
            ggplot(data = School_Demographics, aes_string("bcbg18a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg18b") {
            ggplot(data = School_Demographics, aes_string("bcbg18b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg19") {
            ggplot(data = School_Demographics, aes_string("bcbg19", y = "..prop..", group = 1)) +
              geom_histogram(bins = 20) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg20") {
            ggplot(data = School_Demographics, aes_string("bcbg20", y = "..prop..", group = 1)) +
              geom_histogram(bins = 30) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "Teachers_PD") {
          if (input$item_pd == "btbm25.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm25.math)), aes_string("btbm25.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_pd == "btbm24a.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24a.math)), aes_string("btbm24a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_pd == "btbm24b.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24b.math)), aes_string("btbm24b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_pd == "btbm24c.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24c.math)), aes_string("btbm24c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_pd == "btbm24d.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24d.math)), aes_string("btbm24d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_pd == "btbm24e.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24e.math)), aes_string("btbm24e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_pd == "btbm24f.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24f.math)), aes_string("btbm24f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_pd == "btbm24g.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24g.math)), aes_string("btbm24g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } 
        } else if (input$var == "Teachers_Tests") {
          if (input$item_test == "btbm23a.math") {
            ggplot(data = subset(Teachers_Tests, !is.na(btbm23a.math)), aes_string("btbm23a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_test == "btbm23b.math") {
            ggplot(data = subset(Teachers_Tests, !is.na(btbm23b.math)), aes_string("btbm23b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_test == "btbm23c.math") {
            ggplot(data = subset(Teachers_Tests, !is.na(btbm23c.math)), aes_string("btbm23c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Teachers_Hwk") {
          if (input$item_hwk == "btbm22a.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22a.math)), aes_string("btbm22a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_hwk == "btbm22b.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22b.math)), aes_string("btbm22b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_hwk == "btbm22ca.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22ca.math)), aes_string("btbm22ca.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_hwk == "btbm22cb.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22cb.math)), aes_string("btbm22cb.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_hwk == "btbm22cc.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22cc.math)), aes_string("btbm22cc.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_hwk == "btbm22cd.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22cd.math)), aes_string("btbm22cd.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_hwk == "btbm22ce.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22ce.math)), aes_string("btbm22ce.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } 
        } else if (input$var == "Teachers_Pedagogy") {
          if (input$item_ped == "btbm16.math") {
            ggplot(data = Teachers_Pedagogy, aes_string("btbm16.math", y = "..prop..", group = 1)) +
              geom_histogram(bins = 18)
          } else if (input$item_ped == "btbm18a.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18a.math)), aes_string("btbm18a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18b.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18b.math)), aes_string("btbm18b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18c.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18c.math)), aes_string("btbm18c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18d.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18d.math)), aes_string("btbm18d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18e.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18e.math)), aes_string("btbm18e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18f.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18f.math)), aes_string("btbm18f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18g.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18g.math)), aes_string("btbm18g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18h.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18h.math)), aes_string("btbm18h.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18i.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18i.math)), aes_string("btbm18i.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_ped == "btbm18j.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18j.math)), aes_string("btbm18j.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } 
        } else if (input$var == "Teachers_Confidence") {
          if (input$item_conf == "btbm17a.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17a.math)), aes_string("btbm17a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_conf == "btbm17b.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17b.math)), aes_string("btbm17b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_conf == "btbm17c.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17c.math)), aes_string("btbm17c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_conf == "btbm17d.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17d.math)), aes_string("btbm17d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_conf == "btbm17e.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17e.math)), aes_string("btbm17e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_conf == "btbm17f.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17f.math)), aes_string("btbm17f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_conf == "btbm17g.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17g.math)), aes_string("btbm17g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_conf == "btbm17h.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17h.math)), aes_string("btbm17h.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_conf == "btbm17i.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17i.math)), aes_string("btbm17i.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } 
        } else if (input$var == "Teachers_MathMajors") {
          if (input$item_tmm == "btdm05.math") {
            ggplot(data = subset(Teachers_MathMajors, !is.na(btdm05.math)), aes_string("btdm05.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tmm == "btbg05a.math") {
            ggplot(data = subset(Teachers_MathMajors, !is.na(btbg05a.math)), aes_string("btbg05a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tmm == "btbg05f.math") {
            ggplot(data = subset(Teachers_MathMajors, !is.na(btbg05f.math)), aes_string("btbg05f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Teachers_StudentLimits") {
          if (input$item_tsl == "NA") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btdglsn.math)), aes_string("btdglsn.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tsl == "btbg15a.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15a.math)), aes_string("btbg15a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tsl == "btbg15b.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15b.math)), aes_string("btbg15b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tsl == "btbg15c.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15c.math)), aes_string("btbg15c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tsl == "btbg15d.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15d.math)), aes_string("btbg15d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tsl == "btbg15e.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15e.math)), aes_string("btbg15e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tsl == "btbg15f.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15f.math)), aes_string("btbg15f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tsl == "btbg15g.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15g.math)), aes_string("btbg15g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Teachers_Challenges") {
          if (input$item_tch == "NA") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btdgcft.math)), aes_string("btdgcft.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tch == "btbg11a.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11a.math)), aes_string("btbg11a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tch == "btbg11b.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11b.math)), aes_string("btbg11b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tch == "btbg11c.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11c.math)), aes_string("btbg11c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tch == "btbg11d.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11d.math)), aes_string("btbg11d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tch == "btbg11e.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11e.math)), aes_string("btbg11e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tch == "btbg11f.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11f.math)), aes_string("btbg11f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tch == "btbg11g.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11g.math)), aes_string("btbg11g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tch == "btbg11h.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11h.math)), aes_string("btbg11h.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Teachers_Satisfied") {
          if (input$item_sat == "NA") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btdgtjs.math)), aes_string("btdgtjs.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sat == "btbg10a.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10a.math)), aes_string("btbg10a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sat == "btbg10b.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10b.math)), aes_string("btbg10b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sat == "btbg10c.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10c.math)), aes_string("btbg10c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sat == "btbg10d.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10d.math)), aes_string("btbg10d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sat == "btbg10e.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10e.math)), aes_string("btbg10e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sat == "btbg10f.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10f.math)), aes_string("btbg10f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sat == "btbg10g.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10g.math)), aes_string("btbg10g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Teachers_CondRes") {
          if (input$item_tcr == "NA") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btdgscr.math)), aes_string("btdgscr.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tcr == "btbg08a.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08a.math)), aes_string("btbg08a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tcr == "btbg08b.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08b.math)), aes_string("btbg08b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tcr == "btbg08c.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08c.math)), aes_string("btbg08c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tcr == "btbg08d.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08d.math)), aes_string("btbg08d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tcr == "btbg08e.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08e.math)), aes_string("btbg08e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tcr == "btbg08f.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08f.math)), aes_string("btbg08f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tcr == "btbg08g.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08g.math)), aes_string("btbg08g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Teachers_SafeSchools") {
          if (input$item_tss == "NA") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btdgsos.math)), aes_string("btdgsos.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tss == "btbg07a.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07a.math)), aes_string("btbg07a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tss == "btbg07b.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07b.math)), aes_string("btbg07b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tss == "btbg07c.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07c.math)), aes_string("btbg07c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tss == "btbg07d.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07d.math)), aes_string("btbg07d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tss == "btbg07e.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07e.math)), aes_string("btbg07e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tss == "btbg07f.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07f.math)), aes_string("btbg07f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tss == "btbg07g.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07g.math)), aes_string("btbg07g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_tss == "btbg07h.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07h.math)), aes_string("btbg07h.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Teachers_AcadSuc") {
          if (input$item_tac == "NA") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btdgeas.math)), aes_string("btdgeas.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06a.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06a.math)), aes_string("btbg06a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06b.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06b.math)), aes_string("btbg06b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06c.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06c.math)), aes_string("btbg06c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06d.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06d.math)), aes_string("btbg06d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06e.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06e.math)), aes_string("btbg06e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06f.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06f.math)), aes_string("btbg06f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06g.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06g.math)), aes_string("btbg06g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06h.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06h.math)), aes_string("btbg06h.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06i.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06i.math)), aes_string("btbg06i.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06j.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06j.math)), aes_string("btbg06j.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06k.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06k.math)), aes_string("btbg06k.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06l.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06l.math)), aes_string("btbg06l.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06m.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06m.math)), aes_string("btbg06m.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_tac == "btbg06o.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06o.math)), aes_string("btbg06o.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          }
        } else if (input$var == "Teachers_Demographics") {
          if (input$item_td == "btbg01.math") {
            ggplot(data = Teachers_Demographics, aes_string("btbg01.math", y = "..prop..", group = 1)) +
              geom_histogram(na.rm = TRUE, bins = 22)
          } else if (input$item_td == "btbg02.math") {
            ggplot(data = subset(Teachers_Demographics, !is.na(btbg02.math)), aes_string("btbg02.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_td == "btbg03.math") {
            ggplot(data = subset(Teachers_Demographics, !is.na(btbg03.math)), aes_string("btbg03.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count", na.rm = TRUE) +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_td == "btbg12.math") {
            ggplot(data = Teachers_Demographics, aes_string("btbg12.math", y = "..prop..", group = 1)) +
              geom_histogram(bins = 14)
          } else if (input$item_td == "btbg13.math") {
            ggplot(data = Teachers_Demographics, aes_string("btbg13.math", y = "..prop..", group = 1)) +
              geom_histogram(bins = 18)
          }
        } else if (input$var == "Weekly_Hwk") {
          if (input$item_wh == "bsdmwkhw") {
            ggplot(data = Weekly_Hwk, aes_string("bsdmwkhw", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_wh == "bsbm25aa") {
            ggplot(data = Weekly_Hwk, aes_string("bsbm25aa", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_wh == "bsbm25ba") {
            ggplot(data = Weekly_Hwk, aes_string("bsbm25ba", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Students_ValueMath") {
          if (input$item_vm == "NA") {
            ggplot(data = Students_ValueMath, aes_string("bsdgsvm", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20a") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20b") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20c") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20d") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20e") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20f") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20f", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20g") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20g", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20h") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20h", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_vm == "bsbm20i") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20i", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Students_Confidence") {
          if (input$item_sc == "NA") {
            ggplot(data = Students_Confidence, aes_string("bsdgscm", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19a") {
            ggplot(data = Students_Confidence, aes_string("bsbm19a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19b") {
            ggplot(data = Students_Confidence, aes_string("bsbm19b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19c") {
            ggplot(data = Students_Confidence, aes_string("bsbm19c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19d") {
            ggplot(data = Students_Confidence, aes_string("bsbm19d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19e") {
            ggplot(data = Students_Confidence, aes_string("bsbm19e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19f") {
            ggplot(data = Students_Confidence, aes_string("bsbm19f", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19g") {
            ggplot(data = Students_Confidence, aes_string("bsbm19g", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19h") {
            ggplot(data = Students_Confidence, aes_string("bsbm19h", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sc == "bsbm19i") {
            ggplot(data = Students_Confidence, aes_string("bsbm19i", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Engaging_Teaching") {
          if (input$item_et == "NA") {
            ggplot(data = Engaging_Teaching, aes_string("bsdgeml", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18a") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18b") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18c") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18d") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18e") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18f") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18f", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18g") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18g", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18h") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18h", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18i") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18i", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_et == "bsbm18j") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18j", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Students_Bullying") {
          if (input$item_bull == "NA") {
            ggplot(data = Students_Bullying, aes_string("bsdgsb", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16a") {
            ggplot(data = Students_Bullying, aes_string("bsbg16a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16b") {
            ggplot(data = Students_Bullying, aes_string("bsbg16b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16c") {
            ggplot(data = Students_Bullying, aes_string("bsbg16c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16d") {
            ggplot(data = Students_Bullying, aes_string("bsbg16d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16e") {
            ggplot(data = Students_Bullying, aes_string("bsbg16e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16f") {
            ggplot(data = Students_Bullying, aes_string("bsbg16f", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16g") {
            ggplot(data = Students_Bullying, aes_string("bsbg16g", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16h") {
            ggplot(data = Students_Bullying, aes_string("bsbg16h", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_bull == "bsbg16i") {
            ggplot(data = Students_Bullying, aes_string("bsbg16i", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Students_Belonging") {
          if (input$item_sb == "NA") {
            ggplot(data = Students_Belonging, aes_string("bsdgssb", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sb == "bsbg15a") {
            ggplot(data = Students_Belonging, aes_string("bsbg15a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sb == "bsbg15b") {
            ggplot(data = Students_Belonging, aes_string("bsbg15b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sb == "bsbg15c") {
            ggplot(data = Students_Belonging, aes_string("bsbg15c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sb == "bsbg15d") {
            ggplot(data = Students_Belonging, aes_string("bsbg15d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sb == "bsbg15e") {
            ggplot(data = Students_Belonging, aes_string("bsbg15e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sb == "bsbg15f") {
            ggplot(data = Students_Belonging, aes_string("bsbg15f", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_sb == "bsbg15g") {
            ggplot(data = Students_Belonging, aes_string("bsbg15g", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Like_Math") { 
          if (input$item_lm == "NA") {
            ggplot(data = Like_Math, aes_string("bsdgslm", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17a") {
            ggplot(data = Like_Math, aes_string("bsbm17a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17b") {
            ggplot(data = Like_Math, aes_string("bsbm17b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17c") {
            ggplot(data = Like_Math, aes_string("bsbm17c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17d") {
            ggplot(data = Like_Math, aes_string("bsbm17d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17e") {
            ggplot(data = Like_Math, aes_string("bsbm17e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17f") {
            ggplot(data = Like_Math, aes_string("bsbm17f", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17g") {
            ggplot(data = Like_Math, aes_string("bsbm17g", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17h") {
            ggplot(data = Like_Math, aes_string("bsbm17h", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_lm == "bsbm17i") {
            ggplot(data = Like_Math, aes_string("bsbm17i", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        } else if (input$var == "Plausible_Value1") {
          ggplot(data = Plausible_Value1, aes_string("bsmmat01", y = "..prop..", group = 1)) +
            geom_histogram()
        } else if (input$var == "Plausible_Value2") {
          ggplot(data = Plausible_Value2, aes_string("bsmmat02", y = "..prop..", group = 1)) +
            geom_histogram()
        } else if (input$var == "Plausible_Value3") {
          ggplot(data = Plausible_Value3, aes_string("bsmmat03", y = "..prop..", group = 1)) +
            geom_histogram()
        } else if (input$var == "Plausible_Value4") {
          ggplot(data = Plausible_Value4, aes_string("bsmmat04", y = "..prop..", group = 1)) +
            geom_histogram()
        } else if (input$var == "Plausible_Value5") {
          ggplot(data = Plausible_Value5, aes_string("bsmmat05", y = "..prop..", group = 1)) +
            geom_histogram()
        } else if (input$var == "Benchmark1") {
          ggplot(data = Benchmark1, aes_string("bsmibm01", y = "..prop..", group = 1)) +
            geom_bar(stat = "count")
        } else if (input$var == "Benchmark2") {
          ggplot(data = Benchmark2, aes_string("bsmibm02", y = "..prop..", group = 1)) +
            geom_bar(stat = "count")
        } else if (input$var == "Benchmark3") {
          ggplot(data = Benchmark3, aes_string("bsmibm03", y = "..prop..", group = 1)) +
            geom_bar(stat = "count")
        } else if (input$var == "Benchmark4") {
          ggplot(data = Benchmark4, aes_string("bsmibm04", y = "..prop..", group = 1)) +
            geom_bar(stat = "count")
        } else if (input$var == "Benchmark5") {
          ggplot(data = Benchmark5, aes_string("bsmibm05", y = "..prop..", group = 1)) +
            geom_bar(stat = "count")
        } else if (input$var == "TooLowAch") {
          ggplot(data = TooLowAch, aes_string("bsdmlowp", y = "..prop..", group = 1)) +
            geom_bar(stat = "count")
        } else if (input$var == "HomeEdResources") {
          if (input$item_her == "NA") {
            ggplot(data = HomeEdResources, aes_string("bsdgher", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_her == "bsbg04") {
            ggplot(data = HomeEdResources, aes_string("bsbg04", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_her == "bsbg06d") {
            ggplot(data = HomeEdResources, aes_string("bsbg06d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_her == "bsbg06e") {
            ggplot(data = HomeEdResources, aes_string("bsbg06e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count")
          } else if (input$item_her == "bsbg07a") {
            ggplot(data = HomeEdResources, aes_string("bsbg07a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_her == "bsbg07b") {
            ggplot(data = HomeEdResources, aes_string("bsbg07b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_her == "bsdg06s") {
            ggplot(data = HomeEdResources, aes_string("bsdg06s", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_her == "bsdgedup") {
            ggplot(data = HomeEdResources, aes_string("bsdgedup", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        }
      }
      else {
        if (input$var == "School_Discipline") {
          if (input$item_disc == "NA") {
            ggplot(data = School_Discipline, aes_string("bcdgdas", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15a") {
            ggplot(data = School_Discipline, aes_string("bcbg15a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15b") {
            ggplot(data = School_Discipline, aes_string("bcbg15b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15c") {
            ggplot(data = School_Discipline, aes_string("bcbg15c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15d") {
            ggplot(data = School_Discipline, aes_string("bcbg15d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15e") {
            ggplot(data = School_Discipline, aes_string("bcbg15e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15f") {
            ggplot(data = School_Discipline, aes_string("bcbg15f", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15g") {
            ggplot(data = School_Discipline, aes_string("bcbg15g", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15h") {
            ggplot(data = School_Discipline, aes_string("bcbg15h", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15i") {
            ggplot(data = School_Discipline, aes_string("bcbg15i", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15j") {
            ggplot(data = School_Discipline, aes_string("bcbg15j", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_disc == "bcbg15k") {
            ggplot(data = School_Discipline, aes_string("bcbg15k", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "School_AcadSuc") {
          if (input$item_sas == "NA") {
            ggplot(data = School_AcadSuc, aes_string("bcdgeas", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14a") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14b") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14c") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14d") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14e") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14f") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14f", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14g") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14g", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14h") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14h", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14i") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14i", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14j") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14j", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14k") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14k", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14l") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14l", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sas == "bcbg14m") {
            ggplot(data = School_AcadSuc, aes_string("bcbg14m", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "School_Resources") {
          if (input$item_res == "NA") {
            ggplot(data = School_Resources, aes_string("bcdgmrs", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13aa") {
            ggplot(data = School_Resources, aes_string("bcbg13aa", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13ab") {
            ggplot(data = School_Resources, aes_string("bcbg13ab", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13ac") {
            ggplot(data = School_Resources, aes_string("bcbg13ac", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13ad") {
            ggplot(data = School_Resources, aes_string("bcbg13ad", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13ae") {
            ggplot(data = School_Resources, aes_string("bcbg13ae", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13af") {
            ggplot(data = School_Resources, aes_string("bcbg13af", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13ag") {
            ggplot(data = School_Resources, aes_string("bcbg13ag", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13ah") {
            ggplot(data = School_Resources, aes_string("bcbg13ah", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13ai") {
            ggplot(data = School_Resources, aes_string("bcbg13ai", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13ba") {
            ggplot(data = School_Resources, aes_string("bcbg13ba", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13bb") {
            ggplot(data = School_Resources, aes_string("bcbg13bb", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13bc") {
            ggplot(data = School_Resources, aes_string("bcbg13bc", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13bd") {
            ggplot(data = School_Resources, aes_string("bcbg13bd", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_res == "bcbg13be") {
            ggplot(data = School_Resources, aes_string("bcbg13be", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "School_Demographics") {
          if (input$item_sd == "bcbg03a") {
            ggplot(data = School_Demographics, aes_string("bcbg03a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcdg03") {
            ggplot(data = School_Demographics, aes_string("bcdg03", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg03b") {
            ggplot(data = School_Demographics, aes_string("bcbg03b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg04") {
            ggplot(data = School_Demographics, aes_string("bcbg04", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg05b") {
            ggplot(data = School_Demographics, aes_string("bcbg05b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg06a") {
            ggplot(data = School_Demographics, aes_string("bcbg06a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg06b") {
            ggplot(data = School_Demographics, aes_string("bcbg06b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg07a") {
            ggplot(data = School_Demographics, aes_string("bcbg07a", y = "..prop..", group = 1)) +
              geom_histogram(bins = 16) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg07b") {
            ggplot(data = School_Demographics, aes_string("bcbg07b", y = "..prop..", group = 1)) +
              geom_histogram(bins = 16) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg07c") {
            ggplot(data = School_Demographics, aes_string("bcbg07c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg08a") {
            ggplot(data = School_Demographics, aes_string("bcbg08a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg08b") {
            ggplot(data = School_Demographics, aes_string("bcbg08b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg09a") {
            ggplot(data = School_Demographics, aes_string("bcbg09a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg10") {
            ggplot(data = School_Demographics, aes_string("bcbg10", y = "..prop..", group = 1)) +
              geom_histogram(bins = 22) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg12") {
            ggplot(data = School_Demographics, aes_string("bcbg12", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg16a") {
            ggplot(data = School_Demographics, aes_string("bcbg16a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg17a") {
            ggplot(data = School_Demographics, aes_string("bcbg17a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg18a") {
            ggplot(data = School_Demographics, aes_string("bcbg18a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg18b") {
            ggplot(data = School_Demographics, aes_string("bcbg18b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg19") {
            ggplot(data = School_Demographics, aes_string("bcbg19", y = "..prop..", group = 1)) +
              geom_histogram(bins = 20) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sd == "bcbg20") {
            ggplot(data = School_Demographics, aes_string("bcbg20", y = "..prop..", group = 1)) +
              geom_histogram(bins = 30) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "Teachers_PD") {
          if (input$item_pd == "btbm25.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm25.math)), aes_string("btbm25.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_pd == "btbm24a.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24a.math)), aes_string("btbm24a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_pd == "btbm24b.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24b.math)), aes_string("btbm24b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_pd == "btbm24c.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24c.math)), aes_string("btbm24c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_pd == "btbm24d.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24d.math)), aes_string("btbm24d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_pd == "btbm24e.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24e.math)), aes_string("btbm24e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_pd == "btbm24f.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24f.math)), aes_string("btbm24f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_pd == "btbm24g.math") {
            ggplot(data = subset(Teachers_PD, !is.na(btbm24g.math)), aes_string("btbm24g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "Teachers_Tests") {
          if (input$item_test == "btbm23a.math") {
            ggplot(data = subset(Teachers_Tests, !is.na(btbm23a.math)), aes_string("btbm23a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_test == "btbm23b.math") {
            ggplot(data = subset(Teachers_Tests, !is.na(btbm23b.math)), aes_string("btbm23b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_test == "btbm23c.math") {
            ggplot(data = subset(Teachers_Tests, !is.na(btbm23c.math)), aes_string("btbm23c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Teachers_Hwk") {
          if (input$item_hwk == "btbm22a.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22a.math)), aes_string("btbm22a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_hwk == "btbm22b.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22b.math)), aes_string("btbm22b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_hwk == "btbm22ca.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22ca.math)), aes_string("btbm22ca.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_hwk == "btbm22cb.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22cb.math)), aes_string("btbm22cb.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_hwk == "btbm22cc.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22cc.math)), aes_string("btbm22cc.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_hwk == "btbm22cd.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22cd.math)), aes_string("btbm22cd.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_hwk == "btbm22ce.math") {
            ggplot(data = subset(Teachers_Hwk, !is.na(btbm22ce.math)), aes_string("btbm22ce.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "Teachers_Pedagogy") {
          if (input$item_ped == "btbm16.math") {
            ggplot(data = Teachers_Pedagogy, aes_string("btbm16.math", y = "..prop..", group = 1)) +
              geom_histogram(bins = 18) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18a.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18a.math)), aes_string("btbm18a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18b.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18b.math)), aes_string("btbm18b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18c.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18c.math)), aes_string("btbm18c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18d.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18d.math)), aes_string("btbm18d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18e.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18e.math)), aes_string("btbm18e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18f.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18f.math)), aes_string("btbm18f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18g.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18g.math)), aes_string("btbm18g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18h.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18h.math)), aes_string("btbm18h.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18i.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18i.math)), aes_string("btbm18i.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_ped == "btbm18j.math") {
            ggplot(data = subset(Teachers_Pedagogy, !is.na(btbm18j.math)), aes_string("btbm18j.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "Teachers_Confidence") {
          if (input$item_conf == "btbm17a.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17a.math)), aes_string("btbm17a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_conf == "btbm17b.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17b.math)), aes_string("btbm17b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_conf == "btbm17c.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17c.math)), aes_string("btbm17c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_conf == "btbm17d.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17d.math)), aes_string("btbm17d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_conf == "btbm17e.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17e.math)), aes_string("btbm17e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_conf == "btbm17f.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17f.math)), aes_string("btbm17f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_conf == "btbm17g.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17g.math)), aes_string("btbm17g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_conf == "btbm17h.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17h.math)), aes_string("btbm17h.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_conf == "btbm17i.math") {
            ggplot(data = subset(Teachers_Confidence, !is.na(btbm17i.math)), aes_string("btbm17i.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } 
        } else if (input$var == "Teachers_MathMajors") {
          if (input$item_tmm == "btdm05.math") {
            ggplot(data = subset(Teachers_MathMajors, !is.na(btdm05.math)), aes_string("btdm05.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tmm == "btbg05a.math") {
            ggplot(data = subset(Teachers_MathMajors, !is.na(btbg05a.math)), aes_string("btbg05a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tmm == "btbg05f.math") {
            ggplot(data = subset(Teachers_MathMajors, !is.na(btbg05f.math)), aes_string("btbg05f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Teachers_StudentLimits") {
          if (input$item_tsl == "NA") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btdglsn.math)), aes_string("btdglsn.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tsl == "btbg15a.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15a.math)), aes_string("btbg15a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tsl == "btbg15b.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15b.math)), aes_string("btbg15b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tsl == "btbg15c.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15c.math)), aes_string("btbg15c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tsl == "btbg15d.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15d.math)), aes_string("btbg15d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tsl == "btbg15e.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15e.math)), aes_string("btbg15e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tsl == "btbg15f.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15f.math)), aes_string("btbg15f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tsl == "btbg15g.math") {
            ggplot(data = subset(Teachers_StudentLimits, !is.na(btbg15g.math)), aes_string("btbg15g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Teachers_Challenges") {
          if (input$item_tch == "NA") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btdgcft.math)), aes_string("btdgcft.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tch == "btbg11a.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11a.math)), aes_string("btbg11a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tch == "btbg11b.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11b.math)), aes_string("btbg11b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tch == "btbg11c.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11c.math)), aes_string("btbg11c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tch == "btbg11d.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11d.math)), aes_string("btbg11d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tch == "btbg11e.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11e.math)), aes_string("btbg11e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tch == "btbg11f.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11f.math)), aes_string("btbg11f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tch == "btbg11g.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11g.math)), aes_string("btbg11g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tch == "btbg11h.math") {
            ggplot(data = subset(Teachers_Challenges, !is.na(btbg11h.math)), aes_string("btbg11h.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Teachers_Satisfied") {
          if (input$item_sat == "NA") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btdgtjs.math)), aes_string("btdgtjs.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sat == "btbg10a.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10a.math)), aes_string("btbg10a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sat == "btbg10b.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10b.math)), aes_string("btbg10b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sat == "btbg10c.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10c.math)), aes_string("btbg10c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sat == "btbg10d.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10d.math)), aes_string("btbg10d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sat == "btbg10e.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10e.math)), aes_string("btbg10e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sat == "btbg10f.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10f.math)), aes_string("btbg10f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sat == "btbg10g.math") {
            ggplot(data = subset(Teachers_Satisfied, !is.na(btbg10g.math)), aes_string("btbg10g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Teachers_CondRes") {
          if (input$item_tcr == "NA") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btdgscr.math)), aes_string("btdgscr.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tcr == "btbg08a.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08a.math)), aes_string("btbg08a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tcr == "btbg08b.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08b.math)), aes_string("btbg08b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tcr == "btbg08c.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08c.math)), aes_string("btbg08c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tcr == "btbg08d.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08d.math)), aes_string("btbg08d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tcr == "btbg08e.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08e.math)), aes_string("btbg08e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tcr == "btbg08f.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08f.math)), aes_string("btbg08f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tcr == "btbg08g.math") {
            ggplot(data = subset(Teachers_CondRes, !is.na(btbg08g.math)), aes_string("btbg08g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Teachers_SafeSchools") {
          if (input$item_tss == "NA") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btdgsos.math)), aes_string("btdgsos.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tss == "btbg07a.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07a.math)), aes_string("btbg07a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tss == "btbg07b.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07b.math)), aes_string("btbg07b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tss == "btbg07c.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07c.math)), aes_string("btbg07c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tss == "btbg07d.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07d.math)), aes_string("btbg07d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tss == "btbg07e.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07e.math)), aes_string("btbg07e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tss == "btbg07f.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07f.math)), aes_string("btbg07f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tss == "btbg07g.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07g.math)), aes_string("btbg07g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tss == "btbg07h.math") {
            ggplot(data = subset(Teachers_SafeSchools, !is.na(btbg07h.math)), aes_string("btbg07h.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Teachers_AcadSuc") {
          if (input$item_tac == "NA") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btdgeas.math)), aes_string("btdgeas.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06a.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06a.math)), aes_string("btbg06a.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06b.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06b.math)), aes_string("btbg06b.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06c.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06c.math)), aes_string("btbg06c.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06d.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06d.math)), aes_string("btbg06d.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06e.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06e.math)), aes_string("btbg06e.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06f.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06f.math)), aes_string("btbg06f.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06g.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06g.math)), aes_string("btbg06g.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06h.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06h.math)), aes_string("btbg06h.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06i.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06i.math)), aes_string("btbg06i.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06j.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06j.math)), aes_string("btbg06j.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06k.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06k.math)), aes_string("btbg06k.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06l.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06l.math)), aes_string("btbg06l.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06m.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06m.math)), aes_string("btbg06m.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_tac == "btbg06o.math") {
            ggplot(data = subset(Teachers_AcadSuc, !is.na(btbg06o.math)), aes_string("btbg06o.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Teachers_Demographics") {
          if (input$item_td == "btbg01.math") {
            ggplot(data = Teachers_Demographics, aes_string("btbg01.math", y = "..prop..", group = 1)) +
              geom_histogram(na.rm = TRUE, bins = 22) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_td == "btbg02.math") {
            ggplot(data = subset(Teachers_Demographics, !is.na(btbg02.math)), aes_string("btbg02.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_td == "btbg03.math") {
            ggplot(data = subset(Teachers_Demographics, !is.na(btbg03.math)), aes_string("btbg03.math", y = "..prop..", group = 1)) +
              geom_bar(stat = "count", na.rm = TRUE) +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_td == "btbg12.math") {
            ggplot(data = Teachers_Demographics, aes_string("btbg12.math", y = "..prop..", group = 1)) +
              geom_histogram(bins = 14) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_td == "btbg13.math") {
            ggplot(data = Teachers_Demographics, aes_string("btbg13.math", y = "..prop..", group = 1)) +
              geom_histogram(bins = 18) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Weekly_Hwk") {
          if (input$item_wh == "bsdmwkhw") {
            ggplot(data = Weekly_Hwk, aes_string("bsdmwkhw", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_wh == "bsbm25aa") {
            ggplot(data = Weekly_Hwk, aes_string("bsbm25aa", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_wh == "bsbm25ba") {
            ggplot(data = Weekly_Hwk, aes_string("bsbm25ba", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Students_ValueMath") {
          if (input$item_vm == "NA") {
            ggplot(data = Students_ValueMath, aes_string("bsdgsvm", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20a") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20b") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20c") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20d") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20e") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20f") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20f", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20g") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20g", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20h") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20h", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_vm == "bsbm20i") {
            ggplot(data = Students_ValueMath, aes_string("bsbm20i", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Students_Confidence") {
          if (input$item_sc == "NA") {
            ggplot(data = Students_Confidence, aes_string("bsdgscm", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19a") {
            ggplot(data = Students_Confidence, aes_string("bsbm19a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19b") {
            ggplot(data = Students_Confidence, aes_string("bsbm19b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19c") {
            ggplot(data = Students_Confidence, aes_string("bsbm19c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19d") {
            ggplot(data = Students_Confidence, aes_string("bsbm19d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19e") {
            ggplot(data = Students_Confidence, aes_string("bsbm19e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19f") {
            ggplot(data = Students_Confidence, aes_string("bsbm19f", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19g") {
            ggplot(data = Students_Confidence, aes_string("bsbm19g", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19h") {
            ggplot(data = Students_Confidence, aes_string("bsbm19h", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sc == "bsbm19i") {
            ggplot(data = Students_Confidence, aes_string("bsbm19i", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Engaging_Teaching") {
          if (input$item_et == "NA") {
            ggplot(data = Engaging_Teaching, aes_string("bsdgeml", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18a") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18b") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18c") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18d") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18e") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18f") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18f", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18g") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18g", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18h") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18h", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18i") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18i", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_et == "bsbm18j") {
            ggplot(data = Engaging_Teaching, aes_string("bsbm18j", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Students_Bullying") {
          if (input$item_bull == "NA") {
            ggplot(data = Students_Bullying, aes_string("bsdgsb", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16a") {
            ggplot(data = Students_Bullying, aes_string("bsbg16a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16b") {
            ggplot(data = Students_Bullying, aes_string("bsbg16b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16c") {
            ggplot(data = Students_Bullying, aes_string("bsbg16c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16d") {
            ggplot(data = Students_Bullying, aes_string("bsbg16d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16e") {
            ggplot(data = Students_Bullying, aes_string("bsbg16e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16f") {
            ggplot(data = Students_Bullying, aes_string("bsbg16f", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16g") {
            ggplot(data = Students_Bullying, aes_string("bsbg16g", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16h") {
            ggplot(data = Students_Bullying, aes_string("bsbg16h", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_bull == "bsbg16i") {
            ggplot(data = Students_Bullying, aes_string("bsbg16i", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Students_Belonging") {
          if (input$item_sb == "NA") {
            ggplot(data = Students_Belonging, aes_string("bsdgssb", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sb == "bsbg15a") {
            ggplot(data = Students_Belonging, aes_string("bsbg15a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sb == "bsbg15b") {
            ggplot(data = Students_Belonging, aes_string("bsbg15b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sb == "bsbg15c") {
            ggplot(data = Students_Belonging, aes_string("bsbg15c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sb == "bsbg15d") {
            ggplot(data = Students_Belonging, aes_string("bsbg15d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sb == "bsbg15e") {
            ggplot(data = Students_Belonging, aes_string("bsbg15e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sb == "bsbg15f") {
            ggplot(data = Students_Belonging, aes_string("bsbg15f", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_sb == "bsbg15g") {
            ggplot(data = Students_Belonging, aes_string("bsbg15g", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Like_Math") {
          if (input$item_lm == "NA") {
            ggplot(data = Like_Math, aes_string("bsdgslm", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17a") {
            ggplot(data = Like_Math, aes_string("bsbm17a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17b") {
            ggplot(data = Like_Math, aes_string("bsbm17b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17c") {
            ggplot(data = Like_Math, aes_string("bsbm17c", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17d") {
            ggplot(data = Like_Math, aes_string("bsbm17d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17e") {
            ggplot(data = Like_Math, aes_string("bsbm17e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17f") {
            ggplot(data = Like_Math, aes_string("bsbm17f", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17g") {
            ggplot(data = Like_Math, aes_string("bsbm17g", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17h") {
            ggplot(data = Like_Math, aes_string("bsbm17h", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_lm == "bsbm17i") {
            ggplot(data = Like_Math, aes_string("bsbm17i", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
              facet_wrap(. ~ eval(as.name(input$group)))
          }
        } else if (input$var == "Plausible_Value1") {
          ggplot(data = Plausible_Value1, aes_string("bsmmat01", y = "..prop..", group = 1)) +
            geom_histogram() +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Plausible_Value2") {
          ggplot(data = Plausible_Value2, aes_string("bsmmat02", y = "..prop..", group = 1)) +
            geom_histogram() +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Plausible_Value3") {
          ggplot(data = Plausible_Value3, aes_string("bsmmat03", y = "..prop..", group = 1)) +
            geom_histogram() +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Plausible_Value4") {
          ggplot(data = Plausible_Value4, aes_string("bsmmat04", y = "..prop..", group = 1)) +
            geom_histogram() +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Plausible_Value5") {
          ggplot(data = Plausible_Value5, aes_string("bsmmat05", y = "..prop..", group = 1)) +
            geom_histogram() +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Benchmark1") {
          ggplot(data = Benchmark1, aes_string("bsmibm01", y = "..prop..", group = 1)) +
            geom_bar(stat = "count") +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Benchmark2") {
          ggplot(data = Benchmark2, aes_string("bsmibm02", y = "..prop..", group = 1)) +
            geom_bar(stat = "count") +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Benchmark3") {
          ggplot(data = Benchmark3, aes_string("bsmibm03", y = "..prop..", group = 1)) +
            geom_bar(stat = "count") +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Benchmark4") {
          ggplot(data = Benchmark4, aes_string("bsmibm04", y = "..prop..", group = 1)) +
            geom_bar(stat = "count") +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Benchmark5") {
          ggplot(data = Benchmark5, aes_string("bsmibm05", y = "..prop..", group = 1)) +
            geom_bar(stat = "count") +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "TooLowAch") {
          ggplot(data = TooLowAch, aes_string("bsdmlowp", y = "..prop..", group = 1)) +
            geom_bar(stat = "count") +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "HomeEdResources") {
          if (input$item_her == "NA") {
            ggplot(data = HomeEdResources, aes_string("bsdgher", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_her == "bsbg04") {
            ggplot(data = HomeEdResources, aes_string("bsbg04", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_her == "bsbg06d") {
            ggplot(data = HomeEdResources, aes_string("bsbg06d", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_her == "bsbg06e") {
            ggplot(data = HomeEdResources, aes_string("bsbg06e", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group)))
          } else if (input$item_her == "bsbg07a") {
            ggplot(data = HomeEdResources, aes_string("bsbg07a", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group))) +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_her == "bsbg07b") {
            ggplot(data = HomeEdResources, aes_string("bsbg07b", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group))) +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_her == "bsdg06s") {
            ggplot(data = HomeEdResources, aes_string("bsdg06s", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group))) +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          } else if (input$item_her == "bsdgedup") {
            ggplot(data = HomeEdResources, aes_string("bsdgedup", y = "..prop..", group = 1)) +
              geom_bar(stat = "count") +
              facet_wrap(. ~ eval(as.name(input$group))) +
              scale_x_discrete(guide = guide_axis(n.dodge = 2))
          }
        }
      }
    }
  })
}