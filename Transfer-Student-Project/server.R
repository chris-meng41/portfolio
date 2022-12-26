# Load data and libraries
library(ggplot2)
library(readxl)

transfer_data <- read_excel("transfer_data.xlsx")

Favorite_Subject <- transfer_data %>% 
  select(HS_FavSubject_Math:HS_FavSubject_Other, `2Year_4Year`, Transfer_Year) %>% 
  pivot_longer(cols = HS_FavSubject_Math:HS_FavSubject_Other, 
               names_to = c("Label"),
               values_to = c("Favorite Subject")) %>% 
  na.omit()

HS_Social <- transfer_data %>% 
  select(HS_Social_Friends:HS_Social_Outside, `2Year_4Year`, Transfer_Year) %>% 
  pivot_longer(cols = HS_Social_Friends:HS_Social_Outside, 
               names_to = c("Label"),
               values_to = c("HS Social")) %>% 
  na.omit()

Household_Structure <- transfer_data %>%
  select(Household_Mother:Household_Other, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = Household_Mother:Household_Other,
               names_to = c("Label"),
               values_to = c("Household Structure")) %>%
  na.omit()

Family_College <- transfer_data %>%
  select(FamilyCollege_None:FamilyCollege_Other, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = FamilyCollege_None:FamilyCollege_Other,
               names_to = c("Label"),
               values_to = c("Family College Education")) %>%
  na.omit()

Family_EdView <- transfer_data %>%
  select(Family_EdView_Individual:Family_EdView_ForOthers, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = Family_EdView_Individual:Family_EdView_ForOthers,
               names_to = c("Label"),
               values_to = c("Family View of Education")) %>%
  na.omit()

preTransfer_Resources <- transfer_data %>%
  select(preTransfer_Resources_Advisor:preTransfer_Resources_Other, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = preTransfer_Resources_Advisor:preTransfer_Resources_Other,
               names_to = c("Label"),
               values_to = c("Resources Available at Pre-Transfer Institution")) %>%
  na.omit()

preTransfer_ResourcesUsed <- transfer_data %>%
  select(preTransfer_ResourceUse_Advisor:preTransfer_ResourceUse_Other, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = preTransfer_ResourceUse_Advisor:preTransfer_ResourceUse_Other,
               names_to = c("Label"),
               values_to = c("Resources Used at Pre-Transfer Institution")) %>%
  na.omit()

Transfer_HearSwat <- transfer_data %>%
  select(Transfer_HearSwat_Online:Transfer_HearSwat_Other, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = Transfer_HearSwat_Online:Transfer_HearSwat_Other,
               names_to = c("Label"),
               values_to = c("How did transfers hear about Swat?")) %>%
  na.omit()

Transfer_WantSwat <- transfer_data %>%
  select(Transfer_WantSwat_Credits:Transfer_WantSwat_Other, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = Transfer_WantSwat_Credits:Transfer_WantSwat_Other,
               names_to = c("Label"),
               values_to = c("What made transfers want to go to Swat?")) %>%
  na.omit()

Expected_Major <- transfer_data %>%
  select(MajorExp_AncientHistory:MajorExp_Special, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = MajorExp_AncientHistory:MajorExp_Special,
               names_to = c("Label"),
               values_to = c("Expected Major")) %>%
  na.omit()

Actual_Major <- transfer_data %>%
  select(MajorEnd_AncientHistory:MajorEnd_Special, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = MajorEnd_AncientHistory:MajorEnd_Special,
               names_to = c("Label"),
               values_to = c("Actual Major")) %>%
  na.omit()

Change_Major_Reasons <- transfer_data %>%
  select(MajorChange_Faculty:MajorChange_Other, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = MajorChange_Faculty:MajorChange_Other,
               names_to = c("Label"),
               values_to = c("Reasons Major Changed")) %>%
  na.omit()

TransitionHelp_Swat <- transfer_data %>%
  select(SwatTransition_NewFriends:SwatTransition_Other, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = SwatTransition_NewFriends:SwatTransition_Other,
               names_to = c("Label"),
               values_to = c("Help Transition to Swat")) %>%
  na.omit()

Swat_CareerCenter_Use <- transfer_data %>%
  select(Swat_CareerCenter_SummerIntern:Swat_CareerCenter_Other, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = Swat_CareerCenter_SummerIntern:Swat_CareerCenter_Other,
               names_to = c("Label"),
               values_to = c("How Use Swat Career Center")) %>%
  na.omit()

Extracurriculars <- transfer_data %>% 
  select(Extra_Sports_HS:Extra_Other_Swat, `2Year_4Year`, Transfer_Year) %>% 
  pivot_longer(cols = Extra_Sports_HS:Extra_Other_Swat, 
               names_to = c("grp", ".value"), 
               names_sep = "_") %>%
  select(-grp) %>%
  pivot_longer(cols = Sports:Other,
               names_to = "Activity",
               values_to = "When") %>%
  na.omit()

Swat_Hopes <- transfer_data %>%
  select(Swat_Hopes_Professional:Swat_Hopes_Other, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = Swat_Hopes_Professional:Swat_Hopes_Other,
               names_to = c("Label"),
               values_to = c("Swat Hopes")) %>%
  na.omit()

Swat_TransferActivities_Hope <- transfer_data %>%
  select(Swat_Transfer_Activities_Orientation:Swat_Transfer_Activities_Other, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = Swat_Transfer_Activities_Orientation:Swat_Transfer_Activities_Other,
               names_to = c("Label"),
               values_to = c("Desired Swat Transfer Activities")) %>%
  na.omit()

WorkMoney_HS <- transfer_data %>%
  select(WorkMoney_HS_Personal:WorkMoney_HS_Other, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = WorkMoney_HS_Personal:WorkMoney_HS_Other,
               names_to = c("Label"),
               values_to = c("How do you spend HS work money?")) %>%
  na.omit()

WorkMoney_preSwat <- transfer_data %>%
  select(WorkMoney_preSwat_Personal:WorkMoney_preSwat_Other, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = WorkMoney_preSwat_Personal:WorkMoney_preSwat_Other,
               names_to = c("Label"),
               values_to = c("How do you spend pre-Swat work money?")) %>%
  na.omit()

WorkMoney_Swat <- transfer_data %>%
  select(WorkMoney_Swat_Personal:WorkMoney_Swat_Other, `2Year_4Year`, Transfer_Year) %>%
  pivot_longer(cols = WorkMoney_Swat_Personal:WorkMoney_Swat_Other,
               names_to = c("Label"),
               values_to = c("How do you spend Swat work money?")) %>%
  na.omit()

bar_values <- transfer_data %>%
  select(Grad_Year,
         HS_Population,
         HS_Location,
         HS_Grades,
         HS_AP,
         HS_AP_Num,
         HS_IB,
         HS_IB_yn,
         HS_Counselor,
         HS_Counselor_Reg,
         HS_Counselor_Acad,
         HS_Counselor_SEL,
         HS_Counselor_UniChoose,
         HS_Counselor_UniApp,
         HS_Satisfy_Acad,
         HS_Satisfy_Social,
         HS_Satisfy_Extra,
         HS_Satisfy_AcadSupport,
         HS_Satisfy_SEL,
         HS_Satisfy_Counsel,
         HS_PostPlans,
         FamilyExpect_PostHS,
         Family_Relationship,
         Family_RelationshipChange,
         Education_Mother,
         Education_Father,
         Family_EdSupport_HwHelp,
         Family_EdSupport_HwExpect,
         Family_EdSupport_Conference,
         Family_EdSupport_Events,
         Family_EdSupport_GradeExpect,
         Family_EdSupport_Other,
         Work_HS,
         Work_preSwat,
         Work_Swat,
         Transfer_Year,
         preTransfer_SchoolType,
         preTransferChoice_FA,
         preTransferChoice_Credits,
         preTransferChoice_Courses,
         preTransferChoice_Cost,
         preTransferChoice_Family,
         preTransferChoice_Friends,
         preTransferChoice_Job,
         preTransferChoice_Reputation,
         preTransferChoice_Immigration,
         preTransferChoice_Acad,
         preTransferChoice_Info,
         preTransferChoice_FTFaculty,
         preTransferChoice_Vocation,
         preTransferChoice_Expect,
         preTransferChoice_Other,
         preTransferAdvice_Counselor,
         preTransferAdvice_Friends,
         preTransferAdvice_Parents,
         preTransferAdvice_Siblings,
         preTransferAdvice_Family,
         preTransferAdvice_Other,
         preTransfer_FTy1,
         preTransfer_FTy2,
         Transfer_WhenDecide,
         TransferReason_Always,
         TransferReason_Associates,
         TransferReason_AcadFocus,
         TransferReason_Financial,
         TransferReason_AcadSupport,
         TransferReason_Acad,
         TransferReason_Social,
         TransferReason_Wellbeing,
         TransferReason_Family,
         TransferReason_Athletics,
         TransferReason_Extra,
         TransferReason_Friends,
         TransferReason_Other,
         AcadPrep_Analysis,
         AcadPrep_Discussion,
         AcadPrep_LiteraryAnalysis,
         AcadPrep_ScienceLab,
         AcadPrep_SciencePaper,
         AcadPrep_SSPaper,
         AcadPrep_ResearchPaper,
         AcadPrep_Textbooks,
         AcadPrep_Literature,
         AcadPrep_ScientificMaterials,
         AcadPrep_LiteraryAnalysisR,
         AcadPrep_SSTheory,
         AcadPrep_SourcesUseful,
         AcadPrep_IndepResearch,
         AcadPrep_LitReview,
         AcadPrep_ResearchStatement,
         AcadPrep_AnnotatedBib,
         AcadPrep_FindSources,
         AcadPrep_Primary_Analysis,
         AcadPrep_Primary_Discussion,
         AcadPrep_Primary_LiteraryAnalysis,
         AcadPrep_Primary_ScienceLab,
         AcadPrep_Primary_SciencePaper,
         AcadPrep_Primary_SSPaper,
         AcadPrep_Primary_ResearchPaper,
         AcadPrep_Primary_Textbooks,
         AcadPrep_Primary_Literature,
         AcadPrep_Primary_ScientificMaterials,
         AcadPrep_Primary_LiteraryAnalysisR,
         AcadPrep_Primary_SSTheory,
         AcadPrep_Primary_SourcesUseful,
         AcadPrep_Primary_IndepResearch,
         AcadPrep_Primary_LitReview,
         AcadPrep_Primary_ResearchStatement,
         AcadPrep_Primary_AnnotatedBib,
         AcadPrep_Primary_FindSources,
         TransferAdvice_Counselor,
         TransferAdvice_Teacher,
         TransferAdvice_Friends,
         TransferAdvice_Parents,
         TransferAdvice_Siblings,
         TransferAdvice_Family,
         TransferAdvice_Other,
         TransferAdjust_Acad,
         TransferAdjust_Residential,
         TransferAdjust_Friends,
         TransferAdjust_AwayHome,
         TransferAdjust_Extra,
         TransferAdjust_AcadSupport,
         TransferAdjust_SELSupport,
         TransferAdjust_Other,
         SwatSupport_Advisor,
         SwatSupport_MajorFaculty,
         SwatSupport_NonMajorFaculty,
         SwatSupport_Dean,
         SwatSupport_WA,
         SwatSupport_Tutor,
         SwatSupport_Friends,
         SwatSupport_Other,
         SwatSEL_Family,
         SwatSEL_Friends,
         SwatSEL_Faculty,
         SwatSEL_CAPS,
         SwatSEL_Counsel,
         SwatSEL_Religion,
         SwatSEL_Other,
         SwatFaculty_Schedule,
         SwatFaculty_CourseProf,
         SwatFaculty_AcadSupport,
         SwatFaculty_NonAcadSupport,
         SwatFaculty_AcadWork,
         SwatFaculty_SEL,
         Swat_CareerCenter,
         Swat_TransferAccept,
         HS_Friends_Similar_Econ,
         HS_Friends_Similar_REI,
         HS_Friends_Similar_Location,
         HS_Friends_Similar_Acad,
         HS_Friends_Similar_Extra,
         HS_Friends_Similar_Docu,
         preSwat_Friends_Similar_Econ,
         preSwat_Friends_Similar_REI,
         preSwat_Friends_Similar_Location,
         preSwat_Friends_Similar_Acad,
         preSwat_Friends_Similar_Extra,
         preSwat_Friends_Similar_Transfer,
         preSwat_Friends_Similar_School,
         preSwat_Friends_Similar_Docu,
         Swat_Friends_Similar_Econ,
         Swat_Friends_Similar_REI,
         Swat_Friends_Similar_Location,
         Swat_Friends_Similar_Acad,
         Swat_Friends_Similar_Extra,
         Swat_Friends_Similar_Transfer,
         Swat_Friends_Similar_School,
         Swat_Friends_Similar_Docu,
         Outside_Friends_Similar_Econ,
         Outside_Friends_Similar_REI,
         Outside_Friends_Similar_Location,
         Outside_Friends_Similar_Acad,
         Outside_Friends_Similar_Extra,
         Outside_Friends_Similar_Transfer,
         Outside_Friends_Similar_School,
         Outside_Friends_Similar_Docu,
         Swat_MeetExp_Professional,
         Swat_MeetExp_CredentialJob,
         Swat_MeetExp_CollegeExp,
         Swat_MeetExp_FutureCareer,
         Swat_MeetExp_WorldUnderstanding,
         Swat_MeetExp_Friends,
         Swat_MeetExp_GradSchool,
         Swat_MeetExp_Other,
         Belong_Swat,
         Belong_Major,
         Belong_Friends,
         Belong_OrgsClubs,
         Current_Work,
         Current_GradSchool,
         Current_GradSchool_Degree,
         Transfer_Swat_Impact,
         Transfer_Grad_Impact,
         Transfer_WorkGradSchool_Impact,
         Swat_TransferOrg)

histogram_values <- transfer_data %>%
  select(HS_College_Perc,
         HS_GPA)

# Create server
server <- function(input, output) {
  output$plot <- renderPlot({
    req(input$var, input$group, input$display)
    if (input$display == "Count") {
      if (input$group == "NA") {
        if (input$var %in% colnames(bar_values)) {
          ggplot(data = transfer_data , aes_string(input$var)) +
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var %in% colnames(histogram_values)) {
          ggplot(data = transfer_data, aes_string(input$var)) +
            geom_histogram()
        } else if (input$var == "Favorite_Subject") {
          ggplot(data = Favorite_Subject, aes_string("`Favorite Subject`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "HS_Social") {
          ggplot(data = HS_Social, aes_string("`HS Social`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "Household_Structure") {
          ggplot(data = Household_Structure, aes_string("`Household Structure`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "Family_College") {
          ggplot(data = Family_College, aes_string("`Family College Education`")) + 
            geom_bar(stat = "count")
        } else if (input$var == "Family_EdView") {
          ggplot(data = Family_EdView, aes_string("`Family View of Education`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "preTransfer_Resources") {
          ggplot(data = preTransfer_Resources, aes_string("`Resources Available at Pre-Transfer Institution`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "preTransfer_ResourcesUsed") {
          ggplot(data = preTransfer_ResourcesUsed, aes_string("`Resources Used at Pre-Transfer Institution`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "Transfer_HearSwat") {
          ggplot(data = Transfer_HearSwat, aes_string("`How did transfers hear about Swat?`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 3))
        } else if (input$var == "Transfer_WantSwat") {
          ggplot(data = Transfer_WantSwat, aes_string("`What made transfers want to go to Swat?`")) + 
            geom_bar(stat = "count") +
            theme(axis.text.x = element_text(angle = 90))
        } else if (input$var == "Expected_Major") {
          ggplot(data = Expected_Major, aes_string("`Expected Major`")) + 
            geom_bar(stat = "count") +
            theme(axis.text.x = element_text(angle = 90))
        } else if (input$var == "Actual_Major") {
          ggplot(data = Actual_Major, aes_string("`Actual Major`")) + 
            geom_bar(stat = "count") +
            theme(axis.text.x = element_text(angle = 90))
        } else if (input$var == "Change_Major_Reasons") {
          ggplot(data = Change_Major_Reasons, aes_string("`Reasons Major Changed`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 3))
        } else if (input$var == "TransitionHelp_Swat") {
          ggplot(data = TransitionHelp_Swat, aes_string("`Help Transition to Swat`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "Swat_CareerCenter_Use") {
          ggplot(data = Swat_CareerCenter_Use, aes_string("`How Use Swat Career Center`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "Extracurriculars") {
          ggplot(data = Extracurriculars, aes_string("`Activity`", fill = "When")) + 
            geom_bar(position = "dodge", stat = "count") +
            theme(axis.text.x = element_text(angle = 90))
        } else if (input$var == "Swat_Hopes") {
          ggplot(data = Swat_Hopes, aes_string("`Swat Hopes`")) + 
            geom_bar(stat = "count") +
            theme(axis.text.x = element_text(angle = 90))
        } else if (input$var == "Swat_TransferActivities_Hope") {
          ggplot(data = Swat_TransferActivities_Hope, aes_string("`Desired Swat Transfer Activities`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 3))
        } else if (input$var == "WorkMoney_HS") {
          ggplot(data = WorkMoney_HS, aes_string("`How do you spend HS work money?`")) +
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "WorkMoney_preSwat") {
          ggplot(data = WorkMoney_preSwat, aes_string("`How do you spend pre-Swat work money?`")) +
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "WorkMoney_Swat") {
          ggplot(data = WorkMoney_Swat, aes_string("`How do you spend Swat work money?`")) +
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        }
      }
      else {
        if (input$var %in% colnames(bar_values)) {
          ggplot(data = transfer_data , aes_string(input$var)) +
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var %in% colnames(histogram_values)) {
          ggplot(data = transfer_data, aes_string(input$var)) +
            geom_histogram() +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Favorite_Subject") {
          ggplot(data = Favorite_Subject, aes_string("`Favorite Subject`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "HS_Social") {
          ggplot(data = HS_Social, aes_string("`HS Social`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Household_Structure") {
          ggplot(data = Household_Structure, aes_string("`Household Structure`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Family_College") {
          ggplot(data = Family_College, aes_string("`Family College Education`")) + 
            geom_bar(stat = "count") +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Family_EdView") {
          ggplot(data = Family_EdView, aes_string("`Family View of Education`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "preTransfer_Resources") {
          ggplot(data = preTransfer_Resources, aes_string("`Resources Available at Pre-Transfer Institution`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "preTransfer_ResourcesUsed") {
          ggplot(data = preTransfer_ResourcesUsed, aes_string("`Resources Used at Pre-Transfer Institution`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Transfer_HearSwat") {
          ggplot(data = Transfer_HearSwat, aes_string("`How did transfers hear about Swat?`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Transfer_WantSwat") {
          ggplot(data = Transfer_WantSwat, aes_string("`What made transfers want to go to Swat?`")) + 
            geom_bar(stat = "count") +
            theme(axis.text.x = element_text(angle = 90)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Expected_Major") {
          ggplot(data = Expected_Major, aes_string("`Expected Major`")) + 
            geom_bar(stat = "count") +
            theme(axis.text.x = element_text(angle = 90)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Actual_Major") {
          ggplot(data = Actual_Major, aes_string("`Actual Major`")) + 
            geom_bar(stat = "count") +
            theme(axis.text.x = element_text(angle = 90)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Change_Major_Reasons") {
          ggplot(data = Change_Major_Reasons, aes_string("`Reasons Major Changed`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "TransitionHelp_Swat") {
          ggplot(data = TransitionHelp_Swat, aes_string("`Help Transition to Swat`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Swat_CareerCenter_Use") {
          ggplot(data = Swat_CareerCenter_Use, aes_string("`How Use Swat Career Center`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Extracurriculars") {
          ggplot(data = Extracurriculars, aes_string("`Activity`", fill = "When")) + 
            geom_bar(position = "dodge", stat = "count") +
            theme(axis.text.x = element_text(angle = 90)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Swat_Hopes") {
          ggplot(data = Swat_Hopes, aes_string("`Swat Hopes`")) + 
            geom_bar(stat = "count") +
            theme(axis.text.x = element_text(angle = 90)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Swat_TransferActivities_Hope") {
          ggplot(data = Swat_TransferActivities_Hope, aes_string("`Desired Swat Transfer Activities`")) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "WorkMoney_HS") {
          ggplot(data = WorkMoney_HS, aes_string("`How do you spend HS work money?`")) +
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "WorkMoney_preSwat") {
          ggplot(data = WorkMoney_preSwat, aes_string("`How do you spend pre-Swat work money?`")) +
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "WorkMoney_Swat") {
          ggplot(data = WorkMoney_Swat, aes_string("`How do you spend Swat work money?`")) +
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        }
      }  
    }
    else if (input$display == "Proportion") {
      if (input$group == "NA") {
        if (input$var %in% colnames(bar_values)) {
          ggplot(data = transfer_data , aes_string(input$var, y = "..prop..", group = 1)) +
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var %in% colnames(histogram_values)) {
          ggplot(data = transfer_data, aes_string(input$var, y = "..prop..", group = 1)) +
            geom_histogram()
        } else if (input$var == "Favorite_Subject") {
          ggplot(data = Favorite_Subject, aes_string("`Favorite Subject`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "HS_Social") {
          ggplot(data = HS_Social, aes_string("`HS Social`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "Household_Structure") {
          ggplot(data = Household_Structure, aes_string("`Household Structure`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "Family_College") {
          ggplot(data = Family_College, aes_string("`Family College Education`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count")
        } else if (input$var == "Family_EdView") {
          ggplot(data = Family_EdView, aes_string("`Family View of Education`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "preTransfer_Resources") {
          ggplot(data = preTransfer_Resources, aes_string("`Resources Available at Pre-Transfer Institution`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "preTransfer_ResourcesUsed") {
          ggplot(data = preTransfer_ResourcesUsed, aes_string("`Resources Used at Pre-Transfer Institution`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "Transfer_HearSwat") {
          ggplot(data = Transfer_HearSwat, aes_string("`How did transfers hear about Swat?`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 3))
        } else if (input$var == "Transfer_WantSwat") {
          ggplot(data = Transfer_WantSwat, aes_string("`What made transfers want to go to Swat?`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            theme(axis.text.x = element_text(angle = 90))
        } else if (input$var == "Expected_Major") {
          ggplot(data = Expected_Major, aes_string("`Expected Major`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            theme(axis.text.x = element_text(angle = 90))
        } else if (input$var == "Actual_Major") {
          ggplot(data = Actual_Major, aes_string("`Actual Major`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            theme(axis.text.x = element_text(angle = 90))
        } else if (input$var == "Change_Major_Reasons") {
          ggplot(data = Change_Major_Reasons, aes_string("`Reasons Major Changed`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 3))
        } else if (input$var == "TransitionHelp_Swat") {
          ggplot(data = TransitionHelp_Swat, aes_string("`Help Transition to Swat`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "Swat_CareerCenter_Use") {
          ggplot(data = Swat_CareerCenter_Use, aes_string("`How Use Swat Career Center`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "Extracurriculars") {
          ggplot(data = Extracurriculars, aes_string("`Activity`", fill = "When", y = "..prop..", group = 1)) + 
            geom_bar(position = "dodge", stat = "count") +
            theme(axis.text.x = element_text(angle = 90))
        } else if (input$var == "Swat_Hopes") {
          ggplot(data = Swat_Hopes, aes_string("`Swat Hopes`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            theme(axis.text.x = element_text(angle = 90))
        } else if (input$var == "Swat_TransferActivities_Hope") {
          ggplot(data = Swat_TransferActivities_Hope, aes_string("`Desired Swat Transfer Activities`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 3))
        } else if (input$var == "WorkMoney_HS") {
          ggplot(data = WorkMoney_HS, aes_string("`How do you spend HS work money?`", y = "..prop..", group = 1)) +
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "WorkMoney_preSwat") {
          ggplot(data = WorkMoney_preSwat, aes_string("`How do you spend pre-Swat work money?`", y = "..prop..", group = 1)) +
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        } else if (input$var == "WorkMoney_Swat") {
          ggplot(data = WorkMoney_Swat, aes_string("`How do you spend Swat work money?`", y = "..prop..", group = 1)) +
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2))
        }
      }
      else {
        if (input$var %in% colnames(bar_values)) {
          ggplot(data = transfer_data , aes_string(input$var, y = "..prop..", group = 1)) +
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var %in% colnames(histogram_values)) {
          ggplot(data = transfer_data, aes_string(input$var, y = "..prop..", group = 1)) +
            geom_histogram() +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Favorite_Subject") {
          ggplot(data = Favorite_Subject, aes_string("`Favorite Subject`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "HS_Social") {
          ggplot(data = HS_Social, aes_string("`HS Social`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Household_Structure") {
          ggplot(data = Household_Structure, aes_string("`Household Structure`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Family_College") {
          ggplot(data = Family_College, aes_string("`Family College Education`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Family_EdView") {
          ggplot(data = Family_EdView, aes_string("`Family View of Education`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "preTransfer_Resources") {
          ggplot(data = preTransfer_Resources, aes_string("`Resources Available at Pre-Transfer Institution`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "preTransfer_ResourcesUsed") {
          ggplot(data = preTransfer_ResourcesUsed, aes_string("`Resources Used at Pre-Transfer Institution`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Transfer_HearSwat") {
          ggplot(data = Transfer_HearSwat, aes_string("`How did transfers hear about Swat?`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Transfer_WantSwat") {
          ggplot(data = Transfer_WantSwat, aes_string("`What made transfers want to go to Swat?`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            theme(axis.text.x = element_text(angle = 90)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Expected_Major") {
          ggplot(data = Expected_Major, aes_string("`Expected Major`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            theme(axis.text.x = element_text(angle = 90)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Actual_Major") {
          ggplot(data = Actual_Major, aes_string("`Actual Major`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            theme(axis.text.x = element_text(angle = 90)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Change_Major_Reasons") {
          ggplot(data = Change_Major_Reasons, aes_string("`Reasons Major Changed`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "TransitionHelp_Swat") {
          ggplot(data = TransitionHelp_Swat, aes_string("`Help Transition to Swat`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Swat_CareerCenter_Use") {
          ggplot(data = Swat_CareerCenter_Use, aes_string("`How Use Swat Career Center`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Extracurriculars") {
          ggplot(data = Extracurriculars, aes_string("`Activity`", fill = "When", y = "..prop..", group = 1)) + 
            geom_bar(position = "dodge", stat = "count") +
            theme(axis.text.x = element_text(angle = 90)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Swat_Hopes") {
          ggplot(data = Swat_Hopes, aes_string("`Swat Hopes`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            theme(axis.text.x = element_text(angle = 90)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "Swat_TransferActivities_Hope") {
          ggplot(data = Swat_TransferActivities_Hope, aes_string("`Desired Swat Transfer Activities`", y = "..prop..", group = 1)) + 
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "WorkMoney_HS") {
          ggplot(data = WorkMoney_HS, aes_string("`How do you spend HS work money?`", y = "..prop..", group = 1)) +
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "WorkMoney_preSwat") {
          ggplot(data = WorkMoney_preSwat, aes_string("`How do you spend pre-Swat work money?`", y = "..prop..", group = 1)) +
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        } else if (input$var == "WorkMoney_Swat") {
          ggplot(data = WorkMoney_Swat, aes_string("`How do you spend Swat work money?`", y = "..prop..", group = 1)) +
            geom_bar(stat = "count") +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            facet_wrap(. ~ eval(as.name(input$group)))
        }
      }  
    }
  })
  output$plotGY <- renderPlot({
    ggplot(data = transfer_data , aes_string("Grad_Year")) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotHSloc <- renderPlot({
    ggplot(data = transfer_data , aes_string("HS_Location", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotHSgrades <- renderPlot({
    ggplot(data = transfer_data , aes_string("HS_Grades", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ Transfer_Year)
  })
  output$plotHSAP <- renderPlot({
    ggplot(data = transfer_data , aes_string("HS_AP_Num", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ Transfer_Year)
  })
  output$plotHSsat <- renderPlot({
    ggplot(data = transfer_data , aes_string("HS_Satisfy_Counsel", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotHScsl <- renderPlot({
    ggplot(data = transfer_data , aes_string("HS_Counselor", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotHScslC <- renderPlot({
    ggplot(data = transfer_data , aes_string("HS_Counselor_UniChoose", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotHScslA <- renderPlot({
    ggplot(data = transfer_data , aes_string("HS_Counselor_UniApp", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotHSplans <- renderPlot({
    ggplot(data = transfer_data , aes_string("HS_PostPlans", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotFamExp <- renderPlot({
    ggplot(data = transfer_data , aes_string("FamilyExpect_PostHS", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotFamExp1 <- renderPlot({
    ggplot(data = transfer_data , aes_string("FamilyExpect_PostHS", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `Transfer_Year`)
  })
  output$plotFamEd <- renderPlot({
    ggplot(data = Family_College, aes_string("`Family College Education`", y = "..prop..", group = 1)) + 
      geom_bar(stat = "count") +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotWhen <- renderPlot({
    ggplot(data = transfer_data , aes_string("Transfer_WhenDecide", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotTranAdvT <- renderPlot({
    ggplot(data = transfer_data , aes_string("TransferAdvice_Teacher", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotTranAdvP <- renderPlot({
    ggplot(data = transfer_data , aes_string("TransferAdvice_Parents", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotHearSwat <- renderPlot({
    ggplot(data = Transfer_HearSwat, aes_string("`How did transfers hear about Swat?`", y = "..prop..", group = 1)) + 
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotHopeSwat <- renderPlot({
    ggplot(data = Swat_Hopes, aes_string("`Swat Hopes`")) + 
      geom_bar(stat = "count") +
      theme(axis.text.x = element_text(angle = 90)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotDeanSS <- renderPlot({
    ggplot(data = transfer_data , aes_string("SwatSupport_Dean", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotWASS <- renderPlot({
    ggplot(data = transfer_data , aes_string("SwatSupport_WA", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotHopeSwat <- renderPlot({
    ggplot(data = TransitionHelp_Swat, aes_string("`Help Transition to Swat`")) + 
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotExp <- renderPlot({
    ggplot(data = transfer_data , aes_string("Swat_MeetExp_CollegeExp", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotChg <- renderPlot({
    ggplot(data = Change_Major_Reasons, aes_string("`Reasons Major Changed`")) + 
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotTSI <- renderPlot({
    ggplot(data = transfer_data , aes_string("Transfer_Swat_Impact", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
  output$plotOrg <- renderPlot({
    ggplot(data = transfer_data , aes_string("Swat_TransferOrg", y = "..prop..", group = 1)) +
      geom_bar(stat = "count") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap(. ~ `2Year_4Year`)
  })
}

