# Load data
library(readxl)
library(tidyverse)

transfer_data <- read_excel("transfer_data.xlsx")

# Data Preprocessing
Favorite_Subject <- transfer_data %>% 
  select(HS_FavSubject_Math:HS_FavSubject_Other) %>% 
  pivot_longer(cols = HS_FavSubject_Math:HS_FavSubject_Other, 
               names_to = c("Label"),
               values_to = c("Favorite Subject")) %>% 
  select("Favorite Subject") %>% 
  na.omit()

HS_Social <- transfer_data %>% 
  select(HS_Social_Friends:HS_Social_Outside) %>% 
  pivot_longer(cols = HS_Social_Friends:HS_Social_Outside, 
               names_to = c("Label"),
               values_to = c("HS Social")) %>% 
  select("HS Social") %>% 
  na.omit()

Household_Structure <- transfer_data %>%
  select(Household_Mother:Household_Other) %>%
  pivot_longer(cols = Household_Mother:Household_Other,
               names_to = c("Label"),
               values_to = c("Household Structure")) %>%
  na.omit()

Family_College <- transfer_data %>%
  select(FamilyCollege_None:FamilyCollege_Other) %>%
  pivot_longer(cols = FamilyCollege_None:FamilyCollege_Other,
               names_to = c("Label"),
               values_to = c("Family College Education")) %>%
  na.omit()

Family_EdView <- transfer_data %>%
  select(Family_EdView_Individual:Family_EdView_ForOthers) %>%
  pivot_longer(cols = Family_EdView_Individual:Family_EdView_ForOthers,
               names_to = c("Label"),
               values_to = c("Family View of Education")) %>%
  na.omit()

preTransfer_Resources <- transfer_data %>%
  select(preTransfer_Resources_Advisor:preTransfer_Resources_Other) %>%
  pivot_longer(cols = preTransfer_Resources_Advisor:preTransfer_Resources_Other,
               names_to = c("Label"),
               values_to = c("Resources Available at Pre-Transfer Institution")) %>%
  na.omit()

preTransfer_ResourcesUsed <- transfer_data %>%
  select(preTransfer_ResourceUse_Advisor:preTransfer_ResourceUse_Other) %>%
  pivot_longer(cols = preTransfer_ResourceUse_Advisor:preTransfer_ResourceUse_Other,
               names_to = c("Label"),
               values_to = c("Resources Used at Pre-Transfer Institution")) %>%
  na.omit()

Transfer_HearSwat <- transfer_data %>%
  select(Transfer_HearSwat_Online:Transfer_HearSwat_Other) %>%
  pivot_longer(cols = Transfer_HearSwat_Online:Transfer_HearSwat_Other,
               names_to = c("Label"),
               values_to = c("How did transfers hear about Swat?")) %>%
  na.omit()

Transfer_WantSwat <- transfer_data %>%
  select(Transfer_WantSwat_Credits:Transfer_WantSwat_Other) %>%
  pivot_longer(cols = Transfer_WantSwat_Credits:Transfer_WantSwat_Other,
               names_to = c("Label"),
               values_to = c("What made transfers want to go to Swat?")) %>%
  na.omit()

Expected_Major <- transfer_data %>%
  select(MajorExp_AncientHistory:MajorExp_Special) %>%
  pivot_longer(cols = MajorExp_AncientHistory:MajorExp_Special,
               names_to = c("Label"),
               values_to = c("Expected Major")) %>%
  na.omit()

Actual_Major <- transfer_data %>%
  select(MajorEnd_AncientHistory:MajorEnd_Special) %>%
  pivot_longer(cols = MajorEnd_AncientHistory:MajorEnd_Special,
               names_to = c("Label"),
               values_to = c("Actual Major")) %>%
  na.omit()

Change_Major_Reasons <- transfer_data %>%
  select(MajorChange_Faculty:MajorChange_Other) %>%
  pivot_longer(cols = MajorChange_Faculty:MajorChange_Other,
               names_to = c("Label"),
               values_to = c("Reasons Major Changed")) %>%
  na.omit()

TransitionHelp_Swat <- transfer_data %>%
  select(SwatTransition_NewFriends:SwatTransition_Other) %>%
  pivot_longer(cols = SwatTransition_NewFriends:SwatTransition_Other,
               names_to = c("Label"),
               values_to = c("Help Transition to Swat")) %>%
  na.omit()

Swat_CareerCenter_Use <- transfer_data %>%
  select(Swat_CareerCenter_SummerIntern:Swat_CareerCenter_Other) %>%
  pivot_longer(cols = Swat_CareerCenter_SummerIntern:Swat_CareerCenter_Other,
               names_to = c("Label"),
               values_to = c("How Use Swat Career Center")) %>%
  na.omit()

Extracurriculars <- transfer_data %>% 
  select(Extra_Sports_HS:Extra_Other_Swat) %>% 
  pivot_longer(cols = Extra_Sports_HS:Extra_Other_Swat, 
               names_to = c("grp", ".value"), 
               names_sep = "_") %>%
  select(-grp) %>%
  pivot_longer(cols = Sports:Other,
               names_to = "Activity",
               values_to = "When") %>%
  na.omit()

Swat_Hopes <- transfer_data %>%
  select(Swat_Hopes_Professional:Swat_Hopes_Other) %>%
  pivot_longer(cols = Swat_Hopes_Professional:Swat_Hopes_Other,
               names_to = c("Label"),
               values_to = c("Swat Hopes")) %>%
  na.omit()

Swat_TransferActivities_Hope <- transfer_data %>%
  select(Swat_Transfer_Activities_Orientation:Swat_Transfer_Activities_Other) %>%
  pivot_longer(cols = Swat_Transfer_Activities_Orientation:Swat_Transfer_Activities_Other,
               names_to = c("Label"),
               values_to = c("Desired Swat Transfer Activities")) %>%
  na.omit()

WorkMoney_HS <- transfer_data %>%
  select(WorkMoney_HS_Personal:WorkMoney_HS_Other) %>%
  pivot_longer(cols = WorkMoney_HS_Personal:WorkMoney_HS_Other,
               names_to = c("Label"),
               values_to = c("How do you spend HS work money?")) %>%
  na.omit()

WorkMoney_preSwat <- transfer_data %>%
  select(WorkMoney_preSwat_Personal:WorkMoney_preSwat_Other) %>%
  pivot_longer(cols = WorkMoney_preSwat_Personal:WorkMoney_preSwat_Other,
               names_to = c("Label"),
               values_to = c("How do you spend pre-Swat work money?")) %>%
  na.omit()

WorkMoney_Swat <- transfer_data %>%
  select(WorkMoney_Swat_Personal:WorkMoney_Swat_Other) %>%
  pivot_longer(cols = WorkMoney_Swat_Personal:WorkMoney_Swat_Other,
               names_to = c("Label"),
               values_to = c("How do you spend Swat work money?")) %>%
  na.omit()

# Bar Charts
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

# Histograms
histogram_values <- transfer_data %>%
  select(HS_College_Perc,
         HS_GPA)

# Panels
## 1: Visualization
sidebar_content <- sidebarPanel(
  selectInput(
    "var",
    label = "Variable",
    choices = c(colnames(cbind(histogram_values, bar_values)), 
                "Favorite_Subject", 
                "HS_Social", 
                "Household_Structure", 
                "Family_College", 
                "Family_EdView", 
                "preTransfer_Resources", 
                "preTransfer_ResourcesUsed", 
                "Transfer_HearSwat", 
                "Transfer_WantSwat", 
                "Expected_Major", 
                "Actual_Major", 
                "Change_Major_Reasons", 
                "TransitionHelp_Swat", 
                "Swat_CareerCenter_Use", 
                "Extracurriculars", 
                "Swat_Hopes", 
                "Swat_TransferActivities_Hope",
                "WorkMoney_HS",
                "WorkMoney_preSwat",
                "WorkMoney_Swat"),
    selected = "Grad_Year"
    
  ),
  selectInput(
    "group",
    label = "Group By",
    choices = c("NA",
                "2Year_4Year",
                "Transfer_Year")
  ),
  selectInput(
    "display",
    label = "Display",
    choices = c("Count",
                "Proportion"),
    selected = "Proportion"
  )
)

main_content <- mainPanel(
  plotOutput("plot")
)

second_panel <- tabPanel(
  "Visualization",
  titlePanel("Transfer Students at Swarthmore"),
  p("Use the selector input below to choose which variable you would like to see."),
  sidebarLayout(
    sidebar_content, main_content
  )
)

## 2: Report
report_panel <- tabPanel(id = "top",
  "Report",
  
#  titlePanel("Introduction"),
  
#  img(src = "mariokart8.jpg", height = 400, width = 800),
#  br(), br(),
  fluidRow(
    actionButton("b100","DESCRIBING THE DATA",
                 onclick = "location.href='#DD';"),
    actionButton("b111","Graduation Year",
                 onclick = "location.href='#GY';"),
    actionButton("b112","HS Location",
                 onclick = "location.href='#HSL';"),
    actionButton("b113","HS Grades + APs",
                 onclick = "location.href='#AP';"),
    actionButton("b114","HS Counseling Satisfaction",
                 onclick = "location.href='#sat';"),
    actionButton("b115","Post-HS Plans",
                 onclick = "location.href='#pla';"),
    actionButton("b116","Family Expectations",
                 onclick = "location.href='#famexp';"),
    actionButton("b117","Family Education",
                 onclick = "location.href='#famed';"),
    actionButton("b118","Initial College Choice",
                 onclick = "location.href='#icc';")
  ),
  fluidRow(
    actionButton("b121","When Decide to Transfer",
                 onclick = "location.href='#when';"),
    actionButton("b122","Transfer Reason & Advice",
                 onclick = "location.href='#tra';"),
    actionButton("b123","How did you hear about Swat?",
                 onclick = "location.href='#hear';"),
    actionButton("b124","What did you hope for Swat?",
                 onclick = "location.href='#hope';"),
    actionButton("b125","Academic Preparation",
                 onclick = "location.href='#acad';"),
    actionButton("b126","Support at Swat",
                 onclick = "location.href='#ss';"),
    actionButton("b127","Transition Help at Swat",
                 onclick = "location.href='#help';"),
    actionButton("b128","Did Swat meet expectations?",
                 onclick = "location.href='#exp';"),
    actionButton("b129","Reasons for Changing Major",
                 onclick = "location.href='#chg';"),
    actionButton("b130","Belonging at Swat",
                 onclick = "location.href='#bel';"),
    actionButton("b131","Transfer Status Impact",
                 onclick = "location.href='#tsi';"),
    actionButton("b132","Want Transfer Org at Swat?",
                 onclick = "location.href='#org';")
  ),
  br(),
  fluidRow(
    actionButton("b141","TRANSFER STUDENT IMPACT",
                 onclick = "location.href='#TSI';"),
    actionButton("b142","Positives",
                 onclick = "location.href='#pos';"),
    actionButton("b143","Do I Belong?",
                 onclick = "location.href='#ie';"),
    actionButton("b144","Making Friends",
                 onclick = "location.href='#newold';"),
    actionButton("b145","Adjustment in Less Time",
                 onclick = "location.href='#behind';")
  ),
  br(),
  fluidRow(
    actionButton("b151","RECOMMENDATIONS",
                 onclick = "location.href='#rec';"),
    actionButton("b152","Housing",
                 onclick = "location.href='#house';"),
    actionButton("b153","Orientation",
                 onclick = "location.href='#ori';"),
    actionButton("b154","Support",
                 onclick = "location.href='#sup';"),
    actionButton("b155","Conclusion",
                 onclick = "location.href='#end';")
  ),
  br(),
  p("In this report, we look at survey data from Swarthmore transfer students on their backgrounds and experiences with regards to their time at the institution. The names and contacts of all available transfers, current and five years back, were obtained, and an email inviting them to fill out the survey was sent out to this mailing list. 83 responses were recorded, with 74 responses providing answers to at least one question in the survey."),
  p("First, I will summarize what I noticed from the quantitative questions in the survey. Then, I will present my findings on respondent’s qualitative responses to two questions: (1) “In which ways did being a transfer student impact your Swarthmore experience?” and (2) “What other recommendations would you make to Swarthmore to improve the experience of transfer students?” Finally, I will conclude with an integrative reflection and summary of my findings."),
#  p(a(href = "https://www.kaggle.com/barelydedicated/mariokart8?select=characters.csv", 
#      "Data Source (Kaggle)"))
  h2(id = "DD", "Describing the Data",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  p("The responses collected are observational data. As our sample is not a random sample of all Swarthmore transfers, we should neither generalize beyond this group nor draw any causal conclusions. The goal of this description is just that: to describe what is going on with this group of people and provide a broader overview to complement interview data. Additionally, it’s important to keep in mind, especially since we are exploring many variables, that some of the patterns may have occurred by chance (e.g. a fair coin sometimes lands heads 7 or 8 times out of 10). Rather than focusing on any one specific finding with scrutiny, the data is more suited to providing information about the larger picture. No formal statistical tests were run; the patterns/trends are what I noticed from looking at the graphs for each variable on the visualization tab, overall and broken down by transfer year or 2-year/4-year transfers. (There are 18 2-year transfers and 43 4-year transfers. While the numbers fluctuate and tend to decrease towards the end of the survey, it’s important to keep in mind the difference in sample size and the relatively small sample size of 2-year transfers.)"),
  h4("Background",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  h6(id = "GY", "Graduation Year",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  fluidRow(
    column(width = 4,
           p("There have been more transfers from 2-year institutions in recent years. In this sample, 2-year transfers are synonymous with transfers from community college, with the exception of one respondent who transferred from a “2-year liberal arts college.” Prior to the class of 2019, there was exactly one 2-year transfer from a community college, while the other 17 2-year transfers were the class of 2019 or later. This trend may be more indicative of the sampling technique or sampling bias (e.g. maybe older alumni are harder to reach), but I’m also curious if there was an intentional initiative from admissions to do more outreach to community college transfers.")),
    column(width = 8,
           plotOutput("plotGY"))
  ),
  h6(id = "HSL","HS Location",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  fluidRow(
    column(width = 4,
           p("A greater proportion of 4-year transfers (27/43 ≈ 63%) than 2-year transfers (6/18 ≈ 33%) come from suburban high schools, while a greater proportion of 2-year transfers (10/18 ≈ 56%) than 4-year transfers (10/43 ≈ 23%) come from urban high schools.")),
    column(width = 8,
           plotOutput("plotHSloc"))
  ),
  h6(id = "AP", "HS Grades, Number of AP Classes",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  fluidRow(
    column(width = 4,
           p("While there is no discernible difference in high school grades between 2-year and 4-year transfers, junior transfers had a greater proportion of A and B students (~28%) and less mostly A students (65%) than sophomore transfers (9% and 82% respectively).")),
    column(width = 8,
           plotOutput("plotHSgrades"))
  ),
  fluidRow(
    column(width = 4,
           p("Additionally, junior transfers took less APs on average than sophomore transfers; namely, the vast majority of sophomore transfers took 6+ APs while junior transfers were more evenly spread between 0 and 6+.")),
    column(width = 8,
           plotOutput("plotHSAP"))
  ),
  h6(id = "sat", "HS (Counseling) Satisfaction",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  fluidRow(
    column(width = 4,
           p("Overall, 2-year transfers seemed less satisfied with their high schools than 4-year transfers, particularly with regards to post-secondary counseling. Over 60% of 2-year transfers reported being dissatisfied with their counseling experiences compared to just 25% of 4-year transfers.")),
    column(width = 8,
           plotOutput("plotHSsat"))
  ),
  fluidRow(
    column(width = 4,
           p("Around 80% of 2-year transfers used their high school counselors for any reason between 0 and 3 times, while 45% of 4-year transfers used their HS counselors 1-3 times (and none reported never using their counselor).")),
    column(width = 8,
           plotOutput("plotHScsl"))
  ),
  fluidRow(
    column(width = 2,
           p("In particular for choosing and applying to university, we see that 2-year transfers were most likely to answer that they “rarely” used their counselor for this purpose, while 4-year transfers were most likely to answer that they “often” used their counselor for this purpose.")),
    column(width = 5,
           plotOutput("plotHScslC")),
    column(width = 5,
           plotOutput("plotHScslA"))
  ),
  h6(id = "pla", "Post-High School Plans; Family Expectations, Education, Support",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  fluidRow(
    column(width = 4,
           p("A greater proportion of 4-year transfers (around 90%) planned to get a BA or higher degree than 2-year transfers (around 55%).")),
    column(width = 8,
           plotOutput("plotHSplans"))
  ),
  fluidRow(id = "famexp",
    column(width = 4,
           p("Likewise, a greater proportion of 4-year transfers (over 90%) were expected by their family to get their BA or higher degree than 2-year transfers (around 65%).")),
    column(width = 8,
           plotOutput("plotFamExp"))
  ),
  fluidRow(
    column(width = 4,
           p("On another note, a significantly greater percentage of sophomore transfers were expected by their family to get a higher degree (37%) than junior transfers (17%).")),
    column(width = 8,
           plotOutput("plotFamExp1"))
  ),
  p("Again, parents of 4-year and sophomore transfers tended to be more educated than 2-year and junior transfers respectively. (Graphs not shown — see Education_Mother and Education_Father on Visualization tab.)"),
  fluidRow(id = "famed",
    column(width = 4,
           p("Response rates were lower towards the end of the survey, but a greater percentage of 4-year transfers reported that at least one of their parents went to college.")),
    column(width = 8,
           plotOutput("plotFamEd"))
  ),  
  p("Finally, while educational expectations were high across the board, 4-year transfers reported that their families more often were able to support their education before college (e.g. homework, conferences, events). (Graphs not shown — see Family_EdSupport_... on Visualization tab.)"),
  h6(id = "icc", "Initial College Choice",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  p("Financial aid, academic credits, cost, job availability, and immigration considerations were significantly more important factors to 2-year transfers than 4-year transfers on their initial college choice. (Graphs not shown — see preTransferChoice_… on Visualization tab.) Additionally, 4-year transfers were more likely to receive advice from their friends, parents, and siblings on this initial college choice. (Graphs not shown — see preTransferAdvice_… on Visualization tab.)"),
  h4("Transfer Process and Swarthmore Experience",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  h6(id = "when", "When Decide to Transfer",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  fluidRow(
    column(width = 4,
           p("As expected, more 2-year students always knew that they would transfer.")),
    column(width = 8,
           plotOutput("plotWhen"))
  ),
  h6(id = "tra", "Transfer Reason & Advice",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  p("Financial factors, academic support, and family issues were more important factors, on average, to 2-year transfers. On the other hand, social factors and well-being were more important to 4-year transfers. (Graphs not shown — see TransferChoice_... in Visualization tab.)"),
  fluidRow(
    column(width = 2,
           p("As for who students turned to for transfer advice, 2-year transfers received “lots of advice” from teachers around 45% of the time as compared to 10% of 4-year transfers, while over 50% of 2-year transfers received “no advice” from parents compared to under 25% for 4-year transfers.")),
    column(width = 5,
           plotOutput("plotTranAdvT")),
    column(width = 5,
           plotOutput("plotTranAdvP"))
  ),
  h6(id = "hear", "How did you hear about Swat?",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  fluidRow(
    column(width = 4,
           p("Over 35% of 2-year transfers heard about Swat through a counselor or teacher recommendation, compared to only 15% of 4-year transfers. Both 2-year and 4-year transfers frequently heard about Swat through an online search (>30% for both).")),
    column(width = 8,
           plotOutput("plotHearSwat"))
  ),
  h6(id = "hope", "What did you hope for Swat?",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  fluidRow(
    column(width = 4,
           p("4-year transfers were more likely to report that they hoped to make lifelong friends at Swarthmore (30/35, 86%) than 2-year transfers (9/18, 50%).")),
    column(width = 8,
           plotOutput("plotHopeSwat"))
  ),
  h6(id = "acad", "Academic Preparation",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  p("2-year transfers reported feeling less prepared than 4-year transfers in the general areas of analysis, the research process, and in particular, science labs and papers. Interestingly, 4-year transfers reported receiving more of their preparation for science labs and papers in high school, while 2-year transfers received more of their science preparation in college. There was a similar phenomenon for the research process. (Graphs not shown — see AcadPrep_... and AcadPrep_Primary_... in Visualization tab.)"),
  h6(id = "ss", "Support at Swat",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  fluidRow(
    column(width = 4,
           p("About 25% of 4-year transfers sometimes or always received support from a dean, while about 50% of 2-year transfers sometimes or always received support from a dean.")),
    column(width = 8,
           plotOutput("plotDeanSS"))
  ),
  fluidRow(
    column(width = 4,
           p("Similar numbers were reported for WAs, with less than 25% of 4-years transfers sometimes or always received support from WAs and around 60% of 2-year transfers.")),
    column(width = 8,
           plotOutput("plotWASS"))
  ),
  h6(id = "help", "Transition Help at Swat",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  fluidRow(
    column(width = 4,
           p("Coursework was more likely to help 4-year transfers with their transition (19/37, 51%) compared to 2-year transfers (2/18, 11%).")),
    column(width = 8,
           plotOutput("plotHelp"))
  ),
  h6(id = "exp", "Did Swat meet expectations?",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  fluidRow(
    column(width = 4,
           p("No 4-year transfers reported that Swat did not meet expectations at all as a place to have a college experience, with almost 50% reporting that Swat was excellent in this regard. Strikingly, only 20% of 2-year transfers reported that Swat was excellent in meeting expectations of a college experience, with 50% of 2-year transfers reporting “somewhat” instead.")),
    column(width = 8,
           plotOutput("plotExp"))
  ),
  h6(id = "chg", "Reasons for Changing Major",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  fluidRow(
    column(width = 4,
           p("Note: numbers are particularly small for this question. 2-year transfers are more likely to change their major because of their background skills (n=3, 25%) compared to 4-year transfers (n=3, 7%), while more 4-year transfers changed their major because they liked the department or wanted to try something new.")),
    column(width = 8,
           plotOutput("plotChg"))
  ),
  h6(id = "bel", "Belonging at Swat",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  p("In every aspect measured in this survey, 4-year transfers reported feeling like they belong more at Swat overall, in their major, with their friends, and in orgs/clubs. (Graphs not shown — see Belong_Swat, Belong_Major, Belong_Friends, Belong_OrgsClubs on Visualization tab.)"),
  h6(id = "tsi", "Transfer Status Impact",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  fluidRow(
    column(width = 4,
           p("100% of 2-year transfers (n=18) said that their transfer status impacted their Swarthmore experience a lot. There were no missing responses and no responses for “somewhat” or “not at all.” On the other hand, 4-year transfers were evenly split between “a lot” and “somewhat”/”not at all”.")),
    column(width = 8,
           plotOutput("plotTSI"))
  ),
  h6(id = "org", "Want Transfer Student Org?",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  fluidRow(
    column(width = 4,
           p("A greater proportion of 2-year transfers want a transfer student organization at Swat (around 2/3) than 4-year transfers.")),
    column(width = 8,
           plotOutput("plotOrg"))
  ),
  h2(id = "TSI", "Transfer Status Impact on Swarthmore Experience",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  p("Respondents that selected “somewhat” or “a lot” in response to the question “How much did being a transfer student impact your Swarthmore experience?” were asked to expand upon in which ways their transfer status had an impact. 3 out of 54 respondents who answered this question selected that their transfer status did not impact their experience at all."),
  h4(id = "pos","'Positives'",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  p("I would first like to note that “positives” is a subjective judgment, and I come from my own bias as someone who likes to seek out new opportunities such as the exchange program from Pomona to Swarthmore. However, 3 people spoke about their greater appreciation and gratitude for Swarthmore coming from another institution/culture and/or having less time. Additionally, transfer students came in with a different mindset/background, which led to e.g. a better understanding of themselves and a willingness to try new things. In addition to the 3 people above who answered that their transfer status didn’t impact their Swat experience “at all,” there were 3 people who spoke about how they felt integrated most of the time and that their transfer status didn’t make a significant difference."),
  tags$blockquote("Because I belonged there, after a while, I forgot that I had transferred."),
  p("The transfer experience is NOT defined by its negatives. However, it is important to recognize that the vast majority of the data in the responses to this question speak about specific challenges that students faced because of their transfer status. We can recognize the problems that transfers face, while also holding space for the simultaneous truth that transfers can have positive or neutral experiences at the same time."),
  h4(id = "ie","Inclusion/Exclusion: Do I Belong Here?",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  tags$blockquote("People kept asking me why [I transferred]"),
  p("Though this particular respondent didn’t mind the question, I thought this sentiment was representative of how transferring schools is seen as non-normative and needing explanation. Another layer within transfer status that may have others view transfers differently is the association that people have with community college. [On a completely unnecessary aside, I recently had a Fulbright advising meeting, where I mentioned a possible interest in a European country. The other person seemed surprised, as if it somehow didn’t make sense that I (an Asian-appearing person) would go to Europe, when I could go to Asia. I just think that these reactions and what people decide to question can unearth the ideas that people hold about something. I may be completely taking this quote out of context, and it has many other explanations.]"),
  tags$blockquote("There's a divide in the lived-experiences between transfer students and traditional students who came to Swarthmore directly from High School."),
  p("Transfer students are qualitatively different, as they have at least a year of experience at another college. One of the most frequently repeated sentiments was that respondents felt like they did not belong at Swarthmore, that they didn’t feel like a “true Swattie.” The two main dimensions of exclusion were socially and academically."),
  h6(id = "newold", "New Student, Older Student: Making Friends at a Small School",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  p("One of the reasons that I picked a small school was the community and the ability to form close relationships to professors and peers. However, I never considered the flip side of that idea, where new students might have a hard time finding their place in a small school that has these relationships already. By far the most common theme in the response to this question was the difficulty that transfers had in making connections, particularly with peers in their class year, as friendships had already formed. Beyond peers, a few people mentioned a similar sentiment in their department, where their major peers already had relationships with their professor and other majors. Unless an intentional and concerted effort was made to integrate with their class year, transfers didn’t end up connecting with those classmates. In fact, many transfers ended up making friends with the first-year class since they went through orientation at the same time and the experience of being new to campus. Some had other transfer friends, though a couple of people mentioned that they wished there was more opportunity to connect with other transfers."),
  p("Not making friends in their year brought along some challenges. For example, one respondent talked about how they lacked a support group that they could have sorely benefitted from during the thesis process. Another respondent talked about the isolation that they felt during senior week because few if any of their friends were in their class. As I mentioned above, several transfers come from a different background than the “typical” Swarthmore student, and all transfers are no longer first-years in college experiencing college for the first time. This theme aligns with one of hte strongest recommendations from transfers to help integrate transfers into the community and make these connections with each other, with their class year, and the wider Swarthmore community."),
  h6(id = "behind", "Starting from Behind: Adjusting to Swarthmore in Less Time",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  p("Fewer options, limited opportunities, less time to do everything: those were the three (intertwined) sentiments that I heard expressed from the respondents, especially with regards to academics. While liberal arts colleges often tout themselves for their flexibility and ability to explore, several transfers talked about having to fulfill major and graduation requirements in less time. Consequently, the lack of time made it difficult for transfers to go abroad, explore different classes, and engage with Swarthmore beyond the requirements. A couple transfers talked about feeling “behind” their peers, which points to (1) a social comparison that brings differences to light and (2) the social and academic capital that transfers have to (re)learn aspects to succeed in Swarthmore culture. Additionally, the first year of college is often thought of as a transition year to learn the ropes, but transfers have less time to adjust and fulfill all their goals. Furthermore, layer the pandemic on top of that for those classes of transfers, and respondents missed out on significant relationship-building opportunities and time on campus to have the “Swarthmore experience.”"),
  p("Before they even entered Swarthmore, respondents spoke about how they now realized that “there were many resources and things that I felt like I needed to be made aware of prior to actually starting my Swarthmore journey.” Often, respondents found out about resources later, which may have contributed to the recurring theme in the data of impostor syndrome and feeling inadequately prepared."),
  tags$blockquote("And as someone coming from a community college and a public school experience, I also feel like I have had to relearn how to read and write all over again in order to survive at Swarthmore"),
  p("One respondent didn’t even fully realize how “academically rigorous and intense Swarthmore is compared to my other institution, which was another private 4-year university.” Another respondent spoke how about “it was difficult to utilise and comprehend the amount of resources that are available at Swat… since they were not part of my college experience prior to Swat.” The differing academic backgrounds and the lack of information about Swarthmore culture on top of the context of less time and individual circumstances made it difficult to adjust for many transfer students in this sample."),
  tags$blockquote("I had to use all available support at my disposal."),
  p("I want to end this section with how it is unfortunately necessary but admirable how transfers can recognize, articulate, and respond to these challenges, like the realization of the respondent in the above quote. Transfers have to adjust quickly, academically, socially, and institutionally, even though Swarthmore does not make it easy for them."),
  h2(id = "rec","Recommendations from Transfer Students",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  p("While respondents ended up talking about recommendations that were referenced in the multiple choice questions above this free response, it’s possible that some answers may preclude choices that were selected in the question before: “What kinds of transfer activities would you like to see?” For example, mentoring by older transfer students was the most frequently selected choice (n=37 out of 46 people who completed this question). However, only one person directly mentioned transfer student mentoring in their free response. In this specific example, it could also be that there wasn’t much for respondents to expand upon in the free response, as many people did talk about housing and orientation."),
  p("In addition to what was said before with transfers tending to make more friends with the first-years and not their class year, some respondents talked about the lack of transfer community and the desire for more intentional connection/opportunities to meet both other transfers and class year peers. However, one person emphasized how it’s important to integrate transfers into the overall community rather than a transfer-only niche. How do you strike a balance between the potential value of a strong transfer community and overall social integration?"),
  p("All of this is compounded by the context that there are few transfer students at Swarthmore. 5 respondents talked about the need to continue increasing the number of transfer students and create more recognition and visibility of transfer students on campus. However, Swarthmore also needs to consider how to support transfer students so that they have a good experience when they get here."),
  h4(id = "house", "Housing",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  tags$blockquote("I think being placed and oriented with the freshmen didn't make a lot of sense."),
  p("Some respondents talked about how transfers are in a socially different place than first years, and one was even “sick of living with freshmen.” However, there were two different alternatives that respondents proposed: (1) transfer-specific housing and (2) housing with sophomores and juniors (i.e. their own classmates)."),
  h4(id = "ori", "Orientation",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  p("One of the biggest requests was to change orientation to make it more relevant to transfer students. As one respondent put it, “It is a shame that Transfer students who are primarily Sophomores and Juniors are lumped into the same orientation seminars that first-year student are in.” Transfers have different needs, and “It would be helpful to have the same welcoming and support that first years have” targeted to the transfer experience. Some specific suggestions were to create a separate transfer convocation and to create a transfer student orientation group. One person even mentioned that the school shouldn’t rely on transfer students to take initiative in making their own groups; it would be helpful to have the structure to integrate transfers from the beginning. Transfers tend to have different life experiences on average — this diversity needs to be recognized while also helping transfers to “blend in” to Swarthmore."),
  p("While transfers have prior college experience, they are also still new students. They need all the Swarthmore specific information to navigate campus. As an exchange student this semester, I can name some examples from my experience as well that came up for me as I was reading these responses. I still don’t really know what a Swarthmore ID is. Nobody ever taught me how to use MySwarthmore. I didn’t know how to check my grades at the end of the semester. I certainly had tools to navigate a new place more so than I did my first semester, but like this respondent said, “we are not actually told how or what we can do.” "),
  h4(id = "sup", "Support",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  p("The propositions for support beyond housing and orientation touched upon many different aspects of the college experience. Generally, it was mentioned again that transfers have less time to figure everything out, and that makes it doubly important that resources are accessible and upfront. One suggestion was to have “more advice from previous transfer students on how to integrate.” Beyond this mentorship, 3 respondents requested general check-ins with transfer students or starting a support group."),
  h2(id = "end", "Conclusion",
     actionButton("b1","Back to Top",
                  onclick = "location.href='#top';")),
  p("My main takeaway from the survey was that, while there were some students who had no recommendations to improve Swarthmore for transfers, transfer students are not set up for success, socially and academically, at Swarthmore. Specifically, friend groups are already formed at such a small college, and transfers have a difficult time integrating into their class year, especially since they have orientation and housing with first-years it seems. Academically, students don’t get to have the “liberal arts” experience due to their limited time, and some students struggled with impostor syndrome and adjusting to the rigor of Swarthmore. Overall, transfers did not feel well informed of the resources available and for some, the Swarthmore culture."),
  p("However, there are concrete suggestions that could better the experience. Connect transfers together, but intentionally integrate them into the larger community as well. Mentorship from older transfers could be one way to do this. Make orientation relevant to transfers, recognizing that they are new students, but also place them in appropriate housing to match where they are at socially (i.e. not with first years generally). At this orientation, explain the resources available and how to use them. Finally, consider academic support like first-years have during their first semester: add/drop and/or pass/fail. Additionally (and especially if those are not implemented), reach out to transfers to see how they are doing.")
)

# UI
ui <- navbarPage(
  "By: Christopher Meng",
  second_panel,
  report_panel
)