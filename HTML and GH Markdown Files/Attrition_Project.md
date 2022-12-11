
<center>
<h1>
FritoLay Attrition Case Study
</h1>
</center>
<hr>

## [To watch a presentation of this case study click here!](https://www.youtube.com/watch?v=4uoISwAoATg)

### Introduction:

- Steven Williams - CEO - FritoLay
- Jamie Caulfield - CFO - FritoLay

Thank you for allowing us the opportunity to work with FritoLay on an
analysis of your current employee management and attrition data.

### About DDS Analytics:

At DDS Analytics we specialize in talent management solutions for
fortune 100 companies, specifically in the iterative process of
developing and retaining employees. Our analytics and solutions may
include workforce planning, employee training programs, identifying
high-potential employees and reducing/preventing voluntary employee
turnover(attrition).

### Attrition Defined:

According to the 2021 bureau of labor statistics report, 25% of the
turnover rate seen within the labor market is due to voluntary turn
over. As a general rule employee retention rates of 90% or higher are
considered good, and a company should aim for a turnover rate of 10% or
less. some of the general factors that attrition can be attributed to
are:

- Lack of training and development
- Need for more flexible working hours
- Stage of Career
- Salary/Pay

The value in this venture for a company is multifaceted but can be
realized in minimizing the expenses associated with cost-per-hires,
ensuring internal projects are completed efficiently and on time, and
promoting healthy relationships with external partners by keeping
persons of contacts and relationships consistent over periods of time.

<hr>
<center>
<h1>
Exploratory Data Analysis
</h1>
</center>
<hr>

### Inspection of data:

Before working with the data, the first step in our process is to get a
snapshot of what types of variables we will be working with, if there
are any missing pieces of information, potential computer created errors
based on the methods of data creation, and whether or not the data
values themselves need intervention. The data provided needs minimal or
arguably no intervention. There does seem to be variables which may be
interpreted and handled with some slight modifications which we will
address shortly.

``` r
# glimpse(data)
# get_dupes(data)
# missing <- sapply(data, function(x) sum(is.na(x)))
# sum(missing)
# summary(data)
```

### Visualzing data and Relationships:

Before we begin to deep dive into any aspect of the data, DDS Analytics
ops to engage in a visualization of the data, so that we can discover,
and view different relationships that will help us not only identify
potential answers to explicit questions you may have but the
visualizations may also serve as a catalyst to uncovering new questions
and ideas we may not know exist.

``` r
#### Plotting Continuous Var Dist. of Data ####
cont_var <- vizdata %>% select(Attrition, Age, DistanceFromHome,
                            NumCompaniesWorked, PercentSalaryHike,TotalWorkingYears,
                            TrainingTimesLastYear,YearsAtCompany, YearsInCurrentRole,
                            YearsSinceLastPromotion,YearsWithCurrManager)

for (j in names(cont_var)) {
  plot_var_name <- str_c(c("ggplot",j), collapse ="_")
  print(plot_var_name)
  temp_plot <- ggplot(cont_var, aes_string(j,fill = 'Attrition')) +
    geom_bar(aes(y = ..count../sum(..count..)), position = "dodge", show.legend = FALSE)+
    scale_y_continuous(labels = percent_format()) +
    ylab("% of Attrition")
  assign(plot_var_name, temp_plot)
}
my_plots_list <- lapply(names(cont_var), function(j) {
  ggplot(cont_var, aes_string(j,fill = "Attrition")) +
    geom_bar(aes(y = ..count../sum(..count..)), position = "dodge", show.legend = FALSE) +
    scale_y_continuous(labels = percent_format()) +
    ylab("% of Attrition") +
    scale_color_fivethirtyeight("Attrition") +
    theme_hc()
})
my_plots_list[[1]] = NULL # to get rid of Attrition / Attrition Plot
gridExtra::grid.arrange(grobs= my_plots_list, ncol = 4)
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

``` r
#### Plotting Categorical Var Dist. of Data ####
cat_var <- vizdata %>% select(Attrition, JobSatisfaction, JobLevel, JobInvolvement,
                            StockOptionLevel, BusinessTravel, JobRole, EducationField,
                           Gender, MaritalStatus, Department,OverTime,
                            Education, RelationshipSatisfaction, WorkLifeBalance)
for (j in names(cat_var)) {
  plot_var_name <- str_c(c("ggplot",j), collapse ="_")
  print(plot_var_name)
  temp_plot <- ggplot(cat_var, aes_string(j,fill = 'Attrition')) +
    geom_bar(aes(y = ..count../sum(..count..)), position = "dodge", show.legend = F)+
    scale_y_continuous(labels = percent_format()) +
    ylab("% of Attrition")
  assign(plot_var_name, temp_plot)
}
my_plots_cat <- lapply(names(cat_var), function(j) {
  ggplot(cat_var, aes_string(j,fill = "Attrition"))+
    geom_bar(aes(y = ..count../sum(..count..)), color = "black", position = "dodge", show.legend = F)+
    scale_y_continuous(labels = percent_format()) +
    ylab("% of Attrition") +
    scale_color_fivethirtyeight("Attrition") +
    theme_hc()
})
my_plots_cat[[1]] = NULL # to get rid of Attrition / Attrition Plot
gridExtra::grid.arrange(grobs= my_plots_cat, ncol = 4)
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-1-2.png" style="display: block; margin: auto;" />

### Creating Variable Subcategories:

To intuitively interpret attrition within our visualizations, we created
a “vizdata” data frame which changes the “yes” or “no” logical status of
attrition to “Left” or “Stayed”.

Additionally we have identified variables such as “Education Level”
which is listed in a numerical format, and have changed it into Levels
of “No College”. “Some College”, “Bachelors”, “Masters”, and “PhD”. We
will use this process with other variables such as Age, so that we can
better understand different classifications of variable groups to help
identify specific parties within the company.

``` r
# CREATING INTUITIVE TITLE FOR VIZ OF DATA
vizdata$Attrition <- ifelse(vizdata$Attrition == "Yes", "Left", "Stayed")

data$EducationLevel <- ifelse(data$Education == 1, "No College",
                              ifelse(data$Education == 2, "Some College",
                                     ifelse(data$Education == 3, "Bachelors",
                                            ifelse(data$Education == 4, "Masters", "Phd"))))


vizdata$EducationLevel <- ifelse(data$Education == 1, "No College",
                              ifelse(data$Education == 2, "Some College",
                                     ifelse(data$Education == 3, "Bachelors",
                                            ifelse(data$Education == 4, "Masters", "Phd"))))


# Breaking Age into Generational Groups
data$Generations <- ifelse(data$Age > 26 & data$Age < 42, "Millennials",
                           ifelse(data$Age <= 26, "Gen Z",
                                  ifelse(data$Age >= 42 & data$Age <57, "Gen X",
                                         ifelse(data$Age >=57 & data$Age <= 67, "Boomers II",
                                                ifelse(data$Age >= 68, "Boomers I", 'Silent')))))

# Breaking Age into Generational Groups - VIZ DATA
vizdata$Generations <- ifelse(data$Age > 26 & data$Age < 42, "Millennials",
                           ifelse(data$Age <= 26, "Gen Z",
                                  ifelse(data$Age >= 42 & data$Age <57, "Gen X",
                                         ifelse(data$Age >=57 & data$Age <= 67, "Boomers II",
                                                ifelse(data$Age >= 68, "Boomers I", 'Silent')))))
```

### Encoding Variables for Model Usage:

The data we are working with have many different categorical variables.
One of the objectives of our work is to create a predictive attrition
model that utilizes the data we have to make predictions on future
attrition. So that we can build an effective and efficient model, we
will convert categorical data into numerical data by process of “One-Hot
Encoding”.

One-Hot Encoding is simply the process of assigning replacement binary
variables to categorical variables given in the data.

### Encoding variables and Feature Engineering for model use and investigations:

After Encoding and engineering new features to use in our investigation
and modeling we will subset the data for columns that represent logical
baseline reference points, and create a scaled data set to use for
running the KNN prediction model.

``` r
# CAT CODING BELOW:
data$Attrition <- ifelse(data$Attrition == "Yes",1,0)
data$OverTime <- ifelse(data$OverTime == "Yes",1,0)
data$Gender <- ifelse(data$Gender == "Male",1,0)
data$BusinessTravel <- as.numeric(factor(data$BusinessTravel, 
                                         levels=c("Non-Travel", "Travel_Rarely", "Travel_Frequently"))) -1
data$HumanResources <- ifelse(data$Department == "Human Resources",1,0)
data$ResearchDevelopment <- ifelse(data$Department == "Research & Development",1,0)
data$Sales <- ifelse(data$Department == "Sales",1,0)
data$Single <- ifelse(data$MaritalStatus == "Single",1,0)
data$Married <- ifelse(data$MaritalStatus == "Married",1,0)
data$Divorced <- ifelse(data$MaritalStatus == "Divorced",1,0)
data$EduHumanResources <- ifelse(data$EducationField == "Human Resources",1,0)
data$EduLifeSciences <- ifelse(data$EducationField == "Life Sciences",1,0)
data$EduMedical <- ifelse(data$EducationField == "Medical",1,0)
data$EduMarketing <- ifelse(data$EducationField == "Marketing",1,0)
data$EduTechnicalDegree <- ifelse(data$EducationField == "Technical Degree",1,0)
data$EduOther <- ifelse(data$EducationField == "Other",1,0)
data$JobSalesExecutive <- ifelse(data$JobRole == "Sales Executive",1,0)
data$JobResearchDirector <- ifelse(data$JobRole == "Research Director",1,0)
data$JobManufacturingDirector <- ifelse(data$JobRole == "Manufacturing Director",1,0)
data$JobResearchScientist <- ifelse(data$JobRole == "Research Scientist",1,0)
data$JobSalesExecutive <- ifelse(data$JobRole == "Sales Executive",1,0)
data$JobSalesRepresentative <- ifelse(data$JobRole == "Sales Representative",1,0)
data$JobManager <- ifelse(data$JobRole == "Manager",1,0)
data$JobHealthcareRepresentative <- ifelse(data$JobRole == "Healthcare Representative",1,0)
data$JobHumanResources <- ifelse(data$JobRole == "Human Resources",1,0)
data$JobLaboratoryTechnician <- ifelse(data$JobRole == "Laboratory Technician",1,0)
data$Bachelors <- ifelse(data$EducationLevel == 'Bachelors', 1, 0)
data$Masters <- ifelse(data$EducationLevel == "Masters", 1, 0)
data$SomeCollege <- ifelse(data$EducationLevel == "Some College", 1, 0)
data$NoCollege <- ifelse(data$EducationLevel == "No College", 1, 0)
data$Phd <- ifelse(data$EducationLevel == "Phd", 1, 0)
data$Millennials <- ifelse(data$Generations == "Millennials", 1, 0)
data$GenZ <- ifelse(data$Generations == "Gen Z", 1, 0)
data$GenX <- ifelse(data$Generations == "Gen X", 1, 0)
data$Boomersii <- ifelse(data$Generations == "Boomers II", 1, 0)

# NUMERICAL ENCODING BELOW:
data$LessThan4k <- ifelse(data$MonthlyIncome < 4000, 1, 0)
data$FreshWorker <- ifelse(data$NumCompaniesWorked <=1.25, 1, 0)
data$LowLevel <- ifelse(data$JobLevel == 1, 1, 0)
data$FreshHire <- ifelse(data$YearsAtCompany <=4, 1, 0) 
data$WorkMore30 <- ifelse(data$TotalWorkingYears >=30, 1, 0)
data$LowInvolve <- ifelse(data$JobInvolvement <2, 1, 0)
data$NewRole <- ifelse(data$YearsInCurrentRole <=2, 1, 0)
data$NoBalance <- ifelse(data$WorkLifeBalance <2, 1, 0)
data$SalaryHike <- ifelse(data$PercentSalaryHike  >17, 1, 0) 
data$HighSatisfaction <- ifelse(data$JobSatisfaction >= 3, 1, 0) 
data$LongCommute <- ifelse(data$DistanceFromHome >= 13, 1, 0)
data$AgeUnder35 <- ifelse(data$Age <=35, 1, 0) 
data$DueForPromotion <- ifelse(!data$YearsSinceLastPromotion %in% c(1,5,6,7), 1, 0) 
data$HighPerform <- ifelse(data$PerformanceRating >= 3, 1, 0) 
data$NoStock <- ifelse(data$StockOptionLevel < 1, 1 , 0)  
data$LowTraining <- ifelse(data$TrainingTimesLastYear < 2, 1, 0)
data$HourlyOver40 <- ifelse(data$HourlyRate > 40, 1, 0) 
data$MonthlyOver15k <- ifelse(data$MonthlyRate > 15000, 1, 0) 
data$LogIncome <- log(data$MonthlyIncome) 
```

### Scaling Data:

The reason for scaling the data for the KNN model is because KNN uses
Euclidean Distance to find similarities between observations. To ensure
a quality model we need to account for certain variables which may have
a higher magnitude than others so that it is not biased towards those
variables with higher magnitudes.

``` r
# Factor Sub-setting for columns that represent logical baseline reference points from which the model will assume all included 
# predictor variables to deviate, and scaling data set for use in modeling
rdata <- subset(data, select = -c(Over18, Department, EducationLevel, 
                                  Generations, JobRole, MaritalStatus,
                                  EducationField, EmployeeCount,
                                  StandardHours))


# Scaling data for use in the KNN model 
scaled_data <- data.frame(apply(rdata, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
```

### Visualizing Correlative Relationships

To fully understand attrition and its relationship to multiple variables
we will create a correlation plot heat map. This heat map will
illustrate which variables have a positive, neutral, or negative
correlative relationship with attrition so that we can further explore
each piece of data.

``` r
# this  tot_correlation will be used to plot the entire correlation matrix
tot_correlation <- subset(rdata, select = -c(HighPerform))
tot_correlation <- cor(tot_correlation)

# data_corr will be used to view a smaller scale of "core" var. 
data_corr <- cor(rdata %>% select(Age, Attrition,MonthlyIncome, DistanceFromHome,
                                  Education, NumCompaniesWorked, EnvironmentSatisfaction,
                                  Gender,HourlyRate, JobSatisfaction, PercentSalaryHike,
                                  OverTime, TotalWorkingYears, WorkLifeBalance, 
                                  YearsAtCompany:YearsWithCurrManager))

# this df will be used for analysis of Attrition and Salary data correlatives
crdata <- tot_correlation[,c("Attrition", "MonthlyIncome")]
crdata <- data.frame(rbind(names(crdata), crdata))
crdata <- tibble::rownames_to_column(crdata,"Feature")


# for use with core feats.
#crdata <- data_corr[,c("Attrition", "MonthlyIncome")]
#crdata <- data.frame(rbind(names(crdata), crdata))
#crdata <- tibble::rownames_to_column(crdata,"Feature")

# income correlations
IncomeCorrelation <- crdata %>% select(Feature, MonthlyIncome) %>%
  filter(!Feature %in% c("MonthlyIncome", "LogIncome")) %>%
  arrange(abs(MonthlyIncome))

# attrition correlations
AttritionCorrelation <- crdata %>% select(Feature, Attrition) %>% arrange(abs(Attrition)) %>% filter(Feature != "Attrition")
AttritionCorrelation$Feature <- as.factor(AttritionCorrelation$Feature)
```

<center>

![Correlation Plot With All Features](./project2/CorPlot.png)

</center>
<hr>

### Correlative Ranking for Attrition and Income:

``` r
AttritionCorrelation %>% top_n(10) %>% mutate(Feature = factor(Feature, Feature)) %>%
  ggplot(aes(Feature, Attrition, fill = Feature)) + geom_col(show.legend = FALSE) +
  coord_flip() + labs(title = "Top 10 Correlative Factors for Attrition") +
  ylab("Correlation Between Features and Attrition") +
  scale_fill_discrete(guide = guide_legend(reverse = TRUE))+
    scale_color_fivethirtyeight() +
    theme_hc()
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

``` r
IncomeCorrelation %>% top_n(10) %>% mutate(Feature = factor(Feature, Feature)) %>% 
  ggplot(aes(Feature, MonthlyIncome, fill = Feature)) + 
  geom_col(show.legend = FALSE) +
  labs(title = "Top 10 Correlative Factors for Monthly Income") +
  ylab("Correlation Between Features and Monthly Income") + coord_flip() +
    scale_color_fivethirtyeight() +
    theme_hc()
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-7-2.png" style="display: block; margin: auto;" />

``` r
#gridExtra::grid.arrange(acp, icp, nrow = 2)
```

<hr>
<center>
<h1>
Question of Interest
</h1>
<h3>
Identify The Top 3 Factors of Attrition and Monthly Income
</h3>
<center>
<hr>

### Top 3 Factors of Attrition/Voluntary Turnover

- Over Time
- No Stock Option
- Low Employee Level

Employees that fall into the Over Time category are those which are
hourly employees. The two other factors that follow make intuitive sense
since stock options are rarely given to hourly employees, and hourly
employees tend to make up low level employees.

``` r
# Plot for job Correlation based on the 10 ranks
job_corr <- cor(rdata %>% select(Attrition, OverTime, NoStock,
                                 LowLevel, JobSalesRepresentative,LowInvolve,
                                 LessThan4k, FreshHire,Single, NewRole,
                                 AgeUnder35))

# Job_Corr Plot with Core Feature Selection
ggcorrplot(corr = job_corr, hc.order = TRUE,insig = 'blank',lab = TRUE,
           lab_size = 2,
           colors = c("#6D9EC1", "white", "#E46726")) +
  labs(title = "Correlation Between Variables and Attrition",
       subtitle = "Netural and Positive Correlation") +
  theme(plot.title = element_text(hjust = 0.5),
       plot.subtitle = element_text(hjust = 0.5)) 
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

### Top 3 Factors of Monthly Income

- Job Level
- Total Working Years
- Manager Job Level

Similar to the factors of Attrition, Job Level which holds rank 1 in our
analysis is followed by total working years, and manager job level,
which both are variables that all make sense to be together intuitive.

``` r
# plot for income corr based on 10 ranks
income_corr <- cor(rdata %>% select(MonthlyIncome, JobLevel,TotalWorkingYears,
                                    JobManager,JobResearchDirector,YearsAtCompany,
                                    Age,GenX,WorkMore30,YearsInCurrentRole,YearsWithCurrManager))

# Corr Plot with Core Feature Selection
ggcorrplot(corr = income_corr, hc.order = TRUE,insig = 'blank',lab = TRUE,
           lab_size = 2,
           colors = c("#6D9EC1", "white", "#E46726")) +
  labs(title = "Correlation Between Variables and Attrition",
       subtitle = "Netural and Positive Correlation") +
  theme(plot.title = element_text(hjust = 0.5),
       plot.subtitle = element_text(hjust = 0.5))
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

### Attrition for Specific Job Role:

Upon discovering the top factors that contribute to attrition at
FritoLay, we noticed the 4th rank to be the job role Sales
Representative. This captured our interest and led us to ask the
question “Which job roles are seeing the most amount of attrition?”

Asking this question helped to uncover the top 3 job roles in which
FritoLay is experiencing the greatest percentage of turnover within each
role.

- Sales Representative - 45.28% of the individuals in this role left.
- Human Resources - 22.22% of the individuals in this role left.
- Laboratory Technicians - 19.61% of the individuals in this role left.

As you may recall, the 2021 bureau of labor statistics report, 25% of
the turnover rate seen within the labor market is due to voluntary turn
over. As a general rule employee retention rates of 90% or higher are
considered good, and a company should aim for a turnover rate of 10% or
less.

Based on these numbers, the Sales Representative position is well above
the typical 25% turnover rate within the labor market. Additionally, the
Human Resources and Laboratory Technicians roles are each nearly double
the ideal turnover rate of 10% or less.

``` r
churn_jr <- as.data.frame(vizdata %>% count(JobRole, Attrition))
# creating a DF that shows the COUNT of ATTRITION for each role
churn_jr <- as.data.frame(vizdata %>% count(JobRole, Attrition))
# Creating a DF that has PERCENT of LEAVE/STAY for each Job Role
churn_percent_jr <- 
  as.data.frame(churn_jr %>% group_by(JobRole) %>%
                  mutate(Percentage = paste0(round(n/sum(n)*100,2))))

churn_percent_jr$Percentage <- as.numeric(churn_percent_jr$Percentage)

# Tree map of Employees that leave per Job Role
churn_percent_jr %>% 
  filter(Attrition == "Left") %>%
  ggplot(aes(area = Percentage, fill = JobRole, label = paste(JobRole, "\n",Percentage,"%")))+
  geom_treemap(show.legend = FALSE, color = 'black') +
  geom_treemap_text(color = 'white', place = "center") +
  ggtitle('Percentage of Attrition Within Each Job Role') + 
  theme(plot.title = element_text(hjust = .5))
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

### Various Age Group Representation Within Various Roles:

After discovering the percent of attrition seen within each of the job
roles, we felt the need to take it a level further and investigate the
age categories to understand if there was any visual evidence that
certain age groups may see higher instances of attrition, and then
observing how those age groups are allocated around various job roles.

A breakdown of age groups are by the following:

- Gen Z: 26 or younger
- Millennial: older than 26 but younger than 42
- Gen X: 42 or older, but younger than 57
- Boomers II: older than 57 but younger than 68
- Boomers I: 68 or older

``` r
# Attrition percentage for each age group:
churn_gen <- as.data.frame(vizdata %>% count(Generations, Attrition))
# creating a DF that shows the COUNT of ATTRITION for each role
churn_percent_gen <- 
  as.data.frame(churn_gen %>% group_by(Generations) %>%
                  mutate(Percentage = paste0(round(n/sum(n)*100,2))))

churn_percent_gen$Percentage <- as.numeric(churn_percent_gen$Percentage)

# Tree map of Employees that leave per Job Role
churn_percent_gen %>% 
  filter(Attrition == "Left") %>%
  ggplot(aes(area = Percentage, fill = Generations, label = paste(Generations, "\n",Percentage,"%")))+
  geom_treemap(show.legend = FALSE, color = 'black') +
  geom_treemap_text(color = 'white', place = "center") +
  ggtitle('Percentage of Attrition Within Each Age Category') + 
  theme(plot.title = element_text(hjust = .5))
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

``` r
# bar plot of generation breakdown in total company
vizdata %>% ggplot(aes(Generations, fill = Generations)) + 
  geom_bar(show.legend = F, color = "black") +
  ggtitle("Amount of Employees Within Various Age Categories") +
  ylab("Number of Employees") + xlab("Age Categories") +
    scale_color_fivethirtyeight() +
    theme_hc() + theme(plot.title = element_text(hjust = .5)) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -.25)
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-11-2.png" style="display: block; margin: auto;" />

``` r
# correlation plot for age categories and specific job roles
age_job_cor <- cor(rdata %>% select(JobLaboratoryTechnician, JobResearchScientist,
                                    JobSalesRepresentative, JobSalesExecutive,JobManager,
                                    JobHumanResources, JobHealthcareRepresentative,
                                    JobManufacturingDirector,JobResearchDirector,
                                    GenX,GenZ,Millennials,Boomersii))


# Corr Plot with Core Feature Selection
ggcorrplot(corr = age_job_cor, hc.order = TRUE,insig = 'blank',lab = TRUE,
           lab_size = 2, type = 'upper',
           colors = c("#6D9EC1", "white", "#E46726")) +
  labs(title = "Correlation Between Age and Job Role",
       subtitle = "Netural and Positive Correlation") +
  theme(plot.title = element_text(hjust = 0.5),
       plot.subtitle = element_text(hjust = 0.5))
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-11-3.png" style="display: block; margin: auto;" />

``` r
# age groups within each jobrole
vizdata %>% select(Generations, Department) %>%
  group_by(Generations, Department) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = fct_reorder(Generations, n), y = n, fill = Generations)) +
  geom_bar(stat = "identity", color = "black") + facet_wrap(.~Department) + 
  ggtitle("Allocation of Various Age Groups in Each Department") + ylab("Amount of Employees") +
  xlab("Age Groups") + theme(axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank()) +
    scale_color_fivethirtyeight() +
    theme_hc() + theme(plot.title = element_text(hjust = .5))
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-11-4.png" style="display: block; margin: auto;" />

``` r
# age groups within each jobrole
vizdata %>% select(Generations, JobRole) %>%
  group_by(Generations, JobRole) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = fct_reorder(Generations, n), y = n, fill = Generations)) +
  geom_bar(stat = "identity", color = "black") + facet_wrap(.~JobRole) + 
  ggtitle("Allocation of Various Age Groups in Each Job Role") + ylab("Amount of Employees") +
  xlab("Age Groups") + theme(axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank()) +
    scale_color_fivethirtyeight() +
    theme_hc() + theme(plot.title = element_text(hjust = .5))
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-11-5.png" style="display: block; margin: auto;" />
After looking at the data, we can see some obvious takeaways.

- It appears that the company has a high proportion of Millennial
  employees (61%) followed by a 26% allocation of employees in the Gen X
  category.

- Gen X has the lowest amount of attrition within it’s age group at
  10.34% while Gen Z has a staggering 30.43% percentage of attrition
  within it’s age group.

- Sales Representatives are made up of mostly younger age groups. This
  makes sense since a sales rep is typically considered more of an entry
  level sales position.

<hr>
<center>
<h1>
Predictive Models for Attrition
</h1>
</center>
<hr>

### Naive Bayes

Brief Model Overview: A Naive Bayes model relies on conditional
probability, which is the probability that something will happen given
that something else has already occurred.

#### Results:

``` r
iterations = 100
masterAcc_nb = matrix(nrow = iterations)
masterSpec_nb = matrix(nrow = iterations)
masterSen_nb = matrix(nrow = iterations)
splitPerc <- .7

nbArray <- c("OverTime","NewRole", "WorkLifeBalance", "JobInvolvement",
             "JobSatisfaction", "Gender", "EnvironmentSatisfaction",
             "BusinessTravel", "MonthlyIncome", "FreshHire", "AgeUnder35",
             "LogIncome", "Divorced", "HourlyOver40", "NoStock","HighPerform")
set.seed(7)
for(j in 1:iterations){
  trainIndices = sample(1:dim(data)[1],round(splitPerc * dim(data)[1]))
  train = data[trainIndices,]
  test = data[-trainIndices,]
  
  model = naiveBayes(train[,nbArray],as.factor(train$Attrition))
  
  CM_NB = confusionMatrix(table(predict(model,test[,nbArray]),as.factor(test$Attrition),
                                dnn = c("Prediction", "Reference")), positive = '1')
  
  masterAcc_nb[j] = CM_NB$overall[1]
  masterSen_nb[j] = CM_NB$byClass[1]
  masterSpec_nb[j] = CM_NB$byClass[2]
}

specs_nb <- c(colMeans(masterAcc_nb),colMeans(masterSen_nb),colMeans(masterSpec_nb))

names(specs_nb) <- c("Avg Accuracy ", "Avg Sensitivity ", "Avg Specificity")
specs_nb
```

    ##    Avg Accuracy  Avg Sensitivity   Avg Specificity 
    ##        0.8319157        0.5737351        0.8822349

``` r
model2 = naiveBayes(train[,nbArray], as.factor(train$Attrition))
nbCM = confusionMatrix(table(predict(model2,test[,nbArray]), as.factor(test$Attrition),
                      dnn = c("Prediction", "Reference")), positive = '1')
```

``` r
# Confusion Matrix code
nb_CM <- function(m){
  plot <- ggplot(data = as.data.frame(m$table),
              aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)),
              color = "black", show.legend = FALSE) +
    scale_fill_gradient(low = "light blue", high = "steelblue") +
    geom_text(show.legend = FALSE ,aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = 'none') +
    ggtitle("Naive Bayes Attrition Confusion Matrix") +
    scale_color_fivethirtyeight() +
    theme_hc() + theme(plot.title = element_text(hjust = .5))
    
  return(plot)
}
nb_CM(nbCM)  
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

<hr>

### K Nearest Neighbor

Brief Model Overview: K Nearest Neighbor (KNN) uses proximity to make
predictions about an individual data point. It is used as a
classification model which assumes similar points can be found near one
another.

#### Results:

``` r
# Creating a loop with test/train split 
# to get avgs and find best k value to run model with. 

iterations = 50
set.seed(7)
numks = round(sqrt(dim(scaled_data)[1])*1.2)
masterAcc = matrix(nrow = iterations, ncol = numks)
masterSpec= matrix(nrow = iterations, ncol = numks)
masterSen = matrix(nrow = iterations, ncol = numks)
splitPerc = .7

# dataset to run into loop/etc
knnVar <- c("OverTime","NoStock","LowLevel","JobSalesRepresentative",
              "LowInvolve","LessThan4k","FreshHire","Single","NewRole",
              "AgeUnder35")

# loop to generate the average perf. of model
for(j in 1:iterations) {
  trainIndices = sample(1:dim(scaled_data)[1],round(splitPerc * dim(scaled_data)[1]))
  train = scaled_data[trainIndices,]
  test = scaled_data[-trainIndices,]
  
  for(i in 1:numks) {
    knnClassification = knn(train[,knnVar],test[,knnVar],as.factor(train$Attrition),
                          prob=TRUE, k = i)
    
    CM = confusionMatrix(table(as.factor(test$Attrition), knnClassification,
                              dnn =c("Prediction", "Reference")), positive = '1')
    
    masterAcc[j,i] = CM$overall[1]
    masterSen[j,i] = CM$byClass[1]
    masterSpec[j,i] = CM$byClass[2]
  }
}
# pulling the avgs.
MeanAcc <- colMeans(masterAcc)
MeanSen <- colMeans(masterSen)
MeanSpec <- colMeans(masterSpec)
k <- which.max(MeanAcc)

specs <- c(MeanAcc[k], MeanSen[k], MeanSpec[k])
names(specs) <- c("Avg Acc", "Avg Sen", "Avg Spec")
specs
```

    ##   Avg Acc   Avg Sen  Avg Spec 
    ## 0.8595402 0.7800963 0.8633373

``` r
knnClassification = knn(train[,knnVar],test[,knnVar],as.factor(train$Attrition),
                      prob=TRUE, k = k)

# confusionMatrix(table(test$Attrition, knnClassification,
#                       dnn = c("Prediction","Reference")), positive = '1')
```

``` r
# KNN CONFUSION MATRIX - K PLOT - ROC/AUC PLOT

# plotting kth values
plot(seq(1,numks), MeanAcc, main="K Value Plot", xlab="ith Value of K",
     ylab = "Mean Accuracy")
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

``` r
u <- union(knnClassification, test$Attrition)
t <- table(factor(knnClassification, u), factor(test$Attrition, u),
           dnn = c("Prediction", "Reference"))

CMP <- confusionMatrix(t)

knn_cm_plot <- as.data.frame(CMP$table)

knn_cm_plot$Prediction <- factor(knn_cm_plot$Prediction,
                                 levels = rev(levels(knn_cm_plot$Reference)))

ggplot(knn_cm_plot, aes(Prediction, Reference, fill=(Freq))) +
  geom_tile(show.legend = FALSE, color = 'black') + 
  geom_text(aes(label=(Freq))) +
  scale_fill_gradient(low = "light blue", high = "steelblue") +
  labs(x = "Reference", y = "Prediction") +
    ggtitle("KNN Attrition Confusion Matrix") +
    scale_color_fivethirtyeight() +
    theme_hc()+ theme(plot.title = element_text(hjust = .5))
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-15-2.png" style="display: block; margin: auto;" />

``` r
plot(roc(test$Attrition, attributes(knnClassification)$prob),
     print.thres = T, print.auc=T, main = "AUC Curve for KNN Performance")
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-15-3.png" style="display: block; margin: auto;" />

<hr>
<center>
<h1>
Predictive Model for Income
</h1>
</center>
<hr>

### Linear Regression for Monthly Income

Brief Model Overview: Linear Regression is a model that finds the best
“fit” line between the independent and dependent variable(s).

#### Results and Assumptions:

``` r
# ORIGINAL MODEL
set.seed(7)
splitPerc = .7
trainIndices = sample(1:dim(data)[1],round(splitPerc * dim(data)[1]))
train = data[trainIndices,]
test = data[-trainIndices,]

income_fit <- lm(MonthlyIncome ~ 
               JobLevel +
               TotalWorkingYears +
               JobRole, data=train)

ols_plot_diagnostics(income_fit)
```

<img src="Attrition_Project_files/figure-gfm/unnamed-chunk-16-1.png" style="display: block; margin: auto;" /><img src="Attrition_Project_files/figure-gfm/unnamed-chunk-16-2.png" style="display: block; margin: auto;" /><img src="Attrition_Project_files/figure-gfm/unnamed-chunk-16-3.png" style="display: block; margin: auto;" />

``` r
income_pred <- predict(income_fit, interval="predict",newdata = test)

RMSE <- sqrt(mean((income_pred[,1] - test$MonthlyIncome)^2))
cat("RMSE = ",RMSE)
```

    ## RMSE =  1033.745

``` r
# allows us to see vars in model and their stats. 
stargazer(income_fit, type = 'text') # adj r2 .92
```

    ## 
    ## =========================================================
    ##                                   Dependent variable:    
    ##                               ---------------------------
    ##                                      MonthlyIncome       
    ## ---------------------------------------------------------
    ## JobLevel                             2,762.115***        
    ##                                        (98.253)          
    ##                                                          
    ## TotalWorkingYears                      49.968***         
    ##                                         (9.242)          
    ##                                                          
    ## JobRoleHuman Resources                 -409.005          
    ##                                        (297.717)         
    ##                                                          
    ## JobRoleLaboratory Technician          -580.693***        
    ##                                        (217.614)         
    ##                                                          
    ## JobRoleManager                       4,097.501***        
    ##                                        (277.600)         
    ##                                                          
    ## JobRoleManufacturing Director           116.621          
    ##                                        (206.533)         
    ##                                                          
    ## JobRoleResearch Director             4,059.775***        
    ##                                        (264.269)         
    ##                                                          
    ## JobRoleResearch Scientist              -357.981*         
    ##                                        (216.054)         
    ##                                                          
    ## JobRoleSales Executive                  -50.411          
    ##                                        (186.455)         
    ##                                                          
    ## JobRoleSales Representative            -494.479*         
    ##                                        (263.957)         
    ##                                                          
    ## Constant                                -25.671          
    ##                                        (260.270)         
    ##                                                          
    ## ---------------------------------------------------------
    ## Observations                              609            
    ## R2                                       0.948           
    ## Adjusted R2                              0.947           
    ## Residual Std. Error              1,076.603 (df = 598)    
    ## F Statistic                   1,096.268*** (df = 10; 598)
    ## =========================================================
    ## Note:                         *p<0.1; **p<0.05; ***p<0.01

``` r
# prints out vif 
car::vif(income_fit)
```

    ##                       GVIF Df GVIF^(1/(2*Df))
    ## JobLevel          6.263038  1        2.502606
    ## TotalWorkingYears 2.693155  1        1.641083
    ## JobRole           4.443834  8        1.097703

<hr>
<center>
<h1>
Conclusion
</h1>
<center>
<hr>

Creating visual representation of the data was very helpful in
understanding any correlative relationships between the key response
variables and meaningful factors. upon investigating some of the
relationships there is a number of extra digging that can be done to
truly determine ways the company can address and improve the overall
percentage of attrition.

The 2 models utilized for prediction of employee attrition were Naieve
Bayes, and KNN. While both models were similarly accurate (NB - 83%,
KNN - 86%) the KNN model produced the best overall results and is the
model that we have chosen to deploy for our initial prediction of
attrition with the employees currently at the company.

One final takeaway and recommendation is that because each role in the
company or department does not have an equal proportion of attrition
within their respective spaces, they should be analyzed on a per basis
approach. While it would be nice to have an overall assumptive approach,
the pains employees experience within certain job roles vary and should
be addressed as such. Given more time and resources I would be
interested in trying to apply different feature selection approach such
as lasso, and also apply more potentially appropriate machine learning
models such a decision tree for predicting attrition.
