<!-- --- -->
<!-- class: inverse, left, top -->
<!-- ### 1. Read in some data -->
<!-- ```{r message=FALSE, warning=FALSE} -->
<!-- # Read in retention file. These data are "synthetic" -->
<!-- d <- read.csv("data/retention_file_raw.csv",stringsAsFactors = F) #<< -->
<!-- ``` -->
    
    
    ---
    class: center, middle
background-image: url(/images/handup.jpeg)
background-size: contain
<!-- url https://unsplash.com/photos/DRzYMtae-vA -->
    <!-- Nadine Shaabana -->
    ### Let's Talk About The Data
    ---
    
    class: inverse, center, top
background-image: url(titanic.jpg)
background-size: contain
#### The data set is one of the most common in Machine Learning
---
    class: inverse, left, top
```{r echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}
t <- read.csv("https://gist.githubusercontent.com/michhar/2dfd2de0d4f8727f873422c5d959fff5/raw/fa71405126017e6a37bea592440b4bee94bf7b9e/titanic.csv",stringsAsFactors = F)

t %<>% clean_names(.)

t.agg <- t %>%
    select(-ticket,-name,-cabin) %>%
    as.data.frame(.)


kable(head(t.agg),align = "l",caption = "Titanic") %>% column_spec(2, bold = T, color = "white", background = "green") %>% kable_styling(font_size = 12)
```

<br>
    <br>
    --
    ```{r echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}
d.sub <- d %>% select(student_id,retained,income_group,sex,age,siblings_enrolled,net_tuition,residency)  

kable(head(d.sub),align = "l",caption = "Titanic -> Retention") %>% kable_styling(font_size = 12) %>% column_spec(2, bold = T, color = "white", background = "green")
```
---
    class: inverse,center,middle
### Titanic Model
```{r, echo=FALSE, message=FALSE, warning=FALSE}
library(fastDummies)
library(scales)
t <- read.csv("https://gist.githubusercontent.com/michhar/2dfd2de0d4f8727f873422c5d959fff5/raw/fa71405126017e6a37bea592440b4bee94bf7b9e/titanic.csv",stringsAsFactors = F)

t %<>% clean_names(.)

t.agg <- t %>%
    select(-ticket,-name,-cabin) %>%
    mutate(income_group = case_when(pclass == 1 ~ "first_class",
                                    pclass == 2 ~ "second_class",
                                    pclass == 3 ~ "third_class"),
           group_size = sib_sp + parch) %>%
    mutate_at(c("fare","age"), ~ scale(.)) %>%
    as.data.frame(.)

t.agg %<>% dummy_columns(.,select_columns = c("sex","income_group"))


mod.2 <- glm(survived ~  #<<
                 group_size + #<<
                 fare + #<<
                 sex_female + #<<
                 income_group_first_class, #<<
             data = t.agg, #<<
             family = "binomial") #<<
```

```{r echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}
kable(tidy(mod.2,exponentiate = TRUE),digits = 3) %>% kable_styling() %>%
    row_spec(5, bold = T, color = "white", background = "green") %>%
    row_spec(4, bold = T, color = "white", background = "green") %>%
    row_spec(2, bold = T, color = "white", background = "red")
```

--
    ### Interpretation 
    
    --
    High income passengers in first class were more likely to survive because their cabins were closer to lifeboats


--
    Female passengers were more likely to survive because social norms of the day put them first


        