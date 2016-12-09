library(haven)
require(dplyr)
require(ggplot2)
require(magrittr)
require(mirt)
require(tidyr)

Dataset_Pew_Research_Center_2014_Religious_Landscape_Study_National_Telephone_Survey_Version_1_1_December_1_2016 <- read_sav("~/R_Projects/pew/Dataset - Pew Research Center 2014 Religious Landscape Study National Telephone Survey - Version 1.1 - December 1 2016.sav")

data <- Dataset_Pew_Research_Center_2014_Religious_Landscape_Study_National_Telephone_Survey_Version_1_1_December_1_2016

#Need to use survey weights. Won't change actual analysis very much, but still a good thing to do

weights <- Dataset_Pew_Research_Center_2014_Religious_Landscape_Study_National_Telephone_Survey_Version_1_1_December_1_2016$WEIGHT

# Keep variables

data %<>% select(qi2g,qi2d,qi2b,qi2a,qi1,qh2,qh1,qg7b,qg7,qg6,qg5,qg1c,qg3,qg1,qg1b,qf2,
                 attend,born,racethn,marital,qb31,qb30,qb22,qb21,qb20,
                 qb21,qb22,qb20,qb2a,qb2c,qb2b,qb2d,qb1c,qb1b,qb1a,qa1,qi3,qp3rec,educ,agerec,
                 qm5a,qm5b,qm5d,qm5e,qm5f,qm5g,party,ideo,income,RELTRAD)

data %<>% mutate_all(as_factor) %>% mutate_all(as.character)

data %<>% mutate_all(funs(ifelse(grepl(x=.,pattern="VOL|NaN|[Dd]on't know|[Rr]efused|Undesignated"),NA,.)))

ordinal_vars <- c('qi2g','qi2d','qi2b','qi2a','qi1','qg1b','qf2','attend','qb22','qb21','qb1c',
                  'qb1b','qb1a','qa1','educ','agerec','ideo','income')
binary_vars <- c('qh2','qh1','qg7','qg7b','qg6','qg5','qg1c','qg3','qg1','born','qb30','qb20',
                 'qb2a','qb2c','qb2b','qb2d','qi3','qp3rec','qm5a','qm5b','qm5d','qm5e','qm5f','qm5g')
nominal_vars <- names(data)[!(names(data) %in% c(ordinal_vars,binary_vars))]

all_vars <- c(ordinal_vars,binary_vars,nominal_vars)

# Order by these categories to make it easier to code them for the IRT model

data %<>% select(one_of(all_vars))

item_types <- c(rep('graded',length(ordinal_vars)),
                rep('2PL',length(binary_vars)),
                rep('nominal',length(nominal_vars)))

data %<>% mutate_all(as.factor)

numeric <- data.matrix(data)
colnames(numeric) <- names(data)

#Unidimensional 2PL Rasch combined ordinal/binary/nominal factor analysis
model <- mirt(numeric,model=2,itemtype=item_types,survey.weights=weights)

all_coefs <- coef(model)

all_coefs$GroupPars <- NULL

ordinal_coefs <- all_coefs[item_types=='graded' | item_types=='2PL']
nominal_coefs <- all_coefs[item_types=='nominal']
ordinal_coefs <- lapply(ordinal_coefs,function(x) {
  x <- x[grepl(pattern='a[0-9]',x=attr(x,'dimnames')[[2]])]
  return(x)})

ordinal_coefs$GroupPars <- NULL

ordinal_data <- as_data_frame(ordinal_coefs)
ordinal_data$discrim <- c('First','Second')
ordinal_data %<>% gather(parameter,score,-discrim)

#Match back in full labels

all_var_labels <- sapply(Dataset_Pew_Research_Center_2014_Religious_Landscape_Study_National_Telephone_Survey_Version_1_1_December_1_2016,attr,'label')
labels_key <- data_frame(var_labels=as.character(all_var_labels),
                         var_ids=as.character(names(Dataset_Pew_Research_Center_2014_Religious_Landscape_Study_National_Telephone_Survey_Version_1_1_December_1_2016)))


ordinal_data %<>% mutate(labels=labels_key$var_labels[match(parameter,labels_key$var_ids)])


