############################################################################
############################################################################
##                        importing the libraries                         
############################################################################
############################################################################

library(mlr)
library("taRifx")
library("tm")
library("caret")
set.seed(4205)

path = "Newsgroups-1\\Newsgroups"

############################################################################
############################################################################
##                            Creating the corpus                     
############################################################################
############################################################################

#function to build the vocabolary/corpus 
Corpus= function(path,word_sample){
  
  files =list.files(path = path)
  list_of_words=c()
  index=1
  n=length(files)
  for (i in 1:n){
    p = paste(path,"\\",files[i], sep = "")
  
    class_files=list.files(path = p)
    n1=length(class_files)
    for (j in 1:n1){
      p1=paste(p,"\\",class_files[j], sep="")
    
      Newsgroup_data = file(p1, open = "r")
      lines = readLines(Newsgroup_data)
      n2=length(lines)
      for (k in 1:n2){
        if(lines[k] == "") {
          next
        }
        if(lines[k]==" "){
          next
        }
        words=strsplit(lines[k]," ")[[1]]
        word_row=word_sample
        if(word_row>length(words)){
          word_row=length(words)
        }
        if(word_row==0){
          word_row=length(words)
        }
        for (w in 1:word_row){
          if(words[w] == "") {
            next
          }
          if(words[w]==" "){
            next
          }
          list_of_words[index]=words[w]
          index=index+1
        }
    
      }
    close(Newsgroup_data)
    }
  }

  tokens =list_of_words
  return(tokens)

}

#input word_sample=0 if no memory limit issues
tokens=Corpus(path,4)
#View(tokens)

#function to output top n most occuring words
top_n= function(tokens,n){
  
  data=data.frame(tokens)
  topn= sort(table(data),decreasing = T)
  return(head(topn,n))
}
top=top_n(tokens,200)
#View(top)


#function to build the vocabolary/corpus with word length in range 4-20
corpus_range=function(path,word_sample){
  
  files =list.files(path = path)
  list_of_words=c()
  index=1
  n=length(files)
  for (i in 1:n){
    p = paste(path,"\\",files[i], sep = "")
    
    class_files=list.files(path = p)
    n1=length(class_files)
    for (j in 1:n1){
      p1=paste(p,"\\",class_files[j], sep="")
      
      Newsgroup_data = file(p1, open = "r")
      lines = readLines(Newsgroup_data)
      n2=length(lines)
      for (k in 1:n2){
        if(lines[k] == "") {
          next
        }
        if(lines[k]==" "){
          next
        }
        words=strsplit(lines[k]," ")[[1]]
        word_row=word_sample
        if(word_row>length(words)){
          word_row=length(words)
        }
        if(word_row==0){
          word_row=length(words)
        }
        for (w in 1:word_row){
          if(words[w] == "") {
            next
          }
          if(words[w]==" "){
            next
          }
          if((nchar(words[w])<4)){
            next
          }
          if((nchar(words[w])>20)){
            next
          }
          list_of_words[index]=words[w]
          index=index+1
        }
        
      }
      close(Newsgroup_data)
    }
  }
  
  tokens =list_of_words
  return(tokens)
          
          
}

#input word_sample=0 if no memory constraint
range_tokens=corpus_range(path,4)
#View(range_tokens)


#top 200 most occuring words with word length in range 4-20
top_range=top_n(range_tokens,200)

############################################################################
############################################################################
##                        Creating the bag of words                     
############################################################################
############################################################################

#function to find bag of words
Bag_Of_Words= function(path,token,word_sample){
  
  token=unique(token)
  
  bag_of_words= data.frame(token)
  bag_of_words=t(bag_of_words)
  colnames(bag_of_words) = bag_of_words[1,]
  bag_of_words=bag_of_words[-1,]
  
  files =list.files(path = path)
  list_of_words=c()
  index=1
  n=length(files)
  for (i in 1:n){
    p = paste(path,"\\",files[i], sep = "")
    
    class_files=list.files(path = p)
    n1=length(class_files)
    for (j in 1:n1){
      p1=paste(p,"\\",class_files[j], sep="")
      
      Newsgroup_data = file(p1, open = "r")
      lines = readLines(Newsgroup_data)
      n2=length(lines)
      rowvar = rep(0, length(token))
      for (k in 1:n2){
        if(lines[k] == "") {
          next
        }
        if(lines[k]==" "){
          next
        }
        words=strsplit(lines[k]," ")[[1]]
        word_row=word_sample
        if(word_row>length(words)){
          word_row=length(words)
        }
        if(word_row==0){
          word_row=length(words)
        }
        for (w in 1:word_row){
          if(words[w] == "") {
            next
          }
          if(words[w]==" "){
            next
          }
          for (m in 1:length(token)){
            
            if(words[w] == token[m]) {
              rowvar[m] = rowvar[m] + 1
              break
            }
          }
          
          
        }
        
      }
      bag_of_words = rbind(bag_of_words, t(rowvar));
      
      
      
    }
    
  }
  class=c()
  for (i in (1:4)){
    for (j in (1:100)){
      class=append(class,i)
    }
  }
  bag_of_words=cbind(bag_of_words,class)
  return(data.frame(bag_of_words))
  
}
bag_of_words=Bag_Of_Words(path,tokens,4)


#Handling column names
data=bag_of_words
colname=colnames(data)
colnames(data)=c(paste("column",1:length(colname),sep=""))

#splitting the dataset
n=nrow(data)
train_data=sample(n,0.7*n)
test_data=setdiff(1:n,train_data)

#changing data class from factor to numeric
data=japply(data,which(sapply(data[-length(colname)], class)=="factor"),as.character)
data= japply(data,which(sapply(data[-length(colname)], class)=="character"),as.numeric)

#create a task
task=makeClassifTask(data = data,target=paste("column",length(colname),sep=""))
testdata=test_data[-length(colname)]

############################################################################
############################################################################
##                            Basic Evaluation                     
############################################################################
############################################################################

#Naive Bayes Self implemented
training[,ncol(data)]
training = data[train_data,]
testing = data[test_data,]
class1 = which(training[,ncol(data)] == "1")  
class2 = which(training[,ncol(data)] == "2")
class3 = which(training[,ncol(data)] == "3")
class4 = which(training[,ncol(data)] == "4")
prior_c1 = length(class1)/nrow(training)
prior_c2 = length(class2)/nrow(training)
prior_c3 = length(class3)/nrow(training)
prior_c4 = length(class4)/nrow(training)

x=ncol(data)-1
freqcountc1 = freqcountc2 = freqcountc3 = freqcountc4 =numeric(x)
posterior_p1 = posterior_p2 = posterior_p3 = posterior_p4 = numeric(x)

for (i in (1:x)){
  freqcountc1[i] = sum((data[class1,i]))
  freqcountc2[i] = sum(data[class2,i])
  freqcountc3[i] = sum(data[class3,i])
  freqcountc4[i] = sum(data[class4,i])
}
len_c1 = sum(freqcountc1)
len_c2 = sum(freqcountc2)
len_c3 = sum(freqcountc3)
len_c4 = sum(freqcountc4)


for (p in (1:x)){
  posterior_p1[p] = (freqcountc1[p] + 1)/(len_c1 + x)
  posterior_p2[p] = (freqcountc2[p] + 1)/(len_c2 + x)
  posterior_p3[p] = (freqcountc3[p] + 1)/(len_c3 + x)
  posterior_p4[p] = (freqcountc4[p] + 1)/(len_c4 + x)
}

p1 = p2 = p3 = p4 = 1
yres = c()

for (i in 1:120){
  for (k in (1:x)){
    if (testing[i,k]==0){next}
    p1 = p1*(posterior_p1[k]*testing[i,k]*prior_c1)
    p2 = p2*(posterior_p2[k]*testing[i,k]*prior_c2)
    p3 = p3*(posterior_p3[k]*testing[i,k]*prior_c3)
    p4 = p4*(posterior_p4[k]*testing[i,k]*prior_c4)
    yres[i] = which.max(c(p1,p2,p3,p4))
  }
}
y=x+1
y=paste("column",y,sep="")
yres <- as.factor(yres)
cm_nb=confusionMatrix(yres,testing$column15731)
plot(cm_nb$table)

#Naive Bayes Using Library
lrn_nb=makeLearner("classif.naiveBayes")
mod_nb=mlr::train(lrn_nb,task,subset = train_data)
pred_nb=predict(mod_nb,task,subset=test_data[-ncol(new_data)])
acc_nb=performance(pred_nb,measures = acc)
cm_nb_library=confusionMatrix(pred_nb[["data"]][["truth"]],pred_nb[["data"]][["response"]])
plot(cm_nb_library$table)

#KNN
lrn_knn=makeLearner("classif.knn",k=4)
mod_knn= mlr::train(lrn_knn,task,subset = train_data)
pred_knn=predict(mod_knn,task=task,subset = test_data)
acc_knn=performance(pred_knn,measures = acc)
cm_knn=confusionMatrix(pred_knn[["data"]][["truth"]],pred_knn[["data"]][["response"]])
plot(cm_knn$table)
#print(acc_knn)


#Random Forest
lrn_rf=makeLearner("classif.randomForest",predict.type = "prob")
mod_rf=mlr::train(lrn_rf,task,subset = train_data)
pred_rf=predict(mod_rf,task,subset=test_data[-length(colname)])
acc_rf=performance(pred_rf,measures = acc)
cm_rf=confusionMatrix(pred_rf[["data"]][["truth"]],pred_rf[["data"]][["response"]])
plot(cm_rf$table)
#print(acc_rf)

############################################################################
############################################################################
##                            Robust Evaluation                     
############################################################################
############################################################################

##Applying preprocessing techniques like lowercasing,whitespace,stopwords and punctuation removal, 
#function to build the vocabolary/corpus
preprocessed_corpus= function(tokens){
  
  tokens=tm::removePunctuation(tokens)
  tokens=tm::removeWords(tokens,tm::stopwords(kind = "en"))
  tokens=tolower(tokens)
  tokens=tm::stripWhitespace(tokens)
  
  return(tokens)
  
}

#bag of words creation input word_sample=0 if no memory limit issues
Preprocessed_Bag_Of_Words= function(tokens,word_sample){
  token=preprocessed_corpus(tokens)
  token=unique(token)
  
  bag_of_words= data.frame(token)
  bag_of_words=t(bag_of_words)
  colnames(bag_of_words) = bag_of_words[1,]
  bag_of_words=bag_of_words[-1,]
  
  files =list.files(path = path)
  list_of_words=c()
  index=1
  n=length(files)
  for (i in 1:n){
    p = paste(path,"\\",files[i], sep = "")
    
    class_files=list.files(path = p)
    n1=length(class_files)
    for (j in 1:n1){
      p1=paste(p,"\\",class_files[j], sep="")
      
      Newsgroup_data = file(p1, open = "r")
      lines = readLines(Newsgroup_data)
      n2=length(lines)
      rowvar = rep(0, length(token))
      for (k in 1:n2){
        if(lines[k] == "") {
          next
        }
        if(lines[k]==" "){
          next
        }
        words=strsplit(lines[k]," ")[[1]]
        word_row=word_sample
        if(word_row>length(words)){
          word_row=length(words)
        }
        if(word_row==0){
          word_row=length(words)
        }
        for (w in 1:word_row){
          if(words[w] == "") {
            next
          }
          if(words[w]==" "){
            next
          }
          words[w]=tm::removePunctuation(words[w])
          words[w]=tolower(words[w])
          for (m in 1:length(token)){
            
            if(words[w] == token[m]) {
              rowvar[m] = rowvar[m] + 1
              break
            }
          }
          
          
        }
        
      }
      bag_of_words = rbind(bag_of_words, t(rowvar));
      
      
      
    }
    
  }
  class=c()
  for (i in (1:4)){
    for (j in (1:100)){
      class=append(class,i)
    }
  }
  bag_of_words=cbind(bag_of_words,class)
  return(data.frame(bag_of_words))
  
}
new_data=Preprocessed_Bag_Of_Words(tokens,4)
#View(new_data$)
class(new_data$class.1)

############################################################################
############################################################################
##                            Feature Selection                     
############################################################################
############################################################################

#function to implement feature_selection
feature_selection=function(task){
  #feature selection
  fv = generateFilterValuesData(task, method = "FSelectorRcpp_information.gain")
  filtered.task = filterFeatures(task, fval = fv, perc = 0.1)
  return (filtered.task)
}

############################################################################
############################################################################
##                Cross Validation and Hyper Parameter tuning                     
############################################################################
############################################################################

#funciton to implementcross-validation and hyper parameter tuning
CV_Hyper_parameter_tuning=function(task,learner,ps){
  set.seed(4205)
  #Cross Validation
  rdesc = makeResampleDesc("CV", iters=3)
  #rsi=makeResampleInstance(rdesc,task)
  
  #hyperparameter tuning
  ctrl = makeTuneControlGrid()
  res = tuneParams(learner, task = task, resampling = rdesc, par.set = ps, control =ctrl, measures = acc)
  
  return(res)
}

#changing data class from factor to numeric
new_data=japply(new_data,which(sapply(new_data[-ncol(new_data)], class)=="factor"),as.character)
new_data= japply(new_data,which(sapply(new_data[-ncol(new_data)], class)=="character"),as.numeric)

#creating a new task after feature selection
new_task=makeClassifTask(data = new_data,target="class.1")
new_task=feature_selection(task)

############################################################################
############################################################################
##                            Applying Models                     
############################################################################
############################################################################

#Decision Tree
lrn_dt=makeLearner("classif.rpart")
#print(getParamSet(lrn_dt))
ps_dt=makeParamSet(makeIntegerParam("minsplit", lower=1, upper=100),
                  makeIntegerParam("maxdepth", lower=2, upper=50),
                  makeDiscreteParam("cp", values = seq(0.001, 0.006, 0.002)))



res_dt=CV_Hyper_parameter_tuning(new_task,lrn_dt,ps_dt)
#View(res_dt$x)
#View(res_dt$y)


lrn_dt=makeLearner("classif.rpart", minsplit=res_dt$x$minsplit, maxdepth=res_dt$x$maxdepth,cp=res_dt$x$cp)
mod_dt=mlr::train(lrn_dt,new_task,subset = train_data)
pred_dt=predict(mod_dt,new_task,subset=test_data[-ncol(new_data)])
acc_dt=performance(pred_dt,measures = acc)
cm_dt=confusionMatrix(pred_dt[["data"]][["truth"]],pred_dt[["data"]][["response"]])
plot(cm_dt$table)
#print(acc_dt)


#SVM
lrn_svm=makeLearner("classif.svm")
#print(getParamSet(lrn_svm))
ps_svm=makeParamSet(makeDiscreteParam("gamma", values = seq(1, 5, 0.5)),
                   makeDiscreteParam("cost", values = c(0.1,1,10,100,1000) ),
                   makeDiscreteParam("kernel", values = c("linear","polynomial")))
res_svm=CV_Hyper_parameter_tuning(new_task,lrn_svm,ps_svm)
#View(res_dt$x)
#View(res_dt$y)

lrn_svm=makeLearner("classif.svm", gamma=res_svm$x$gamma, cost=res_svm$x$cost,kernel=res_svm$x$kernel)
mod_svm=mlr::train(lrn_svm,new_task,subset = train_data)
pred_svm=predict(mod_svm,new_task,subset=test_data[-length(colname)])
acc_svm=performance(pred_svm,measures = acc)
cm_svm=confusionMatrix(pred_svm[["data"]][["truth"]],pred_svm[["data"]][["response"]])
plot(cm_svm$table)
#print(acc_svm)

#KNN Robust evaluation

lrn_knnr=makeLearner("classif.knn")
print(getParamSet(lrn_knnr))
ps_knnr=makeParamSet(makeDiscreteParam("k", values = seq(1,30,1))
)
res_knnr=CV_Hyper_parameter_tuning(new_task,lrn_knnr,ps_knnr)
#View(res_knnr$x)
#View(res_knnr$y)

lrn_knnr=makeLearner("classif.knn",k=res_knnr$x$k)
mod_knnr= mlr::train(lrn_knnr,task,subset = train_data)
pred_knnr=predict(mod_knnr,task=task,subset = test_data)
acc_knnr=performance(pred_knnr,measures = acc)
cm_knnr=confusionMatrix(pred_knnr[["data"]][["truth"]],pred_knnr[["data"]][["response"]])
plot(cm_knnr$table)
#print(acc_knnr)

#Random Forest robust evaluation
lrn_rfr=makeLearner("classif.randomForest")
#print(getParamSet(lrn_rfr))
ps_rfr=makeParamSet(makeDiscreteParam("ntree",values=seq(10,500,10))
)
res_rfr=CV_Hyper_parameter_tuning(new_task,lrn_rfr,ps_rfr)
#View(res_rfr$x)
#View(res_rfr$y)

lrn_rfr=makeLearner("classif.randomForest",ntree=res_rfr$x$ntree)
mod_rfr=mlr::train(lrn_rfr,task,subset = train_data)
pred_rfr=predict(mod_rfr,task,subset=test_data[-length(colname)])
acc_rfr=performance(pred_rfr,measures = acc)
cm_rfr=confusionMatrix(pred_rfr[["data"]][["truth"]],pred_rfr[["data"]][["response"]])
plot(cm_rfr$table)
#print(acc_rfr)

#Naive Bayes
#Applying feature selection
new_task=makeClassifTask(data = new_data,target="class.1")
fv = generateFilterValuesData(new_task, method = "FSelectorRcpp_information.gain")
new_task = filterFeatures(new_task, fval = fv, perc = 0.0125)
filtered_data=new_task[["env"]][["data"]]
training = filtered_data[train_data,]
testing = filtered_data[test_data,]
class1 = which(training$class.1 == "1")  
class2 = which(training$class.1 == "2")
class3 = which(training$class.1 == "3")
class4 = which(training$class.1 == "4")
prior_c1 = length(class1)/nrow(training)
prior_c2 = length(class2)/nrow(training)
prior_c3 = length(class3)/nrow(training)
prior_c4 = length(class4)/nrow(training)


freqcountc1 = freqcountc2 = freqcountc3 = freqcountc4 =numeric(x)
posterior_p1 = posterior_p2 = posterior_p3 = posterior_p4 = numeric(x)
x=ncol(filtered_data)-1
for (i in (1:x)){
  freqcountc1[i] = sum((data[class1,i]))
  freqcountc2[i] = sum(data[class2,i])
  freqcountc3[i] = sum(data[class3,i])
  freqcountc4[i] = sum(data[class4,i])
}
len_c1 = sum(freqcountc1)
len_c2 = sum(freqcountc2)
len_c3 = sum(freqcountc3)
len_c4 = sum(freqcountc4)


for (p in (1:x)){
  posterior_p1[p] = (freqcountc1[p] + 1)/(len_c1 + x)
  posterior_p2[p] = (freqcountc2[p] + 1)/(len_c2 + x)
  posterior_p3[p] = (freqcountc3[p] + 1)/(len_c3 + x)
  posterior_p4[p] = (freqcountc4[p] + 1)/(len_c4 + x)
}

p1 = p2 = p3 = p4 = 1
yres = c()

for (i in 1:120){
  for (k in (1:x)){
    if (testing[i,k]==0){next}
    p1 = p1*(posterior_p1[k]*testing[i,k]*prior_c1)
    p2 = p2*(posterior_p2[k]*testing[i,k]*prior_c2)
    p3 = p3*(posterior_p3[k]*testing[i,k]*prior_c3)
    p4 = p4*(posterior_p4[k]*testing[i,k]*prior_c4)
    yres[i] = which.max(c(p1,p2,p3,p4))
  }
}

yres<-as.factor(yres)
cm_nbr=confusionMatrix(yres,filtered_data$class.1)
