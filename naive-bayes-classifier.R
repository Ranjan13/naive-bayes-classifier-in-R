metric_calc <- function(tdata, test_data) {
lastCol = ncol(tdata)
classes = unique(tdata[ ,lastCol])
classes = as.vector(classes)

# Number of classes
nclass = length (classes)

# Counting Attribute values with respect to indexes For each Class
flag = 0
lst = list( )
for(i in 1:nclass) {
	for(j in 1:(lastCol-1)) {
		index = which (tdata[ ,lastCol] == classes[i])
		v = table (tdata[index, ][ ,j]) 
		if(flag == 0) {
			lst[[j]] = v 
		} else {
			lst[[j]] = rbind(lst[[j]], v)	
		}
	}
	flag = 1 
}

# Naming the list heading with attribute name
  names(lst) = colnames(tdata[ ,-lastCol])
# Naming the row with class label
  for(i in 1:length(lst)) {
  rownames(lst[[i]]) = classes
}
  cat("\nCalculated Statistics:","\n\n")
#  print(lst)
  probability_calc (classes, test_data, tdata, lst, lastCol)
}
###############################################################################

probability_calc <- function(classes, vtest, tdata, lst, lastCol ) {
max.ind <- 0
cal.label = c( )
print("Test SET::")
vtest_label = as.vector(vtest[ ,ncol(vtest)])
vtest = vtest[ ,-ncol(vtest)]

for(i in 1:nrow(vtest)) {
  prob <- c()
  test.inst = vtest[i, ]
  for(c in classes) {
  	
	p_ai <- 1
	cindex = which(c == classes)
	count_c = length (which (tdata[ ,lastCol]  == classes[cindex]))
	P = count_c/length (tdata[ ,lastCol])
	for(val in test.inst) {
		val_v = as.vector (as.matrix (val))
		cat("\nval_v : ", val_v,"\n")
		ind = which (val_v == test.inst)
		for(indo in ind) {
		cat("Indexes: ",indo,"\n")
		cname = colnames (test.inst[indo])
		cat("Col names: ",cname,"\n")
		p_ai <- p_ai *((lst[[cname]][c, val_v])/count_c) 
		}
		print(p_ai)
	}
	P = prod(P,p_ai)
	prob = c(prob, P)
  }
  max.val <- max (prob)
  max.ind <- which(prob %in% max.val)
  max.ind <- max.ind[1]
  cal.label = c(cal.label, classes[max.ind])
}
#cat ("\nCalculated Label : ",cal.label,"\n")
#cat ("\nTest label : ",vtest_label,"\n")
	error_cal(cal.label,vtest_label)
}

###############################################################################

# Error Calculation 

error_cal <- function(cal_lbl,test_lbl) {
	len_cal_lbl=length(cal_lbl)
	len_test_lbl=length(test_lbl)
	match_count=0;
	if(len_cal_lbl == len_test_lbl) {
	 for(i in 1:len_cal_lbl) {
	  if( cal_lbl[i] == test_lbl[i]) {
		 match_count = match_count + 1;
		}
	     error_per <- (match_count / len_cal_lbl) * 100
	   }
	} else {
          cat("\nError::Label Not Matched\n\n")
	  quit()
	}
	cat("\n\n Accuracy  :",error_per,"%","\n\n")
}


###############################################################################




###############################################################################
preprocess <- function(){
	args <- commandArgs(TRUE)
	if(length(args)!=3){
	cat("\n Command Line Arguement Error \n")
	cat("\nRscript <prog_name.R> <InputDataset> <1>(header=TRUE) <1>(row.numbering = TRUE)  \n\n ")
	
	quit()
	}
	input<-args[1]

#Pass Arguement 2 as 1 if header exists else as 0 if header doen't exists
	if(args[2] == 1){
	dataset = read.table(input,header=TRUE)
	} else {
		dataset = read.delim(input,header=FALSE,sep=",")
	}
#Pass Arguement 3 as 1 if rowname exists in the dataset
	if(args[3] == 1){
		dataset = dataset[ ,-1]
	}	
	train_index = sample(1:nrow(dataset),size = 0.75*nrow(dataset))
	train_data = dataset[train_index, ]
	
	cat("\n75% Trainning Data::","\n")

	test_data = dataset[-train_index, ]

        cat("\n\n25% Test Data::","\n\n")

  	metric_calc(train_data, test_data)
        }

###############################################################################




preprocess()



