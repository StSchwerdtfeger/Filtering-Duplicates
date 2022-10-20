# For new release of current preprint by Rico Schmitt:
https://journal.medicine.berlinexchange.de/pub/nqjpou17/release/1

# Filtering function für Rico:

filtering = function(x,y){ # x = id column vector; y = full data set
  library(dplyr) # load dplyr
  inter = duplicated(x)*1  # duplicated gives a logical, multiplied by 1
                           # transforms it into a binary vector         
  binded = as.data.frame(cbind(inter,x)) # bind id column and binary vector
                                         # + change class to data frame,
                                         # otherwise upcoming dplyr filter 
                                         # function wont work:   
  
  inter2=filter(binded, inter == "1" )   # filter rows with binary 0 :=
                                         # explicit non (!) duplicate values
  
  blank = matrix(0,length(x))            # matrix vector list for the for loop:
  
  for(i in 1:length(inter2$x)){          # for each individual[i] value of inter2
                                         # i.e., each value with duplicates
     for(j in 1:length(x)){              # + for each respective duplicate
                                         #  value in column vector with the ids
       if(x[j] == inter2$x[i]){          # if i_dth value is equivalent to 
                                         # the evaluated explicit duplcate id values
          blank[j] = 1  # then / in such a case assing a 1 to the redundant binary/logical column
       }
     }
   }
  
  data_frame = cbind(y,blank)   # Now combinde the redundant binary column 
                                # with the whole data frame that carries 
                                # the id column
  data_frame = as.data.frame(data_frame) # turn it into a data frame
  zzzz=filter(data_frame, blank != "1" ) # ... in order to finaly filter 
                                         # all redundant id values, no 
                                         # matter how many dubplicates there are,
    #  in other words: if any id value had a duplicate, both the original 
    #  and duplicate were erased, due to the fact that column and row wise
    #  relation cannot be evaluated unabivalently. 
  return(zzzz) # return zzzz returns the values of the respective object, 
               # i.e., the filtered full data frame
} # End of function
  

