source("Slot_function_file.R")#loading function file


#Creating a matrix by combining the reel vectors, each column represent a reel
reel.matrix<-ReelMatrix()

#creating a matrix where the rows correspond to the reels for i.e row 1 
#corresponds to reel 1. The columns correspond to the top, middle, and bottom 
#display spots for each reel. The elements correspond to the row # of the reel
#matrix.I.E reel.matrix[window.matrix[1,1],1], will show the top element of the 
#display spot for reel 1. 
window<-c(1, 1, 1, 2, 2, 2, 3, 3, 3)
window.matrix<-matrix(window,ncol=3,nrow=3)
SlotManager(reel.matrix,window.matrix)