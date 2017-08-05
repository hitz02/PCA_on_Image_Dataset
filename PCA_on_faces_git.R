install.packages("R.matlab")
library(R.matlab)

faces <- readMat("faces-dataset.mat")
X_faces <- faces$X

#Your task: Display the number of rows and columns in the matrix X

dim(X_faces) #5000 1024

face1 <- X_faces[1,] #assign 1st image to img1
face1_m <- matrix(face1, 32, 32) #reshape the matrix
image(face1_m, axes = FALSE, col = grey(seq(0, 1, length = 256))) #this will display the image in the plots

#Your task: Display images 2, 3, 4 of the matrix X and store them in variables img2, img3, img4

face2 = X_faces[2,]
face3 = X_faces[3,]
face4 = X_faces[4,]

face2_m = matrix(face2,32,32)
face3_m = matrix(face3,32,32)
face4_m = matrix(face4,32,32)

#Lets plot all the 4 images

op <- par(mfrow=c(2,2))
image(face1_m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(face2_m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(face3_m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(face4_m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
par(op)


#Your task: Apply PCA on X

X_faces_PCA = prcomp(X_faces, center = T)
X_faces_PCA
summary(X_faces_PCA)

plot(X_faces_PCA, type = "l")


# dim(X_faces_PCA$)

dim(X_faces_PCA$rotation)[1,1:5]

#Taking 96 as it is multiple of 32 and subsequently 192 and 384.

z1 = t(X_faces_PCA$rotation[,1:96])%*%X_faces[1,]
z2 = t(X_faces_PCA$rotation[,1:96])%*%X_faces[2,]
z3 = t(X_faces_PCA$rotation[,1:96])%*%X_faces[3,]
z4 = t(X_faces_PCA$rotation[,1:96])%*%X_faces[4,]


# z(i) = t(Ureduced)*x(i) [k*1= k*n   n*1]
# n*1 = n*k K*1


Xhat1 = matrix(X_faces_PCA$rotation[,1:96]%*%z1, 32, 32)
Xhat2 = matrix(X_faces_PCA$rotation[,1:96]%*%z2, 32, 32)
Xhat3 = matrix(X_faces_PCA$rotation[,1:96]%*%z3, 32, 32)
Xhat4 = matrix(X_faces_PCA$rotation[,1:96]%*%z4, 32, 32)

#Your task: Plot the original images and their approximations, 
#i.e. img1 and Xhat1, img2 and Xhat2, and so on for the first 4 images and observe
# the approximation quality

op <- par(mfrow=c(2,4))
image(face1_m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(Xhat1, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(face2_m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(Xhat2, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(face3_m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(Xhat3, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(face4_m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(Xhat4, axes = FALSE, col = grey(seq(0, 1, length = 256)))
par(op)

#Your task: Repeat the process of computing Z and Xhat and plot of original and approximate
#images by increasing the number of PCA components, from 100 to 200 to 400

#PCA Components - 192

z1 = t(X_faces_PCA$rotation[,1:192])%*%X_faces[1,]
z2 = t(X_faces_PCA$rotation[,1:192])%*%X_faces[2,]
z3 = t(X_faces_PCA$rotation[,1:192])%*%X_faces[3,]
z4 = t(X_faces_PCA$rotation[,1:192])%*%X_faces[4,]

Xhat1 = matrix(X_faces_PCA$rotation[,1:192]%*%z1, 32, 32)
Xhat2 = matrix(X_faces_PCA$rotation[,1:192]%*%z2, 32, 32)
Xhat3 = matrix(X_faces_PCA$rotation[,1:192]%*%z3, 32, 32)
Xhat4 = matrix(X_faces_PCA$rotation[,1:192]%*%z4, 32, 32)

op <- par(mfrow=c(2,4))
image(face1_m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(Xhat1, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(face2_m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(Xhat2, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(face3_m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(Xhat3, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(face4_m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(Xhat4, axes = FALSE, col = grey(seq(0, 1, length = 256)))
par(op)

#PCA Components - 384

z1 = t(X_faces_PCA$rotation[,1:384])%*%X_faces[1,]
z2 = t(X_faces_PCA$rotation[,1:384])%*%X_faces[2,]
z3 = t(X_faces_PCA$rotation[,1:384])%*%X_faces[3,]
z4 = t(X_faces_PCA$rotation[,1:384])%*%X_faces[4,]

Xhat1 = matrix(X_faces_PCA$rotation[,1:384]%*%z1, 32, 32)
Xhat2 = matrix(X_faces_PCA$rotation[,1:384]%*%z2, 32, 32)
Xhat3 = matrix(X_faces_PCA$rotation[,1:384]%*%z3, 32, 32)
Xhat4 = matrix(X_faces_PCA$rotation[,1:384]%*%z4, 32, 32)

op <- par(mfrow=c(2,4))
image(face1_m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(Xhat1, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(face2_m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(Xhat2, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(face3_m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(Xhat3, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(face4_m, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(Xhat4, axes = FALSE, col = grey(seq(0, 1, length = 256)))
par(op)