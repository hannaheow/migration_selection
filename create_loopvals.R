# ki = seq(-200, 200, by = 50)
# kj = ki
# kl = ki
# kk = ki

ki = seq(-125, 125, by = 50)
kj = ki
kl = ki
kk = ki 


testcon = file("temp_inc25.txt", open = "a")
isOpen(testcon)


for(i in 1:length(ki)) {
  for(j in 1:length(kj)) {
    for (k in 1:length(kk)) {
      for (l in 1:length(kl)) {
        cat(paste(ki[i], kj[j], kk[k], kl[l], sep = ", "), file = testcon, append = TRUE, sep = "\n")
      }
    }
  }
}
close(testcon)



files = list.files("C:/Users/holsonwillia/Documents/fromchtc", full.names = TRUE)

chtctot = data.frame()

for (i in 1:length(files)) {
  load(files[i])
  tempdat = bickij_int
  chtctot = rbind(chtctot, tempdat)
}

save(chtctot, file = "C:/Users/holsonwillia/Documents/fromchtc/chtctot.RData")


#checking to see which combos are missing 
plot(x = chtctot$kur, y = chtctot$kru)
plot(x = chtctot$kur, y = chtctot$kuu)
plot(x = chtctot$kur, y = chtctot$krr)
plot(x = chtctot$kuu, y = chtctot$krr)
plot(x = chtctot$kuu, y = chtctot$kru)
plot(x = chtctot$kuu, y = chtctot$kur)
plot(x = chtctot$kru, y = chtctot$kur)
plot(x = chtctot$kru, y = chtctot$krr)
plot(x = chtctot$kru, y = chtctot$kuu)
plot(x = chtctot$krr, y = chtctot$kur)
plot(x = chtctot$krr, y = chtctot$kru)
plot(x = chtctot$krr, y = chtctot$kuu)
lattice::wireframe(as.numeric(kuu) ~ as.numeric(kur) * as.numeric(kru), data = chtctot)
lattice::wireframe(as.numeric(kuu) ~ as.numeric(kur) * as.numeric(krr), data = chtctot)


lattice::wireframe(as.numeric(krr) ~ as.numeric(kur) * as.numeric(kru), data = chtctot)
lattice::wireframe(as.numeric(krr) ~ as.numeric(kur) * as.numeric(kuu), data = chtctot)


lattice::wireframe(as.numeric(kru) ~ as.numeric(kur) * as.numeric(kuu), data = chtctot)
lattice::wireframe(as.numeric(kru) ~ as.numeric(kur) * as.numeric(krr), data = chtctot)




lattice::wireframe(bicspat ~ as.numeric(kur) * as.numeric(kuu), data = chtctot)
lattice::wireframe(bicspat ~ as.numeric(kru) * as.numeric(krr), data = chtctot)
lattice::wireframe(bicspat ~ as.numeric(kru) * as.numeric(kuu), data = chtctot)
lattice::wireframe(bicspat ~ as.numeric(kur) * as.numeric(krr), data = chtctot)

#the best ones are kur, krr and kru, krr 
# meaning that urban to urban migration does not have a clear pattern 


#the best fitting model has urban to rural and rural to urban migrants being slightly less healthy than non movers
#meanwhile, urban to urban movers and rural to rural moves are no different than non movers 
chtctot[chtctot$bicspat == min(chtctot$bicspat),]