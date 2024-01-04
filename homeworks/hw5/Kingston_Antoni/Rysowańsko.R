#tło
choinka <- matrix(rep("#000000",25600),ncol=160)
#pień
for (i in 129:160)
{
  for(j in 70:90)
  {
    choinka[i,j] <- "#964B00"
  }
}
#pierwszy trapez
for (i in 105:128)
{
  for (j in as.integer(round((80-24)-(i-105)*(5/3))):as.integer(round((80+24)+(i-105)*(5/3))))
  {
    choinka[i,j] <- "#00FF00"
  }
}
#drugi trapez
for (i in 81:104)
{
  for (j in as.integer(round((80-16)-(i-81)*(5/3))):as.integer(round((80+16)+(i-81)*(5/3))))
  {
    choinka[i,j] <- "#00FF00"
  }
}
#trzeci trapez
for (i in 57:80)
{
  for (j in as.integer(round((80-8)-(i-57)*(5/3))):as.integer(round((80+8)+(i-57)*(5/3))))
  {
    choinka[i,j] <- "#00FF00"
  }
}
#górny trójkąt
for (i in 25:56)
{
  for (j in as.integer(round((80)-(i-25))):as.integer(round((80)+(i-25))))
  {
    choinka[i,j] <- "#00FF00"
  }
}
#trzy pętle od gwiazdy
for (i in 9:40)
{
  for (j in as.integer(round((80)-(i-9)*(1/4))):as.integer(round((80)+(i-9)*(1/4))))
  {
    choinka[i,j] <- "#FF0000"
  }
}
for (i in 26:40)
{
  for (j in as.integer(round((80)-(i-26)*1)):as.integer(round((80)+(i-26)*1)))
  {
    choinka[i,j] <- "#00FF00"
  }
}
for (i in 17:26)
{
  for (j in as.integer(round((70)+(i-17)*1.2)):as.integer(round((90)-(i-17)*1.2)))
  {
    choinka[i,j] <- "#FF0000"
  }
}
  






image_width <- ncol(choinka)
image_height <- nrow(choinka)


plot(1, type = "n", xlim = c(0, image_width+1), ylim = c(0, image_height+1), xlab = "", ylab = "")


rgb_colors <- col2rgb(choinka)
a=1

for (i in 1:image_width) {
  for (j in image_height:1) {
    rect(i - 0.5, j - 0.5, i + 0.5, j + 0.5, col = rgb(rgb_colors[1, a], rgb_colors[2, a], rgb_colors[3, a], maxColorValue = 255), border = NA)
    a=a+1
  }
}
