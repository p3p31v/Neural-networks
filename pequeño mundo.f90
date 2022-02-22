program pequemundo
real s(100),w(100),d
integer i,m,k,N,j,t,ran,ran2
print*, "escribe el numero de vecinos a un lado m"
read*,m
k=2*m
!inicio los valores de las s
do i=1,N
s(i)=rand()
if (s(i)>0.5) then
s(i)=1
else
s(i)=-1
end if
end do



do t=1,100
!el ran genera un intervalo entre 0 y k

do i=1,N
ran2=rand()
ran=int(m*rand())
if(ran2>0.5) then
s(i)=s(i+ran)
else
s(i)=s(i-ran)
end if
end do
end do









 do i=1,N
 w(i)=0
 end do
!calculo la probabilidad de cada i en el primer paso temporal
do t=1,100
    ran=rand()
do i=1,N
  do j=1,N
w(i)=w(i)+(d/4)*(1-(1/(2*d))*s(i)*s(j))
end do
!no se si lo siguiente es mayor o menor, dejemoslo para luego lol
if(ran>w(i))then
s(i)=-s(i)
end if


end do
end do





end program


