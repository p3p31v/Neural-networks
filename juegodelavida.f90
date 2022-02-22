program juegodelavida
real a(100,100)
integer i,j,n,b,t

n=50

do i=1,n
do j=1,n
a(i,j)=rand()
if(a(i,j)<0.5) then
a(i,j)=1
else
a(i,j)=0
end if

end do
end do

 do t=1,4
do i=2,n-1
do j=2,n-1
if (a(i,j)==0) then
b=a(i-1,j-1)+a(i-1,j)+a(i-1,j-1)+a(i,j-1)+a(i,j-1)+a(i,j)+a(i,j+1)+a(i+1,j-1)+a(i+1,j)+a(i+1,j+1)
if (b>=3) then
a(i,j)=1
end if
else
b=a(i-1,j-1)+a(i-1,j)+a(i-1,j-1)+a(i,j-1)+a(i,j-1)+a(i,j)+a(i,j+1)+a(i+1,j-1)+a(i+1,j)+a(i+1,j+1)
if (b<2) then
a(i,j)=0
else if (b==2.or.b==3) then
a(i,j)=1
else if (b>3) then
a(i,j)=0
end if
end if
end do
end do



do i=1,n
do j=1,n
print*,"a,i,j,t",a(i,j),i,j,t
end do
end do




 !tiempo
end do






end program


