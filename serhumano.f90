program overlap
real*4 chi(1000),s(1000),a,epsilon(1000,1000),h(1000),ran(1000),w(1000,9999),the(1000),p(1000),m(1000,1000),l,actu(1000)
integer tem,t
a=0.5
print*, "scribe el n£mero de neuronas"

do tem=1,100
m(tem,1)=0

end do
read*,n
do i=1,n
ran(i)=rand()
h(i)=0
the(i)=0

p(i)=0
chi(i)=rand()
if (chi(i)<0.5) then
chi(i)=1
else
chi(i)=0
end if

end do
do i=1,n
s(i)=rand()
if (s(i)<0.5) then
s(i)=1
else
s(i)=0
end if
actu(i)=s(i)


end do





do i=1,n
do j=1,n
if (i==j) then
epsilon(i,j)=0
else
epsilon(i,j)=1
end if

end do
end do




do i=1,n
do j=1,n
if(i==j) then
w(i,j)=0
else
w(i,j)=(1/(a*(1-a)*n))*(chi(i)-a)*(chi(j)-a)
end if


end do
end do
do i=1,n
do j=1,n
the(i)=the(i)+0.5*w(i,j)
end do

end do


   do tem=1,100
   do i=1,n
      s(i)=actu(i)
      end do
   do t=1,9999
     do i=1,n
   h(i)=0

   end do
   do i=1,n
     do j=1,n
       h(i)=h(i)+w(i,j)*(s(j))*epsilon(i,j)
       end do

       end do
 do i=1,n

       p(i)=0.5*(1+tanh((20/(0.1*tem))*(h(i)-the(i))))


       if(ran(i)<p(i)) then
       s(i)=1
       else
       s(i)=0
       end if

       end do



       do i=1,n
       m(tem,t)=m(tem,t)+(1/(a*(1-a)*n))*(chi(i)-a)*(s(i)-a)
       end do
       end do

             print*,tem, m(tem,9998)
       end do
       read*,l
       end program
