                             program pequemundo
real s(9000),w(100),d,rano(9000),ran2(9000),rana(9000),ran(9000)
integer i,m,k,N,j,t,i1,j1,a
print*, "escribe el numero de vecinos a un lado m"
read*,m
k=2*m

N=40
!inicio los valores de las s
do i=1,N
s(i)=rand()
if (s(i)>0.5) then
s(i)=1
else
s(i)=-1
end if

print*, "s(i)", s(i)
end do
 !numero aleatorio que va de cero a N es N*rand
        !elijo unvalor entre cero y N

             !solo necesitas poner prints e identificar cadas pararepresentar lassfrente al tiempo
        do t=1,8500
        rano(t)=rand()
        rana(t)=rand()
        end do




do t=1,1000

ran(t)=int(N*rano(t))



i1=ran(t)   !se lo asocio a i
  ran2(t)=i1+int(m*rano(t))


  a=0
do while (ran2(t)>40)

ran2(t)=ran2(t)-40
print*,"holi"
                 end do


j1=ran2(t)

s(i1)=s(j1)



end do

do i=1,N
print*,"s(i) actualizado", s(i)
end do

read*, d




end program
