 program hopfield

integer i,n,t,j,k,tem ,g,pa

real l,m

      real*8, allocatable :: epsilon(:,:), chi(:,:), s(:,:),x(:,:),ov(:,:),prob(:),po(:),pe(:),the(:)
      real*8, allocatable ::    suma(:),promedio(:),h(:),we(:,:)
      allocate (epsilon(2000,2000))

      allocate  (chi(2000,2000))
       allocate  (s(2000,30000))


        allocate (x(2000,2000))

         allocate (ov(110,30000))
         allocate (prob(2000))
         allocate (po(2000))
         allocate (pe(2000))
         allocate (the(2000))
         allocate (we(2000,2000))
         allocate (suma(110))
         allocate (promedio(110))
         allocate (h(2000))



pa=0
k=40
 do tem=1,10
 do t=1,30000
ov(tem,t)=0
end do
end do
print*, "escribe el número de neuronas"

read*,n
  do j=1,n
 do i=1,n
x(i,j)=rand()
the(i)=0
 end do
 end do


do i=1,n

           h(i)=0

           !se construyen los patrones aleatorios con 0.5 de probabilidad de tener valor 0 o 1
		chi(i,1)=rand()
			if (chi(i,1)>0.5) then
				chi(i,1)=1
			else
				chi(i,1)=0
				end if

				s(i,1)=rand()

				if (s(i,1)>0.5) then
				s(i,1)=1
			else
				s(i,1)=0
				end if





end do
read*, l
!se crea la red "fully conected" con todos los elementos igual a uno menos la diagonal
do i=1,n

	do j=1,n
		if (i==j) then
			epsilon(i,j)=0
		else
			epsilon(i,j)=1
		end if

	end do

end do
  do tem=100,100
  do i=1,n
       the(i)=0
        h(i)=0
       end do
do i=1,n
!se construye la matriz de pesos
do j=1,n
if (i==j)   then
we(i,j)=0
else
we(i,j)=(2*chi(i,1)-1)*(2*chi(j,1)-1)/n
end if
!se construye el valor del limite (threshold)
the(i)=the(i)+0.5*we(i,j)
end do
end do
do t=1,30000
do i=1,n
!hallamos la entrada h de las corrientes sin pticas que llegan a la neurona i
do j=1,n
h(i)=h(i)+we(i,j)*s(j,t)*epsilon(i,j)
end do  !jotas
 po(i)=(h(i)-the(i))*20/(0.1*tem)
!calculamos la probabilidad de que el estado de la neurona i sea 1 en un tiempo t+1
  prob(i)=0.5*(1+tanh(po(i)))

       if(x(i,3)<prob(i)) then
       s(i,t+1)=1
       else
       s(i,t+1)=0
       end if



end do  !ies
     do i=1,n
                   if (t>0) then
       ov(tem,t)=ov(tem,t)+((2*chi(i,1)-1)*(2*s(i,t)-1))/n

       end if
       end do
       print*,"ov de 80, t", ov(tem,t),t
  do i=1,n
  h(i)=0
  end do
         suma(tem)=suma(tem)+ov(tem,t)
end do  !tiempo
promedio(tem)=suma(tem)/10000




!calculo la funci¢n overlap

   print*, tem, promedio(tem)
 end do   !temperatura
       DEALLOCATE(epsilon,chi,s,x,ov,prob,po,pe,the,suma,promedio,h,we)

       read*,l
end program
