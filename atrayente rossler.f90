program atrayentedelorentz
real h,x,y,z,a,b,c,l
integer i,n
h=0.1
a=0.1
b=0.1
c=14
x=0.2
y=0.2
z=0.2
n=2000

do i=1,n
x=x+h*(-y-z)
y=y+h*(x+a*y)
z=z+h*(b+z*(x-c))
print*,x,y

end do
read*,l
end program
