program atrayentedelorentz
real h,x,y,z,a,b,c,l
integer i,n
h=0.01
a=10
b=28
c=8/3
x=1
y=1
z=1
n=2000

do i=1,n
x=x+h*a*(y-x)
y=y+h*(x*(b-z)-y)
z=z+h*(x*y-c*z)
print*,x,y

end do
read*,l
end program


