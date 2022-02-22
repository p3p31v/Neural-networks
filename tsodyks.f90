program tsodyks
real taurec,tauin,taufac,tsp,use,u,taum,Rin,x,y,z,ase,h,l
integer i,n,t
tauin=3
taurec=800
use=0.5
h=0.01
  taum=1
  taufac=530
  rin=10
   tsp=20
   x=1
   y=1
   z=1
   u=use
   ase=1

      do t=1,100

      if (t==tsp) then

            x=x+h*((z/taurec) -u*x)
            y=y-h*((y/tauin)+u*x)
            z=z+h*((y/tauin)-(z/taurec))
            
            
            u=u-h*(((u-use)/taufac)+use*(1-u))
            v=v+h*((1/taum)*(-v+rin*ase*y))


            else

                x=x+h*(z/taurec)
            y=y-h*(y/tauin)
            z=z+h*((y/tauin)-(z/taurec))


            u=u-h*((u-use)/taufac)
            v=v+h*(1/taum)*(-v+rin*ase*y)


            end if
                 print*,"x,y,z,t,v",x,y,z,t,v
            end do
            read*,l
            



     end program
