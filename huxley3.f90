program hodgkin
real am,an,ah,bh,bm,bn,V,taum,tauh,taun,minf,hinf,ninf,gl,gk,gna,vl,vk,vna,F,n,m,hache,h,I,l,o
integer k,s
s=200
  I=150
gl=0.3
gk= 36
gna=120
vl=-54.402
vk=-77
vna=50



V=0
m=0
h=1
n=0
h=0.1!tiempo



am=0.1*(V+40)/(1-exp(-0.1*(V+40)))
an=0.01*(V+55)/(1-exp(-0.1*(V+55)))
bh=(V+40)/(1+exp(-0.1*(V+35)))
ah=0.07*exp(-0.05*(V+65))
bm=4*exp(-0.0556*(V+65))
bn=0.125*exp(-0.0125*(V+65))


taum=1/(am+bm)
tauh=1/(ah+bh)
taun=1/(an+bn)


minf=am/(am+bm)
hinf=ah/(ah+bh)
ninf=an/(an+bn)


F=gl*(V-vl)+gk*(n**(4))*(V-vk)+gna*h*(m**3)*(V-vna)
                        l=0

do k=1,s
        V=V+h*(-F+I)




        m= m+h*(minf-m)/(taum)
        hache=hache + h*(hinf-hache)/tauh
        n=n+h*(ninf-n)/taun
        l=l+h

                  am=0.1*(V+40)/(1-exp(-0.1*(V+40)))

        an=0.01*(V+55)/(1-exp(-0.1*(V+55)))
bh=(V+40)/(1+exp(-0.1*(V+35)))
ah=0.07*exp(-0.05*(V+65))
bm=4*exp(-0.0556*(V+65))
bn=0.125*exp(-0.0125*(V+65))
         taum=1/(am+bm)
tauh=1/(ah+bh)
taun=1/(an+bn)


minf=am/(am+bm)
hinf=ah/(ah+bh)
ninf=an/(an+bn)

F=gl*(V-vl)+gk*(n**(4))*(V-vk)+gna*h*(m**3)*(V-vna)


        print*,l,v
  end do
  read*,o





end program
