#include <math.h>
#define PI 3.141592653589793

void gauleg(int *n, double *eps, double *A, double *W)
{ int i,j, m=((*n)+1)/2; double z1,z,pp,p1,p2,p3;
      for(i=1;i<=m;i++){
        z=cos(PI*(i-0.25)/((*n)+0.5));
        do{ p1=1.0;
            p2=0.0;
            for(j=1;j<=(*n);j++){
                p3=p2;
                p2=p1;
                p1=((2.0*j-1.0)*z*p2-(j-1.0)*p3)/j;
            }
            pp=(*n)*(z*p1-p2)/(z*z-1.0);
            z1=z;
            z=z-p1/pp;
        } while(fabs(z-z1)>(*eps));
        A[i-1]=-z;
        A[(*n)-i]=z;
        W[i-1]=2.0/((1.0-z*z)*pp*pp);
        W[(*n)-i]=W[i-1];
    }
}
