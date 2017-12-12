#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define PI 3.141592653
#define E 0.00005

int main() {
  double X1, X2, Y1, Y2, X3, Y3;
  scanf("%lf %lf\n", &X1, &Y1);
  scanf("%lf %lf\n", &X2, &Y2);
  scanf("%lf %lf\n", &X3, &Y3);

  if(Y1 == Y2) {
    double TX2 = X2;
    double TY2 = Y2;
    X2 = X3;
    Y2 = Y3;
    X3 = TX2;
    Y3 = TY2;
  }
  if(Y3 == Y2) {
    double TX2 = X2;
    double TY2 = Y2;
    X2 = X1;
    Y2 = Y1;
    X1 = TX2;
    Y1 = TY2;
  }

  double Xo = (((X3*X3-X2*X2)/(Y3-Y2))-((X1*X1-X2*X2)/(Y1-Y2)) + Y3 - Y1) / (2*((X3-X2)/(Y3-Y2))-2*((X1-X2)/(Y1-Y2)));
  double Yo = (-2 * Xo * ( (X1-X2)/(Y1-Y2) ) + (X1*X1-X2*X2)/(Y1-Y2) + Y1 + Y2) / 2;
  double R = sqrt(pow(X1-Xo, 2) + pow(Y1 -Yo, 2));

  double Xr = X1, Yr = Y1, X, Y;
  int i, j, flag_p2, flag_p3;

  for(i = 3; i <= 100; i++) {
    flag_p2 = 0;
    flag_p3 = 0;
    for(j = 0; j < i; j++) {
      X = Xr - Xo;
      Y = Yr - Yo;
      Xr = Xo + X * cos(2*PI/i) - Y * sin(2*PI/i);
      Yr = Yo + X * sin(2*PI/i) + Y * cos(2*PI/i);
      if(fabs(X2-Xr) < E && fabs(Y2-Yr) < E) {
        flag_p2 = 1;
      }
      if(fabs(X3-Xr) < E && fabs(Y3-Yr) < E) {
        flag_p3 = 1;
      }
    }
    if(fabs(X1-Xr) < E && fabs(Y1-Yr) < E && flag_p2 && flag_p3) {
      break;
    }
  }
  double N = i;
  double area = (N/2) * sin(2*PI/N)*R*R;
  printf("%.8f\n", area);
  return 0;
}
