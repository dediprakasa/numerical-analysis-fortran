clear all
close
clc


%Determining the limits of integration
a = 0;
b = 2;

%Determining the width of quadrature
h = b - a;

%Calculating the result of integration
Int = 0.5*h * (func(a) + func(b));

Int