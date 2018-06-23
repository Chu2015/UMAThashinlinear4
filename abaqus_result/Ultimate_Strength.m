
% Simulation Result：
% 0
x1=[0.375 0.5 0.625];
y1=[87.36E+03 71.05E+03 52.31E+03];
scatter(x1,y1,'o','k');hold on;

% 0-90
x2=[0.375 0.5 0.625];
y2=[53.01e+03  48.29e+03  38.90E+03];
scatter(x2,y2,'x','r');hold on;

%  +-45
x3=[0.375 0.5 0.625];
y3=[25.86E+03  20.56E+03  15.05E+03];
scatter(x3,y3,'s','b');hold on;

% legend([c,a,b],'[0]_{10}-simulation','[0/90]_{5}-simulation','[45/-45]_{5}-simulation');

%experimental results
% 0
a1=[0.375 0.5 0.625];
b1=[78.23E+03 69.725E+03 35.00E+03];
scatter(a1,b1,'o','k');

%0-90
a2=[0.375 0.5 0.625];
b2=[59.315E+03 47.2E+03 37.537E+03];
scatter(a2,b2,'x','r');hold on;

%+-45
a3=[0.375 0.5 0.625];
b3=[28.717E+03 23.785E+03 17.602E+03];
scatter(a3,b3,'s','b');hold on;

figure(1);
a = plot(x1,y1,'k','LineWidth',3);
b = plot(x2,y2,'r','LineWidth',3);
c = plot(x3,y3,'b','LineWidth',3);

d = plot(a1,b1,'--k','LineWidth',3);
e = plot(a2,b2,'--r','LineWidth',3);
f = plot(a3,b3,'--b','LineWidth',3);

% legend([d,c,e,a,f,b],'[0]_{10}-实验值','[0]_{10}-计算值','[0/90]_{5}-实验值','[0/90]_{5}-计算值','[45/-45]_{5}-实验值','[45/-45]_{5}-计算值');
legend([d,a,e,b,f,c],'[0]_{10}-expriment','[0]_{10}-calculation','[0/90]_{5}-expriment','[0/90]_{5}-calculation','[45/-45]_{5}-expriment','[45/-45]_{5}-calculation');
axis([0.35 0.65 0 100000]);
grid on
xlabel('Aperture width ratio');
ylabel('Ultimate load(N)')


