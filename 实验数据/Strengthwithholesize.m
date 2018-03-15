% % 0-90
% x=[0.25 0.3125 0.375 0.4375 0.5 0.5625 0.625 0.6875 0.75];
% y1=[73.2809e+03  66.1594e+03   56.9104E+03  50.7105E+03 45.8887E+03  40.1744E+03  32.991E+03  27.3757E+03 22.532E+03];
% a=scatter(x,y1,'x');hold on;
% 
% %  +-45
% x=[0.25 0.3125 0.375 0.4375 0.5 0.5625 0.625 0.6875 0.75];
% y2=[27.5181E+03  24.7136E+03 22.4644E+03 21.5952E+03 17.8852E+03 15.9841E+03 13.3849E+03 11.2799E+03 9.05923E+03];
% b=scatter(x,y2,'s');hold on;
% 
% % 0
% x=[0.25 0.3125 0.375 0.4375 0.5 0.5625 0.625 0.6875 0.75];
% y3=[138.798E+03  123.124E+03 113.016E+03 97.5256E+03 85.8616E+03  82.617E+03   66.9218E+03  57.7659E+03  46.8244E+03 ];
% c=scatter(x,y3,'o');hold on;
% 
% legend([c,a,b],'0','0-90','±45');

%Simulation Result：
% 0
% x1=[0.375 0.5 0.625];
% y1=[78.74E+03 61.55E+03 47.79E+03];
% c=scatter(x1,y1,'o','b');hold on;

% 0-90
% x2=[0.375 0.5 0.625];
% y2=[56.72e+03  48.17e+03  34.59E+03];
% a=scatter(x2,y2,'x','k');hold on;

%  +-45
% x3=[0.375 0.5 0.625];
% y3=[27.34E+03  21.73E+03 16.09E+03];
% b=scatter(x3,y3,'s','r');hold on;

% legend([c,a,b],'0','0-90','±45');

%experimental results
%0
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
% a = plot(x1,y1,'k','LineWidth',3);
% b = plot(x2,y2,'r','LineWidth',3);
% c = plot(x3,y3,'b','LineWidth',3);

d = plot(a1,b1,'--k','LineWidth',3);
e = plot(a2,b2,'--r','LineWidth',3);
f = plot(a3,b3,'--b','LineWidth',3);
legend([d,e,f],'[0]_{10}-experiment','[0/90]_{5}-experiment','[45/-45]_{5}-experiment');
% legend([a,d,b,e,c,f],'[0]_{10}-numerical','[0]_{10}-experiment','[0/90]_{5}-numerical','[0/90]_{5}-experiment','[45/-45]_{5}-numerical','[45/-45]_{5}-experiment');
 grid on
xlabel('直径/宽度');
ylabel('极限载荷(N)')

figure(2);
plot(X,temp_1,X1,temp_2,X2,temp_3,'b');
grid on

