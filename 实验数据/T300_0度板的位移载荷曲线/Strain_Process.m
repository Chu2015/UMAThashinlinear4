clear
clc
   
Load_data=importdata('复材开口[0]10-H100_1.txt'); % 控制系统载荷-位移曲线
Load_data2=importdata('力-位移 [0]10-H80mm.txt'); % 控制系统载荷-位移曲线
Load_data3=importdata('力-位移 [0]10-H60mm.txt'); % 控制系统载荷-位移曲线

Nf=length(Load_data);
Nf2=length(Load_data2);
Nf3=length(Load_data3);

%% 载荷-位移曲线
Nf=(Nf-1)/2+1;
f=zeros(1,Nf);
s=zeros(1,Nf);
for i=1:Nf-1
    f(i+1)=Load_data(i*2,1);
    s(i+1)=Load_data(i*2,4); % 位移单位：mm
end
f=f'/1000; % 载荷单位：kN
Max_f=max(f);
disp(['最大荷载' num2str(Max_f) 'kN']); 
s=s';

[A Nf]=min(abs(f-Max_f));

h=1; % h点求平均
M=floor(Nf/h)+500;
Force=zeros(M,1);
ss=zeros(M,1);
for i=1:M
    Force(i,1)=mean(f((i-1)*h+1:i*h));
    ss(i)=mean(s((i-1)*h+1:i*h));
end
figure(1)
dl60=plot(ss,Force,'r','LineWidth',3);
% legend([dl60],'[0]10-H100mm-Expt.');
% xlim([0,1.1*max(ss)])
% ylim([0,1.1*Max_f])
% ylabel('Load(kN)','FontName','Times New Roman','fontsize',24)
% xlabel('Displacement(mm)','FontName','Times New Roman','fontsize',24)
% h=legend('Load-Displacement Curve',2);
% set(gca,'FontName','Times New Roman','fontsize',24)
% set(h,'FontName','Times New Roman','fontsize',24)
% set(h, 'Box', 'off')
grid on
hold on

%% 载荷-位移曲线
Nf2=(Nf2-1)/2+1;
f2=zeros(1,Nf);
s2=zeros(1,Nf);
for i=1:Nf2-1
    f2(i+1)=Load_data2(i*2,1);
    s2(i+1)=Load_data2(i*2,4); % 位移单位：mm
end
f2=f2'/1000; % 载荷单位：kN
Max_f2=max(f2);
disp(['最大荷载' num2str(Max_f2) 'kN']); 
s2=s2';

% [A Nf]=min(abs(f-Max_f));

h2=1; % h点求平均
M2=floor((Nf2-1)/h2);
Force2=zeros(M2,1);
ss2=zeros(M2,1);
for i=1:M2
    Force2(i,1)=mean(f2((i-1)*h2+1:i*h2));
    ss2(i)=mean(s2((i-1)*h2+1:i*h2));
end
dl80=plot(ss2,Force2,'k','LineWidth',3);
% xlim([0,1.1*max(ss2)])
% ylim([0,1.15*max(Force2)])
% ylabel('Load(kN)','FontName','Times New Roman','fontsize',24)
% xlabel('Displacement(mm)','FontName','Times New Roman','fontsize',24)
% h=legend('Load-Displacement Curve',2);
% set(gca,'FontName','Times New Roman','fontsize',24)
% set(h,'FontName','Times New Roman','fontsize',24)
% set(h, 'Box', 'off')
grid on
hold on

%% 载荷-位移曲线
Nf3=(Nf3-1)/2+1;
f3=zeros(1,Nf3);
s3=zeros(1,Nf3);
for i=1:Nf3-1
    f3(i+1)=Load_data3(i*2,1);
    s3(i+1)=Load_data3(i*2,4); % 位移单位：mm
end
f3=f3'/1000; % 载荷单位：kN
Max_f3=max(f3);
disp(['最大荷载' num2str(Max_f3) 'kN']); 
s3=s3';

h3=1; % h点求平均
M3=floor((Nf3-1)/h3);
Force3=zeros(M3,1);
ss3=zeros(M3,1);
for i=1:M3
    Force3(i,1)=mean(f3((i-1)*h3+1:i*h3));
    ss3(i)=mean(s3((i-1)*h3+1:i*h3));
end
dl100=plot(ss3,Force3,'b','LineWidth',3);
legend([dl60,dl80,dl100],'[0]_1_0-H100mm-Expt.','[0]_1_0-H80mm-Expt.','[0]_1_0-H60mm-Expt.');
xlim([0,1.1*max(ss3)])
ylim([0,1.15*max(Force3)])
ylabel('Load(kN)','FontName','Times New Roman','fontsize',24)
xlabel('Displacement(mm)','FontName','Times New Roman','fontsize',24)
set(gca,'FontName','Times New Roman','fontsize',24)
set(h,'FontName','Times New Roman','fontsize',24)
set(h, 'Box', 'off')
grid on
hold on