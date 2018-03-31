u_60_45=importdata('60_45_dis.txt');
f_60_45=importdata('60_45_force.txt');
u_80_45=importdata('80_45_dis.txt');
f_80_45=importdata('80_45_force.txt');
u_100_45=importdata('100_45_dis.txt');
f_100_45=importdata('100_45_force.txt');

f_60_45=f_60_45';
f_80_45=f_80_45';
f_100_45=f_100_45';

figure(2);

d = plot(u_60_45,f_60_45,'b','LineWidth',3);hold on;
e = plot(u_80_45,f_80_45,'k','LineWidth',3);hold on;
f = plot(u_100_45,f_100_45,'r','LineWidth',3);hold on;

dd = plot(u_60_45_exp_n2(1:25),f_60_45_exp_n2(1:25)*0.001,':b','LineWidth',3);hold on;
ee = plot(u_80_45_exp_n2(1:30),f_80_45_exp_n2(1:30)*0.001,':k','LineWidth',3);hold on;
ff = plot(u_100_45_exp_n2(1:18),f_100_45_exp_n2(1:18)*0.001,':r','LineWidth',3);hold on;

legend([d,dd,e,ee,f,ff],'H60mm-实验','H60mm-计算','H80mm-实验','H80mm-计算','H100mm-实验','H100mm-计算');
xlim([0,1.1*max(u_80_45)])
ylim([0,1.15*max(f_60_45)])
grid on
ylabel('Load(kN)','FontName','Times New Roman','fontsize',24)
xlabel('Displacement(mm)','FontName','Times New Roman','fontsize',24)
set(gca,'FontName','宋体','fontsize',24)
set(h,'FontName','Times New Roman','fontsize',24)
set(h, 'Box', 'off')
Y=max(f_60_45_exp);
 
 
 