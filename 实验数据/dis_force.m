u_60_0_90=importdata('60_0-90_dis.txt');
f_60_0_90=importdata('60_0-90_force.txt');
u_80_0_90=importdata('80_0-90_dis.txt');
f_80_0_90=importdata('80_0-90_force.txt');
u_100_0_90=importdata('100_0-90_dis.txt');
f_100_0_90=importdata('100_0-90_force.txt');

u_60_45=importdata('60_45_dis.txt');
f_60_45=importdata('60_45_force.txt');
u_80_45=importdata('80_45_dis.txt');
f_80_45=importdata('80_45_force.txt');
u_100_45=importdata('100_45_dis.txt');
f_100_45=importdata('100_45_force.txt');

f_60_0_90=f_60_0_90';
f_80_0_90=f_80_0_90';
f_100_0_90=f_100_0_90';
f_60_45=f_60_45';
f_80_45=f_80_45';
f_100_45=f_100_45';

% figure(1);
% 
% a = plot(u_60_0_90,f_60_0_90,'b','LineWidth',3);hold on;
% b = plot(u_80_0_90,f_80_0_90,'k','LineWidth',3);hold on;
% c = plot(u_100_0_90,f_100_0_90,'r','LineWidth',3);hold on;
% 
% legend([a,b,c],'[0/90]_{5}-H60mm-Expt.','[0/90]_{5}-H80mm-Expt.','[0/90]_{5}-H100mm-Expt.');
% xlim([0,1.1*max(u_60_0_90)])
% ylim([0,1.15*max(f_60_0_90)])
% grid on
% ylabel('Load(kN)','FontName','Times New Roman','fontsize',24)
% xlabel('Displacement(mm)','FontName','Times New Roman','fontsize',24)
% set(gca,'FontName','Times New Roman','fontsize',24)
% set(h,'FontName','Times New Roman','fontsize',24)
% set(h, 'Box', 'off')
% hold on

figure(2);

d = plot(u_60_45,f_60_45,'b','LineWidth',3);hold on;
e = plot(u_80_45,f_80_45,'k','LineWidth',3);hold on;
f = plot(u_100_45,f_100_45,'r','LineWidth',3);hold on;

legend([d,e,f],'[45/-45]_{5}-H60mm-Expt.','[45/-45]_{5}-H80mm-Expt.','[45/-45]_{5}-H100mm-Expt.');
xlim([0,1.1*max(u_80_45)])
ylim([0,1.15*max(f_60_45)])
grid on
ylabel('Load(kN)','FontName','Times New Roman','fontsize',24)
xlabel('Displacement(mm)','FontName','Times New Roman','fontsize',24)
set(gca,'FontName','Times New Roman','fontsize',24)
set(h,'FontName','Times New Roman','fontsize',24)
set(h, 'Box', 'off')
Y=max(f_60_45_exp);
 
 
 