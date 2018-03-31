% figure(1);
%  a = plot(u_60_0_lin(1:1100),f_60_0_lin(1:1100)*0.001,'b','LineWidth',3);hold on;
%  b = plot(u_60_0_exp(1:310),f_60_0_exp(1:310)*0.001,':b','LineWidth',3);hold on;
%  aa = plot(u_80_0_lin(1:2400),f_80_0_lin(1:2400)*0.001,'k','LineWidth',3);hold on;
%  bb = plot(u_80_0_exp(1:260),f_80_0_exp(1:260)*0.001,':k','LineWidth',3);hold on;
%  aaa = plot(u_100_0_lin(1:620),f_100_0_lin(1:620)*0.001,'r','LineWidth',3);hold on;
%  bbb = plot(u_100_0_exp(1:260),f_100_0_exp(1:260)*0.001,':r','LineWidth',3);hold on;
%    
% legend([a,b,aa,bb,aaa,bbb],'[0]_1_0-H60mm-linear.','[0]_1_0-H60mm-exponential.','[0]_1_0-H80mm-linear.','[0]_1_0-H80mm-exponential.','[0]_1_0-H100mm-linear.','[0]_1_0-H100mm-exponential.');
% xlim([0,1.1*max(u_60_0_lin)])
% ylim([0,1.15*max(f_60_0_lin*0.001)])
% axis([0 1.5 0 90]);
% ylabel('Load(kN)','FontName','Times New Roman','fontsize',24)
% xlabel('Displacement(mm)','FontName','Times New Roman','fontsize',24)
% grid on
% set(gca,'FontName','Times New Roman','fontsize',24)
% set(h,'FontName','Times New Roman','fontsize',24)
% set(h, 'Box', 'off')

% figure(2);
% e = plot(u_60_0_90_lin(1:150),f_60_0_90_lin(1:150)*0.001,'b','LineWidth',3);hold on;
% f = plot(u_60_0_90_exp(1:120),f_60_0_90_exp(1:120)*0.001,':b','LineWidth',3);hold on;
% ee = plot(u_80_0_90_lin(1:85),f_80_0_90_lin(1:85)*0.001,'k','LineWidth',3);hold on;
% ff = plot(u_80_0_90_exp(1:90),f_80_0_90_exp(1:90)*0.001,':k','LineWidth',3);hold on;
% eee = plot(u_100_0_90_lin(1:85),f_100_0_90_lin(1:85)*0.001,'r','LineWidth',3);hold on;
% fff = plot(u_100_0_90_exp(1:75),f_100_0_90_exp(1:75)*0.001,':r','LineWidth',3);hold on;
%    
% legend([e,f,ee,ff,eee,fff],'[0/90]_{5}-H60mm-linear.','[0/90]_{5}-H60mm-exponential.','[0/90]_{5}-H80mm-linear.','[0/90]_{5}-H80mm-exponential.','[0/90]_{5}-H100mm-linear.','[0/90]_{5}-H100mm-exponential.');
% xlim([0,1.1*max(u_60_0_90_lin)])
% ylim([0,1.15*max(f_60_0_90_lin*0.001)])
% axis([0 1.2 0 65]);
% ylabel('Load(kN)','FontName','Times New Roman','fontsize',24)
% xlabel('Displacement(mm)','FontName','Times New Roman','fontsize',24)
% grid on
% set(gca,'FontName','Times New Roman','fontsize',24)
% set(h,'FontName','Times New Roman','fontsize',24)
% set(h, 'Box', 'off')
 
figure(3);

c = plot(u_60_45_lin(1:470),f_60_45_lin(1:470)*0.001,'b','LineWidth',3);hold on;
d = plot(u_60_45_exp(1:80),f_60_45_exp(1:80)*0.001,':b','LineWidth',3);hold on;
cc = plot(u_80_45_lin(1:165),f_80_45_lin(1:165)*0.001,'k','LineWidth',3);hold on;
dd = plot(u_80_45_exp(1:65),f_80_45_exp(1:65)*0.001,':k','LineWidth',3);hold on;
ccc = plot(u_100_45_lin(1:135),f_100_45_lin(1:135)*0.001,'r','LineWidth',3);hold on;
ddd = plot(u_100_45_exp(1:38),f_100_45_exp(1:38)*0.001,':r','LineWidth',3);hold on;
   
legend([c,d,cc,dd,ccc,ddd],'[45/-45]_{5}-H60mm-linear.','[45/-45]_{5}-H60mm-exponential.','[45/-45]_{5}-H80mm-linear.','[45/-45]_{5}-H80mm-exponential.','[45/-45]_{5}-H100mm-linear.','[45/-45]_{5}-H100mm-exponential.');
xlim([0,1.1*max(u_60_45_exp)])
ylim([0,1.15*max(f_60_45_exp*0.001)])
axis([0 1.5 0 32]);
ylabel('Load(kN)','FontName','Times New Roman','fontsize',24)
xlabel('Displacement(mm)','FontName','Times New Roman','fontsize',24)
grid on
set(gca,'FontName','Times New Roman','fontsize',24)
set(h,'FontName','Times New Roman','fontsize',24)
set(h, 'Box', 'off')
 