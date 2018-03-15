figure(1);
% a = plot(Disp31,Load31*1000,'b','LineWidth',3);hold on;
% a1 = plot(u_60_45_exp_n1,f_60_45_exp_n1,'-.b','LineWidth',3);hold on;
% a2 = plot(u_60_45_exp_n2,f_60_45_exp_n2,':b','LineWidth',3);hold on;
% a3 = plot(u_60_45_exp_n3,f_60_45_exp_n3,'--b','LineWidth',3);hold on;

b = plot(u_100_0,f_100_0*1000,'k','LineWidth',3);hold on;
% b1 = plot(u_80_45_exp_n1,f_80_45_exp_n1,'-.k','LineWidth',3);hold on;
% b2 = plot(u_80_45_exp_n2,f_80_45_exp_n2,':k','LineWidth',3);hold on;
% b3 = plot(u_80_45_exp_n3,f_80_45_exp_n3,'--k','LineWidth',3);hold on;

% c = plot(Disp33,Load33*1000,'r','LineWidth',3);hold on;
% c1 = plot(u_100_45_exp_n1,f_100_45_exp_n1,'-.r','LineWidth',3);hold on;
% c2 = plot(u_100_45_exp_n2,f_100_45_exp_n2,':r','LineWidth',3);hold on;
% c3 = plot(u_100_45_exp_n3,f_100_45_exp_n3,'--r','LineWidth',3);hold on;

% legend([a,a2,b,b2,c,c2],'[45]10-H60mm','[45]10-H60mm-Shear Nonlinearity','[45]10-H80mm','[45]10-H80mm-Shear Nonlinearity','[45]10-H100mm','[45]10-H100mm-Shear Nonlinearity');
 legend([b],'[0]10-H100mm-Expt.');
grid on
 xlabel('Œª“∆(mm)');
 ylabel('‘ÿ∫…(N)');
 axis([0 6 0 40000]);
 
 Y=max(f_60_45_exp);
 
 
 