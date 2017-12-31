figure(1);
%plot(X1,Force1,X2,Force2,X3,Force3,X4,Force4,'b');
%plot(X,u1f1,X2,u2f2,X3,u3f3,'b');

%  a = plot(u_60_0_lin,f_60_0_lin,'r','LineWidth',3);hold on;
%  b = plot(u_60_0_exp,f_60_0_exp,':r','LineWidth',3);hold on;
%   c = plot(u_60_45_lin,f_60_45_lin,'r','LineWidth',3);hold on;
%   d = plot(u_60_45_exp,f_60_45_exp,':r','LineWidth',3);hold on;
 e = plot(u_60_0_90_lin,f_60_0_90_lin,'r','LineWidth',3);hold on;
 f = plot(u_60_0_90_exp,f_60_0_90_exp,':r','LineWidth',3);hold on;

%  aa = plot(u_80_0_lin,f_80_0_lin,'b','LineWidth',3);hold on;
%  bb = plot(u_80_0_exp,f_80_0_exp,':b','LineWidth',3);hold on;
%  aa2 = plot(u_80_0_lin_new,f_80_0_lin_new,'--b','LineWidth',3);hold on;
%  cc = plot(u_80_45_lin,f_80_45_lin,'b','LineWidth',3);hold on;
%  dd = plot(u_80_45_exp,f_80_45_exp,':b','LineWidth',3);hold on;
 ee = plot(u_80_0_90_lin,f_80_0_90_lin,'b','LineWidth',3);hold on;
 ff = plot(u_80_0_90_exp,f_80_0_90_exp,':b','LineWidth',3);hold on;
   
%  aaa = plot(u_100_0_lin,f_100_0_lin,'k','LineWidth',3);hold on;
%  bbb = plot(u_100_0_exp,f_100_0_exp,':k','LineWidth',3);hold on;
%  aaaa = plot(u_100_0_lin_2_5,f_100_0_lin_2_5,'--k','LineWidth',3);hold on;
%  ccc = plot(u_100_45_lin,f_100_45_lin,'k','LineWidth',3);hold on;
%  ddd = plot(u_100_45_exp,f_100_45_exp,':k','LineWidth',3);hold on;
 eee = plot(u_100_0_90_lin,f_100_0_90_lin,'k','LineWidth',3);hold on;
 fff = plot(u_100_0_90_exp,f_100_0_90_exp,':k','LineWidth',3);hold on;
   
% legend([a,b,aa,bb,aa2,aaa,bbb,aaaa],'60-0-lin','60-0-exp','80-0-lin','80-0-exp','80-0-lin-new','100-0-lin','100-0-exp','100-0-lin-Gm_2_._5');
 legend([e,f,ee,ff,eee,fff],'60-0-90-lin','60-0-90-exp','80-0-90-lin','80-0-90-exp','100-0-90-lin','100-0-90-exp');
% legend([c,d,cc,dd,ccc,ddd],'60-45-lin','60-45-exp','80-45-lin','80-45-exp','100-45-lin','100-45-exp');
 grid on
 xlabel('Œª“∆(mm)');
 ylabel('‘ÿ∫…(N)');
 axis([0 4 0 60000]);
 
 Y=max(f_60_45_exp);
 
 
 