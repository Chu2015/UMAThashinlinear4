figure(1);
%plot(X1,Force1,X2,Force2,X3,Force3,X4,Force4,'b');
%plot(X,u1f1,X2,u2f2,X3,u3f3,'b');
% c = plot(u_60_45_lin(1:500,1),f_60_45_lin(1:500,1),'r','LineWidth',3);hold on;
%  a = plot(u_60_0_lin,f_60_0_lin,'r','LineWidth',3);hold on;
%  b = plot(u_60_0_exp,f_60_0_exp,':r','LineWidth',3);hold on;
%   c = plot(u_60_45_lin,f_60_45_lin,'b','LineWidth',3);hold on;
%   d = plot(u_60_45_exp,f_60_45_exp,':b','LineWidth',3);hold on;
 e = plot(u_60_0_90_lin(1:150),f_60_0_90_lin(1:150),'b','LineWidth',3);hold on;
 f = plot(u_60_0_90_exp(1:120),f_60_0_90_exp(1:120),':b','LineWidth',3);hold on;

%  aa = plot(u_80_0_lin,f_80_0_lin,'b','LineWidth',3);hold on;
%  bb = plot(u_80_0_exp(1:900),f_80_0_exp(1:900),'b','LineWidth',3);hold on;
%  aa2 = plot(u_80_0_lin_new,f_80_0_lin_new,'--b','LineWidth',3);hold on;
%  cc = plot(u_80_45_lin,f_80_45_lin,'k','LineWidth',3);hold on;
%  dd = plot(u_80_45_exp_n2(1:25),f_80_45_exp_n2(1:25),'r','LineWidth',3);hold on;
  ee = plot(u_80_0_90_lin(1:85),f_80_0_90_lin(1:85),'k','LineWidth',3);hold on;
 ff = plot(u_80_0_90_exp(1:90),f_80_0_90_exp(1:90),':k','LineWidth',3);hold on;
%   ee2 = plot(u_80_0_90_lin_new,f_80_0_90_lin_new,'--b','LineWidth',3);hold on;
 
%  aaa = plot(u_100_0_lin,f_100_0_lin,'k','LineWidth',3);hold on;
%  bbb = plot(u_100_0_exp,f_100_0_exp,':k','LineWidth',3);hold on;
%  aaaa = plot(u_100_0_lin_2_5,f_100_0_lin_2_5,'--k','LineWidth',3);hold on;
%  ccc = plot(u_100_45_lin,f_100_45_lin,'r','LineWidth',3);hold on;
%  ddd = plot(u_100_45_exp,f_100_45_exp,':r','LineWidth',3);hold on;
 eee = plot(u_100_0_90_lin(1:85),f_100_0_90_lin(1:85),'r','LineWidth',3);hold on;
 fff = plot(u_100_0_90_exp(1:85),f_100_0_90_exp(1:85),':r','LineWidth',3);hold on;
   
% legend([a,b,aa,bb,aaa,bbb],'ֱ��60-linear','ֱ��60-exponential','ֱ��80-linear','ֱ��80-exponential','ֱ��100-linear','ֱ��100-exponential');
  legend([e,f,ee,ff,eee,fff],'[0/90]_{5}-\Phi60mm-linear','[0/90]_{5}-\Phi60mm-exponential','[0/90]_{5}-\Phi80mm-linear','[0/90]_{5}-\Phi80mm-exponential','[0/90]_{5}-\Phi100mm-linear','[0/90]_{5}-\Phi100mm-exponential');
% legend([c,d,cc,dd,ccc,ddd],'60-45-lin','60-45-exp','80-45-lin','80-45-exp','100-45-lin','100-45-exp');
% legend([bb,dd,ff],'[0]10-H80mm','[45]10-H80mm','[0/90]10-H80mm');
 grid on
 xlabel('λ��(mm)');
 ylabel('�غ�(N)');
 axis([0 1.2 0 60000]);
 
 Y=max(f_60_45_exp);
 
 
 