figure(1);
%plot(X1,Force1,X2,Force2,X3,Force3,X4,Force4,'b');
%plot(X,u1f1,X2,u2f2,X3,u3f3,'b');

 a = plot(u_60_0_lin,f_60_0_lin,'r');hold on;
 b = plot(u_60_0_exp,f_60_0_exp,'r');hold on;
% c = plot(u_60_45_lin,f_60_45_lin,'b');hold on;
% d = plot(u_60_45_exp,f_60_45_exp,'g');hold on;
% e = plot(u_60_0_90_lin,f_60_0_90_lin,'r');hold on;
% f = plot(u_60_0_90_exp2,f_60_0_90_exp2,'b');hold on;

% legend([a,b,c,d,e,f],'60-0-lin','60-0-exp','60-45-lin','60-45-exp','60-0-90-lin','60-0-90-exp');
% grid on
% xlabel('Œª“∆(mm)');
% ylabel('‘ÿ∫…(N)');
% axis([0 4 0 140000]);

 aa = plot(u_80_0_lin,f_80_0_lin,'g');hold on;
 bb = plot(u_80_0_exp,f_80_0_exp,'g');hold on;
 
 aaa = plot(u_100_0_lin,f_100_0_lin,'b');hold on;
 bbb = plot(u_100_0_exp,f_100_0_exp,'b');hold on;
 aaaa = plot(u_100_0_lin_2_5,f_100_0_lin_2_5,'r');hold on;
 legend([a,b,aa,bb,aaa,bbb,aaaa],'60-0-lin','60-0-exp','80-0-lin','80-0-exp','100-0-lin','100-0-exp','100-0-lin-2.5');
 grid on
 xlabel('Œª“∆(mm)');
 ylabel('‘ÿ∫…(N)');
 axis([0 4 0 140000]);
