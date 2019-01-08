function huaquan(~)
%by 聂启阳
disp('*********猜拳游戏启动*********');
while 1
    disp('****< 石头=1；剪刀=2；布=3 >****');
    quan=input('*************出拳*************\n');
    a=rand(1,1);
    b=3.0*a;
    if b>=0&&b<1
        c=1;
    elseif b>=1&&b<2
        c=2;
    elseif b>=2&&b<=3
        c=3;
    end
    if (quan==1&&c==2)||(quan==2&&c==3)||(quan==3&&c==1)
        disp('你赢了，开启程序')
       break;
    end
    disp('啊哈哈哈哈哈~你没赢，重新来')
end
%{
tstart=tic;
%计时的程序
tused=toc(tstart);
if tused>60
    return
%}
 dos('E:\Fortran\蒙特卡罗+CDRMV3\蒙特卡罗+CDRMV3\CDRMV3.exe')
  
 mailme('CDRMV3计算完毕','计算完毕')