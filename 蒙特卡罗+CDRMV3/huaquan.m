function huaquan(~)
%by ������
disp('*********��ȭ��Ϸ����*********');
while 1
    disp('****< ʯͷ=1������=2����=3 >****');
    quan=input('*************��ȭ*************\n');
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
        disp('��Ӯ�ˣ���������')
       break;
    end
    disp('������������~��ûӮ��������')
end
%{
tstart=tic;
%��ʱ�ĳ���
tused=toc(tstart);
if tused>60
    return
%}
 dos('E:\Fortran\���ؿ���+CDRMV3\���ؿ���+CDRMV3\CDRMV3.exe')
  
 mailme('CDRMV3�������','�������')