function mailme(mailtitle,mailcontent)
% �˺�����
mail = '459412615@qq.com';  % �������ַ
password = 'mdnponyiikadbhbc'; % ������

% ����������
setpref('Internet','E_mail',mail);
setpref('Internet','SMTP_Server','smtp.qq.com'); % ��SMTP������
setpref('Internet','SMTP_Username',mail);
setpref('Internet','SMTP_Password',password);
props = java.lang.System.getProperties;
props.setProperty('mail.smtp.auth','true');
props.setProperty('mail.smtp.socketFactory.class', 'javax.net.ssl.SSLSocketFactory');
props.setProperty('mail.smtp.socketFactory.port','465');
% �����ʼ�
receiver='459412615@qq.com'; % ���ҵ��ռ�����
sendmail(receiver,mailtitle,mailcontent);
end