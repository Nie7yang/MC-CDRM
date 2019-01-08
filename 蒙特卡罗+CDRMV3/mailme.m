function mailme(mailtitle,mailcontent)
% 账号设置
mail = '459412615@qq.com';  % ①邮箱地址
password = 'mdnponyiikadbhbc'; % ②密码

% 服务器设置
setpref('Internet','E_mail',mail);
setpref('Internet','SMTP_Server','smtp.qq.com'); % ③SMTP服务器
setpref('Internet','SMTP_Username',mail);
setpref('Internet','SMTP_Password',password);
props = java.lang.System.getProperties;
props.setProperty('mail.smtp.auth','true');
props.setProperty('mail.smtp.socketFactory.class', 'javax.net.ssl.SSLSocketFactory');
props.setProperty('mail.smtp.socketFactory.port','465');
% 发送邮件
receiver='459412615@qq.com'; % ④我的收件邮箱
sendmail(receiver,mailtitle,mailcontent);
end