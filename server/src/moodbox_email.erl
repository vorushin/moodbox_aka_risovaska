-module(moodbox_email).

-export([send_mail/3, send_mail/6]).
-export([test/0, test_password_reminder/2, full_test/0]).

-include("moodbox_config.hrl").

send_mail(UserEmail, Msg) ->
    send_mail(UserEmail, Msg, ?SMTP_SERVER, ?MAIL_LOGIN, ?MAIL_PASSWORD, true).

send_mail(UserEmail, Msg, Server, Login, Password, IsAuthorize) ->
    {ok, Pid} = smtp_fsm:start(Server, ?SMTP_PORT), 
    smtp_fsm:ehlo(Pid),
    if 
	IsAuthorize ->
	    {ok, _Text} = smtp_fsm:login(Pid, Login, Password);
	true ->
	    ok
    end,
    ok = smtp_fsm:sendemail(Pid, ?MAIL_FROM, UserEmail, Msg),
    smtp_fsm:close(Pid).

send_mail(UserEmail, MessageSubject, MessageBody) ->
   Msg = email_msg:simp_msg(?MAIL_FROM, UserEmail, MessageSubject, MessageBody),
   send_mail(UserEmail, Msg).

%% test functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->
    send_mail("abokov@rol.ru", "this is subject", "this is message body!").

test_password_reminder(_UserNick, UserMail) ->
   Subj = ?MAIL_PSWD_REMIND_HEADER,
% UserNick, CheckSum of smth, UserId - for test purposes only
   Body = io_lib:format(?MAIL_PSWD_REMIND_BODY, ["33a949feft3", 1234567]),
   send_mail( UserMail, Subj, Body).

full_test()->
   test_password_reminder("thecat", "abokov@rol.ru"),
   test_password_reminder("lexa-bokov", "rock@pisem.net"),
   test_password_reminder("magic-tolik", "vostrjakov@mail.ru"),
   test_password_reminder("kotzi", "bokov-aleksej@yandex.ru"),
   test_password_reminder("vorushin", "roman.vorushin@gmail.com"),
   test_password_reminder("andrew", "andrewpi@yandex.ru"),
   test_password_reminder("dimsmol", "dimsmol@gmail.com"),
   test_password_reminder("krvss", "stas.kravets@gmail.com"),
   test_password_reminder("denis", "dmalinovsky@gmail.com").

