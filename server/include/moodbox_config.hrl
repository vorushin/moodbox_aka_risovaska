% -define(NOTEST, true). % don't compile test-functions

-define(APP, moodbox).
-define(DEBUG, true).
-define(ParameterFile, "priv/parameters").

-define(MAX_REQUEST_SIZE, (1024*1024*2)).

-define(LOGIN_VALIDATION_REGEXP, "^[a-zA-Z0-9._%+-]+$").
-define(LOGIN_RESERVED_REGEXP, "^Arty$|^Moodbox$|^Moodbox\.com$|^.*\@moodbox\.com$").
-define(LOGIN_MINLENGTH, 3).
-define(LOGIN_MAXLENGTH, 45).

-define(EMAIL_VALIDATION_REGEXP, "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}$").
-define(EMAIL_MAXLENGTH, 50).

-define(PASSWORD_MINLENGTH, 5).
-define(PASSWORD_MAXLENGTH, 45).
-define(PASSWORD_AUTOGENERATED_PASSWORD_LENGTH, 20).

-define(MAX_CONTACTS_COUNT, 200).

-define(MAX_SORTED_RECORDS_COUNT, 500).

-define(MAX_CACHED_MESSAGES_COUNT, 2000).

-define(OBSCENE_CHANNEL_MESSAGE_LIMIT, 3).

-define(MAX_FAILED_LOGON_ATTEMPS_COUNT, 3).
-define(FAILED_ATTEMPS_INTERVAL, moodbox_datetime:minutes(10)).
-define(LOCK_INTERVAL, moodbox_datetime:minutes(10)).

-define(CHANNEL_MESSAGE_QUEUE_LENGTH, 50).
-define(CHANNEL_MESSAGE_QUEUE_LENGTH_FOR_NEWBIE, 20).
-define(PUBLISHED_MESSAGE_QUEUE_LENGTH, 50).
-define(MaxFriendsMessagesInQueue, 200).
-define(MaxPrivateMessagesInQueue, 100).

-define(MAX_COMMANDS_IN_PACKAGE, 50).

-define(MAX_RECORDS_PER_PAGE, 50).

-define(USERPIC_MAX_SIZE, 30000).

-define(URL_WEB, "http://moodbox.com/moodstrip/~.10B").
-define(URL_WEB_RU, "http://risovaska.ru/comic/~.10B").
-define(URL_WIDGET, "<object classid=\"clsid:d27cdb6e-ae6d-11cf-96b8-444553540000\" "
		    "codebase=\"http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=8,0,0,0\" "
		    "width=\"447\" height=\"398\" id=\"moodwidget\" align=\"top\">"
		    "<param name=\"allowScriptAccess\" value=\"sameDomain\" />"
		    "<param name=\"allowFullScreen\" value=\"false\" />"
		    "<param name=\"movie\" value=\"http://static.moodbox.com/img/box.swf\" />"
		    "<param name=\"quality\" value=\"high\" />"
		    "<param name=\"scale\" value=\"noscale\" />"
		    "<param name=\"bgcolor\" value=\"#000000\" />   "
		    "<embed src=\"http://static.moodbox.com/img/box.swf\" quality=\"high\" scale=\"noscale\" bgcolor=\"#000000\" width=\"447\" height=\"398\" name=\"moodwidget\" align=\"top\" allowScriptAccess=\"sameDomain\" allowFullScreen=\"false\" type=\"application/x-shockwave-flash\" pluginspage=\"http://www.macromedia.com/go/getflashplayer\" wmode=\"transparent\" flashvars=\"id=~.10B\" />"
		    "<param name=\"flashvars\" value=\"id=~.10B\" />"
		    "<param name=\"wmode\" value=\"transparent\" /></object>").
-define(URL_WIDGET_RU, "<object classid=\"clsid:d27cdb6e-ae6d-11cf-96b8-444553540000\" "
		    "codebase=\"http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=8,0,0,0\" "
		    "width=\"447\" height=\"398\" id=\"moodwidget\" align=\"top\">"
		    "<param name=\"allowScriptAccess\" value=\"sameDomain\" />"
		    "<param name=\"allowFullScreen\" value=\"false\" />"
		    "<param name=\"movie\" value=\"http://static.risovaska.ru/img/box.swf\" />"
		    "<param name=\"quality\" value=\"high\" />"
		    "<param name=\"scale\" value=\"noscale\" />"
		    "<param name=\"bgcolor\" value=\"#000000\" />   "
		    "<embed src=\"http://static.risovaska.ru/img/box.swf\" quality=\"high\" scale=\"noscale\" bgcolor=\"#000000\" width=\"447\" height=\"398\" name=\"moodwidget\" align=\"top\" allowScriptAccess=\"sameDomain\" allowFullScreen=\"false\" type=\"application/x-shockwave-flash\" pluginspage=\"http://www.macromedia.com/go/getflashplayer\" wmode=\"transparent\" flashvars=\"id=~.10B\" />"
		    "<param name=\"flashvars\" value=\"id=~.10B\" />"
		    "<param name=\"wmode\" value=\"transparent\" /></object>").
-define(LJ_URL_ITEM, "<p><img src=\"~s\" border=\"0\" style=\"border: 1px solid #cdcdcd;\"></p>").
-define(LJ_POST_SIGNATURE_START, "<div><small><a href=\"http://moodbox.com/moodstrip/~.10B\">").
-define(LJ_POST_SIGNATURE_END, "</a>, was painted in <a href=\"http://moodbox.com\">Moodbox</a></small><div>").
-define(LJ_POST_SIGNATURE_START_RU, "<div><small><a href=\"http://risovaska.ru/comic/~.10B\">").
-define(LJ_POST_SIGNATURE_END_RU, "</a>, " ++ [1085,1072,1088,1080,1089,1086,1074,1072,1085,1086,32,1074] ++ " <a href=\"http://risovaska.ru\">" ++ [1056,1080,1089,1086,1074,1072,1089,1100,1082,1077] ++ "</a></small></div>").


-define(MAX_LAST_MOODSTRIPS, 10).

-define(RESET_CODE_LENGTH, 30).
-define(AUTO_GENERATE_PASSWORD_LENGTH, 20).

-define(AUTH_TICKET_TIME_TO_LIVE, moodbox_datetime:minutes(15)).

-define(MAIL_PSWD_REMIND_HEADER, "Moodbox password reminder").
-define(MAIL_PSWD_REMIND_HEADER_RU, "Сброс пароля для Рисоваськи").
-define(MAIL_PSWD_REMIND_BODY,
"  Dear MoodBoxer,\r\n\r\n"
"Forgot your password? No problem!\r\n\r\n"
"Just click on the following link:\r\n\r\n"
"http://moodbox.com/reset?r=~s&u=~.10B\r\n\r\n"
"Here, you'll get a new password.\r\n\r\n\r\n"
"Yours,\r\n"
"MoodBox Team\r\n\r\n").
-define(MAIL_PSWD_REMIND_BODY_RU,
"Привет!\r\n\r\n"
"Чтобы получить новый пароль для Рисоваськи, перейди по этой ссылке:\r\n\r\n"
"http://risovaska.ru/reset?r=~s&u=~.10B\r\n"
"Если ссылка не работает, скопируй её и вставь в адресную строку браузера.\r\n\r\n"
"С уважением,\r\n"
"Команда Рисоваськи").

-define(MAIL_MOODSTRIP_COMMENT_HEADER, "Moodstrip comment").
-define(MAIL_MOODSTRIP_COMMENT_HEADER_RU, "Комментарий на ваш комикс").
-define(MAIL_MOODSTRIP_COMMENT_BEGIN, "User ~s wrote following comment on your moodstrip ").
-define(MAIL_MOODSTRIP_COMMENT_END, "You can see all comments on moodstrip's page: http://moodbox.com/moodstrip/~.10B\r\n\r\n"
"Yours,\r\n"
"MoodBox Team\r\n\r\n").
-define(MAIL_MOODSTRIP_COMMENT_BEGIN_RU, "Пользователь ~s оставил комментарий на ваш комикс ").
-define(MAIL_MOODSTRIP_COMMENT_END_RU, "Вы можете увидеть все комментарии на странице комикса: http://risovaska.ru/comic/~.10B\r\n\r\n"
"С уважением,\r\n"
"Команда Рисоваськи").

-define(MAIL_NEWS_SUBJECT, "Новая версия Рисоваськи - 1.4. Новый формат сообщений, рисованный блоггинг.").
-define(MAIL_NEWS_BODY, "Добрый день!\r\n\r\n"
"Сегодня вышла новая версия Рисоваськи с двумя важными возможностями.\r\n\r\n"
"1. Сообщения занимают в 3 раза меньший размер. Теперь Рисоваська доступна для\r\n"
"всех, а не только для пользователей с выделенным каналом. Кроме того, \r\n"
"значительно возросла скорость приема и отправки сообщений, что порадует и \r\n"
"обладателей высокоскоростного интернета.\r\n\r\n"
"2. Начато движение в сторону рисованного блоггинга - теперь можно отправлять \r\n"
"рисованные сообщения в свой блог прямо из Рисоваськи. Мы просим тебя \r\n"
"попробовать эту возможность и оставить комментарии и предложения в блоге \r\n"
"Рисоваськи - http://community.livejournal.com/risovaska/24400.html\r\n\r\n"
"Важно! Из-за серьёзных изменений протокола текущая версия Рисоваськи \r\n"
"перестанет работать, вы получите сообщение о том, что ваша Рисоваська \r\n"
"устарела и предложение скачать новую версию с сайта. При получении такого \r\n"
"сообщения нужно:\r\n\r\n"
"1. Зайти по указанной ссылке на сайт и скачать новую версию\r\n"
"2. Закрыть текущую версию Рисоваськи\r\n"
"3. По завершению загрузки новой версии - установить её\r\n\r\n"
"Хорошая новость - в новую версию мы встроили функцию автоматического \r\n"
"обновления программы, достаточно будет нажать на кнопку \"Скачать и установить\" \r\n"
"для обновления Рисоваськи до самой свежей версии.\r\n\r\n"
"Новая версия Рисоваськи - http://risovaska.ru/download\r\n\r\n"
"С уважением, \r\n"
"Команда Рисоваськи\r\n"
"http://risovaska.ru\r\n\r\n"
"///////\r\n\r\n"
"If you can’t read this message, plz go here - http://moodbox.com/blog/?p=49\r\n\r\n"
"Вы получили это письмо, поскольку выбрали опцию \"Получать новости Рисоваськи\". \r\n"
"Чтобы отписаться от наших новостей перейдите по ссылке - ").

-define(SMTP_SERVER, "mail.ksan.ru").
-define(SMTP_PORT, 25).
-define(MAIL_FROM, "info@risovaska.ru").
-define(MAIL_LOGIN, "login").
-define(MAIL_PASSWORD, "pass").

-define(AES_KEY, <<"your_key">>).
-define(AES_VECTOR, <<"your_vector">>).
-define(SECRET_CODE, "your_secret_code").

-define(SSL_KEYFILE, "priv/ssl/rsa-key.pem").
-define(SSL_CERTFILE, "priv/ssl/cert.pem").
-define(SSL_CACERTFILE, undefined).
-define(SSL_VERIFY, undefined).
-define(SSL_PASSWORD, undefined).
-define(SSL_CIPHERS, undefined).

%% notification macros
-define(NOTIFICATION_KEY_LENGTH, 10).
-define(SERVER_URL, "https://~s:443/do").
-define(NOTIFICATION_COUNT_OF_CONNECTION_ATTEMPS, 5).
-define(GET_NOTIFICATION_TIMEOUT, moodbox_datetime:minutes(1)).
-define(NOTIFICATION_USER_TIMEOUT, moodbox_datetime:minutes(1)).
-define(CHECK_NOTIFICATION_ERROR_INTERVAL, moodbox_datetime:seconds(90)).
-define(CHECK_NOTIFICATION_USER_INTERVAL, moodbox_datetime:seconds(60)).
-define(NOTIFICATION_SERVER_KILL_INTERVAL, ?CHECK_NOTIFICATION_ERROR_INTERVAL * 3).