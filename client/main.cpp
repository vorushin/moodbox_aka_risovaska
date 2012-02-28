#include <QtGui/QApplication>
#include <QTextCodec>
#include <QDateTime>
#include <QFile>
#include <QDir>
#include <QXmlInputSource>
#include <QXmlSimpleReader>
#include <QFontDatabase>
#include <QStringList>
#include <QTranslator>

#include <QtPlugin>

Q_IMPORT_PLUGIN(qgif);
Q_IMPORT_PLUGIN(qjpeg);
Q_IMPORT_PLUGIN(qmng);
Q_IMPORT_PLUGIN(qsvg);
Q_IMPORT_PLUGIN(qtiff);

#include "mainwindow.h"
#include "common.h"
#include "debug.h"
#include "programsettings.h"
#include "customtranslator.h"

#include "xmlparser.h"
#include "xmlpropertywriter.h"
#include "serverproxysingleton.h"

#include "envelope.h"
#include "debug.h"

#include "qtsingleapplication.h"

#include "international.h"

#ifdef Q_WS_MAC
#include "mactools.h"
#endif

using namespace MoodBox;

void loadStyleSheet()
{
	QFile file(":/MoodBox/Resources/default.qss");
	file.open(QFile::ReadOnly);
	QString styleSheet = QLatin1String(file.readAll());

#ifdef RUSSIAN_VERSION
	QFile file2(":/MoodBox/Resources/default_ru.qss");
	file2.open(QFile::ReadOnly);
	styleSheet += "\r";
	styleSheet += QLatin1String(file2.readAll());
#endif
    
#ifdef Q_WS_WIN
	QFile file3(":/MoodBox/Resources/default_win.qss");
	file3.open(QFile::ReadOnly);
	styleSheet += "\r";
	styleSheet += QLatin1String(file3.readAll());
#endif

#ifdef Q_WS_MAC
	QFile file3(":/MoodBox/Resources/default_mac.qss");
	file3.open(QFile::ReadOnly);
	styleSheet += "\r";
	styleSheet += QLatin1String(file3.readAll());
#endif 

#ifdef Q_WS_X11
	QFile file3(":/MoodBox/Resources/default_linux.qss");
	file3.open(QFile::ReadOnly);
	styleSheet += "\r";
	styleSheet += QLatin1String(file3.readAll());
#endif 

	qApp->setStyleSheet(styleSheet);
}

void initLocalization()
{
	// Localization
	CustomTranslator *translator = new CustomTranslator;
#ifdef RUSSIAN_VERSION
	bool isLoaded = translator->load(":/MoodBox/Resources/moodbox_ru");
#else
	bool isLoaded = translator->load(":/MoodBox/Resources/moodbox_en");
#endif
	
	if (!isLoaded)
		QDEBUG("qtTranslator 'moodbox_*.qm' does NOT loaded!!!");
	else
		qApp->installTranslator(translator);
}

int main(int argc, char *argv[])
{
#ifdef Q_WS_MAC
    CocoaInitializer cocoaInit;
#endif
	QtSingleApplication app("MoodBoxReallyUniqueApplication", argc, argv);

	
	// Check for another instance
	bool isSecondInstance = app.sendMessage(SINGLE_APP_MESSAGE);

	// Parsing command line
	bool trayOnlyStart = false;
	QString message;

	for (int i = 1; i < app.arguments().count(); i++)
	{
		QString value = app.arguments().at(i);

		if (value == "--silentstart")
		{
			trayOnlyStart = true;
		}
		else
		{
			message += " " + value;
		}
	}

	if (isSecondInstance)
	{
		if (!message.isEmpty())
			app.sendMessage(message);

#ifndef UDEBUG
		return 0;
#endif
	}

	Q_INIT_RESOURCE(moodbox);
	QTextCodec::setCodecForTr(QTextCodec::codecForName("UTF-8"));
	app.setQuitOnLastWindowClosed(false);

	// Register URL protocol
	ProgramSettings::registerUrlProtocol();

	// Need it to read settings correctly
	QCoreApplication::setOrganizationName(PUBLISHER_NAME);
	QCoreApplication::setOrganizationDomain(PUBLISHER_DOMAIN_NAME);
	QCoreApplication::setApplicationName(APP_NAME);

#ifndef QT_NO_DEBUG
	QDir::current().mkpath("Logs");
#endif

	loadStyleSheet();
	initLocalization();

	// setup application-level proxy
	ProgramSettings::applyProxySettings();

	if (ProgramSettings::getIsFirstAppStart())
		ProgramSettings::setIsStartingWhenComputerStarts(true);

	app.initialize();

	MainWindow mainWindow(app);

#ifdef Q_WS_X11
        mainWindow.setWindowIcon(QIcon(APP_ICON));
#endif

	mainWindow.connect(&app, SIGNAL(messageReceived(const QString &)), &mainWindow, SLOT(ipcMessage(const QString &)));

	// don't show the window when trayOnlyStart, but if cannot logon automatically - show anyway
	if(!trayOnlyStart || !LOGONPROVIDER->isAutoLogonPossible())
	{
		mainWindow.show();
		mainWindow.activateWindow();
	}

	if (!message.isEmpty())
	{
		mainWindow.ipcMessage(message);
	}

	return app.exec();
}
