#include "uitools.h"

#include <QApplication>
#include <QStyle>
#include <QRect>
#include <QDesktopWidget>
#include <QCommonStyle>
#include <QMouseEvent>
#include <QPixmap>
#include <QPainter>
#include <QTextDocument>
#include <QMenu>
#include <QDesktopServices>
#include <QUrl>
#include <QDir>

#include "international.h"

#ifdef Q_WS_MAC
#include "mactools.h"
#endif
#ifdef Q_WS_X11
#include "linuxtools.h"
#endif

namespace MoodBox
{

QString UiTools::getFaultText(Fault fault)
{
	QString result = fault.getDescription();

	if(result.isEmpty())
	{
		QString code = fault.getCode();
		if(code == FAULT_TRANSPORT_ERROR)
		{
			result = QObject::tr(FAULT_TEXT_TRANSPORT_ERROR);
			QString details = fault.getDetails(); // details can containt network error details
			if(!details.isEmpty())
			{
				result = result.arg(details);
			}
		}
		else if(code == FAULT_REQUEST_TOO_LARGE)
		{
			result = QObject::tr(FAULT_TEXT_REQUEST_TOO_LARGE);
		}
		else if(code == FAULT_REQUEST_CANCELLED)
		{
			result = QObject::tr(FAULT_TEXT_REQUEST_CANCELLED);
		}
		else if(code == FAULT_REQUEST_TIMED_OUT)
		{
			result = QObject::tr(FAULT_TEXT_REQUEST_TIMED_OUT);
		}
		else if(code == FAULT_PARSER_ERROR)
		{
				result = QObject::tr(FAULT_TEXT_PARSER_ERROR);
		}
	}

	return result;
}

void UiTools::handleError(QWidget *parent, QString title, Fault fault)
{
	handleError(parent, title, getFaultText(fault));
}

void UiTools::handleError(QWidget *parent, QString title, QString text)
{
	QMessageBox::warning(parent, title, text);
}

QMessageBox::StandardButton UiTools::showDialog(QWidget *parent, QString title, QString text, QMessageBox::StandardButtons buttons)
{
	return QMessageBox::warning(parent, title, text, buttons);
}

void UiTools::moveWindowToScreenCenter(QWidget *dialog)
{
	QRect availableScreen = QApplication::desktop()->availableGeometry();
	int titleSize = QCommonStyle().pixelMetric(QStyle::PM_TitleBarHeight);
	dialog->move(availableScreen.width()/2 - dialog->width()/2, (availableScreen.height() - titleSize)/2 - dialog->height()/2);
}

QString UiTools::createHtmlLink(const QString &style, const QString &value)
{
	return QString("<a style=\"%1\" href=\"about:blank\">%2</a>").arg(style, Qt::escape(value));
}

QString UiTools::getCountryName(QLocale::Country country)
{
	if (country == QLocale::AnyCountry)
		return QObject::tr(NO_COUNTRY_TEXT);
	
	QString result = QLocale::countryToString(country);
	bool capsFound = false;

	for (int i = 1; i < result.length(); i++)
	{
		if (result.at(i).isUpper())
		{
			capsFound = true;
			result.insert(i, " ");
			i++;
		}
	}

	if (capsFound)
	{
		result.replace(" And ", " and ");
		result.replace(" Of ", " of ");
		result.replace(" The ", " the ");
		result.replace("St ", "St. ");
		result.replace("Mc Donald", "McDonald");
		result.replace("U S ", "US ");
	}

	return QObject::tr(result.toAscii());;
}

// This function fits text in rect (cut by rect)
QString UiTools::cropTextByRect(QFontMetrics *fontMetrics, QRect &rect, QString text, int textOption, bool &isCropped)
{
	isCropped = false;
	if (text.isEmpty())
	{
		return text;
	}

	QRect fullRect = fontMetrics->boundingRect(rect, textOption, text);

	if ((fullRect.height() <= rect.height() && fullRect.width() <= rect.width()) || text.length() < 4)
	{
		return text;
	}

	isCropped = true;
	while (text.length() > 0)
	{
		text.truncate(text.length() - 1);
		text += "...";
		fullRect = fontMetrics->boundingRect(rect, textOption, text);
		if ((fullRect.height() <= rect.height() && fullRect.width() <= rect.width()) || text.length() == 4)
		{
			break;
		}
		text.truncate(text.length() - 3);
	}
	return text;
}

// Function for sorting contacts
bool UiTools::contactLessThan(ContactInfo contact1, ContactInfo contact2)
{
	return contact1 < contact2;
}

void UiTools::showHelp()
{ 
	static QUrl helpUrl = QUrl(HELP_URL);
	QDesktopServices::openUrl(helpUrl);
}

bool UiTools::rmDirRecursive(QString path)
{
	bool isFullSuccess = true;

	QDir dir(path);

	if(dir.exists())
	{

		QStringListIterator filesIterator(dir.entryList(QDir::Files | QDir::Hidden | QDir::System));
		while (filesIterator.hasNext())
			isFullSuccess = dir.remove(filesIterator.next()) && isFullSuccess;

		QStringListIterator dirsIterator(dir.entryList(QDir::Dirs | QDir::NoDotAndDotDot));
		while (dirsIterator.hasNext())
			isFullSuccess = rmDirRecursive(dir.filePath(dirsIterator.next())) && isFullSuccess;

		QString name = dir.dirName();
		isFullSuccess = isFullSuccess && dir.cdUp() && dir.rmdir(name);
	}

	return isFullSuccess;
}
    
// MoodBoxDialog class
MoodBoxDialog::MoodBoxDialog(QWidget *parent, Qt::WindowFlags flags)
	: QDialog(parent, flags), isMousePressed(false)
{
#ifdef Q_WS_MAC
    MacTools::addWindow(this);
#endif
#ifdef Q_WS_X11
    LinuxTools::addWindow(this);
#endif

}
    
MoodBoxDialog::~MoodBoxDialog()
{
#ifdef Q_WS_MAC
    MacTools::removeWindow(this);
#endif
#ifdef Q_WS_X11
    LinuxTools::removeWindow(this);
#endif

}
    

void MoodBoxDialog::mousePressEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
	{
		dragPosition = event->globalPos() - frameGeometry().topLeft();
		isMousePressed = true;
		event->accept();
	}
}

void MoodBoxDialog::mouseMoveEvent(QMouseEvent *event)
{
	if (event->buttons() & Qt::LeftButton && isMousePressed)
	{
		move(event->globalPos() - dragPosition);
		event->accept();
	}
}

void MoodBoxDialog::mouseReleaseEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
	{
		isMousePressed = false;
	}
}

void MoodBoxDialog::resizeEvent(QResizeEvent *event)
{
	setMask(WidgetMaskCreator::createMask(WidgetMaskCreator::WidgetMaskSmall, width(), height()));
	QDialog::resizeEvent(event);
}

void MoodBoxDialog::changeEvent(QEvent *event)
{
	if (event->type() == QEvent::LanguageChange) 
	{
		retranslate();
	}

	QDialog::changeEvent(event);
}
    

// MovableWidget class
MovableWidget::MovableWidget(QWidget *parent, Qt::WFlags flags)
	: QWidget(parent, flags), isMousePressed(false)
{
#ifdef Q_WS_MAC
    MacTools::addWindow(this);
#endif
#ifdef Q_WS_X11
    LinuxTools::addWindow(this);
#endif

}
    
MovableWidget::~MovableWidget()
{
#ifdef Q_WS_MAC
    MacTools::removeWindow(this);
#endif
#ifdef Q_WS_X11
    LinuxTools::removeWindow(this);
#endif

}

void MovableWidget::mousePressEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
	{
		dragPosition = event->globalPos() - frameGeometry().topLeft();
		isMousePressed = true;
		event->accept();

		return;
	}

	QWidget::mousePressEvent(event);
}

void MovableWidget::mouseMoveEvent(QMouseEvent *event)
{
	if (event->buttons() & Qt::LeftButton && isMousePressed)
	{
		QPoint newPos = event->globalPos() - dragPosition;
		move(newPos);
		event->accept();

		emit dragged(newPos);

		return;
	}

	QWidget::mouseMoveEvent(event);
}

void MovableWidget::mouseReleaseEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
	{
		isMousePressed = false;
	}

	QWidget::mouseReleaseEvent(event);
}

// WidgetMaskCreator class
QBitmap WidgetMaskCreator::createMask(WidgetMaskType maskType, int width, int height)
{
	int x = 0, y = 0; // corner size
	QString maskFile;

	if (maskType == WidgetMaskMain)
	{
		maskFile = ":/MoodBox/Resources/panel_main_mask.png";
		x = 14;
		y = 14;
	}
	else if (maskType == WidgetMaskSmall)
	{
		maskFile = ":/MoodBox/Resources/panel_mask.png";
		x = 9;
		y = 9;
	}
	else if (maskType == ContactListMenu)
	{
		maskFile = ":/MoodBox/Resources/cl_menu_mask.png";
		x = 5;
		y = 5;
	}
	else if (maskType == WidgetMaskHistory)
	{
		maskFile = ":/MoodBox/Resources/panel_history_mask.png";
		x = 20;
		y = 20;
	}
	else if (maskType == WidgetMaskContacts)
	{
		maskFile = ":/MoodBox/Resources/panel_contacts_mask.png";
		x = 20;
		y = 20;
	}

	QPixmap mask(maskFile);
	QPixmap resizedMask(width, height);

	QPainter p(&resizedMask);
	
	// 4 corners
	p.drawPixmap(QRectF(0, 0, x, y), mask, QRectF(0, 0, x, y));
	p.drawPixmap(QRectF(0, height - y, x, y), mask, QRectF(0, mask.height() - y, x, y));
	p.drawPixmap(QRectF(width - x , 0, x, y), mask, QRectF(mask.width() - x, 0, x, y));
	p.drawPixmap(QRectF(width - x, height - y, x, y), mask, QRectF(mask.width() - x, mask.height() - y, x, y));

	// 4 sides
	p.drawPixmap(QRectF(QPointF(0, y), QPointF(x, height - y)), mask, 
		QRectF(QPointF(0, y), QPointF(x, mask.height() - y)));
	p.drawPixmap(QRectF(QPointF(width - x, y), QPointF(width, height - y)), mask, 
		QRectF(QPointF(mask.width() - x, y), QPointF(mask.width(), mask.height() - y)));

	p.drawPixmap(QRectF(QPointF(x, 0), QPointF(width - x, y)), mask, 
		QRectF(QPointF(x, 0), QPointF(mask.width() - x, y)));
	p.drawPixmap(QRectF(QPointF(x, height - y), QPointF(width - x, height)), mask, 
		QRectF(QPointF(x, mask.height() - y), QPointF(mask.width() - x, mask.height())));

	// middle
	p.drawPixmap(QRectF(QPointF(x, y), QPointF(width - x, height - y)), mask, 
		QRectF(QPointF(x, y), QPointF(mask.width() - x, mask.height() - y)));

	return resizedMask.createMaskFromColor(Qt::black, Qt::MaskOutColor);
}

// ContactTable class
ContactTable::ContactTable(QWidget *parent)
	: QTableWidget(parent), contextMenu(NULL) 
{
	createContextMenu();
}

void ContactTable::mousePressEvent(QMouseEvent *event)
{
	if (event->button() == Qt::RightButton)
	{
		QTableWidgetItem *item = itemAt(event->x(), event->y());
		
		if (item != NULL)
		{
			currentRow = row(item);
			setCurrentCell(currentRow, 0);
			contextMenu->exec(QCursor::pos());
		}

		event->accept();
	}
	else
		QTableWidget::mousePressEvent(event);
}

void ContactTable::createContextMenu()
{
	contextMenu = new QMenu(this);

	contextMenu->addAction(tr(ADD_USER_MENU_ITEM), this, SIGNAL(addUserTriggered()));
	contextMenu->addAction(tr(USER_INFO_MENU_ITEM), this, SLOT(onUserInfoTriggered()));
	contextMenu->addAction(tr(WEB_PROFILE_MENU_ITEM), this, SLOT(onWebProfileTriggered()));
}

void ContactTable::onUserInfoTriggered()
{
	emit userInfoTriggered(currentRow);
}

void ContactTable::onWebProfileTriggered()
{
	emit webProfileTriggered(currentRow);
}

}
