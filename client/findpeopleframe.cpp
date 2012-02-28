#include "findpeopleframe.h"

#include <QTableWidgetItem>
#include <QHeaderView>
#include <QMouseEvent>
#include <QMessageBox>
#include <QUrl>
#include <QDesktopServices>

#include "uitools.h"
#include "contactinfo.h"
#include "serverproxysingleton.h"
#include "authorizationdialog.h"
#include "peopleinfomanager.h"
#include "international.h"
#include "formblocker.h"
#include "contactinfodialog.h"

#include "debug.h"
#include "testtools.h"

namespace MoodBox
{

static const float SearchColumnWidthsInPercent[4] = {LOGIN_COLUMN_PERCENT_WIDTH, NAME_COLUMN_PERCENT_WIDTH, LOCATION_COLUMN_PERCENT_WIDTH, PROFILE_COLUMN_PERCENT_WIDTH};

// SearchPeopleRequest class
// Constructor for simple search
SearchPeopleRequest::SearchPeopleRequest(FindPeopleFrame *parent, qint32 pageNumber, qint32 recordsPerPage, QString value)
	: ServerRequest()
{
	// Connect parent update
	connect(this, SIGNAL(searchCompleted(Fault, UserSearchResult)), parent, SLOT(onSearchContacts(Fault, UserSearchResult)));

	SERVER->simpleSearchContacts(CALLBACK(this, onSearchResult, UserSearchResult), pageNumber, recordsPerPage, value);
}

// Constructor for advanced search
SearchPeopleRequest::SearchPeopleRequest(FindPeopleFrame *parent, qint32 pageNumber, qint32 recordsPerPage, QString value, QLocale::Country country, QString city, Sex::SexEnum sex, qint32 minAge, qint32 maxAge)
	: ServerRequest()
{
	// Connect parent update
	connect(this, SIGNAL(searchCompleted(Fault, UserSearchResult)), parent, SLOT(onSearchContacts(Fault, UserSearchResult)));

	SERVER->advancedSearchContacts(CALLBACK(this, onSearchResult, UserSearchResult), pageNumber, recordsPerPage, value, country, city, sex, minAge, maxAge);
}

void SearchPeopleRequest::onSearchResult(QVariant state, Fault fault, UserSearchResult result)
{
	Q_UNUSED(state)
	
	if (active)
		emit searchCompleted(fault, result);

	deleteLater();
}

// PeopleProfileButton - to show contact info (for FindPeopleFrame only)
class PeopleProfileButton : public QToolButton
{
public:
	PeopleProfileButton(QWidget *parent = NULL) : QToolButton(parent), isSelected(false)
	{
		this->setCursor(Qt::PointingHandCursor); 

		this->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
	}

	void setSelected(bool isSelected)
	{
		this->isSelected = isSelected; 
	
		if (isSelected) 
			this->setIcon(QIcon(":/MoodBox/Resources/button_info.png"));
		else
			this->setIcon(QIcon(":/MoodBox/Resources/transparent.png")); 
	}

	void setHovered(bool isHovered)
	{
		if (isSelected)
			return;

		if (isHovered)
			this->setIcon(QIcon(":/MoodBox/Resources/button_info.png"));
		else
			this->setIcon(QIcon(":/MoodBox/Resources/transparent.png")); 
	}

protected:
	virtual void mousePressEvent(QMouseEvent *event)
	{
		Q_UNUSED(event)

		event->ignore();
	}

	virtual void mouseReleaseEvent(QMouseEvent *event)
	{
		Q_UNUSED(event)

		event->ignore();
	}

	virtual void enterEvent(QEvent *event)
	{
		Q_UNUSED(event)

		this->setIcon(QIcon(":/MoodBox/Resources/button_info.png")); 
	}

	virtual void leaveEvent(QEvent *event)
	{
		Q_UNUSED(event)

		if (!isSelected) 
			this->setIcon(QIcon(":/MoodBox/Resources/transparent.png"));
	}

private:
	bool isSelected;
};

// FindPeopleFrame class
FindPeopleFrame::FindPeopleFrame(QWidget *parent)
	: ServerFrame(parent), currentRequest(NULL), previousHoveredRow(-1)
{
	TimeMeasure t("FindPeopleFrame");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	// Setup countries controls
	for (int i = QLocale::AnyCountry; i != QLocale::LastCountry; i++)
		countryCombo->addItem(UiTools::getCountryName((QLocale::Country)i), i);

	// Setup gender controls
	genderCombo->addItem(tr(SEX_UNDEFINED), Sex::Undefined);
	genderCombo->addItem(tr(SEX_MALE), Sex::Male);
	genderCombo->addItem(tr(SEX_FEMALE), Sex::Female);

	// Setup age range controls
	ageRangeCombo->addItem(tr(AGE_RANGE_ALL));
	ageRangeCombo->addItem(AGE_RANGE_1_17);
	ageRangeCombo->addItem(AGE_RANGE_18_23);
	ageRangeCombo->addItem(AGE_RANGE_24_30);
	ageRangeCombo->addItem(AGE_RANGE_31_200);
	
	// Setup contacts table
	QStringList labels;
	labels << tr(LOGIN_COLUMN_NAME) << tr(NAME_COLUMN_NAME) << tr(LOCATION_COLUMN_NAME) << tr(PROFILE_COLUMN_NAME);

	contactTable->setHorizontalHeaderLabels(labels);

	updateColumnsWidth();
	
	contactTable->horizontalHeader()->setCascadingSectionResizes(true);
	contactTable->verticalHeader()->setVisible(false);
	contactTable->setMouseTracking(true);
	
	connect(contactTable, SIGNAL(cellDoubleClicked(int, int)), this, SLOT(onCellDoubleClicked(int, int)));
	connect(contactTable, SIGNAL(cellClicked(int, int)), this, SLOT(onCellClicked(int, int)));
	connect(contactTable, SIGNAL(currentCellChanged(int, int, int, int)), this, SLOT(onCurrentCellChanged(int, int, int, int)));
	connect(contactTable, SIGNAL(cellEntered(int, int)), this, SLOT(onCellEntered(int, int)));
	connect(contactTable, SIGNAL(addUserTriggered()), this, SLOT(addAsFriend()));
	connect(contactTable, SIGNAL(userInfoTriggered(int)), this, SLOT(onUserInfoTriggered(int)));
	connect(contactTable, SIGNAL(webProfileTriggered(int)), this, SLOT(onWebProfileTriggered(int)));

	// Blocker
	formBlocker->addWidget(this);

	advancedSearchGroup->hide();	
}

void FindPeopleFrame::clearData()
{
	searchNameEdit->setText(QString());

	contactTable->hide();
	contactTable->setSortingEnabled(false);
	contactTable->setRowCount(0);

	foundUsersLabel->hide();

	contacts.clear();

	frameMid->show();
}

void FindPeopleFrame::addAsFriend()
{
	if (!INFOMANAGER->isKnownPerson(selectedContact.getUserId()))
	{
		AuthorizationDialog *authDialog = new AuthorizationDialog(this);
		authDialog->request(selectedContact);
	}
	else
	{
		// The user is already in contact list
		if (selectedContact.getUserId() == INFOMANAGER->getUserAccount().getId())
			QMessageBox::information(this, tr(TRY_TO_ADD_YOURSELF_IN_CONTACTLIST_TITLE), tr(TRY_TO_ADD_YOURSELF_IN_CONTACTLIST_MESSAGE));
		else
			QMessageBox::information(this, tr(ALREADY_IN_CONTACTLIST_TITLE), tr(ALREADY_IN_CONTACTLIST_MESSAGE).arg(selectedContact.getDisplayName()));
	}
}

void FindPeopleFrame::onSearchContacts(Fault fault, UserSearchResult result)
{
	currentRequest = NULL;
	setBlock(false);

	if (!fault.isNull())
	{
		UiTools::handleError(this, tr(SEARCH_CONTACTS_ERROR_TITLE), fault);
		return;
	}

	// Copy and show found contacts
	contacts = result.getItems();
	showFoundContacts();
}

void FindPeopleFrame::resizeEvent(QResizeEvent *event)
{
	QFrame::resizeEvent(event);

	updateColumnsWidth();
}

void FindPeopleFrame::showEvent(QShowEvent *event)
{
	QFrame::showEvent(event);

	searchNameEdit->setFocus();
}

void FindPeopleFrame::onRequestCancelled()
{
	if (currentRequest != NULL)
		currentRequest->detach();

	setBlock(false);
}

void FindPeopleFrame::getMinMaxAgeRange(qint32 &minAge, qint32 &maxAge) const
{
	// Check if age range is set to "All"
	if (ageRangeCombo->currentIndex() == 0)
	{
		minAge = 0;
		maxAge = 0;

		return;
	}

	// Check if age range is set to last "31+"
	if (ageRangeCombo->currentIndex() == AGE_RANGE_LAST_INDEX)
	{
		minAge = 31;
		maxAge = 200;

		return;
	}

	// In other case - parse age range string
	QString range = ageRangeCombo->currentText();
	QStringList bounds = range.split(AGE_RANGE_SEPARATOR, QString::SkipEmptyParts);

	minAge = bounds.at(0).toInt();
	maxAge = bounds.at(1).toInt();
}

void FindPeopleFrame::updateColumnsWidth()
{
	int currentTableWidth = width() - 66;

	for (int column = 0; column <= PROFILE_COLUMN_NUMBER; column++)
	{
		int columnWidth = currentTableWidth * SearchColumnWidthsInPercent[column];
		contactTable->setColumnWidth(column, columnWidth);
	}
}

void FindPeopleFrame::setBlock(bool on)
{
	if (on)
		formBlocker->block();
	else
		formBlocker->unblock();

	emit canAddFriend(!on && (contactTable->currentItem() != NULL));
}

void FindPeopleFrame::setRowHovered(int row, bool isHovered)
{
	QWidget *buttonWidget = contactTable->cellWidget(row, PROFILE_COLUMN_NUMBER);

	PeopleProfileButton *button = static_cast<PeopleProfileButton *>(buttonWidget);
	
	if (button != NULL)
		button->setHovered(isHovered);
}

void FindPeopleFrame::setRowSelected(int row, bool isSelected)
{
	if (isSelected)
	{
		contactTable->setFocus();
		contactTable->setCurrentCell(row, 0);
	}

	QWidget *buttonWidget = contactTable->cellWidget(row, PROFILE_COLUMN_NUMBER);

	PeopleProfileButton *button = static_cast<PeopleProfileButton *>(buttonWidget);

	if (button != NULL)
		button->setSelected(isSelected);
}

void FindPeopleFrame::on_findPeopleButton_clicked()
{
	setBlock(true);

	if (!advancedSearchCheckBox->isChecked())
	{
		// Simple search
		currentRequest = new SearchPeopleRequest(this, 1, RECORDS_PER_PAGE, searchNameEdit->text().trimmed());
		return;
	}
	
	// Advanced search
	QLocale::Country country = (QLocale::Country) countryCombo->currentIndex();
	Sex::SexEnum sex = (Sex::SexEnum) genderCombo->currentIndex();

	qint32 minAge, maxAge;
	getMinMaxAgeRange(minAge, maxAge);

	currentRequest = new SearchPeopleRequest(this, 1, RECORDS_PER_PAGE, searchNameEdit->text(), country, cityEdit->text(), sex, minAge, maxAge);
}

void FindPeopleFrame::on_advancedSearchCheckBox_stateChanged(int state)
{
	if (state != Qt::Checked)
		advancedSearchGroup->hide();
	else
		advancedSearchGroup->show();
}

void FindPeopleFrame::onCurrentCellChanged(int currentRow, int currentColumn, int previousRow, int previousColumn)
{
	Q_UNUSED(currentColumn)
	Q_UNUSED(previousColumn)

	QTableWidgetItem *item = contactTable->item(currentRow, LOGIN_COLUMN_NUMBER);

	emit canAddFriend(item != NULL);

	if (item == NULL)
		return;

	setRowSelected(previousRow, false);
	setRowSelected(currentRow, true);

	// Store selected contact
	selectedContact = item->data(SEARCH_USERINFO_ROLE).value<UserInfo>();
}

void FindPeopleFrame::onCellClicked(int row, int column)
{
	if (column != PROFILE_COLUMN_NUMBER)
		return;

	QTableWidgetItem *item = contactTable->item(row, LOGIN_COLUMN_NUMBER);

	if (item == NULL)
		return;

	UserInfo clickedContact = item->data(SEARCH_USERINFO_ROLE).value<UserInfo>();
	ContactInfoDialog *infoDialog = new ContactInfoDialog(clickedContact, this);
	infoDialog->show();
}

void FindPeopleFrame::onCellDoubleClicked(int row, int column)
{
	Q_UNUSED(row)

	if (column == PROFILE_COLUMN_NUMBER)
		return;

	addAsFriend();
}

void FindPeopleFrame::onCellEntered(int row, int column)
{
	Q_UNUSED(column)

	if (previousHoveredRow != -1)
		setRowHovered(previousHoveredRow, false);

	setRowHovered(row, true);

	previousHoveredRow = row;
}

void FindPeopleFrame::onUserInfoTriggered(int row)
{
	onCellClicked(row, PROFILE_COLUMN_NUMBER);
}

void FindPeopleFrame::onWebProfileTriggered(int row)
{
	QTableWidgetItem *item = contactTable->item(row, LOGIN_COLUMN_NUMBER);

	if (item == NULL)
		return;

	UserInfo triggeredContact = item->data(SEARCH_USERINFO_ROLE).value<UserInfo>();
	QUrl profileUrl = QUrl(QString(PROFILE_LINK).arg(triggeredContact.getUserId()));
	QDesktopServices::openUrl(profileUrl);
}

void FindPeopleFrame::showFoundContacts()
{
	contactTable->setSortingEnabled(false);
	contactTable->setRowCount(contacts.count());

	if (contacts.count() == 0)
	{
		// Users not found
		foundUsersLabel->setText(tr(USERS_NOT_FOUND_LABEL));
		foundUsersLabel->show();
		contactTable->setDisabled(true);
		return;
	}
	
	foundUsersLabel->setText(tr(USERS_FOUND_LABEL).arg(contacts.count()));
	foundUsersLabel->show();
	frameMid->hide();
	contactTable->setDisabled(false);
	contactTable->show();
	
	selectedContact = contacts.first();

	int row = 0;
	foreach (UserInfo currentContact, contacts)
	{
		// Get icon
		QIcon statusIcon;

		if (currentContact.getStatus() == UserStatus::Online)
			statusIcon = QIcon(ONLINE_STATUS_ICON);
		else
			statusIcon = QIcon(OFFLINE_STATUS_ICON);

		// Add item
		QTableWidgetItem *loginItem = new QTableWidgetItem(statusIcon, currentContact.getLogin());
		QVariant contactData;
		contactData.setValue<UserInfo>(currentContact);
		loginItem->setData(SEARCH_USERINFO_ROLE, contactData);

		contactTable->setItem(row, LOGIN_COLUMN_NUMBER, loginItem);
		contactTable->setItem(row, NAME_COLUMN_NUMBER, new QTableWidgetItem(currentContact.getName()));
		
		// Fill location
		QString location("%1");
		if (!currentContact.getCity().isEmpty())
		{
			location = ", %1";
			location.push_front(currentContact.getCity());
		}
		contactTable->setItem(row, LOCATION_COLUMN_NUMBER, new QTableWidgetItem(location.arg(UiTools::getCountryName(currentContact.getCountry()))));
		
		// Add profile button
		PeopleProfileButton *profileButton = new PeopleProfileButton;
		profileButton->setIconSize(QSize(ROW_HEIGHT, ROW_HEIGHT));
		profileButton->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
		profileButton->setAccessibleName("infoButton");
		
		contactTable->setCellWidget(row, PROFILE_COLUMN_NUMBER, profileButton);

		contactTable->setRowHeight(row, ROW_HEIGHT);

		row++;
	}

	contactTable->setSortingEnabled(true);
	
	// Sort
	contactTable->sortItems((selectedContact.getName().isEmpty()) ? LOGIN_COLUMN_NUMBER : NAME_COLUMN_NUMBER);

	// Select first
	setRowSelected(0, true);
}

}