#ifndef FINDPEOPLEFRAME_H
#define FINDPEOPLEFRAME_H

#include "servercontrol.h"

#include "ui_findpeopleframe.h"

#include <QList>

#include "usersearchresult.h"
#include "serverrequest.h"

class QMouseEvent;

namespace MoodBox
{

// Labels and titles
#define SEARCH_CONTACTS_ERROR_TITLE			QT_TRANSLATE_NOOP("MoodBox::FindPeopleFrame", "SearchContactsErrorTitle")

#define USERS_FOUND_LABEL					QT_TRANSLATE_NOOP("MoodBox::FindPeopleFrame", "UsersFound%1")
#define USERS_NOT_FOUND_LABEL				QT_TRANSLATE_NOOP("MoodBox::FindPeopleFrame", "UsersNotFound")

// Information message box
#define ALREADY_IN_CONTACTLIST_TITLE				QT_TRANSLATE_NOOP("MoodBox::FindPeopleFrame", "UserAlreadyInContactListDialogTitle")
#define ALREADY_IN_CONTACTLIST_MESSAGE				QT_TRANSLATE_NOOP("MoodBox::FindPeopleFrame", "UserAlreadyInContactListDialogText%1")
#define TRY_TO_ADD_YOURSELF_IN_CONTACTLIST_TITLE	QT_TRANSLATE_NOOP("MoodBox::FindPeopleFrame", "TryToAddYourselfInContactListDialogTitle")
#define TRY_TO_ADD_YOURSELF_IN_CONTACTLIST_MESSAGE	QT_TRANSLATE_NOOP("MoodBox::FindPeopleFrame", "TryToAddYourselfInContactListDialogText")


// Output table
// Column names
#define LOGIN_COLUMN_NAME					QT_TRANSLATE_NOOP("MoodBox::FindPeopleFrame", "LoginColumn")
#define NAME_COLUMN_NAME					QT_TRANSLATE_NOOP("MoodBox::FindPeopleFrame", "NameColumn")
#define LOCATION_COLUMN_NAME				QT_TRANSLATE_NOOP("MoodBox::FindPeopleFrame", "LocationColumn")
#define PROFILE_COLUMN_NAME					QT_TRANSLATE_NOOP("MoodBox::FindPeopleFrame", "ProfileColumn")

// Column number
#define LOGIN_COLUMN_NUMBER					0
#define NAME_COLUMN_NUMBER					1
#define LOCATION_COLUMN_NUMBER				2
#define PROFILE_COLUMN_NUMBER				3

// Columns Row and Height
#define LOGIN_COLUMN_PERCENT_WIDTH			0.22f
#define NAME_COLUMN_PERCENT_WIDTH			0.32f
#define LOCATION_COLUMN_PERCENT_WIDTH		0.32f
#define PROFILE_COLUMN_PERCENT_WIDTH		0.12f

#define ROW_HEIGHT							22

// Age range
#define AGE_RANGE_ALL						QT_TRANSLATE_NOOP("MoodBox::FindPeopleFrame", "AgeAll")
#define AGE_RANGE_1_17						"1-17"
#define AGE_RANGE_18_23						"18-23"
#define AGE_RANGE_24_30						"24-30"
#define AGE_RANGE_31_200					"31+"

#define AGE_RANGE_LAST_INDEX				4
#define AGE_RANGE_SEPARATOR					"-"

// Search records per page
#define RECORDS_PER_PAGE					50

// User Info role in item
#define SEARCH_USERINFO_ROLE				Qt::UserRole + 1
	
class FindPeopleFrame;

using namespace Ui;

// People search request
class SearchPeopleRequest : public ServerRequest
{
	Q_OBJECT

public:
	SearchPeopleRequest(FindPeopleFrame *parent, qint32 pageNumber, qint32 recordsPerPage, QString value);
	SearchPeopleRequest(FindPeopleFrame *parent, qint32 pageNumber, qint32 recordsPerPage, QString value, QLocale::Country country, QString city, Sex::SexEnum sex, qint32 minAge, qint32 maxAge);
	
signals:
	void searchCompleted(Fault fault, UserSearchResult result);

private slots:
	void onSearchResult(QVariant state, Fault fault, UserSearchResult result);
};

// Find people frame - search, add friends to contact list
class FindPeopleFrame : public ServerFrame, public FindPeopleFrameClass
{
	Q_OBJECT

public:
	FindPeopleFrame(QWidget *parent = 0);
	
	void clearData();	

signals:
	void canAddFriend(bool can);

public slots:
	void addAsFriend();

	void onSearchContacts(Fault fault, UserSearchResult result);

	virtual void onRequestCancelled();

protected:
	virtual void resizeEvent(QResizeEvent *event);
	virtual void showEvent(QShowEvent *event);

private:
	QList <UserInfo> contacts;
	UserInfo selectedContact;

	SearchPeopleRequest *currentRequest;

	int previousHoveredRow;

	void getMinMaxAgeRange(qint32 &minAge, qint32 &maxAge) const;

	void updateColumnsWidth();
	void setBlock(bool on);

	void setRowHovered(int row, bool isHovered);
	void setRowSelected(int row, bool isSelected);
	
private slots:
	void on_findPeopleButton_clicked();		
	void on_advancedSearchCheckBox_stateChanged(int state);

	void onCurrentCellChanged(int currentRow, int currentColumn, int previousRow, int previousColumn);
	void onCellClicked(int row, int column);
	void onCellDoubleClicked(int row, int column);
	void onCellEntered(int row, int column);

	void onUserInfoTriggered(int row);
	void onWebProfileTriggered(int row);

	void showFoundContacts();
};

}

#endif // FINDPEOPLEFRAME_H
