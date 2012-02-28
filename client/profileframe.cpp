#include "profileframe.h"

#include <QShortcut>
#include <QMessageBox>
#include <QFileDialog>
#include <QPixmap>
#include <QBuffer>
#include <QByteArray>

#include "verifiers.h"
#include "international.h"
#include "peopleinfomanager.h"
#include "selectavatardialog.h"
#include "uitools.h"
#include "changepassworddialog.h"
#include "common.h"
#include "testtools.h"

namespace MoodBox
{

#define YEAR_BASE					1902
#define DEFAULT_USER_AGE			25

// UserAccountUpdateRequest class
UserAccountUpdateRequest::UserAccountUpdateRequest(ProfileFrame *parent, const UserAccount &account,  bool hasUserPicture, const QPixmap userPicture) 
	: ServerRequest()
{
	this->account = account;
	this->hasUserPicture = hasUserPicture;
	this->userPicture = userPicture;

	// Connect parent update
	connect(this, SIGNAL(accountUpdateCompleted(Fault, AccountResultCode::AccountResultCodeEnum)), parent, SLOT(onAccountUpdateCompleted(Fault, AccountResultCode::AccountResultCodeEnum)));

	// Save picture bytes
	QByteArray pictureByteArray;

	if (hasUserPicture)
	{
		QBuffer buffer(&pictureByteArray);
		buffer.open(QIODevice::WriteOnly);
		userPicture.save(&buffer, PICTURES_EXTENSION);
	}

	SERVER->updateAccount(CALLBACK(this, onUpdateAccountResult, AccountResultCode::AccountResultCodeEnum), account, hasUserPicture, pictureByteArray, PICTURES_CONTENTTYPE);
}

void UserAccountUpdateRequest::onUpdateAccountResult(QVariant state, Fault fault, AccountResultCode::AccountResultCodeEnum result)
{
	Q_UNUSED(state)
	
	if (fault.isNull())
	{
		if (result == AccountResultCode::Ok && INFOMANAGER->isUserOnline())
		{
			INFOMANAGER->updateUserAccount(account);
			
			if (hasUserPicture)
				INFOMANAGER->setUserPicture(userPicture);
		}
	}

	if (active)
		emit accountUpdateCompleted(fault, result);

	// No need for this request anymore
	deleteLater();
}

// ProfileFrame class
ProfileFrame::ProfileFrame(QWidget *parent)
	:	SetupDialogFrame(parent), profileWasChanged(false), 
		avatarWasChanged(false), currentRequest(NULL)
{
	TimeMeasure t("ProfileFrame");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	// Setup countries and cities
	for (int i = QLocale::AnyCountry; i != QLocale::LastCountry; i++)
		countryCombo->addItem(UiTools::getCountryName((QLocale::Country)i), i);
	
	// Setup birthdate controls
	fillBirthdayControls();

	connect(monthCombo, SIGNAL(currentIndexChanged(int)), this, SLOT(onMonthComboChanged()));
	// For leap years we need to check month as well
	connect(yearCombo, SIGNAL(currentIndexChanged(int)), this, SLOT(onMonthComboChanged()));

	// Shortcut to clear birthday controls
	QShortcut *birthdayClearShortCut = new QShortcut(this);
	birthdayClearShortCut->setKey(Qt::Key_Delete);
	connect(birthdayClearShortCut, SIGNAL(activated()), this, SLOT(clearBirthday()));
	
	// Setup gender controls
	genderCombo->addItem(tr(SEX_UNDEFINED), Sex::Undefined);
	genderCombo->addItem(tr(SEX_MALE), Sex::Male);
	genderCombo->addItem(tr(SEX_FEMALE), Sex::Female);

	// Verifiers
	formVerifier->addVerifier(new LineEditVerifier(true, this, emailLabel, StandardVerifier::Email));
	formVerifier->addVerifier(new TripleDateVerifier(false, this, birthdayLabel, dayCombo, monthCombo, yearCombo));

	// Blocking controls
	formBlocker->addWidget(mainFrame);

	// Listen if any field in profile form was changed
	connect(mottoTextEdit, SIGNAL(textChanged()), this, SLOT(profileChanged()));
	connect(nameEdit, SIGNAL(textChanged(const QString &)), this, SLOT(profileChanged()));
	connect(countryCombo, SIGNAL(currentIndexChanged(int)), this, SLOT(profileChanged()));
	connect(cityEdit, SIGNAL(textChanged(const QString &)), this, SLOT(profileChanged()));
	connect(genderCombo, SIGNAL(currentIndexChanged(int)), this, SLOT(profileChanged()));
	connect(dayCombo, SIGNAL(currentIndexChanged(int)), this, SLOT(profileChanged()));
	connect(monthCombo, SIGNAL(currentIndexChanged(int)), this, SLOT(profileChanged()));
	connect(yearCombo, SIGNAL(currentIndexChanged(int)), this, SLOT(profileChanged()));
	connect(aboutTextEdit, SIGNAL(textChanged()), this, SLOT(profileChanged()));
	connect(emailEdit, SIGNAL(textChanged(const QString &)), this, SLOT(profileChanged()));
	connect(receiveNewsCheckBox, SIGNAL(stateChanged(int)), this, SLOT(profileChanged()));
	connect(allowWebTranslationCheckBox, SIGNAL(stateChanged(int)), this, SLOT(profileChanged()));
	connect(showMyFriendsCheckBox, SIGNAL(stateChanged(int)), this, SLOT(profileChanged()));

	// Connect change avatar
	connect(avatarToolButton, SIGNAL(clicked()), this, SLOT(onUserAvatarButtonClicked()));

	connect(changePasswordButton, SIGNAL(clicked()), this, SLOT(onChangePasswordButtonClicked()));
}

bool ProfileFrame::isValid()
{
	// If no changes all is good
	if (!profileWasChanged)
		return true;

	if (!formVerifier->verifyAndHighlight(true))
		return false;

	// Hide previous errors, if any
	formVerifier->highlight(WidgetVerifier::Normal);

	return true;
}

bool ProfileFrame::isAccessible()
{
	return (INFOMANAGER->getIsLoggedOn());
}

void ProfileFrame::startUpdate()
{
	if (!profileWasChanged)
	{
		emit updateFinished();
		return;
	}

	if (!isValid())
	{
		emit updateError();
		return;
	}

	// Update contact info from fields
	userAccount.setMotto(mottoTextEdit->toPlainText().trimmed());
	
	userAccount.setName(nameEdit->text().trimmed());
	userAccount.setCountry((QLocale::Country) countryCombo->currentIndex());
	userAccount.setCity(cityEdit->text().trimmed());
	userAccount.setSex((Sex::SexEnum) genderCombo->currentIndex());

	if (dayCombo->currentIndex() > 0 && monthCombo->currentIndex() > 0 && yearCombo->currentIndex() > 0)
		userAccount.setBirthDay(QDate(getYearFromCombo(), monthCombo->currentIndex(), dayCombo->currentIndex()));
	else
		userAccount.setBirthDay(QDate());

	userAccount.setAboutMe(aboutTextEdit->toPlainText().trimmed());
	userAccount.setEmail(emailEdit->text().trimmed());
	userAccount.setAllowNews(receiveNewsCheckBox->isChecked());
	
	userAccount.setAllowPublishing(allowWebTranslationCheckBox->isChecked());
	userAccount.setAllowShowFriends(showMyFriendsCheckBox->isChecked());
	
	currentRequest = new UserAccountUpdateRequest(this, userAccount, avatarWasChanged, avatarToolButton->getNewAvatar());

	profileWasChanged = false;
}

void ProfileFrame::onAccountUpdateCompleted(Fault fault, AccountResultCode::AccountResultCodeEnum result)
{
	currentRequest = NULL;
	formBlocker->unblock();
	
	if (!fault.isNull())
	{
		emit updateError();
		UiTools::handleError(this, tr(ACCOUNT_UPDATE_ERROR_TITLE), fault);
	}
	else
	{
		if (result != AccountResultCode::Ok)
		{
			emit updateError();
			UiTools::handleError(QApplication::activeWindow(), tr(ACCOUNT_UPDATE_ERROR_TITLE), PromptHelper::getAccountUpdateErrorName(result));
		}
		else
		{
			emit updateFinished();
		}
	}
}

void ProfileFrame::onRequestCancelled()
{
	if (currentRequest != NULL)
		currentRequest->detach();

	formBlocker->unblock();
}

void ProfileFrame::showEvent(QShowEvent *event)
{
	SetupDialogFrame::showEvent(event);

	if (INFOMANAGER->getIsLoggedOn())
	{
		formBlocker->unblock();
	}
	else
	{
		formBlocker->block();
	}
}

void ProfileFrame::updateControls()
{
	if (INFOMANAGER->getIsLoggedOn())
	{
		userAccount = INFOMANAGER->getUserAccount();

		// Update fields from contact info
		mottoTextEdit->setPlainText(userAccount.getMotto());
	
		nameEdit->setText(userAccount.getName());
		countryCombo->setCurrentIndex(userAccount.getCountry());
		cityEdit->setText(userAccount.getCity());
		genderCombo->setCurrentIndex(genderCombo->findData(userAccount.getSex()));
	
		if (userAccount.getBirthDay().isValid())
		{
			setYearInCombo(userAccount.getBirthDay().year());
			monthCombo->setCurrentIndex(userAccount.getBirthDay().month());
			dayCombo->setCurrentIndex(userAccount.getBirthDay().day());
		}
		else
			clearBirthdayControls();

		aboutTextEdit->setPlainText(userAccount.getAboutMe());
		emailEdit->setText(userAccount.getEmail());
		receiveNewsCheckBox->setChecked(userAccount.getAllowNews());
	
		allowWebTranslationCheckBox->setChecked(userAccount.getAllowPublishing());
		showMyFriendsCheckBox->setChecked(userAccount.getAllowShowFriends());
	}
	profileWasChanged = false;
}

void ProfileFrame::fillBirthdayControls()
{
	monthCombo->addItem(QString(), 0);
	for (int i = 1; i <= 12; i++)
		monthCombo->addItem(QDate::longMonthName(i), i);

	yearCombo->addItem(QString(), 0);
	for (int i = YEAR_BASE; i <= QDate::currentDate().year(); i++)
		yearCombo->addItem(QString().setNum(i), i);

	yearCombo->setDefaultItemIndex(yearCombo->count() - DEFAULT_USER_AGE);
}

void ProfileFrame::clearBirthdayControls()
{
	yearCombo->setCurrentIndex(-1);
	monthCombo->setCurrentIndex(-1);
	dayCombo->setCurrentIndex(-1);
}

void ProfileFrame::setYearInCombo(int year)
{
	yearCombo->setCurrentIndex(year - YEAR_BASE + 1);
}

int ProfileFrame::getYearFromCombo() const
{
	return (yearCombo->currentIndex() + YEAR_BASE - 1);
}

void ProfileFrame::onMonthComboChanged()
{
	int index = dayCombo->currentIndex();
	QDate dt(getYearFromCombo(), monthCombo->currentIndex(), 1);
	
	dayCombo->clear();
	dayCombo->addItem(QString(), 0);
	for (int i = 1; i <= dt.daysInMonth(); i++)
		dayCombo->addItem(QString().setNum(i), i);
	
	// Set the previous value if possible
	dayCombo->setCurrentIndex( (dayCombo->count() > index) ? index : 1 );
}

void ProfileFrame::onUserAvatarButtonClicked()
{
	SelectAvatarDialog *selectAvatarDialog = new SelectAvatarDialog(this);
	
	if (selectAvatarDialog->exec() == QDialog::Accepted)
	{
		avatarToolButton->setNewAvatar(selectAvatarDialog->getSelectedAvatar());
		avatarWasChanged = true;
		profileWasChanged = true;
	}
}

void ProfileFrame::onChangePasswordButtonClicked()
{
	ChangePasswordDialog *passwordDialog = new ChangePasswordDialog(this);
	
	passwordDialog->show();
}

void ProfileFrame::clearBirthday()
{
	if (yearCombo->hasFocus() || monthCombo->hasFocus() || dayCombo->hasFocus())
		clearBirthdayControls();
}

void ProfileFrame::profileChanged()
{
	profileWasChanged = true;
}

}
