#include "verifiers.h"

#include <QWidget>
#include <QLabel>
#include <QLineEdit>
#include <QValidator>
#include <QComboBox>
#include <QDate>
#include <QEvent>
#include <QMessageBox>
#include <QApplication>

#include "common.h"

namespace MoodBox
{

// Class WidgetVerifier
WidgetVerifier::WidgetVerifier(bool required, QObject *parent) 
	: QObject(parent) 
{ 
	this->required = required; 
}

void WidgetVerifier::highlight()
{
	highlight((isValid()) ? Normal : Error);
}

QString WidgetVerifier::getFormattedText(const QString &text, TextFormat format)
{
	QString formatted = text;

	formatted.replace(tr(VERIFIER_FORMAT_ERROR_START), QString());
	formatted.replace(tr(VERIFIER_FORMAT_ERROR_END), QString());

	if (format == Error)
		formatted = tr(VERIFIER_FORMAT_ERROR_START) + formatted + tr(VERIFIER_FORMAT_ERROR_END);

	return formatted;
}

void WidgetVerifier::formatLabel(QLabel *label, TextFormat format)
{
	label->setText(getFormattedText(label->text(), format));
}

// Class LineEditVerifier
LineEditVerifier::LineEditVerifier(bool required, QObject *parent, QLabel *label, StandardVerifier::Type verifierType)
	: StandardVerifier(required, verifierType, parent)
{
	this->label = label;

	this->editor = (label->buddy() != NULL) ? qobject_cast <QLineEdit *> (label->buddy()) : NULL;

	switch (verifierType)
	{
		case Login: 
			this->validator = createLoginValidator(this);
			break;

		case NotEmpty: 
			this->validator = createNotEmptyValidator(this);
			break;

		case Email: 
			this->validator = createEmailValidator(this);
			break;
	}
}

LineEditVerifier::LineEditVerifier(bool required, QObject *parent, QLabel *label, QValidator *validator, const QString &message)
	: StandardVerifier(required, parent)
{
	this->label = label;

	this->editor = (label->buddy() != NULL) ? qobject_cast <QLineEdit *> (label->buddy()) : NULL;
	
	this->validator = validator;

	if (validator != NULL && validator->parent() == NULL)
		validator->setParent(this);

	this->message = message;
}

bool LineEditVerifier::isValid() const
{
	// Empty text is correct when this field is not required
	int p = 0;
	QString text = editor->text();

	return (isRequired() || !text.isEmpty()) ? (validator->validate(text, p) == QValidator::Acceptable) : true;
}

QString LineEditVerifier::getErrorMessage() const
{
	QString errorMessage = message;

	switch (verifierType)
	{
		case Login: 
			errorMessage = tr(CHECK_LOGIN_ERROR_MESSAGE);
			break;
	}

	return errorMessage;
}

void LineEditVerifier::highlight(TextFormat format)
{
	formatLabel(label, format);
}

QList <QWidget *> LineEditVerifier::getVerifyingWidgets() const
{
	QList <QWidget *> list;
	list << editor;

	return list;
}

QValidator *LineEditVerifier::createLoginValidator(QObject * parent) 
{ 
	return new QRegExpValidator(QRegExp(VALIDATOR_LOGIN_STRING), parent); 
}

QValidator *LineEditVerifier::createNotEmptyValidator(QObject * parent) 
{ 
	return new QRegExpValidator(QRegExp(VALIDATOR_NOTEMPTY_STRING), parent); 
}

QValidator *LineEditVerifier::createEmailValidator(QObject * parent) 
{ 
	return new QRegExpValidator(QRegExp(VALIDATOR_EMAIL_STRING), parent); 
}

// ComboBoxVerifier class
ComboBoxVerifier::ComboBoxVerifier(bool required, QObject *parent, QLabel *label, StandardVerifier::Type verifierType)
	: StandardVerifier(required, verifierType, parent)
{
	this->label = label;

	this->comboBox = (label->buddy() != NULL) ? qobject_cast <QComboBox *> (label->buddy()) : NULL;

	switch (verifierType)
	{
		case Login: 
			this->validator = LineEditVerifier::createLoginValidator(this);
			break;

		case NotEmpty: 
			this->validator = LineEditVerifier::createNotEmptyValidator(this);
			break;

		case Email: 
			this->validator = LineEditVerifier::createEmailValidator(this);
			break;
	}
}

ComboBoxVerifier::ComboBoxVerifier(bool required, QObject *parent, QLabel *label, QValidator *validator, const QString &message)
	: StandardVerifier(required, parent)
{
	this->label = label;

	this->comboBox = (label->buddy() != NULL) ? qobject_cast <QComboBox *> (label->buddy()) : NULL;
	
	this->validator = validator;

	if (validator != NULL && validator->parent() == NULL)
		validator->setParent(this);

	this->message = message;
}

bool ComboBoxVerifier::isValid() const 
{
  	// Empty text is correct when this field is not required
	int p = 0;
	QString text = comboBox->currentText();

	return (isRequired() || !text.isEmpty()) ? (validator->validate(text, p) == QValidator::Acceptable) : true;
}

QString ComboBoxVerifier::getErrorMessage() const
{
	QString errorMessage = message;

	switch (verifierType)
	{
		case Login: 
			errorMessage = tr(CHECK_LOGIN_ERROR_MESSAGE);
			break;
	}

	return errorMessage;
}

void ComboBoxVerifier::highlight(TextFormat format)
{
	formatLabel(label, format);
}

QList <QWidget *> ComboBoxVerifier::getVerifyingWidgets() const
{
	QList <QWidget *> list;
	list << comboBox;

	return list;
}

// Class PasswordsEqualityVerifier
PasswordsVerifier::PasswordsVerifier(bool required, QObject *parent, QLabel *label1, QLabel *label2)
: WidgetVerifier(required, parent), editor2(NULL)
{
	this->validator = createPasswordValidator(this);

	this->label1 = label1;
	editor1 = qobject_cast <QLineEdit *> (label1->buddy());
	
	this->label2 = label2;

	if (label2 != NULL)
	{
		editor2 = qobject_cast <QLineEdit *> (label2->buddy());
	}
}

bool PasswordsVerifier::isValid() const
{
	VerifyResult verifyResult = verify();

	if (verifyResult != Ok)
		return false;

	return true;
}

QString PasswordsVerifier::getErrorMessage() const
{
	QString errorMessage = tr(CHECK_PASSWORD_ERROR_MESSAGE);

	VerifyResult verifyResult = verify();

	if (verifyResult == NotEqualPasswords)
		errorMessage = tr(CHECK_PASSWORDS2_ERROR_MESSAGE);

	return errorMessage;
}

void PasswordsVerifier::highlight(TextFormat format)
{
	formatLabel(label1, format);
	if (label2 != NULL)
		formatLabel(label2, format);
}

QList <QWidget *> PasswordsVerifier::getVerifyingWidgets() const
{
	QList <QWidget *> list;
	list << editor1;

	if (editor2 != NULL)
		list << editor2;

	return list;
}

QValidator *PasswordsVerifier::createPasswordValidator(QObject * parent) 
{ 
	return new QRegExpValidator(QRegExp(VALIDATOR_PASSWORD_STRING), parent); 
}

PasswordsVerifier::VerifyResult PasswordsVerifier::verify() const
{
	VerifyResult verifyResult = Ok;

	int position = 0;
	QString text1 = editor1->text();

	bool isValidPassword1 = false;
	isValidPassword1 = (isRequired()) ? (validator->validate(text1, position) == QValidator::Acceptable) : true;
	if (!isValidPassword1)
		verifyResult = InvalidPassword;

	if (editor2 != NULL)
	{
		QString text2 = editor2->text();
		if (text1 != text2)
			verifyResult = NotEqualPasswords;
	}

	return verifyResult;
}

// Class TripleDateVerifier
TripleDateVerifier::TripleDateVerifier(bool required, QObject *parent, QLabel *label, QComboBox *day, QComboBox *month, QComboBox *year)
: WidgetVerifier(required, parent)
{
	this->label = label;

	this->day = day;
	this->month = month;
	this->year = year;
}

bool TripleDateVerifier::isValid() const
{
	if (!isRequired())
	{
		// if this is not required field this verifier is valid when all is empty
		if (this->day->currentText().isEmpty() && this->month->currentText().isEmpty() && this->year->currentText().isEmpty())
			return true;
	}

	QDate date;
	int d = this->day->currentText().toInt();

	bool goodMonth;

	int m = this->month->currentText().toInt(&goodMonth);

	// Try to use index of month
	if (!goodMonth)
	{
		m = this->month->currentIndex();
		if (this->month->count() == 12)
			m++;
	}

	int y = this->year->currentText().toInt();

	date.setYMD(y, m, d);

	return date.isValid() && (y > 0);
}

void TripleDateVerifier::highlight(TextFormat format)
{
	formatLabel(label, format);
}

QList <QWidget *> TripleDateVerifier::getVerifyingWidgets() const
{
	QList <QWidget *> list;
	list << this->day << this->month << this->year;

	return list;
}

// Class FormVerifier
FormVerifier::FormVerifier(QObject *parent)
	: WidgetVerifier(true, parent), parentWidget(NULL)
{
}

FormVerifier::FormVerifier(QWidget *parentWidget)
	: WidgetVerifier(true, parentWidget)
{
	this->parentWidget = parentWidget;
}

bool FormVerifier::isValid() const
{	
	foreach (WidgetVerifier *verifier, verifiers)
		if (!verifier->isValid())
			return false;

	return true;
}

QString FormVerifier::getErrorMessage() const
{
	QString errorMessage;

	foreach (WidgetVerifier *verifier, verifiers)
	{
		if (!verifier->isValid())
		{
			QString verifierMessage = verifier->getErrorMessage();

			if (!verifierMessage.isEmpty())
				errorMessage += tr(CHECK_ERROR_LIST_ITEM).arg(verifierMessage);
		}
	}

	if (!errorMessage.isNull())
		errorMessage.prepend(tr(CHECK_ERROR_LIST_START).arg(tr(CHECK_ERROR_MESSAGE)));

	return errorMessage;
}

void FormVerifier::highlight()
{
	foreach (WidgetVerifier *verifier, verifiers)
		verifier->highlight();
}

void FormVerifier::highlight(TextFormat format)
{
	foreach (WidgetVerifier *verifier, verifiers)
		verifier->highlight(format);
}

QList <QWidget *> FormVerifier::getVerifyingWidgets() const
{
	QList <QWidget *> list;

	foreach (WidgetVerifier *verifier, verifiers)
		list << verifier->getVerifyingWidgets();

	return list;
}

void FormVerifier::addVerifier(WidgetVerifier *verifier)
{
	if (verifier == NULL)
		return;

	verifiers.append(verifier);
}

void FormVerifier::focusFirstInvalid()
{
	foreach (WidgetVerifier *verifier, verifiers)
	{
		if (!verifier->isValid())
		{
			verifier->getVerifyingWidgets().first()->setFocus();
			break;
		}
	}
}

bool FormVerifier::verifyAndHighlight(bool showErrors)
{
	highlight(Normal);

	if (isValid())
		return true;

	highlight();
	focusFirstInvalid();
	QApplication::beep();

	if (showErrors)
	{		
		QString errorMessage = getErrorMessage();

		if (!errorMessage.isEmpty())
			QMessageBox::warning(parentWidget, tr(CHECK_ERROR_TITLE), errorMessage, QMessageBox::Ok);
	}

	return false;
}

}