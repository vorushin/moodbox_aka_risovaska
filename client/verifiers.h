#ifndef VERIFIERS_H
#define VERIFIERS_H

#include <QObject>
#include <QString>
#include <QList>
#include <QWidget>

class QWidget;
class QLabel;
class QLineEdit;
class QValidator;
class QComboBox;
class QEvent;

namespace MoodBox
{

// Verifier text formatting
#define VERIFIER_FORMAT_ERROR_START			"<font color=red>"
#define VERIFIER_FORMAT_ERROR_END			"</font>"

// Name validation regexp
#define VALIDATOR_LOGIN_STRING				"^[a-zA-Z0-9._%+-]{3,45}$"

// Password validation regexp
#define VALIDATOR_PASSWORD_STRING			"^.{5,45}$"

// Non-empty text validation regexp
#define VALIDATOR_NOTEMPTY_STRING			"^.+$"

// E-mail validation regexp
#define VALIDATOR_EMAIL_STRING				"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,4}$"

// Messages
#define CHECK_LOGIN_ERROR_MESSAGE			QT_TRANSLATE_NOOP("@default", "LoginIs3To45CharsLong")
#define CHECK_PASSWORD_ERROR_MESSAGE		QT_TRANSLATE_NOOP("@default", "PasswordIs5To45CharsLong")
#define CHECK_PASSWORDS2_ERROR_MESSAGE		QT_TRANSLATE_NOOP("@default", "PasswordsAreNotEqual")
#define CHECK_NOTEMPTY_ERROR_MESSAGE		QT_TRANSLATE_NOOP("@default", "CannotBeEmpty_FieldName")
#define CHECK_EMAIL_ERROR_MESSAGE			QT_TRANSLATE_NOOP("@default", "EmailIsInvalid")
#define CHECK_TRIPLEDATE_ERROR_MESSAGE		QT_TRANSLATE_NOOP("@default", "DateIsInvalid")

#define CHECK_ERROR_TITLE					QT_TRANSLATE_NOOP("MoodBox::FormVerifier", "CheckErrorMessageBoxTitle")
#define CHECK_ERROR_MESSAGE					QT_TRANSLATE_NOOP("MoodBox::FormVerifier", "BadFieldsContent")
#define CHECK_ERROR_LIST_START				"%1:\n\n"
#define CHECK_ERROR_LIST_ITEM				" - %1 \n"

// Universal widget verifier class
class WidgetVerifier: public QObject
{
public:
	enum TextFormat {Normal, Error};

public:
	WidgetVerifier(bool required, QObject *parent = 0);

	// Check for correctness
	virtual bool isValid() const = 0;

	// Is this verifier must be valid in order to proceed
	bool isRequired() const { return required; };
	virtual void setRequired(bool required) { this->required = required; }

	virtual QString getErrorMessage() const { return QString(); };
	
	// Highlight
	// Highlight if error
	virtual void highlight();
	// Highlight in specified format
	virtual void highlight(TextFormat format) = 0;

	// List of widgets verified by this verifier
	virtual QList <QWidget *> getVerifyingWidgets() const = 0;

	// Get text formatted by specified format
	static QString getFormattedText(const QString &text, TextFormat format);

	// Format label text by specified format
	static void formatLabel(QLabel *label, TextFormat format);

protected:
	bool required;
};

// Standard verifier
class StandardVerifier : public WidgetVerifier
{
public:
	enum Type {Login, NotEmpty, Email, Custom};

public:
	StandardVerifier(bool required, QObject *parent = 0) : WidgetVerifier(required, parent), verifierType(Custom) {} ;
	StandardVerifier(bool required, Type verifierType, QObject *parent = 0) : WidgetVerifier(required, parent), verifierType(verifierType) {} ;

protected:
	Type verifierType;
};

// Line edit verifier
class LineEditVerifier: public StandardVerifier
{
public:
	LineEditVerifier(bool required, QObject *parent, QLabel *label, StandardVerifier::Type verifierType);
	// Create custom verifier
	LineEditVerifier(bool required, QObject *parent, QLabel *label, QValidator *validator, const QString &message);

	virtual bool isValid() const;

	virtual QString getErrorMessage() const;

	virtual void highlight(TextFormat format);

	virtual QList <QWidget *> getVerifyingWidgets() const;

	// Create standard login Verifier
	static QValidator *createLoginValidator(QObject * parent = NULL);
	// Create standard non-empty field validator
	static QValidator *createNotEmptyValidator(QObject * parent = NULL);
	// Create standard email validator
	static QValidator *createEmailValidator(QObject * parent = NULL);

private:
	QValidator *validator;

	QString message;
	QLabel *label;
	QLineEdit *editor;
};

// Combo box verifier
class ComboBoxVerifier: public StandardVerifier
{
public:
	ComboBoxVerifier(bool required, QObject *parent, QLabel *label, StandardVerifier::Type verifierType);
	// Create custom verifier
	ComboBoxVerifier(bool required, QObject *parent, QLabel *label, QValidator *validator, const QString &message);

	virtual bool isValid() const;

	virtual QString getErrorMessage() const;

	virtual void highlight(TextFormat format);

	virtual QList <QWidget *> getVerifyingWidgets() const;

private:
	QValidator *validator;

	QString message;
	QLabel *label;
	QComboBox *comboBox;

	QString getText(void) const;
};

// Class PasswordsVerifier
class PasswordsVerifier: public WidgetVerifier
{
public:
	PasswordsVerifier(bool required, QObject *parent, QLabel *label1, QLabel *label2 = NULL);

	virtual bool isValid() const;

	virtual QString getErrorMessage() const;

	virtual void highlight(TextFormat format);

	virtual QList <QWidget *> getVerifyingWidgets() const;

	// Create standard password validator
	static QValidator *createPasswordValidator(QObject * parent = NULL);

private:
	enum VerifyResult {Ok, InvalidPassword, NotEqualPasswords};

	QValidator *validator;

	QLabel *label1, *label2;
	QLineEdit *editor1, *editor2;

	VerifyResult verify() const;
};

// Class TripleDateVerifier
class TripleDateVerifier: public WidgetVerifier
{
public:
	TripleDateVerifier(bool required, QObject *parent, QLabel *label, QComboBox *day, QComboBox *month, QComboBox *year);

	virtual bool isValid() const;

	virtual void highlight(TextFormat format);

	virtual QList <QWidget *> getVerifyingWidgets() const;

private:
	QLabel *label;
	QComboBox *day, *month, *year;
};

// Form verifier class: allows to verify multiple verifiers
class FormVerifier: public WidgetVerifier
{
	Q_OBJECT

public:
	FormVerifier(QObject *parent);
	FormVerifier(QWidget *parentWidget);

	virtual bool isValid() const;

	virtual QString getErrorMessage() const;
	
	virtual void highlight();
	virtual void highlight(TextFormat format);

	virtual QList <QWidget *> getVerifyingWidgets() const;

	// Adds new verifier to checking list
	virtual void addVerifier(WidgetVerifier *verifier);

	// Focuses first invalid widget
	void focusFirstInvalid();

	// Performs highlighting and shows errors in message box
	virtual bool verifyAndHighlight(bool showErrors = false);

private:
	// validators list
	QList<WidgetVerifier*> verifiers;
	QWidget *parentWidget;
};

}

#endif // VERIFIERS_H