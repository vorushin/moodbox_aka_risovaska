#include "customtranslator.h"

#include "debug.h"

namespace MoodBox
{

QString CustomTranslator::translate(const char *context, const char *sourceText,
				  const char *comment) const
{
	QString str = QTranslator::translate(context, sourceText, comment);
	
	if (str == "") 
	{
		str = QTranslator::translate("@default", sourceText, comment);
	}

	//QDEBUG("Localization: " << context << " s: " << sourceText << " d: " << str);

	return str;
}

}