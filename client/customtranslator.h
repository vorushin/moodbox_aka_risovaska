#ifndef CUSTOMTRANSLATOR_H
#define CUSTOMTRANSLATOR_H

#include <QTranslator>

namespace MoodBox
{

#define LANGUAGE_FILES_LOCATION		":/MoodBox/Resources/moodbox_"
#define QT_LANGUAGE_FILES_LOCATION	":/MoodBox/Resources/qt_"


class CustomTranslator : public QTranslator
{
public:
        virtual QString translate(const char *context, const char *sourceText,
                              const char *comment = 0) const;
};

}
#endif // CUSTOMTRANSLATOR_H
