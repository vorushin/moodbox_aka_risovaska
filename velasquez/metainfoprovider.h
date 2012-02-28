#ifndef METAINFOPROVIDER_H
#define METAINFOPROVIDER_H

#include <QStringList>

namespace Velasquez
{

#define METAINFO_XML_INFO_STRING	"<%1 %2 />"
#define METAINFO_XML_TAG_STRING		"%1=\"%2\""

// Class to provide meta info for serialization
class MetaInfoProvider
{
public:
	MetaInfoProvider() {};
	virtual ~MetaInfoProvider() {};

	// Get meta information title
	inline virtual QString getInfoTitle() const { return QString(); };

	// List of supported tags
	inline virtual QStringList getTagNames() const { return QStringList(); };

	// Content for tag
	inline virtual QString getTagContent(const QString &tagName) const { Q_UNUSED(tagName); return QString(); };

	// Get meta info as XML
	virtual QString getInfoContentAsXml() const;

	// Get the title string
	static QString getTitleString(const QString &title, const QString &titleContent);

	// Get tag string
	static QString getTagString(const QString &tagName, const QString &tagContent);
};

}

#endif // METAINFOPROVIDER_H