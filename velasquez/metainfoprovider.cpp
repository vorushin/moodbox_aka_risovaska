#include "metainfoprovider.h"

namespace Velasquez
{

QString MetaInfoProvider::getInfoContentAsXml() const
{
	QString title = getInfoTitle();
	QString values;

	foreach (QString tagName, getTagNames())
		values += getTagString(tagName, getTagContent(tagName));

	QString content;
	
	if (!title.isEmpty() || !values.isEmpty() )
		content = getTitleString(title, values);

	return content;
}

QString MetaInfoProvider::getTitleString(const QString &title, const QString &titleContent)
{
	return QString(METAINFO_XML_INFO_STRING).arg(title).arg(titleContent);
}

QString MetaInfoProvider::getTagString(const QString &tagName, const QString &tagContent)
{
	return QString(METAINFO_XML_TAG_STRING).arg(tagName).arg(tagContent);
}

}