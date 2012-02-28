#ifndef VARIANTHASH_H
#define VARIANTHASH_H

#include <QVariant>
#include <QHash>

// Utility class to hold protected collection of variants
class VariantHash 
{
public:
	VariantHash();

	void setData(qint32 key, const QVariant &data);
	QVariant getData(qint32 key) const;
	
	bool hasData(qint32 key) const;

	void removeData(qint32 key);

	void clear();

	int size() const;

private:
	QHash <qint32, QVariant> hash;
	
};

#endif // VARIANTHASH_H
