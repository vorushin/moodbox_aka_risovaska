#include "varianthash.h"

VariantHash::VariantHash()
{

}

void VariantHash::setData(qint32 key, const QVariant &data)
{
	hash[key] = data;
}

QVariant VariantHash::getData(qint32 key) const
{
	return (hasData(key)) ? hash[key] : QVariant();
}
	
bool VariantHash::hasData(qint32 key) const
{
	return hash.contains(key);
}

void VariantHash::removeData(qint32 key)
{
	hash.remove(key);
}

void VariantHash::clear()
{
	hash.clear();
}

int VariantHash::size() const
{
	return hash.count();
}