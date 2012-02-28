#ifndef TRANSPORTABLELISTWRAPPER_H
#define TRANSPORTABLELISTWRAPPER_H

#include <QList>

namespace MoodBox
{

class TransportableObject;
template <class T> class TransportableSharedObjectWrapper;

class AbstractTransportableListWrapper
{
public:
	virtual ~AbstractTransportableListWrapper()
	{
	}

	virtual bool hasNext() = 0;
	virtual TransportableObject *getNextItem() = 0;
};

class AbstractTransportableListOfEnumWrapper
{
public:
	virtual ~AbstractTransportableListOfEnumWrapper()
	{
	}

	virtual qint32 getEnumId() = 0;
	virtual bool hasNext() = 0;
	virtual qint32 getNextItem() = 0;
};

template <class T>
class TransportableListWrapper : public AbstractTransportableListWrapper
{
public:
	TransportableListWrapper(QList<T> &list)
	{
		i = 0;
		this->list = list;
	}
	virtual ~TransportableListWrapper()
	{
	}

	virtual bool hasNext()
	{
		return i < list.count();
	}
	virtual TransportableObject* getNextItem()
	{
		return list.value(i++);
	}

protected:
	QList<T> list;
	qint32 i;
};
template <class T>
class TransportableListOfSharedWrapper : public AbstractTransportableListWrapper
{
public:
	TransportableListOfSharedWrapper(QList<T> &list)
	{
		i = 0;
		this->list = list;
	}
	virtual ~TransportableListOfSharedWrapper()
	{
	}

	virtual bool hasNext()
	{
		return i < list.count();
	}
	virtual TransportableObject* getNextItem()
	{
		return new TransportableSharedObjectWrapper<T>(list.value(i++));
	}

protected:
	QList<T> list;
	qint32 i;
};

template <class T>
class TransportableListOfEnumWrapper : public AbstractTransportableListOfEnumWrapper
{
public:
	TransportableListOfEnumWrapper(QList<T> &list, qint32 enumId)
	{
		i = 0;
		this->list = list;
		this->enumId = enumId;
	}
	virtual ~TransportableListOfEnumWrapper()
	{
	}

	virtual qint32 getEnumId()
	{
		return enumId;
	}
	virtual bool hasNext()
	{
		return i < list.count();
	}
	virtual qint32 getNextItem()
	{
		return (qint32)list.value(i++);
	}

private:
	QList<T> list;
	qint32 enumId;
	qint32 i;
};

}

#endif // TRANSPORTABLELISTWRAPPER_H
