#ifndef TRANSPORTABLEOBJECT_H
#define TRANSPORTABLEOBJECT_H

#include <QtGlobal>
#include <QVariant>

#include "model.h"
#include "propertyreader.h"
#include "propertywriter.h"
#include "callback.h"

namespace MoodBox
{

class ServerProxyBase;

class TransportableObject
{
public:
	virtual ~TransportableObject();

	virtual qint32 getTypeId() const = 0;
	virtual PropertyInfo getPropertyInfo(Model *model, QString name);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);
    virtual void writeProperties(PropertyWriter *writer);

	virtual bool isWrapper() const
	{
		return false;
	}
	virtual bool isNull() const
	{
		return false;
	}
	virtual bool isFault() const
	{
		return false;
	}

	virtual void resultCall(Callback callback, QVariant state)
	{
		Q_UNUSED(callback);
		Q_UNUSED(state);

		throw "Not a call";
	}
	virtual void resultFaultCall(ServerProxyBase* server, Callback callback, QVariant state, qint32 resultTypeId)
	{
		Q_UNUSED(server);
		Q_UNUSED(callback);
		Q_UNUSED(state);
		Q_UNUSED(resultTypeId);

		throw "Not a fault";
	}
};

template <class T>
class TransportableSharedObjectWrapper : public TransportableObject
{
public:
	TransportableSharedObjectWrapper(T value)
	{
		this->value = value;
	}
	virtual ~TransportableSharedObjectWrapper()
	{
	}

	virtual qint32 getTypeId() const
	{
		return value.getTypeId();
	}
	virtual PropertyInfo getPropertyInfo(Model *model, QString name)
	{
		return value.getPropertyInfo(model, name);
	}
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
	{
		return value.readProperty(propertyId, typeId, reader);
	}
    virtual void writeProperties(PropertyWriter *writer)
	{
		value.writeProperties(writer);
	}

	virtual bool isWrapper() const
	{
		return true;
	}
	virtual bool isNull() const
	{
		return value.isNull();
	}

protected:
	T value;
};

template <class T>
class AbstractListWrapperObject : public TransportableObject
{
public:
	AbstractListWrapperObject(PropertyInfo itemPropertyInfo)
	{
		this->itemPropertyInfo = itemPropertyInfo;
	}
	virtual ~AbstractListWrapperObject()
	{
	}

	virtual qint32 getTypeId() const
	{
		return 0;
	}
	virtual PropertyInfo getPropertyInfo(Model *model, QString name)
	{
		Q_UNUSED(model);
		Q_UNUSED(name);

		return itemPropertyInfo;
	}
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
	{
		Q_UNUSED(propertyId);
		Q_UNUSED(typeId);

		return readListItem(reader);
	}
    virtual PropertyReadResult readListItem(PropertyReader *reader) = 0;
    virtual void writeProperties(PropertyWriter *writer)
	{
		// unneñessary
		Q_UNUSED(writer);
	}

	virtual bool isWrapper() const
	{
		return true;
	}

protected:
	PropertyInfo itemPropertyInfo;
};

template <class T>
class ListWrapperObject : public AbstractListWrapperObject<T>
{
public:
	ListWrapperObject(QList<T*> *list, PropertyInfo itemPropertyInfo) : AbstractListWrapperObject<T>(itemPropertyInfo)
	{
		this->list = list;
	}
	virtual ~ListWrapperObject()
	{
	}

    virtual PropertyReadResult readListItem(PropertyReader *reader)
	{
		Q_UNUSED(reader);

		T* item = new T();
		list->append(item);

		return PropertyReadResult(item);
	}

protected:
	QList<T*> *list;
};
template <class T>
class ListOfSharedWrapperObject : public AbstractListWrapperObject<T>
{
public:
	ListOfSharedWrapperObject(QList<T> *list, PropertyInfo itemPropertyInfo) : AbstractListWrapperObject<T>(itemPropertyInfo)
	{
		this->list = list;
	}
	virtual ~ListOfSharedWrapperObject()
	{
	}

    virtual PropertyReadResult readListItem(PropertyReader *reader)
	{
		Q_UNUSED(reader);

		T item = T::empty();
		list->append(item);

		return PropertyReadResult(new TransportableSharedObjectWrapper<T>(item));
	}

protected:
	QList<T> *list;
};

template <class T>
class ListOfSimpleWrapperObject : public AbstractListWrapperObject<T>
{
public:
	ListOfSimpleWrapperObject(QList<T> *list, PropertyInfo itemPropertyInfo) : AbstractListWrapperObject<T>(itemPropertyInfo)
	{
		this->list = list;
	}
	virtual ~ListOfSimpleWrapperObject()
	{
	}

protected:
	QList<T> *list;
};

template <class T>
class ListOfEnumWrapperObject : public AbstractListWrapperObject<T>
{
public:
	ListOfEnumWrapperObject(QList<T> *list, PropertyInfo itemPropertyInfo, qint32 enumId) : AbstractListWrapperObject<T>(itemPropertyInfo)
	{
		this->list = list;
		this->enumId = enumId;
	}
	virtual ~ListOfEnumWrapperObject()
	{
	}

    virtual PropertyReadResult readListItem(PropertyReader *reader)
	{
		T item = (T)reader->readEnum(enumId);
		list->append(item);

		return PropertyReadResult(true);
	}

protected:
	QList<T> *list;
	qint32 enumId;
};

}

#endif // TRANSPORTABLEOBJECT_H
