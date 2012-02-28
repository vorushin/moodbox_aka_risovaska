#ifndef LISTWRAPPEROBJECTS_H
#define LISTWRAPPEROBJECTS_H

#include <QDateTime>
#include <QString>
#include <QDate>
#include <QByteArray>


#include "transportableobject.h"

namespace MoodBox
{

class ListOfInt64WrapperObject : public ListOfSimpleWrapperObject<qint64>
{
public:
    ListOfInt64WrapperObject(QList<qint64> *list, PropertyInfo itemPropertyInfo) : ListOfSimpleWrapperObject<qint64>(list, itemPropertyInfo)
    {
    }
    virtual ~ListOfInt64WrapperObject()
    {
    }

    virtual PropertyReadResult readListItem(PropertyReader *reader)
    {
        list->append(reader->readInt64());

        return PropertyReadResult(true);
    }
};

class ListOfDateWrapperObject : public ListOfSimpleWrapperObject<QDate>
{
public:
    ListOfDateWrapperObject(QList<QDate> *list, PropertyInfo itemPropertyInfo) : ListOfSimpleWrapperObject<QDate>(list, itemPropertyInfo)
    {
    }
    virtual ~ListOfDateWrapperObject()
    {
    }

    virtual PropertyReadResult readListItem(PropertyReader *reader)
    {
        list->append(reader->readDate());

        return PropertyReadResult(true);
    }
};

class ListOfStringWrapperObject : public ListOfSimpleWrapperObject<QString>
{
public:
    ListOfStringWrapperObject(QList<QString> *list, PropertyInfo itemPropertyInfo) : ListOfSimpleWrapperObject<QString>(list, itemPropertyInfo)
    {
    }
    virtual ~ListOfStringWrapperObject()
    {
    }

    virtual PropertyReadResult readListItem(PropertyReader *reader)
    {
        list->append(reader->readString());

        return PropertyReadResult(true);
    }
};

class ListOfInt32WrapperObject : public ListOfSimpleWrapperObject<qint32>
{
public:
    ListOfInt32WrapperObject(QList<qint32> *list, PropertyInfo itemPropertyInfo) : ListOfSimpleWrapperObject<qint32>(list, itemPropertyInfo)
    {
    }
    virtual ~ListOfInt32WrapperObject()
    {
    }

    virtual PropertyReadResult readListItem(PropertyReader *reader)
    {
        list->append(reader->readInt32());

        return PropertyReadResult(true);
    }
};

class ListOfDoubleWrapperObject : public ListOfSimpleWrapperObject<qreal>
{
public:
    ListOfDoubleWrapperObject(QList<qreal> *list, PropertyInfo itemPropertyInfo) : ListOfSimpleWrapperObject<qreal>(list, itemPropertyInfo)
    {
    }
    virtual ~ListOfDoubleWrapperObject()
    {
    }

    virtual PropertyReadResult readListItem(PropertyReader *reader)
    {
        list->append(reader->readDouble());

        return PropertyReadResult(true);
    }
};

class ListOfBoolWrapperObject : public ListOfSimpleWrapperObject<bool>
{
public:
    ListOfBoolWrapperObject(QList<bool> *list, PropertyInfo itemPropertyInfo) : ListOfSimpleWrapperObject<bool>(list, itemPropertyInfo)
    {
    }
    virtual ~ListOfBoolWrapperObject()
    {
    }

    virtual PropertyReadResult readListItem(PropertyReader *reader)
    {
        list->append(reader->readBool());

        return PropertyReadResult(true);
    }
};

class ListOfDateTimeWrapperObject : public ListOfSimpleWrapperObject<QDateTime>
{
public:
    ListOfDateTimeWrapperObject(QList<QDateTime> *list, PropertyInfo itemPropertyInfo) : ListOfSimpleWrapperObject<QDateTime>(list, itemPropertyInfo)
    {
    }
    virtual ~ListOfDateTimeWrapperObject()
    {
    }

    virtual PropertyReadResult readListItem(PropertyReader *reader)
    {
        list->append(reader->readDateTime());

        return PropertyReadResult(true);
    }
};

class ListOfBytesWrapperObject : public ListOfSimpleWrapperObject<QByteArray>
{
public:
    ListOfBytesWrapperObject(QList<QByteArray> *list, PropertyInfo itemPropertyInfo) : ListOfSimpleWrapperObject<QByteArray>(list, itemPropertyInfo)
    {
    }
    virtual ~ListOfBytesWrapperObject()
    {
    }

    virtual PropertyReadResult readListItem(PropertyReader *reader)
    {
        list->append(reader->readBytes());

        return PropertyReadResult(true);
    }
};

}

#endif // LISTWRAPPEROBJECTS_H