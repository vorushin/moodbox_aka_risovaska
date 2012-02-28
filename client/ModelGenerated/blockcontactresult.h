#ifndef BLOCKCONTACTRESULT_H
#define BLOCKCONTACTRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "contactresultcode.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class BlockContactResultData : public QSharedData
{
public:
    BlockContactResultData();
    BlockContactResultData(ContactResultCode::ContactResultCodeEnum result);
    virtual ~BlockContactResultData();

    ContactResultCode::ContactResultCodeEnum result;
};

class BlockContactResult : public TransportableObject
{
public:
    BlockContactResult();
    BlockContactResult(ContactResultCode::ContactResultCodeEnum result);
    virtual ~BlockContactResult();

protected:
    BlockContactResult(BlockContactResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static BlockContactResult* ___new_()
    {
        return new BlockContactResult(new BlockContactResultData());
    }
    static BlockContactResult empty()
    {
        return BlockContactResult(new BlockContactResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    ContactResultCode::ContactResultCodeEnum getResult() const;
    void setResult(ContactResultCode::ContactResultCodeEnum value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<BlockContactResultData> d;
};

class BlockContactResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, ContactResultCode::ContactResultCodeEnum result)
    {
       BlockContactResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        BlockContactResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, ContactResultCode::Ok);
    }

signals:
    void callbackSignal(QVariant state, Fault fault, ContactResultCode::ContactResultCodeEnum result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, ContactResultCode::ContactResultCodeEnum result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, ContactResultCode::ContactResultCodeEnum)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("BlockContactResult", SIGNAL(callbackSignal(QVariant, Fault, ContactResultCode::ContactResultCodeEnum)), callback.method);
        }
    }
};

}

#endif // BLOCKCONTACTRESULT_H