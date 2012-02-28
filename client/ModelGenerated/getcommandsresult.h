#ifndef GETCOMMANDSRESULT_H
#define GETCOMMANDSRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "commandpackage.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetCommandsResultData : public QSharedData
{
public:
    GetCommandsResultData();
    GetCommandsResultData(CommandPackage result);
    virtual ~GetCommandsResultData();

    CommandPackage result;
};

class GetCommandsResult : public TransportableObject
{
public:
    GetCommandsResult();
    GetCommandsResult(CommandPackage result);
    virtual ~GetCommandsResult();

protected:
    GetCommandsResult(GetCommandsResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetCommandsResult* ___new_()
    {
        return new GetCommandsResult(new GetCommandsResultData());
    }
    static GetCommandsResult empty()
    {
        return GetCommandsResult(new GetCommandsResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    CommandPackage getResult() const;
    void setResult(CommandPackage value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetCommandsResultData> d;
};

class GetCommandsResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, CommandPackage result)
    {
       GetCommandsResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetCommandsResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, CommandPackage());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, CommandPackage result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, CommandPackage result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, CommandPackage)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("GetCommandsResult", SIGNAL(callbackSignal(QVariant, Fault, CommandPackage)), callback.method);
        }
    }
};

}

#endif // GETCOMMANDSRESULT_H