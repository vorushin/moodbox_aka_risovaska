#ifndef DELETEUSERFROMCHANNELRESULT_H
#define DELETEUSERFROMCHANNELRESULT_H

#include <QObject>
#include <QSharedData>

#include "changeuserchannelresult.h"
#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class DeleteUserFromChannelResultData : public QSharedData
{
public:
    DeleteUserFromChannelResultData();
    DeleteUserFromChannelResultData(ChangeUserChannelResult::ChangeUserChannelResultEnum result);
    virtual ~DeleteUserFromChannelResultData();

    ChangeUserChannelResult::ChangeUserChannelResultEnum result;
};

class DeleteUserFromChannelResult : public TransportableObject
{
public:
    DeleteUserFromChannelResult();
    DeleteUserFromChannelResult(ChangeUserChannelResult::ChangeUserChannelResultEnum result);
    virtual ~DeleteUserFromChannelResult();

protected:
    DeleteUserFromChannelResult(DeleteUserFromChannelResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static DeleteUserFromChannelResult* ___new_()
    {
        return new DeleteUserFromChannelResult(new DeleteUserFromChannelResultData());
    }
    static DeleteUserFromChannelResult empty()
    {
        return DeleteUserFromChannelResult(new DeleteUserFromChannelResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    ChangeUserChannelResult::ChangeUserChannelResultEnum getResult() const;
    void setResult(ChangeUserChannelResult::ChangeUserChannelResultEnum value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<DeleteUserFromChannelResultData> d;
};

class DeleteUserFromChannelResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, ChangeUserChannelResult::ChangeUserChannelResultEnum result)
    {
       DeleteUserFromChannelResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        DeleteUserFromChannelResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, ChangeUserChannelResult::Undefined);
    }

signals:
    void callbackSignal(QVariant state, Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, ChangeUserChannelResult::ChangeUserChannelResultEnum)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("DeleteUserFromChannelResult", SIGNAL(callbackSignal(QVariant, Fault, ChangeUserChannelResult::ChangeUserChannelResultEnum)), callback.method);
        }
    }
};

}

#endif // DELETEUSERFROMCHANNELRESULT_H