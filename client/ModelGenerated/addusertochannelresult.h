#ifndef ADDUSERTOCHANNELRESULT_H
#define ADDUSERTOCHANNELRESULT_H

#include <QObject>
#include <QSharedData>

#include "changeuserchannelresult.h"
#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class AddUserToChannelResultData : public QSharedData
{
public:
    AddUserToChannelResultData();
    AddUserToChannelResultData(ChangeUserChannelResult::ChangeUserChannelResultEnum result);
    virtual ~AddUserToChannelResultData();

    ChangeUserChannelResult::ChangeUserChannelResultEnum result;
};

class AddUserToChannelResult : public TransportableObject
{
public:
    AddUserToChannelResult();
    AddUserToChannelResult(ChangeUserChannelResult::ChangeUserChannelResultEnum result);
    virtual ~AddUserToChannelResult();

protected:
    AddUserToChannelResult(AddUserToChannelResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static AddUserToChannelResult* ___new_()
    {
        return new AddUserToChannelResult(new AddUserToChannelResultData());
    }
    static AddUserToChannelResult empty()
    {
        return AddUserToChannelResult(new AddUserToChannelResultData());
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
    QExplicitlySharedDataPointer<AddUserToChannelResultData> d;
};

class AddUserToChannelResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, ChangeUserChannelResult::ChangeUserChannelResultEnum result)
    {
       AddUserToChannelResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        AddUserToChannelResultCallbackCaller caller;
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
            CallbackCallerTools::onConnectFail("AddUserToChannelResult", SIGNAL(callbackSignal(QVariant, Fault, ChangeUserChannelResult::ChangeUserChannelResultEnum)), callback.method);
        }
    }
};

}

#endif // ADDUSERTOCHANNELRESULT_H