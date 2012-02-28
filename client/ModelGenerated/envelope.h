#ifndef ENVELOPE_H
#define ENVELOPE_H

#include <QSharedData>

#include "deletemoodstrip.h"
#include "header.h"
#include "notificationregister.h"
#include "notificationunregister.h"
#include "getcontacts.h"
#include "removefromcontacts.h"
#include "simplesearchcontacts.h"
#include "advancedsearchcontacts.h"
#include "getnotifications.h"
#include "getstatus.h"
#include "getcommands.h"
#include "fault.h"
#include "getserverinforesult.h"
#include "getauthticket.h"
#include "getauthticketresult.h"
#include "createaccount.h"
#include "createaccountresult.h"
#include "updateaccount.h"
#include "updateaccountresult.h"
#include "getuserpictureresult.h"
#include "getcontactsresult.h"
#include "processauthorizationrequest.h"
#include "processauthorizationrequestresult.h"
#include "processauthorizationrequestbyloginresult.h"
#include "processauthorizationresponseresult.h"
#include "removefromcontactsresult.h"
#include "sendfriendmessageresult.h"
#include "sendprivatemessageresult.h"
#include "getnextartmessageandreport.h"
#include "getnextartmessageandreportresult.h"
#include "blockcontact.h"
#include "blockcontactresult.h"
#include "unblockcontact.h"
#include "unblockcontactresult.h"
#include "simplesearchcontactsresult.h"
#include "advancedsearchcontactsresult.h"
#include "updatepassword.h"
#include "updatepasswordresult.h"
#include "getuserinforesult.h"
#include "getuserinfobyloginresult.h"
#include "createmoodstripresult.h"
#include "addpicturetomoodstripresult.h"
#include "deletemoodstripresult.h"
#include "resetpassword.h"
#include "resetpasswordresult.h"
#include "getmyaccount.h"
#include "getmyaccountresult.h"
#include "notificationregisterresult.h"
#include "notificationunregisterresult.h"
#include "getnotificationsresult.h"
#include "getcontact.h"
#include "getcontactresult.h"
#include "getstatusresult.h"
#include "getauthorizationresult.h"
#include "getnotificationtimeout.h"
#include "getnotificationtimeoutresult.h"
#include "checkinvitationresult.h"
#include "searchchannelresult.h"
#include "getchannelinforesult.h"
#include "sendchannelmessageresult.h"
#include "getnextchannelmessageresult.h"
#include "getnextchannelmessageurlresult.h"
#include "addusertochannelresult.h"
#include "deleteuserfromchannelresult.h"
#include "obscenechannelmessageresult.h"
#include "deletemessageresult.h"
#include "getcommandsresult.h"
#include "transportableobject.h"
#include "getuserpicture.h"
#include "processauthorizationresponse.h"
#include "sendfriendmessage.h"
#include "sendprivatemessage.h"
#include "sendchannelmessage.h"
#include "getnextchannelmessage.h"
#include "obscenechannelmessage.h"
#include "deletemessage.h"
#include "searchchannel.h"
#include "getnextchannelmessageurl.h"
#include "addusertochannel.h"
#include "deleteuserfromchannel.h"
#include "processauthorizationrequestbylogin.h"
#include "getuserinfobylogin.h"
#include "getauthorization.h"
#include "checkinvitation.h"
#include "getserverinfo.h"
#include "getuserinfo.h"
#include "getchannelinfo.h"
#include "createmoodstrip.h"
#include "addpicturetomoodstrip.h"

namespace MoodBox
{

class EnvelopeData : public QSharedData
{
public:
    EnvelopeData();
    EnvelopeData(Header header, TransportableObject* body);
    virtual ~EnvelopeData();

    Header header;
    TransportableObject* body;
};

class Envelope : public TransportableObject
{
public:
    Envelope();
    Envelope(Header header, TransportableObject* body);
    virtual ~Envelope();

protected:
    Envelope(EnvelopeData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static Envelope* ___new_()
    {
        return new Envelope(new EnvelopeData());
    }
    static Envelope empty()
    {
        return Envelope(new EnvelopeData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    Header getHeader() const;
    void setHeader(Header value);
    TransportableObject* getBody() const;
    void setBody(TransportableObject* value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<EnvelopeData> d;
};

}

#endif // ENVELOPE_H