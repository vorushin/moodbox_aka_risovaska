#ifndef XMLPARSER_H
#define XMLPARSER_H

#include <QXmlDefaultHandler>
#include <QStack>

#include "model.h"
#include "xmlpropertyreader.h"

namespace MoodBox
{

template <class T>
class XmlParser : public QXmlDefaultHandler
{
public:
	XmlParser(): QXmlDefaultHandler()
	{
		setModel(NULL);

		rootObject = NULL;
		currentObject = NULL;
		expectedEntity = Top;

		hasLazy = false;
		lazyPropertyId = 0;
		lazyTypeId = 0;
	}
	XmlParser(Model *model): QXmlDefaultHandler()
	{
		setModel(model);

		rootObject = NULL;
		currentObject = NULL;
		expectedEntity = Top;

		hasLazy = false;
		lazyPropertyId = 0;
		lazyTypeId = 0;
	}
	virtual ~XmlParser()
	{
		//if(rootObject != NULL)
		//{
		//	delete rootObject;
		//	rootObject = 0;
		//	currentObject = 0;
		//}
	}

	T* parse(QIODevice *device)
	{
		QXmlSimpleReader xmlReader;
		xmlReader.setContentHandler(this);
		QXmlInputSource xmlSource(device);
		xmlReader.parse(xmlSource);

		return getRootObject();
	}
	static T* parse(Model* model, QIODevice *device)
	{
		XmlParser<T> parser(model);
		return parser.parse(device);
	}

	void setModel(Model *model)
	{
		this->model = model;
		reader.setModel(model);
	}
	T* getRootObject()
	{
		return rootObject;
	}

	virtual bool startElement(const QString &namespaceURI, const QString &localName, const QString &qName, const QXmlAttributes &attrs)
	{
		Q_UNUSED(namespaceURI);
		Q_UNUSED(qName);
		Q_UNUSED(attrs);

		if(!lazyCreateIfNeed())
			return false;

		switch(expectedEntity)
		{
			case Top:
			{
				stack.push(NULL);
				rootObject = createRootObject();
				currentObject = rootObject;
				expectedEntity = Property;

				return true;
			}

			case Type:
			{
				stack.push(currentObject);

				qint32 typeId = model->getTypeInfo(localName).uid;
				rememberForLazyCreation(propertyInfo.uid, typeId);

				expectedEntity = Property;

				return true;
			}

			case Property:
			{
				stack.push(currentObject);

				propertyInfo = currentObject->getPropertyInfo(model, localName);

				if(attrs.value("isNull") == "true")
					return true;

				if(propertyInfo.isUnion)
					expectedEntity = Type;
				else if(propertyInfo.isCustomTypeOrList)
				{
					rememberForLazyCreation(propertyInfo.uid, 0);
					expectedEntity = Property;
				}
				else
					expectedEntity = Value;

				return true;
			}

			default:
				return false;
		}
	}
	virtual bool endElement(const QString &namespaceURI, const QString &localName, const QString &qName)
	{
		Q_UNUSED(namespaceURI);
		Q_UNUSED(localName);
		Q_UNUSED(qName);

		hasLazy = false;
		TransportableObject* oldObject = currentObject;
		currentObject = stack.pop();
		if(oldObject != currentObject && oldObject != NULL && oldObject->isWrapper())
			delete oldObject;

		expectedEntity = Property;

		return true;
	}
	virtual bool characters(const QString &chars)
	{
		if(expectedEntity != Value) // TODO check for whitespaces only
			return true;

		reader.setCurrentValue(chars);
		PropertyReadResult result = currentObject->readProperty(propertyInfo.uid, 0, &reader);

		if(!result.getIsPropertyFound() || result.getResultObject() != 0)
			return false;

		expectedEntity = Property;
		return true;
	}

private:
	enum ExpectedEntity
	{
		Top,
		Type,
		Property,
		Value
	};

	Model *model;
	XmlPropertyReader reader;

	ExpectedEntity expectedEntity;
	PropertyInfo propertyInfo;

	T* rootObject;
	TransportableObject* currentObject;	
	QStack<TransportableObject*> stack;

	bool hasLazy;
	qint32 lazyPropertyId;
	qint32 lazyTypeId;

	T* createRootObject()
	{
		return T::___new_();
	}
	void rememberForLazyCreation(qint32 propertyId, qint32 typeId)
	{
		hasLazy = true;
		lazyPropertyId = propertyId;
		lazyTypeId = typeId;
	}
	bool lazyCreateIfNeed()
	{
		if(!hasLazy)
			return true;

		PropertyReadResult result = currentObject->readProperty(lazyPropertyId, lazyTypeId, &reader);

		hasLazy = false;

		if(!result.getIsPropertyFound())
			return false;

		currentObject = result.getResultObject();

		return true;
	}
};

}

#endif // XMLPARSER_H
