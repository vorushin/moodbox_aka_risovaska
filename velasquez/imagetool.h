#ifndef IMAGETOOL_H
#define IMAGETOOL_H

#include <QMap>

#include "transformabletool.h"

class QNetworkReply;
class QNetworkAccessManager;

namespace Velasquez
{

// Tool for raster pictures
class ImageTool : public TransformableTool
{
	Q_OBJECT

public:
	ImageTool(QObject *parent);
    virtual ~ImageTool();
	
	virtual qint32 getElementType() const;
	virtual QStringList getFileExtensions() const;

	virtual bool canCreate(QGraphicsSceneMouseEvent *mouseEvent) const;
	virtual bool canCreate(QKeyEvent *keyEvent) const;
	virtual bool canCreate(const QMimeData *data) const;

	virtual void createElement(QGraphicsSceneMouseEvent *mouseEvent);
	virtual void createElement(QKeyEvent *keyEvent);
	virtual void createElement(const QMimeData *data, const QPointF &pos = QPointF());

	void createElement(const QImage &data, const QPointF &pos);

protected:
	virtual bool isMimeSupported(const QMimeData *data) const;
	virtual QStringList getSupportedFilesFromMime(const QMimeData *data) const;

	void loadImageFromUrl(const QString &url, const QPointF &pos = QPointF());
	void clearLoadRequests();

	static QString getImageUrl(const QMimeData *data);

protected slots:
    void onImageLoadReply(QNetworkReply *reply);

private:
	QMap <QNetworkReply*, QPointF> loadRequests;
	QNetworkAccessManager *loader;
};

}

#endif // IMAGETOOL_H
