#ifndef AUTOUPDATERMAC_H
#define AUTOUPDATERMAC_H

#include "autoupdater.h"

namespace MoodBox {
    
class SparkleAutoUpdater: public AutoUpdater
{
public:
	SparkleAutoUpdater(QWidget *parentWidget);
	~SparkleAutoUpdater();
    
	virtual void checkForUpdateInteractive(QWidget *dialogWidget);
    
private:
    class Private;
    Private* d;    
};

}
#endif // AUTOUPDATERMAC_H