#include "linuxtools.h"
#include <QList>

namespace MoodBox 
{
   
// LinuxTools
    
static QList<QWidget*> registeredWindows;
static QList<QWidget*> collapsedWindows;

void LinuxTools::addWindow(QWidget *window)
{
    registeredWindows.append(window);
}
    
void LinuxTools::removeWindow(QWidget *window)
{
    registeredWindows.removeAll(window);
}

void LinuxTools::collapseWindows()
{
    QWidget *window;
    foreach(window, registeredWindows)
    {
        if (window->isVisible())
        {
            window->hide();
            collapsedWindows.append(window);
        }
    }
}

void LinuxTools::restoreWindows()
{
    QWidget *window;
    foreach(window, collapsedWindows)
        window->show();
    collapsedWindows.clear();
}

}
