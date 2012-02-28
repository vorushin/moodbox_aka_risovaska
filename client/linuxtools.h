#ifndef LINUXTOOLS_H
#define LINUXTOOLS_H

#include <QObject>
#include <QWidget>

namespace MoodBox {

/*
  Contains collection of utility functions for Mac specific
  window management

  Every window, which should be hidden while main window
  is minimized, should be registered by calling MacTools::addWindow
  */
class LinuxTools
{
public:
    static void addWindow(QWidget *window);
    static void removeWindow(QWidget *window);
    static void collapseWindows();
    static void restoreWindows();
};

}

#endif // LINUXTOOLS_H
