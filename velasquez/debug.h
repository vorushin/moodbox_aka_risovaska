#ifndef DEBUG_H
#define DEBUG_H

// Usage: include debug.h in .cpp files
// 1. Use #if UDEBUG to check for compilation option on any compiler
// 2. Use QDEBUG("data"); to output data into console 
// when program compiled in debug version, in release version QDEBUG will be changed to comment

// UDEBUG
#if (defined(_DEBUG) || defined(DEBUG))
#define UDEBUG 
#endif

#ifdef UDEBUG

#include <QDebug>

#define QDEBUG(x)	qDebug() << x

#else

#define QDEBUG(x)

#endif

#endif // DEBUG_H