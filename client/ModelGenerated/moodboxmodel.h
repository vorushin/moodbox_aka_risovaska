#ifndef MOODBOXMODEL_H
#define MOODBOXMODEL_H


#include "model.h"

namespace MoodBox
{

class MoodBoxModel : public Model
{
public:
    MoodBoxModel();
    virtual ~MoodBoxModel();

    virtual void fill();
};

}

#endif // MOODBOXMODEL_H