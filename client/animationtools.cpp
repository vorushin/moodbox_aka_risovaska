#include "animationtools.h"

#include <QWidget>

namespace MoodBox
{

WidgetSlideAnimation::WidgetSlideAnimation(QObject *parent)
	: QObject(parent), animationStep(1), animationProgress(0), totalSteps(5), direction(Vertical), widget(NULL)
{
	animationTimer.setInterval(50);

	connect(&animationTimer, SIGNAL(timeout()), this, SLOT(onAnimationTimerTimeout()));
}

void WidgetSlideAnimation::setWidget(QWidget *widget)
{
	animationProgress = 0;

	this->widget = widget;
}

void WidgetSlideAnimation::setDirection(Direction direction)
{
	animationProgress = 0;

	this->direction = direction;
}

void WidgetSlideAnimation::setInterval(qint32 interval)
{
	animationTimer.setInterval(interval);
}

void WidgetSlideAnimation::setTotalSteps(qint32 steps)
{
	totalSteps = steps;
}

void WidgetSlideAnimation::show()
{
	animationStep = 1;
	animationTimer.start();
}

void WidgetSlideAnimation::hide()
{
	animationStep = -1;
	animationTimer.start();
}

bool WidgetSlideAnimation::isInProgress() const
{
	return animationTimer.isActive();
}

void WidgetSlideAnimation::onAnimationTimerTimeout()
{
	animationProgress += animationStep;

	if (animationProgress <= 0) 
	{
		widget->hide();
		animationTimer.stop();
	}
	else 
		if (animationProgress <= totalSteps)
		{
			int invisiblePart = (direction == Vertical) ? widget->geometry().height() * (1 - (float)animationProgress / totalSteps)
														: widget->geometry().width() * (1 - (float)animationProgress / totalSteps);

			QRegion maskRegion;
			QPoint position;
			
			if (direction == Vertical)
			{
				maskRegion = QRegion(0, invisiblePart, widget->geometry().width(), widget->geometry().height());
				position = QPoint(widget->parentWidget()->pos().x(), widget->parentWidget()->pos().y() + widget->parentWidget()->frameGeometry().height() - invisiblePart);
			}
			else
			{
				maskRegion = QRegion(invisiblePart, 0, widget->geometry().width(), widget->geometry().height());
				position = QPoint(widget->parentWidget()->pos().x() + widget->parentWidget()->frameGeometry().width() - invisiblePart, widget->parentWidget()->pos().y());
			}

			if (animationStep > 0) 
			{
				widget->move(position);
				widget->setMask(maskRegion);
			}
			else
			{
				widget->setMask(maskRegion);
				widget->move(position);
			}

			widget->show();
		}
		else 
		{
			animationTimer.stop();
		}
}

}