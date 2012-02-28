#ifndef ANIMATIONTOOLS_H
#define ANIMATIONTOOLS_H

#include <QObject>
#include <QTimer>

class QWidget;

namespace MoodBox
{

// Widget show animation
class WidgetSlideAnimation : public QObject
{
	Q_OBJECT

public:
	enum Direction { Horizontal, Vertical };

	WidgetSlideAnimation(QObject *parent = 0);

	void setWidget(QWidget *widget);
	void setDirection(Direction direction);
	void setInterval(qint32 interval);
	void setTotalSteps(qint32 steps);

	void show();
	void hide();

	bool isInProgress() const;

private:
	QTimer animationTimer;

	int animationStep, animationProgress;	
	int totalSteps;

	Direction direction;
	QWidget *widget;

private slots:
	void onAnimationTimerTimeout();

};

}

#endif //  ANIMATIONTOOLS_H