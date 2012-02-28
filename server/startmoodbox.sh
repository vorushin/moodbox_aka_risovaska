#!/bin/sh

##
## usage startmoodbox.sh {start|stop|debug|init|init_test|init_ssl}
##

SNAME=moodbox_test
PA=./ebin
ERL=erl
HOSTNAME=`hostname`
#export HEART_COMMAND="./startmoodbox.sh start"

case $1 in

    start)
        $ERL -boot ./moodbox -sname $SNAME -pa $PA -config sys.config\
             -s moodbox start_script
        echo  "Starting server $SNAME"
        ;;

    debug)
        $ERL -boot ./moodbox -sname $SNAME -pa $PA -s moodbox start_script -config sys.config
        ;;

    stop)
        $ERL -noshell -sname moodbox_stopper -pa $PA \
            -s moodbox stop_script $SNAME@$HOSTNAME && \
        echo "Stopping  server $SNAME"
        ;;

    init)
        $ERL -noshell -sname $SNAME -pa $PA -config sys.config\
            -s moodbox start_script init
        echo "Initializing server"
        ;;

    init_test)
        $ERL -noshell -sname $SNAME -pa $PA -config sys.config\
            -s moodbox start_script init_test && \
        echo "Test data generated"
        ;;

    init_ssl)
        $ERL -noshell -sname $SNAME -pa $PA -config sys.config\
            -s moodbox start_script init_ssl && \
        echo "SSL initialized"
        ;;

    *)
        echo "Usage: $0 {start|stop|debug|init|init_test|init_ssl}"
        exit 1
esac

exit 0
