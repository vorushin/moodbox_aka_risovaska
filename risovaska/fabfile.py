from fabric.api import *

env.hosts = ['root@linodeserver.com']

PROJECT_NAME = 'risovaska'

def run_in_virtualenv(command):
    run('source /srv/.virtualenvs/' + PROJECT_NAME + '/bin/activate && ' + command)
        
def deploy():
    with cd('/srv/code/' + PROJECT_NAME):
        run('hg update')
        run_in_virtualenv('pip install -r requirements.txt')
        run_in_virtualenv('./manage.py syncdb')
        run('reload ' + PROJECT_NAME)
