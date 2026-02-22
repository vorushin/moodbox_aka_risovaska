import os
import sys

sys.path.append('/root/hgreps/')
sys.path.append('/root/hgreps/risovaska/')
sys.path.append('/root/src/django-trunk/django/')

os.environ['DJANGO_SETTINGS_MODULE'] = 'risovaska.settings_production'

import django.core.handlers.wsgi
application = django.core.handlers.wsgi.WSGIHandler()
