# Django settings for risovaska project.
import os.path
PROJECT_DIR = os.path.dirname(__file__)

DEBUG = True
TEMPLATE_DEBUG = DEBUG

ADMINS = (
	 ('Roman Vorushin', 'roman.vorushin@gmail.com'),
)

MANAGERS = ADMINS

DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.mysql', # Add 'postgresql_psycopg2', 'postgresql', 'mysql', 'sqlite3' or 'oracle'.
        'NAME': 'risovaska',                      # Or path to database file if using sqlite3.
        'USER': '',                      # Not used with sqlite3.
        'PASSWORD': '',                  # Not used with sqlite3.
        'HOST': '',                      # Set to empty string for localhost. Not used with sqlite3.
        'PORT': '',                      # Set to empty string for default. Not used with sqlite3.
    }
}

# Local time zone for this installation. Choices can be found here:
# http://en.wikipedia.org/wiki/List_of_tz_zones_by_name
# although not all choices may be available on all operating systems.
# If running in a Windows environment this must be set to the same as your
# system time zone.
TIME_ZONE = 'Europe/Moscow'

# Language code for this installation. All choices can be found here:
# http://www.i18nguy.com/unicode/language-identifiers.html
LANGUAGE_CODE = 'ru-RU'

SITE_ID = 1
SITE_URL = 'http://risovaska.ru'

# If you set this to False, Django will make some optimizations so as not
# to load the internationalization machinery.
USE_I18N = True

# Absolute path to the directory that holds media.
# Example: "/home/media/media.lawrence.com/"
MEDIA_ROOT = ''

# URL that handles the media served from MEDIA_ROOT. Make sure to use a
# trailing slash if there is a path component (optional in other cases).
# Examples: "http://media.lawrence.com", "http://example.com/media/"
MEDIA_URL = '/media/'

# URL prefix for admin media -- CSS, JavaScript and images. Make sure to use a
# trailing slash.
# Examples: "http://foo.com/media/", "/media/".
ADMIN_MEDIA_PREFIX = '/media/'

# Make this unique, and don't share it with anybody.
SECRET_KEY = os.environ.get('DJANGO_SECRET_KEY', 'change-me-in-production')

# List of callables that know how to import templates from various sources.
TEMPLATE_LOADERS = (
    'django.template.loaders.filesystem.Loader',
    'django.template.loaders.app_directories.Loader',
#     'django.template.loaders.eggs.Loader',
)

MIDDLEWARE_CLASSES = (
	'django.middleware.common.CommonMiddleware',
	'django.contrib.sessions.middleware.SessionMiddleware',
	'django.contrib.auth.middleware.AuthenticationMiddleware',
)

ROOT_URLCONF = 'risovaska.urls'

TEMPLATE_DIRS = (
	os.path.join(PROJECT_DIR, 'templates'),
	# Put strings here, like "/home/html/django_templates" or "C:/www/django/templates".
	# Always use forward slashes, even on Windows.
	# Don't forget to use absolute paths, not relative paths.
)

INSTALLED_APPS = (
	'django.contrib.admin',
	'django.contrib.auth',
	'django.contrib.comments',
	'django.contrib.contenttypes',
	'django.contrib.sessions',
	'django.contrib.sites',
	'risovaska.core',
)

CACHE_BACKEND = 'memcached://127.0.0.1:11211/?timeout=300'

SMALL_CACHE_TIMEOUT = 60

BUCKET = "moodbox-server-data"
ARTMESSAGE_PREFIX = "artmessage/"
USERPIC_PREFIX = "userpic/"
CHANNEL_LOGO_PREFIX = "channel_logo/"
MOODSTRIP_PREFIX = "moodstrip/"
AWS_S3_HOST = "s3.amazonaws.com"

try:
    from local_settings import *
except ImportError:
    import sys
    sys.stderr.write('Unable to read local_settings.py\n')
    # Convenient defaults
    DEBUG = False

try:
    DATABASES.update(LOCAL_DATABASES)
except NameError:
    pass

try:
    INSTALLED_APPS += ADDITIONAL_APPS
except NameError:
    pass

try:
    MIDDLEWARE_CLASSES += ADDITIONAL_MIDDLEWARE
except NameError:
    pass

if not hasattr(globals(), 'TEMPLATE_DEBUG'):
    TEMPLATE_DEBUG = DEBUG
if not hasattr(globals(), 'THUMBNAIL_DEBUG'):
    THUMBNAIL_DEBUG = DEBUG
