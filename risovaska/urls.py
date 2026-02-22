from django.conf.urls.defaults import *

# Uncomment the next two lines to enable the admin:
#from django.contrib import admin
#admin.autodiscover()

urlpatterns = patterns('risovaska.core.views',
	url(r'^$', 'index', name='risovaska-index'),
	url(r'^tour', 'tour', name='risovaska-tour'),
    # url(r'^download', 'download', name='risovaska-download'),
	url(r'^users', 'users', name='risovaska-users'),
	url(r'^user/(\d+)', 'user', name='risovaska-user'),
	url(r'^channels', 'channels', name='risovaska-channels'),
	url(r'^channel/(\d+)', 'channel', name='risovaska-channel'),
	url(r'^manage_channel/(\d+)', 'manage_channel', name='risovaska-manage-channel'),
	url(r'^comics', 'comics', name='risovaska-comics'),
	url(r'^comic/(\d+)', 'comic', name='risovaska-comic'),
	url(r'^about', 'static_page', {'template_name': 'base-internal-about.html'}, name='risovaska-about'),
	url(r'^eula', 'static_page', {'template_name': 'base-internal-eula.html'}, name='risovaska-eula'),
	url(r'^feedback', 'static_page', {'template_name': 'base-internal-feedback.html'}, name='risovaska-feedback'),
	url(r'^help', 'static_page', {'template_name': 'base-internal-help.html'}, name='risovaska-help'),
	url(r'^reset', 'reset', name='risovaska-reset'),
	url(r'^unsubscribe', 'unsubscribe', name='risovaska-unsubscribe'), 
	
	url(r'^do$', 'do', name='old-app-server-mockup'),
)

urlpatterns += patterns ('',
	(r'^comments/', include('django.contrib.comments.urls')),
	# Uncomment the admin/doc line below and add 'django.contrib.admindocs' 
	# to INSTALLED_APPS to enable admin documentation:
	# (r'^admin/doc/', include('django.contrib.admindocs.urls')),

	# Uncomment the next line to enable the admin:
	#(r'^admin/', include(admin.site.urls)),
)