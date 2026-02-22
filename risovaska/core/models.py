from django.db import models
from django.conf import settings
		
class RssReading(models.Model):
	author_id = models.IntegerField()
	subscriber_id = models.CharField(max_length=50)
	date = models.DateTimeField(auto_now=True)
	
class DesktopUser(models.Model):
    login = models.CharField(max_length=100, db_index=True)
    name = models.CharField(max_length=100, null=True, blank=True, db_index=True)
    country = models.CharField(max_length=100, null=True, blank=True)
    city = models.CharField(max_length=100, null=True, blank=True)
    email = models.EmailField(null=True, blank=True, db_index=True)
    created = models.DateTimeField(null=True, blank=True, db_index=True)
    motto = models.CharField(max_length=2000, null=True, blank=True)
    about_me = models.CharField(max_length=4000, null=True, blank=True)
    birthday = models.DateTimeField(null=True, blank=True)
    language = models.CharField(max_length=4, null=True, blank=True)
    sex = models.CharField(max_length=10, null=True, blank=True)
    allow_news = models.BooleanField(blank=True)
    allow_publishing = models.BooleanField(blank=True)
    allow_show_friends = models.BooleanField(blank=True)
    
    def get_userpic_url(self):
        return "http://%s/%s/%s%s" % (settings.AWS_S3_HOST, settings.BUCKET, settings.USERPIC_PREFIX, self.id)

class DesktopUserContact(models.Model):
    user = models.ForeignKey(DesktopUser, related_name='contacts')
    contact_user = models.ForeignKey(DesktopUser)
    message = models.CharField(max_length=1000, null=True, blank=True)
    status = models.CharField(max_length=30, null=True, blank=True)
    is_blocked = models.BooleanField(blank=True)
    
class Channel(models.Model):
    title = models.CharField(max_length=100, null=True, blank=True, db_index=True)
    author = models.ForeignKey(DesktopUser, null=True, blank=True)
    created = models.DateTimeField(null=True, blank=True, db_index=True)
    short_description = models.CharField(max_length=2000, null=True, blank=True)
    full_description = models.CharField(max_length=4000, null=True, blank=True)
    users = models.ManyToManyField(DesktopUser, blank=True, related_name='channels')
    user_count = models.IntegerField(null=True, blank=True)
    order = models.IntegerField(null=True, blank=True)
    order_ru = models.IntegerField(null=True, blank=True)
    moderators = models.ManyToManyField(DesktopUser, blank=True, related_name='moderator_channels')
    blocked_users = models.ManyToManyField(DesktopUser, blank=True, related_name='blocked_channels')
    
    def get_logo_url(self):
        return "http://%s/%s/%s%s" % (settings.AWS_S3_HOST, settings.BUCKET, settings.CHANNEL_LOGO_PREFIX, self.id)
        
class Moodstrip(models.Model):
    title = models.CharField(max_length=400, null=True, blank=True, db_index=True)
    author = models.ForeignKey(DesktopUser, null=True, blank=True)
    send_date = models.DateTimeField(null=True, blank=True, db_index=True)
    is_published = models.BooleanField(blank=True, db_index=True)
    language = models.CharField(max_length=4, null=True, blank=True)
    is_hidden = models.BooleanField(blank=True, db_index=True)
    channel = models.ForeignKey(Channel, null=True, blank=True)
    image_id = models.IntegerField(null=True, blank=True)
    images_count = models.IntegerField(null=True, blank=True)
    
    def get_url(self):
        return "http://%s/%s/%s%s" % (settings.AWS_S3_HOST, settings.BUCKET, settings.MOODSTRIP_PREFIX, self.image_id)
        
    class Meta:
        ordering = ['-send_date']

class MoodstripItem(models.Model):
    moodstrip = models.ForeignKey(Moodstrip, null=True, blank=True)
    author = models.CharField(max_length=100, null=True, blank=True)
    send_date = models.DateTimeField(null=True, blank=True)
    
    def get_url(self):
        return "http://%s/%s/%s%s" % (settings.AWS_S3_HOST, settings.BUCKET, settings.MOODSTRIP_PREFIX, self.id)
    
class ArtMessage(models.Model):
    send_date = models.DateTimeField(null=True, blank=True, db_index=True)
    delivery_state = models.CharField(max_length=30, null=True, blank=True)
    author = models.ForeignKey(DesktopUser, null=True, blank=True)
    is_public = models.BooleanField(blank=True, db_index=True)
    
    def get_url(self):
        return "http://%s/%s/%s%s" % (settings.AWS_S3_HOST, settings.BUCKET, settings.ARTMESSAGE_PREFIX, self.id)
        
class PublishedMessage(models.Model):
    author = models.ForeignKey(DesktopUser)
    artmessage = models.ForeignKey(ArtMessage)
    send_date = models.DateTimeField(null=True, blank=True)
    
class ChannelMessage(models.Model):
    channel = models.ForeignKey(Channel)
    artmessage = models.ForeignKey(ArtMessage)
    author = models.ForeignKey(DesktopUser, null=True, blank=True)
    send_date = models.DateTimeField(null=True, blank=True)
