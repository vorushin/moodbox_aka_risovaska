#-*- coding: utf-8 -*-
from datetime import datetime, timedelta

from django.core.management import base

from risovaska.core.models import ArtMessage, DesktopUser, DesktopUserContact, Channel, Moodstrip, MoodstripItem, \
                                  PublishedMessage, ChannelMessage

class Command(base.BaseCommand):
    u"""
    Импорт данных из текстового файла дампа мнезии
    
    manage.py mnesia_import <filename>
    """
    def handle(self, *args, **options):
        def get_unicode_char(char):
            code = int(char.replace('[', '').replace(']', ''))
            if code > 255:
                return '\\u%04X' % code
            else:
                return chr(code)
        
        def get_unicode_string(counter):
            result = ''
            if items[counter] != 'undefined' and items[counter] != '[]':
                if items[counter].find('"') == 0: # only ascii symbols in quotations
                    result = items[counter][1:]
                    length = len(result)
                    if result.rfind('"') == length-1 and length != 0:
                        try:
                            result = result[:-1].decode('UTF-8')
                        except:
                            result = None
                    else:
                        # combine together ascii strings separated by commas
                        counter += 1
                        while items[counter].rfind('"') != len(items[counter]) - 1:
                            result += ', %s' % items[counter]
                            counter += 1
                        result += ', %s' % items[counter][:-1]
                        try:
                            result = result.decode('UTF-8')
                        except:
                            result = None
                elif items[counter].find('[') >= 0: # erlang unicode string
                    result = ''
                    while items[counter].find(']') < 0:
                        result += get_unicode_char(items[counter])
                        counter += 1
                    result += get_unicode_char(items[counter].replace('}', ''))
                    result = result.decode('unicode_escape')
                else: # only ascii symbols without quotations
                    result = items[counter].replace("'", '').replace('}', '').replace(']', '').replace('.', '')
            
            return counter, result
        
        def get_list(counter):
            result = []
            is_erlang_record = False
            if items[counter] != 'undefined' and items[counter] != '[]':
                while items[counter].find(']') < 0:
                    if items[counter].find('{') >= 0: # erlang record is converted to list
                        is_erlang_record = True
                        element = []
                        while items[counter].find('}') < 0:
                            counter += 1
                            try:
                                element.append(int(items[counter].replace('}', '').replace(']', '').replace('.', '')))
                            except ValueError, TypeError:
                                try:
                                    counter, string = get_unicode_string(counter)
                                except:
                                    string = ''
                                element.append(string)
                            
                    else:
                        try:
                            element = int(items[counter].replace('[', '')) # integer
                        except ValueError, TypeError:
                            counter, element = get_unicode_string(counter) # I hope it's never happend :)
                    result.append(element)

                    if items[counter].rfind(']') == len(items[counter]) - 1:
                        break
                    counter += 1
                if not is_erlang_record: # get last element if it's not erlang record
                    result.append(int(items[counter].replace('[', '').replace(']', '')))
            return counter, result

        def get_published_messages(counter):
            result = []
            if items[counter] != 'undefined' and items[counter] != '[]':
                while items[counter].find(']') < 0:
                    element = []
                    while items[counter].find('}') < 0:
                        element.append(int(items[counter].replace('{', '').replace('[', '').replace('}', '').replace(']', '').replace('.', '')))
                        counter += 1
                    element.append(int(items[counter].replace('{', '').replace('[', '').replace('}', '').replace(']', '').replace('.', '')))
                    result.append(element)

                    if items[counter].find(']') > 0:
                        break
                    counter += 1
            return counter, result
            
        if len(args) < 1:
            print self.__doc__
            return

        filename = args[0]
        f = open(filename, 'r')
        start_erlang_date = datetime(1970, 1, 1)
        
        line = f.readline().replace('\n', '').strip()
        while line != '':
            if line.find('{user_account_data,') == 0: # users
                item = ''
                while line.rfind('}.') != len(line) - 2:
                   item += line
                   line = f.readline().replace('\n', '').strip()
                item += line
                items = item.split(',')
                try:
                    counter = 1
                    id = items[counter]
                    user, is_created = DesktopUser.objects.get_or_create(id=id)
                    # id
                    if is_created:
                        user.id = id
                    # login
                    counter += 1
                    login = items[counter].replace('"', '')
                    print 'Importing %s' % login
                    user.login = login
                    counter += 1
                    # skip password
                    while items[counter].find('>>') < 0:
                        counter += 1
                    # created date
                    user.created = start_erlang_date + timedelta(seconds=int(items[counter+1])/1000000)
                    # motto
                    counter += 3
                    counter, user.motto = get_unicode_string(counter)
                    #about_me
                    counter += 1
                    counter, user.about_me = get_unicode_string(counter)
                    #name
                    counter += 1
                    counter, user.name = get_unicode_string(counter)
                    # skip upper name
                    counter += 1
                    counter, _upper_name = get_unicode_string(counter)
                    #country
                    counter += 1
                    counter, user.country = get_unicode_string(counter)
                    #city
                    counter += 1
                    counter, user.city = get_unicode_string(counter)
                    #email
                    counter += 1
                    if items[counter] != 'undefined':
                        user.email = items[counter].replace('"', '')
                    else:
                        user.email = None
                    #sex
                    counter += 2
                    user.sex = items[counter]
                    # birth date
                    counter += 1
                    if items[counter] != 'undefined':
                        user.birthday = start_erlang_date + timedelta(seconds=int(items[counter])/1000000)
                    else:
                        user.birthday = None
                    # language
                    counter += 1
                    if items[counter] != 'undefined':
                        user.language = items[counter]
                    else:
                        user.language = None
                    #allow_news
                    counter += 1
                    user.allow_news = True if items[counter] == 'true' else False
                    #allow_publishing
                    counter += 1
                    user.allow_publishing = True if items[counter] == 'true' else False
                    #allow_show_friends
                    counter += 1
                    user.allow_show_friends = True if items[counter] == 'true' else False
                    
                    user.save()
                except Exception, e:
                    print 'Can`t import %s because: %s' % (login, e)
                    if is_created:
                        user.delete()
            elif line.find('{channel,') == 0: # channels
                item = ''
                while line.rfind('}.') != len(line) - 2:
                   item += line
                   line = f.readline().replace('\n', '').strip()
                item += line
                items = item.split(',')
                try:
                    counter = 1
                    id = items[counter]
                    channel, is_created = Channel.objects.get_or_create(id=id)
                    # id
                    if is_created:
                        channel.id = id
                    # author
                    counter += 1
                    channel.author = DesktopUser(items[counter])
                    #created
                    counter += 1
                    channel.created = start_erlang_date + timedelta(seconds=int(items[counter])/1000000)
                    # title
                    counter += 1
                    counter, title = get_unicode_string(counter)
                    print 'Importing %s' % title
                    channel.title = title
                    # skip upper title
                    counter += 1
                    counter, _upper_title = get_unicode_string(counter)
                    # short description
                    counter += 1
                    counter, channel.short_description = get_unicode_string(counter)
                    # full description
                    counter += 1
                    counter, channel.full_description = get_unicode_string(counter)
                    # users
                    counter += 1
                    counter, users = get_list(counter)
                    if not is_created:
                        channel.users.clear()
                    channel.users.add(*users)
                    # user_count, order, order_ru
                    counter += 1
                    channel.user_count = items[counter]
                    counter += 1
                    channel.order = items[counter]
                    counter += 1
                    channel.order_ru = items[counter]
                    # TODO moderators and blocked users
                    
                    channel.save()
                except Exception, e:
                    print 'Can`t import channel id#%s because: %s' % (id, e)
                    if is_created:
                        channel.delete()
            elif line.find('{contact,') == 0: # user`s contacts
                item = ''
                while line.rfind('}.') != len(line) - 2:
                   item += line
                   line = f.readline().replace('\n', '').strip()
                item += line
                items = item.split(',')
                try:
                    counter = 1
                    user_id = items[counter]
                    # contacts
                    counter += 1
                    counter, contacts = get_list(counter)
                    for c in contacts:
                        contact, is_created = DesktopUserContact.objects.get_or_create(user=DesktopUser(user_id), 
                                                                                       contact_user=DesktopUser(c[0]))
                        contact.message = c[1]
                        contact.status = c[2]
                        contact.is_blocked = True if c[3] == 'true' else False
                        contact.save()
                    # TODO channels
                except Exception, e:
                    print 'Can`t import contact for user id#%s because: %s' % (user_id, e)
            elif line.find('{moodstrip,') == 0: # moodstrips
                item = ''
                while line.rfind('}.') != len(line) - 2:
                   item += line
                   line = f.readline().replace('\n', '').strip()
                item += line
                items = item.split(',')
                try:
                    counter = 1
                    id = items[counter]
                    print 'Importing moodstrip #%s' % id
                    moodstrip, is_created = Moodstrip.objects.get_or_create(id=id)
                    if is_created:
                       moodstrip.id = id 
                    # author
                    counter += 1
                    moodstrip.author = DesktopUser(items[counter])
                    # skip author_login
                    counter += 1
                    counter, _author_login = get_unicode_string(counter)
                    # send_date
                    counter += 1
                    moodstrip.send_date = start_erlang_date + timedelta(seconds=int(items[counter])/1000000)
                    # title
                    counter += 1
                    counter, moodstrip.title = get_unicode_string(counter)
                    # skip upper title
                    counter += 1
                    counter, _upper_title = get_unicode_string(counter)
                    # is_published
                    counter += 1
                    moodstrip.is_published = True if items[counter] == 'true' else False
                    # items
                    counter += 1
                    counter, moodstrip_items = get_list(counter)
                    if moodstrip_items:
                        moodstrip.image_id = moodstrip_items[0][1]
                    moodstrip.images_count = len(moodstrip_items)
                    # language
                    counter += 1
                    if items[counter] != 'undefined':
                        moodstrip.language = items[counter]
                    else:
                        moodstrip.language = None
                    # is_hidden
                    counter += 1
                    moodstrip.is_hidden = True if items[counter] == 'true' else False
                    # channel
                    counter += 1
                    channel_id = items[counter].replace('}', '').replace('.', '')
                    if channel_id != '-1' and channel_id != 'undefined':
                        moodstrip.channel = Channel(channel_id)
                    else:
                        moodstrip.channel = None
                    moodstrip.save()
                    # moodstrip items
                    for item in moodstrip_items:
                        moodstrip_item, is_created = MoodstripItem.objects.get_or_create(id=item[1])
                        if is_created:
                            moodstrip_item.id = item[1]
                            moodstrip_item.moodstrip = moodstrip
                            moodstrip_item.send_date = start_erlang_date + timedelta(seconds=int(item[3])/1000000)
                            moodstrip_item.author = item[2]
                            moodstrip_item.save()
                            
                except Exception, e:
                    print 'Can`t import moodstrip id#%s because: %s' % (id, e)
            #elif line.find('{art_message,') == 0: # artmessages
            #    item = ''
            #    while line.rfind('}.') != len(line) - 2:
            #       item += line
            #       line = f.readline().replace('\n', '').strip()
            #    item += line
            #    items = item.split(',')
            #    try:
            #        counter = 1
            #        id = items[counter]
            #        print 'Importing artmessage #%s' % id
            #        artmessage, is_created = ArtMessage.objects.get_or_create(id=id)
            #        if is_created:
            #           artmessage.id = id 
            #        # send_date
            #        counter += 1
            #        artmessage.send_date = start_erlang_date + timedelta(seconds=int(items[counter])/1000000)
            #        # skip recepient_ids
            #        counter += 1
            #        counter, _recepient_ids = get_list(counter)
            #        # delivery_state
            #        counter += 1
            #        counter, artmessage.delivery_state = get_unicode_string(counter)
            #        # author
            #        counter += 1
            #        if items[counter] != 'undefined':
            #            artmessage.author = DesktopUser(items[counter])
            #        # is_public
            #        counter += 1
            #        artmessage.is_public = True if items[counter] == 'true' else False       
            #        artmessage.save()
            #    except Exception, e:
            #        print 'Can`t import artmessage id#%s because: %s' % (id, e)
            elif line.find('{channel_message,') == 0: # channel_messages
                item = ''
                while line.rfind('}.') != len(line) - 2:
                   item += line
                   line = f.readline().replace('\n', '').strip()
                item += line
                items = item.split(',')
                try:
                    counter = 1
                    channel = Channel(items[counter])
                    print 'Importing channel messages of channel #%s' % channel.id
                    # messages
                    counter += 1
                    counter, messages = get_list(counter)
                    ChannelMessage.objects.filter(channel=channel).delete()
                    for message in messages:
                        send_date = start_erlang_date + timedelta(seconds=int(message[3])/1000000)
                        channel_message = ChannelMessage(channel=channel, artmessage=ArtMessage(message[0]), 
                                                         author=DesktopUser(message[1]), send_date=send_date)
                        channel_message.save()
                        
                except Exception, e:
                    print 'Can`t import channel messages for channel id#%s because: %s' % (channel.id, e)
            elif line.find('{published_message,') == 0: # published_message
                item = ''
                while line.rfind('}.') != len(line) - 2:
                   item += line
                   line = f.readline().replace('\n', '').strip()
                item += line
                items = item.split(',')
                try:
                    counter = 1
                    author = DesktopUser(items[counter])
                    print 'Importing published messages of author #%s' % author.id
                    # skip message_ids
                    counter += 1
                    counter, _message_ids = get_list(counter)
                    # message_image_ids
                    counter += 1
                    counter, message_image_ids = get_published_messages(counter)
                    PublishedMessage.objects.filter(author=author).delete()
                    for message in message_image_ids:
                        send_date = start_erlang_date + timedelta(seconds=int(message[1])/1000000)
                        published_message = PublishedMessage(author=author, artmessage=ArtMessage(message[0]), 
                                                             send_date=send_date)
                        published_message.save()
                        
                except Exception, e:
                    print 'Can`t import published messages for author "%s" because: %s' % (author, e)

            line = f.readline().replace('\n', '')

        f.close()