# -*- coding: utf-8 -*-
from datetime import datetime, timedelta
from xml.etree.ElementTree import Element, ElementTree, fromstring

from django.conf import settings
from django.core.urlresolvers import reverse
from django.core.paginator import Paginator
from django.db.models import Q
from django.http import Http404, HttpResponse, HttpResponseBadRequest, HttpResponseRedirect
from django.shortcuts import render_to_response
from django.template import RequestContext
from django.utils.encoding import smart_unicode

import appserver
import constants
from models import DesktopUser, Channel, Moodstrip
from utils import get_birthdate_from_age, get_integer_or_none
import wordpress_connect


def index(request):
    return render_to_response(
        'base-index.html', 
        {'blog_posts': wordpress_connect.get_latest_blog_posts()},
        context_instance=RequestContext(request)
    )

def tour(request):
    return render_to_response(
        'base-internal-tour.html', 
        {'menu_item': 2}, 
        context_instance=RequestContext(request)
    )

def download(request):
    return render_to_response(
        'base-internal-download.html', 
        {'menu_item': 3}, 
        context_instance=RequestContext(request)
    )

def get_latest_comics():
    moodstrips = Moodstrip.objects.filter(is_published=True, is_hidden=False).order_by('-send_date')[0:3]
    return moodstrips
    
def do_paged_request(request_name, GET, params, records_per_page=50, cache_timeout=0):
    try:
        params['page_number'] = int(GET.get('p', '1'))
    except ValueError:
        params['page_number'] = 1

    params['records_per_page'] = records_per_page
    
    response = u''
    
    if cache_timeout == 0:
        response = appserver.make_request(request_name, params)
    else:
        response = appserver.make_request(request_name, params, cache_timeout)
        
    items = appserver.parse_list(response)
    
    # paging template parameters
    page_no = 1
    
    try:
        page_no = int(appserver.get_item(response, 'page_number'))
    except TypeError, ValueError:
        page_no = 1

    is_last_page = bool(appserver.get_item(response, 'has_more') == 'false')
    
    pages = []
    
    for i in xrange(page_no - 5, page_no + 5 + 1):
        if i < 1:
            continue
        pages.append(i)
        if i >= page_no and is_last_page:
            break        
    
    template_params = {}
    template_params['page_no'] = page_no
    template_params['min_page'] = pages[0]
    template_params['is_last_page'] = is_last_page
    template_params['navigation_pages'] = pages    
    template_params['from_item'] = (page_no - 1) * records_per_page + 1
    template_params['current_page_size'] = len(items)
    template_params['to_item'] = template_params['from_item'] + template_params['current_page_size'] - 1
    template_params['getparams'] = GET
    
    return {'items': items,
            'template_params': template_params}

def users(request):
    g = request.GET
    time_type = '3'
    if g.get('time-type', '') != '':
        time_type = g.get('time-type', '')    
    
    country = g.get('country', '')
    city = g.get('city', '')
    sex = g.get('sex', '')
    age = g.get('age', '')
    search_type = g.get('search-type', 'simple')
    search_text = g.get('search-text', '')
    
    min_age = max_age = 0
    if age:
        min_age = get_integer_or_none(age.split('-')[0])
        max_age = get_integer_or_none(age.split('-')[1])
    
    result = DesktopUser.objects.all().order_by('-created')
    if search_type == 'simple':
        if search_text:
            result = result.filter(Q(login__icontains=search_text) | Q(name__icontains=search_text) | 
                                                Q(email__icontains=search_text))
        else:
            now = datetime.now()
            if time_type == '0': # today
                result = result.filter(created__gte=now.replace(hour=0, minute=0, second=0))
            elif time_type == '1': # week
                result = result.filter(created__gte=now - timedelta(days=7))
            elif time_type == '2': # month
                result = result.filter(created__gte=now - timedelta(days=30))
    else:
        if search_text:
            result = DesktopUser.objects.filter(Q(login__icontains=search_text) | Q(name__icontains=search_text) | 
                                                Q(email__icontains=search_text))
        if country:
            result = result.filter(country=country)
        if city:
            result = result.filter(city=city)
        if sex:
            result = result.filter(sex=sex)
        if min_age:
            result = result.filter(birthday__gte=get_birthdate_from_age(min_age))
        if max_age:
            result = result.filter(birthday__lte=get_birthdate_from_age(max_age))
            
    try:
        page_no = int(g.get('p', '1'))
    except ValueError:
        page_no = 1

    records_per_page = 50
    paginator = Paginator(result, records_per_page)
    if paginator.num_pages < page_no:
        page_no = paginator.num_pages
    users = paginator.page(page_no)
    is_last_page = not users.has_next()
    
    pages = []
    for i in range(page_no - 5, page_no + 5 + 1):
        if i < 1:
            continue
        pages.append(i)
        if i >= page_no and is_last_page:
            break

    params = {'menu_item': 4, 'layout': '2', 'box_right': 'download_button_last_comics',
         'latest_comics': get_latest_comics(), 'users': users.object_list,
         'time_types': constants.get_time_types(), 'time_type': time_type,
         'countries': constants.get_countries(), 'country': country, 'city': city,
         'sexes': constants.get_sexex(), 'sex': sex,
         'ages': constants.get_ages(), 'age': age,
         'search_type': search_type, 'search_text': search_text,
         'page_no': page_no, 'min_page': pages[0], 'is_last_page': is_last_page, 'navigation_pages': pages,
         'from_item': (page_no - 1) * records_per_page + 1, 'current_page_size': len(users.object_list),
         'getparams': g}
    params['to_item'] = params['from_item'] + params['current_page_size'] - 1
    
    return render_to_response(
        'base-internal-users.html',
        params,
        context_instance=RequestContext(request)
    )

def get_widget_code(flashvars):
    w = '<object classid="clsid:d27cdb6e-ae6d-11cf-96b8-444553540000" codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=8,0,0,0" width="447" height="398" id="got2b_site" align="top">'
    w += '<param name="allowScriptAccess" value="sameDomain" />'
    w += '<param name="allowFullScreen" value="false" />'
    w += '<param name="movie" value="' + settings.MEDIA_URL + '/img/box.swf" />'
    w += '<param name="quality" value="high" />'
    w += '<param name="scale" value="noscale" />'
    w += '<param name="bgcolor" value="#000000" />'
    w += '<embed src="' + settings.MEDIA_URL + '/img/box.swf" quality="high" scale="noscale" bgcolor="#000000" width="447" height="398" name="got2b_site" align="top" allowScriptAccess="sameDomain" allowFullScreen="false" type="application/x-shockwave-flash" pluginspage="http://www.macromedia.com/go/getflashplayer" wmode="transparent" flashvars="' + flashvars + '" />'
    w += '<param name="flashvars" value="' + flashvars + '" />'
    w += '<param name="wmode" value="transparent" /></object>'
    return w
    
def user(request, id):
    try:
        user = DesktopUser.objects.get(id=id)
    except DesktopUser.DoesNotExist:
        return HttpResponseRedirect(reverse('risovaska.core.views.users'))
    
    user_comics = Moodstrip.objects.filter(author=user)
    user_contacts = user.contacts.all().select_related('contact_user')
    user_channels = user.channels.all()
    contacts_are_hidden = not user.allow_show_friends
    user_comics = []
    
    params = {}
    params['user'] = user
    params['contacts_show_limit'] = 30
    params['contacts_are_hidden'] = contacts_are_hidden
    params['user_contacts'] = user_contacts
    params['user_channels'] = user_channels
    params['user_comics'] = user_comics
    params['layout'] = '2'
    params['box_right'] = 'download_button_last_comics'
    params['latest_comics'] = get_latest_comics()
    params['flashvars'] = 'login=%s&contact_type=friend&contact_id=%s' % (user.login, user.id)
    params['widget_code'] = get_widget_code(params['flashvars'])
    
    return render_to_response('base-internal-user.html', params, context_instance=RequestContext(request))

def channels(request):
    channels = Channel.objects.filter(order__gte=1000).order_by('order_ru')
    
    try:
        page_no = int(request.GET.get('p', 1))
    except ValueError:
        page_no = 1
        
    records_per_page = 50
    paginator = Paginator(channels, records_per_page)
    if paginator.num_pages < page_no:
        page_no = paginator.num_pages
    channels = paginator.page(page_no)
    is_last_page = not channels.has_next()
    
    params = {'page_no': page_no,'is_last_page': is_last_page, 'from_item': (page_no - 1) * records_per_page + 1,
              'current_page_size': len(channels.object_list)}
    params['to_item'] = params['from_item'] + params['current_page_size'] - 1
    params['channels'] = channels.object_list
    params['menu_item'] = 5
    params['layout'] = '2'
    params['box_right'] = 'download_button_last_comics'
    params['link_to_comics'] = True
    params['latest_comics'] = get_latest_comics()    
    
    return render_to_response('base-internal-channels.html', params, context_instance=RequestContext(request))

def channel(request, id):
    try:
        channel = Channel.objects.get(id=id)
    except Channel.DoesNotExist:
        return HttpResponseRedirect(reverse('risovaska.core.views.channels'))
    
    channel_comics = Moodstrip.objects.filter(channel=channel)
    try:
        page_no = int(request.GET.get('p', 1))
    except ValueError:
        page_no = 1
    records_per_page = 50
    paginator = Paginator(channel_comics, records_per_page)
    if paginator.num_pages < page_no:
        page_no = paginator.num_pages
    channel_comics = paginator.page(page_no)
    is_last_page = not channel_comics.has_next()
    pages = []
    for i in range(page_no - 5, page_no + 5 + 1):
        if i < 1:
            continue
        pages.append(i)
        if i >= page_no and is_last_page:
            break
            
    params = {'page_no': page_no,'is_last_page': is_last_page, 'from_item': (page_no - 1) * records_per_page + 1,
              'current_page_size': len(channel_comics.object_list), 'navigation_pages': pages, 
              'getparams': request.GET}
    params['to_item'] = params['from_item'] + params['current_page_size'] - 1
    params['channel'] = channel
    params['channel_comics'] = channel_comics.object_list
    params['layout'] = 2
    params['box_right'] = 'download_button_last_comics'
    params['latest_comics'] = get_latest_comics()
    params['moderators'] = channel.moderators.all()
    params['flashvars'] = 'login=' + channel.title
    params['flashvars'] += '&contact_type=channel&contact_id=%s' % channel.id
    params['widget_code'] = get_widget_code(params['flashvars'])
    
    return render_to_response('base-internal-channel.html', params, context_instance=RequestContext(request))
    
def manage_channel(request):
    return render_to_response('base-internal-manage-channel.html', {}, context_instance=RequestContext(request))

def comics(request):
    time_type = '3'
    if request.GET.get('time-type', '') != '':
        time_type = request.GET.get('time-type', '')    
    
    search_text = request.GET.get('search-text', '')
    
    result = Moodstrip.objects.filter(is_published=True, is_hidden=False)
    if search_text:
        result = result.filter(title__icontains=search_text)
    else:
        now = datetime.now()
        if time_type == '0': # today
            result = result.filter(send_date__gte=now.replace(hour=0, minute=0, second=0)).order_by('-send_date')
        elif time_type == '1': # week
            result = result.filter(send_date__gte=now - timedelta(days=7)).order_by('-send_date')
        elif time_type == '2': # month
            result = result.filter(send_date__gte=now - timedelta(days=30)).order_by('-send_date')

    try:
        page_no = int(request.GET.get('p', '1'))
    except ValueError:
        page_no = 1

    records_per_page = 50
    paginator = Paginator(result, records_per_page)
    if paginator.num_pages < page_no:
        page_no = paginator.num_pages
    moodstrips = paginator.page(page_no)
    is_last_page = not moodstrips.has_next()

    pages = []
    for i in range(page_no - 5, page_no + 5 + 1):
        if i < 1:
            continue
        pages.append(i)
        if i >= page_no and is_last_page:
            break
    
    params = {'page_no': page_no, 'min_page': pages[0], 'is_last_page': is_last_page, 'navigation_pages': pages,
              'from_item': (page_no - 1) * records_per_page + 1, 'current_page_size': len(moodstrips.object_list),
              'getparams': request.GET}
    params['to_item'] = params['from_item'] + params['current_page_size'] - 1
    
    params['comics'] = moodstrips.object_list
    params['menu_item'] = 6
    params['layout'] = '2'
    params['box_right'] = 'download_button_last_comics'
    params['latest_comics'] = get_latest_comics()
    params['time_types'] = constants.get_time_types()
    params['time_type'] = time_type
    params['search_text'] = search_text
    
    return render_to_response('base-internal-comics.html', params, context_instance=RequestContext(request))
    
def get_comic_code(id, comic, frames):
    result = u''
    for f in frames:
        result = result + '<p>' + '<img src="' + f.get_url() + '" border="0" style="border: 1px solid #cdcdcd;"></p>'
        
    result = result + '<div><small><a href="http://risovaska.ru'
    result = result + reverse('risovaska-comic', args=[id]) 
    comic_title = comic.title or u'безымянный'
    result = result + '">' + comic_title
    result = result + u'</a>, нарисовано в <a href="http://risovaska.ru">Рисоваське</a></small></div>'
    return result
    
def comic(request, id):
    try:
        comic = Moodstrip.objects.get(id=id)
    except Moodstrip.DoesNotExist:
        return HttpResponseRedirect(reverse('risovaska.core.views.comics'))
    
    params = {}
    params['comic'] = comic
    params['frames'] = comic.moodstripitem_set.all()
    params['flashvars'] = 'id=' + id
    params['widget_code'] = get_widget_code(params['flashvars'])
    params['blog_code'] = get_comic_code(id, comic, params['frames'])
    params['layout'] = 2
    params['box_right'] = 'download_button_last_comics'
    params['latest_comics'] = get_latest_comics()
    
    return render_to_response('base-internal-comic.html', params, context_instance=RequestContext(request))

def static_page(request, template_name):
    return render_to_response(template_name, {'box_right': 'download_tour', 'layout': 2},
                              context_instance=RequestContext(request))

def reset(request):
    if request.method == 'GET':
        new_password = ''
        if 'u' in request.GET and 'r' in request.GET:       
            response = appserver.make_request('get_recovered_password', {'user_id': request.GET['u'], 'reset_code': request.GET['r']}, 0)
            new_password = appserver.get_item(response, 'result')
        return render_to_response('base-internal-reset.html', {'new_password': new_password}, context_instance=RequestContext(request))
    else:
        raise Http404

def unsubscribe(request):
    if request.method == 'GET':
        success = False
        if 'u' in request.GET and 'c' in request.GET:
            response = appserver.make_request('unsubscribe', {'user_id': request.GET['u'], 'unsubscribe_code': request.GET['c']}, 0)
            success = (appserver.get_item(response, 'result') == 'ok')
        return render_to_response('base-internal-unsubscribe.html', {'success': success}, context_instance=RequestContext(request))
    else:
        raise Http404
        
def create_element(name, text):
    el = Element(name)
    el.text = smart_unicode(text)
    return el
        
def do(request):
    if request.method != 'POST':
        return HttpResponseBadRequest()
    x = fromstring(request.raw_post_data)
    action = x.find('body').getchildren()[0]
    if action.tag == 'get_moodstrip':
        moodstrip_id = action.find('moodstrip_id').text
        moodstrip = Moodstrip.objects.get(id=moodstrip_id)
        
        envelope = Element('envelope')        
        body = Element('body')        
        get_moodstrip_result = Element('get_moodstrip_result')
        
        result = Element('result')
        result.append(create_element('author_id', moodstrip.author.id))
        result.append(create_element('author_login', moodstrip.author.login))
        result.append(create_element('title', moodstrip.title))
        result.append(create_element('send_date', moodstrip.send_date))
        
        items = Element('items')        
        for item in moodstrip.moodstripitem_set.all():
            li = Element('li')
            li.append(create_element('url', item.get_url()))
            li.append(create_element('author_login', item.author))
            li.append(create_element('send_date', item.send_date))
            items.append(li)
        result.append(items)
                
        get_moodstrip_result.append(result)
        body.append(get_moodstrip_result)
        envelope.append(body)
        
        response = HttpResponse()    
        ElementTree(envelope).write(response)
        return response
    else:
        return HttpResponse()

