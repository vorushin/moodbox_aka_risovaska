# -*- coding: utf-8 -*-
from urllib import urlencode
import datetime
from django import template
import core.constants

register = template.Library()

@register.filter(name='to_menu_class')
def to_menu_class(value, arg):
    "Returns css-class for 'arg' menu item, providing that 'value' is selected menu item"
    if not value:
        value = 0
    try:
        selected_item = int(value)
        current_item = int(arg)
    except TypeError:
        return ""
    
    if selected_item == 0:
        return ""
    elif current_item == selected_item - 1:
        return "leftact"
    elif current_item == selected_item:
        return "act"
    elif current_item == selected_item + 1:
        return "rightact"
    
    return ""

@register.filter(name='ago')
def ago(date):
    "If 'date' is not more than 24 hours ago, converts it to form '1 min ago', '2 hours ago'"
    if not date:
        return ''
    
    diff = datetime.datetime.utcnow() - date
    
    if diff.seconds >= 0 and diff.days < 1:
        if diff.seconds < 10:
            return 'только что'
        if diff.seconds < 60:
            return str(diff.seconds) + " сек. назад"
        if diff.seconds < 60 * 60:
            return str(diff.seconds / 60) + " мин. назад"
        if diff.days < 1:
            return str(diff.seconds / (60 * 60)) + " час. назад"
    
    return date.strftime('%d.%m.%Y')
    
@register.filter(name='equals')
def equals(lvalue, rvalue):
    return lvalue == rvalue

@register.filter(name='greater_than')
def greater_than(lvalue, rvalue):
    return lvalue > rvalue

@register.filter(name='greater_or_equal')
def greater_or_equal(lvalue, rvalue):
    return lvalue >= rvalue

@register.filter(name='build_page_url')
def build_page_url(page_no, getparams):
    params = {}
    for k, v in getparams.iteritems():
        params[k] = unicode(v)
    params['p'] = unicode(page_no)
    return u'?' + urlencode(dict([k, v.encode('utf-8')] for k, v in params.items()))
    
@register.filter(name='age')
def age(birth_day):
    d1 = datetime.datetime(int(birth_day[0:4]), int(birth_day[5:7]), int(birth_day[8:10]))
    d2 = datetime.datetime.utcnow()
    
    age = d2.year - d1.year - 1
    if d2.month >= d1.month and d2.day >= d1.day:
        age += 1
        
    return age

@register.filter(name='to_country_name')
def to_country_name(country_code):
    if country_code:
        countries = core.constants.get_countries()
        for c in countries:
            if c[0].lower() == country_code.lower():
                return c[1]
    return ''

@register.filter(name='truncate')
def truncate(string, max_length):
    if len(string) > int(max_length):
        return string[:int(max_length)-3] + u"..."
    else:
        return string