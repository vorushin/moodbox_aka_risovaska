# -*- coding: utf-8 -*-
from xml.etree.ElementTree import Element, ElementTree, fromstring
from xml.parsers.expat import ExpatError
import re
import urllib2
import StringIO
from django.core.cache import cache

def make_request(request_name, params, cache_timeout = 300):
	cache_key = re.sub(r'[^\w]', '', request_name + str(params))	
	
	response = cache.get(cache_key)
	if response is not None:
		#return response.encode('utf-8')
		return response
	
	envelope = Element('envelope')	 
	header = Element('header')
	envelope.append(header)
	version_tag = Element('version_tag')
	version_tag.text = 'web_site'
	header.append(version_tag)
	language = Element('language')
	language.text = 'ru'
	header.append(language)
	
	body = Element('body')
	envelope.append(body)
	request = Element(request_name)
	body.append(request)
	
	for key, value in params.iteritems():
		elem = Element(unicode(key))
		elem.text = unicode(value)
		if value == '' and key in ['sex', 'country']:
			elem.set('isNull', 'true')
		request.append(elem)
		
	doc = ElementTree(envelope)
	xml_string = StringIO.StringIO()
	doc.write(xml_string)
		
	try:
		from settings_proxy import HTTP_PROXY_SETTINGS
		proxy_handler = urllib2.ProxyHandler({'http': HTTP_PROXY_SETTINGS})
		opener = urllib2.build_opener(proxy_handler, urllib2.HTTPHandler)
		urllib2.install_opener(opener)
	except ImportError:
		pass
		
	try:
		response = urllib2.urlopen('http://server.moodbox.com/do', xml_string.getvalue()).read()
	except urllib2.URLError:
		return ''
	finally:
		xml_string.close()
	
	cache.set(cache_key, response, cache_timeout)
	
	return response

def get_item(response, item_tag):
	try:
		root = fromstring(response)
	except ExpatError:
		return []
		
	items = root.getiterator(item_tag)
	rc = u''
	for item in items:
		if item.text is not None:
			rc += item.text
	return rc

def get_object(response, object_tag):
	objects = parse_list(response, object_tag)
	if len(objects) == 0:
		return {}
	else:
		return objects[0]
		
def items_to_list(items):
	rc = []
	for item in items:
		itemdata = {}
		subitems = item.getchildren()		 
		for subitem in subitems:
			if subitem.text is not None:
				itemdata[subitem.tag] = subitem.text
		if len(itemdata):
			rc.append(itemdata)
	return rc
		
def parse_list(response, item_tag='li', categories=()):
	try:
		root = fromstring(response)	   
	except ExpatError:
		return []
		
	if not categories:
		return items_to_list(root.getiterator(item_tag))
	else:
		rc = []
		for c in categories:
			rc.append(items_to_list(root.getiterator(c)[0].getiterator(item_tag)))
		return rc