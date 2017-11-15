#! env python3

from urllib import request
from urllib import error
import json
import psycopg2
import psycopg2.extras
from datetime import datetime
import uuid
import os

HTTP_PROTOCOL = 'https'
API_URL = 'api.douban.com/v2/book/isbn/'
COVER_URL = 'covers.openlibrary.org/b/id/'

def http_protocol(url):
  return HTTP_PROTOCOL + "://" + url

def api_url(isbn):
  if isinstance(isbn,int):
    isbn = str(isbn)
  return API_URL + isbn

def fetch_book_json(isbn):
  if isinstance(isbn,int):
    isbn = str(isbn)
  url = http_protocol(api_url(isbn))
  with request.urlopen(url) as r:
    if r.status != 200:
      return None
    return json.loads(r.read())
def toBook(j):
  with request.urlopen(j['images']['large']) as r:
    if r.status != 200:
      img = None
      img_mime = None
    else:
      img = r.read()
      img_mime = r.getheader('Content-Type')
  tags_ = j.get('tags',[])
  tags = []
  for t in tags_:
    k = t.get('title',None)
    if k is None:
      continue
    tags.append(k)
  return Book(
      isbn = j.get('isbn13',None),
      lc = j.get('lc',None),
      title = j.get('title',None),
      auths = j.get('author',[]),
      publish_date = j.get('pubdate',None),
      publisher = j.get('publisher',None),
      img = img,
      img_mime = img_mime,
      img_url = j['images']['large'],
      tags = tags,
      abstract = j.get('summary',None),
      edition = j.get('edition',1),
  )

class Book:

  def __init__(self,isbn=None,title=None,lc=None,auths=[],img_mime=None, img_url=None,
      publish_date=None,publisher=None,img=None,tags=[],abstract=None,edition=None):
    self.isbn = isbn
    self.title = title
    self.lc = lc
    self.auths = auths
    self.publisher = publisher
    if not (publish_date is None):
      try:
        if isinstance(publish_date,str):
          publish_date_ = datetime.strptime(publish_date,'%Y-%M')
      except ValueError as e:
        publish_date_ = datetime.now()
    self.publish_date = publish_date_
    self.img = img
    self.img_mime = img_mime
    self.img_url = img_url
    self.tags = tags
    self.abstract = abstract
    self.edition = edition


  def update(self,conn):
    with conn.cursor() as cur:
      if self.img is None:
        uuid_ = None
      else:
        uuid_ = psycopg2.extras.UUID_adapter(uuid.uuid4())
      try:
        cur.execute("INSERT INTO table_upstream"
                    "(isbn,lc,title,auths,publisher,edition,publish_date,abstract,img_uuid,tags)"
                    "VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)",
                    (self.isbn,self.lc,self.title,self.auths,self.publisher,self.edition,self.publish_date,self.abstract,uuid_,self.tags))
      except psycopg2.InternalError as e:
        conn.rollback()
        print(e)
        print('Has this book')
        return
      if not (self.img is None):
        try:
          cur.execute("INSERT INTO table_image"
                      "(uuid,img,mime)"
                      "VALUES (%s,%s,%s)",
                      (uuid_,psycopg2.Binary(self.img),self.img_mime))
        except psycopg2.InternalError as e:
          conn.rollback()
          print(e)
          print('has such image with uuid')
      conn.commit()
    return self.isbn