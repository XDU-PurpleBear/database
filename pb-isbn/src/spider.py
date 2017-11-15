#! env python3

from urllib import request
from urllib import error
import json
import psycopg2
import psycopg2.extras
from datetime import datetime
import uuid
import os

# curl https://openlibrary.org/api/books\?bibkeys\=ISBN:9780980200447\&jscmd\=details\&format\=json
# isbn = d['identifiers']['isbn_13'][0]
# lc = d['classifications']['lc_classifications'][0]
# title = d['details']['title']
# author = d['authors'][x]['name']
# publisher = d['publishers'][0]['name']
# edition = 1
# publish_date = d['publish_date']
# abstract = d['details']['description']

HTTP_PROTOCOL = 'http'
API_URL = 'openlibrary.org/api/books'
COVER_URL = 'covers.openlibrary.org/b/id/'

class FII_Error(ValueError):
    pass


def fetch_isbn_info(conn,isbn,fetch_img=True,img_size='L'):
    if not isinstance(conn,psycopg2.extensions.connection):
        raise TypeError('bad operand type of cur')
    if isinstance(isbn,int):
        isbn = '%10d' % isbn
    url = HTTP_PROTOCOL+'://'+API_URL+'?bibkeys=ISBN:'+isbn+'&jscmd=details&format=json'
    with request.urlopen(url) as r:
        if r.status != 200:
            raise FII_Error('not 200 skip')
        j = json.loads(r.read())
    has = False
    for k in j:
        has = True
        d = j[k]['details']
        if 'isbn_13' in d:
            isbn_ = d['isbn_13'][0]
        elif 'isbn_10' in d:
            isbn_ = '978'+d['isbn_10'][0]
        elif 'notes' in d:
            dd = d['notes']
            if 'isbn_13' in dd:
                isbn_ = dd['isbn_13'][0]
            elif 'isbn_10' in dd:
                isbn_ = '978'+dd['isbn_10'][0]
        else:
            raise FII_Error('no isbn')
        isbn_ = isbn_.replace('-','')
        print(isbn_)
        lc_ = d.get('lc_classifications',['NaN'])[0]
        title_ = d['title']
        authors_ = []
        for i in d.get('authors',[]):
            authors_.append(i['name'])
        publisher_ = d.get('publishers','unknown publisher')[0]
        edition_ = 1
        publisher_date = d.get('publish_date','')
        print(publisher_date)
        abstract_ = d.get('description',None)
        if type(abstract_) is dict:
            abstract_ = abstract_.get('value','')
        img_id = d.get('covers',[None])[0]
        tags_ = d.get('subjects',[])
    if  not has:
        raise FII_Error('empty info without data')
    publisher_date_ = parseDate(publisher_date)
    img_ = None
    if (not (img_id is None)) and fetch_img:
        try:
            cover = HTTP_PROTOCOL+'://'+COVER_URL+str(img_id)+'-'+img_size+'.jpg'
            print(cover)
            with request.urlopen(cover) as r:
                img_ = r.read()
                img_mime = r.getheader('Content-Type')
        except error.URLError as e:
            print('time out for fetching the images')
            img_ = None

    with conn.cursor() as cur:
        if img_ is None:
            img_uuid = None
        else:
            img_uuid = uuid.uuid4()
        try:
            cur.execute("INSERT INTO table_upstream"
                        "(isbn,lc,title,auths,publisher,edition,publish_date,abstract,img_uuid,tags)"
                        "VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)",
                        (isbn_,lc_,title_,authors_,publisher_,edition_,publisher_date_,abstract_,ad_uuid(img_uuid),tags_))
        except psycopg2.InternalError as e:
            print('Has this book')
        if not (img_uuid is None):
            try:
                cur.execute("INSERT INTO table_image"
                            "(uuid,img,mime)"
                            "VALUES (%s,%s,%s)",
                            (psycopg2.extras.UUID_adapter(img_uuid),psycopg2.Binary(img_),img_mime))
            except psycopg2.InternalError as e:
                print('has such image with uuid')
        conn.commit()
    return isbn_


def parseDate(publisher_date):
    try:
        publisher_date_ = datetime.strptime(publisher_date,'%B %Y')
    except ValueError as e:
        try:
            publisher_date_ = datetime.strptime(publisher_date,'%B %d, %Y')
        except ValueError as e:
            try:
                publisher_date_ = datetime.strptime(publisher_date,'%Y')
            except ValueError as e:
                try:
                    publisher_date_ = datetime.strptime(publisher_date,'%M-$D-%Y')
                except ValueError as e:
                    publisher_date_ = datetime.now()
    return publisher_date_

def ad_uuid(u):
    if u is None:
        return None
    else:
        return psycopg2.extras.UUID_adapter(u)
        

def run_f_t(c,f,t,fetch_img=True,img_size='L'):
    if not (isinstance(f,int) and isinstance(t,int)):
        raise TypeError('f,t should be int')
    for i in range(f,t):
        print("fetch " + str(i))
      try:
        fetch_isbn_info(c,i,fetch_img=fetch_img,img_size=img_size)
        print('done')
      except FII_Error as e:
        print(e)
      except psycopg2.IntegrityError as e:
        print(e)


def fetch_them(c,fs,ts,fetch_img=True,img_size='L'):
  for i in range(0,len(fs)):
    pid = os.fork()
    if pid == 0:
      run_f_t(c,fs[i],ts[i],fetch_img=fetch_img,img_size=img_size)
      break