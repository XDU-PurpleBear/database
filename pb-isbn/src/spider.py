#! env python3

from urllib import request
from urllib import error
import json
import psycopg2
from datetime import datetime
import uuid

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


def fetch_isbn_info(conn,isbn):
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
        print(d['isbn_13'][0])
        isbn_ = d['isbn_13'][0]
        lc_ = d.get('lc_classifications',['NaN'])[0]
        title_ = d['title']
        authors_ = []
        for i in d['authors']:
            authors_.append(i['name'])
        publisher_ = d['publishers'][0]
        edition_ = 1
        publisher_date = d['publish_date']
        print(publisher_date)
        abstract_ = d.get('description',None)
        img_id = d.get('covers',[None])[0]
        tags_ = d.get('subjects',[])
    if  not has:
        raise FII_Error('empty info without data')
    try:
        publisher_date_ = datetime.strptime(publisher_date,'%B %Y')
    except ValueError as e:
        try:
            publisher_date_ = datetime.strptime(publisher_date,'%B %d, %Y')
        except ValueError as e:
            publisher_date_ = datetime.now()

    img_ = None
    if not (img_id is None):
        try:
            cover = HTTP_PROTOCOL+'://'+COVER_URL+str(img_id)+'-M.jpg'
            print(cover)
            with request.urlopen(cover) as r:
                img_ = r.read()
                img_mime = x.getheader('Content-Type')
        except error.URLError as e:
            print('time out for fetching the images')
            img_ = None

    with conn.cursor() as cur:
        if img_ is None:
            img_uuid = None
        else:
            img_uuid = uuid.uuid4()
            try:
                cur.execute("INSERT INTO table_image"
                            "(uuid,img,mime)"
                            "VALUES (%s,%s,%s)",
                            (img_uuid,img_,img_mime))
            except psycopg2.InternalError as e:
                print('has such image with uuid')
        try:
            cur.execute("INSERT INTO table_upstream"
                        "(isbn,lc,title,auths,publisher,edition,publish_date,abstract,img_uuid,tags)"
                        "VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)",
                        (isbn_,lc_,title_,authors_,publisher_,edition_,publisher_date_,abstract_,img_uuid,tags_))
        except psycopg2.InternalError as e:
            print('Has this book')
        conn.commit()
    return isbn_

