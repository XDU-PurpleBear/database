#! env python3

from spider import *
import psycopg2
import os

isbns = os.getenv('ISBNS','').split(';')
CONN_STR = os.getenv('CONN_STR','host=localhost port=5432')

def main():
  with psycopg2.connect(CONN_STR) as c:
    print('begin to fetch')
    for isbn in isbns:
      print('fetch: ' + isbn)
      fetch_isbn_info(c,int(isbn),fetch_img=False)
    print('done')

if __name__ == '__main__':
  main()
