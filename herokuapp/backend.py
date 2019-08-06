import contextlib
import os
import sqlite3
import urllib.parse as urlparse


class SqliteBackend:
    def __init__(self, dbpath):
        self.dbpath = dbpath

    @contextlib.contextmanager
    def cursor(self):
        with sqlite3.connect(self.dbpath) as conn:
            cur = conn.cursor()
            yield cur

    def create_table_with_oids(self, name, columns):
        with self.cursor() as c:
            c.execute('CREATE TABLE %s (%s)' % (name, columns))


class PostgresBackend:
    def __init__(self, dburl):
        self.dburl = urlparse.urlparse(dburl)

    @contextlib.contextmanager
    def cursor(self):
        import psycopg2  # might not be pip-installed locally
        with psycopg2.connect(
            database=self.dburl.path[1:],  # strip the leading '/'
            user=self.dburl.username,
            password=self.dburl.password,
            host=self.dburl.hostname,
            port=self.dburl.port,
        ) as conn:
            with conn.cursor() as cur:
                yield self.make_psycopg2_cursor_behave_like_sqlite3_cursor(cur)

    def make_psycopg2_cursor_behave_like_sqlite3_cursor(self, pgcur):
        result = lambda: None

        def result_execute(q, *args):
            q = q.replace('?', '%s')
            pgcur.execute(q, *args)
            return pgcur

        result.execute = result_execute
        return result

    def create_table_with_oids(self, name, columns):
        with self.cursor() as c:
            c.execute('CREATE TABLE %s (%s) WITH OIDS' % (name, columns))

_db = None
def init():
    global _db
    if 'DATABASE_URL' in os.environ:
        _db = PostgresBackend(os.environ['DATABASE_URL'])
    else:
        _db = SqliteBackend(
            os.path.abspath(os.path.dirname(__file__)) + '/data.db'
        )


def cursor():
    global _db
    return _db.cursor()


def create_table_with_oids(name, columns):
    global _db
    return _db.create_table_with_oids(name, columns)
