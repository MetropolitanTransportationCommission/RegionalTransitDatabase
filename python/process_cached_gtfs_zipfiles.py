#requires a postgres db
#see setup here for mac: https://keita.blog/2016/01/09/homebrew-and-postgresql-9-5/
import pickle
import os
from credentials import APIKEY
import datetime
import requests
from credentials import AWS_KEY, AWS_SECRET
from boto.s3.key import Key
from boto.s3.connection import S3Connection
import pandas as pd
from gtfslib.dao import Dao
import subprocess
import sqlalchemy

from subprocess import STDOUT, check_output

working_dir = "/Users/tommtc/Documents/Projects/rtd2/data"
timestamp = datetime.datetime.now().strftime('%Y-%m-%d-%H-%M-%S')
date = datetime.datetime.now().strftime('%Y.%m.%d')
year = datetime.datetime.now().strftime('%Y')
source = 'mtc511cache'
dbstring = "postgresql:///tmp_gtfs"
cached_gtfs_csv = 'data/cached_gtfs_cut_2012.csv'


from functools import wraps
import errno
import os
import signal

class TimeoutError(Exception):
    pass

def timeout(seconds=2000, error_message=os.strerror(errno.ETIME)):
    def decorator(func):
        def _handle_timeout(signum, frame):
            raise TimeoutError(error_message)

        def wrapper(*args, **kwargs):
            signal.signal(signal.SIGALRM, _handle_timeout)
            signal.alarm(seconds)
            try:
                result = func(*args, **kwargs)
            finally:
                signal.alarm(0)
            return result

        return wraps(func)(wrapper)

    return decorator


def get_511_operators_dict():
	import requests
	import xmltodict
	operator_url = "http://api.511.operator/transit/operators?api_key={}&Format=XML".format(APIKEY)
	j = requests.get(operator_url)
	d = xmltodict.parse(j.content)
	return d

def get_511_gtfs_zip(private_code, apikey=APIKEY):
	request_url = 'http://api.511.operator/transit/datafeeds?api_key={}&operator_id={}'.format(apikey,private_code)	
	return requests.get(request_url, stream=True) #todo: add error handling

def get_cached_gtfs_zip(url):
	return requests.get(url, stream=True)

def get_operator_acronyms_from_511(dictionary):
	operators_list = dictionary['siri:Siri']['siri:ServiceDelivery']['DataObjectDelivery']['dataObjects']['ResourceFrame']['operatoranisations']['Operator']
	operator_acronyms = []
	for operator_acronym in operators_list:
		operator_acronyms.append(operator_acronym['PrivateCode'])
	return(operator_acronyms)

def shp_to_js(shapefile_path):
	import fiona
	import fiona.crs
	geojson_path = shapefile_path.replace('.shp','.geojson')
	with fiona.drivers():
		with fiona.open(shapefile_path) as source:
			meta = source.meta
			meta['driver'] = 'GeoJSON'
			meta['crs'] = fiona.crs.from_epsg(4326)
			with fiona.open(geojson_path, 'w', **meta) as sink:
					for f in source:
					   sink.write(f)
	return geojson_path

@timeout(2000)
def export_shapefiles(operator, operator_base_filename):
	filename1 = '{}/stops.shp'.format(operator_base_filename)
	filename2 = '{}/hops.shp'.format(operator_base_filename)
	shpexport = ['gtfsrun',dbstring,
	'ShapefileExport',
	'--feed_id={}'.format(operator),
	'--cluster=50',
	'--stopshp={}'.format(filename1),
	'--hopshp={}'.format(filename2)]
	print(subprocess.call(shpexport))
	if os.path.exists(filename1):
		filename1 = filename1
		filename1 = shp_to_js(filename1)
	else:
		filename1 = False
	if os.path.exists(filename2):
		filename2 = filename2
		filename2 = shp_to_js(filename2)
	else:
		filename2 = False
	return({'stopsfile':filename1,'hopsfile':filename2})

@timeout(2000)
def export_frequencies(operator, operator_base_filename):
	filename_freq = '{}/freq.csv'.format(operator_base_filename)
	freqexport = ['gtfsrun',dbstring,
	'Frequencies',
	'--cluster=100',
	'--csv={}'.format(filename_freq)]
	print(subprocess.call(freqexport))
	if os.path.exists(filename_freq):
		return(filename_freq)
	else:
		return("na")

def try_to_clear_db(dao, operator):
	try:
		dao.delete_feed(operator)
		return("na")
	except Exception as e:
		return(e)

@timeout(2000)
def try_to_load_db(dao,operator,operator_zip):
	try:
		print("loading {} to database".format(operator))
		dao.load_gtfs(operator_zip,feed_id=operator)
		print("loaded {} to database".format(operator))
		return("loaded {} to database".format(operator))
	except Exception as e:
		return(e)

def try_to_write_processed_files_to_s3(filedict, processing_dict):
	try:
		s3dict = {key:write_to_s3(value) if value else "na"
				for key, value 
				in filedict.items()}
		processing_dict["processed"] = 1
		processing_dict.update(s3dist)
		return(processing_dict)
	except Exception as e:
		print(e)
		return(processing_dict)

def get_cached_zipfile(operator_base_filename, url):
	operator_zip_name = '{}.zip'.format(operator_base_filename)
	if not os.path.exists(os.path.dirname(operator_zip_name)):
		os.makedirs(os.path.dirname(operator_zip_name))
	if not os.path.exists(operator_zip_name):
		r = get_cached_gtfs_zip(url)
		write_zip_to_disk(r, operator_zip_name)
	return(operator_zip_name)

def get_stops_and_frequencies(dao,operator,operator_base_filename,processing_dict):
	local_files_dict = {}
	try:
		local_files_dict = export_shapefiles(operator,operator_base_filename)
		processing_dict["frequencies_error"] = "none"
	except Exception as e:
		processing_dict["stopsfile_error"] = e
		print("error exporting stops for operator:".format(operator))
		print(e)
	try:
		local_files_dict["frequencies"] = export_frequencies(operator,operator_base_filename)
		processing_dict["frequencies_error"] = "na"
	except Exception as e:
		processing_dict["frequencies_error"] = e
		print("error exporting frequencies for operator:".format(operator))
		print(e)
	try_to_clear_db(dao,operator)
	processing_dict["local_files_dict"] = local_files_dict
	return(processing_dict)


def process_one(dao, operator, url, processing_dict, operator_base_filename, path = "."):
	operator_zip_name = get_cached_zipfile(operator_base_filename, url)
	if os.path.exists(operator_zip_name):
		processing_dict["db_clear_error"] = try_to_clear_db(dao, operator)
		processing_dict["db_load_error"] = try_to_load_db(dao,operator,operator_zip_name)
		processing_dict	= get_stops_and_frequencies(dao,operator, operator_base_filename, processing_dict)
		local_files_dict = processing_dict["local_files_dict"]
		s3dict = try_to_write_processed_files_to_s3(local_files_dict, processing_dict)
	return(processing_dict)

			
def write_zip_to_disk(r, path):
	import shutil
	if r.status_code == 200:
		with open(path, 'wb') as f:
			shutil.copyfileobj(r.raw, f)

def upload_file_from_local(filename,k):
    file_handle = open(filename, 'rb')
    s3name = "mtc_cache/" + filename.replace("/Users/tommtc/Documents/Projects/rtd2/data","")
    k.key = s3name
    print("uploading:" + s3name)
    k.set_contents_from_file(file_handle)
    k.make_public()
    return(s3name)

def write_to_s3(filename):
	print("writing to s3:" + filename)
	try:
		aws_connection = S3Connection(AWS_KEY, AWS_SECRET)
		bucket = aws_connection.get_bucket('mtc511gtfs')
		k = Key(bucket)
		s3name = upload_file_from_local(filename,k)
	except Exception as e:
		print(e)
		s3name = "na"
	return(s3name)

def update_df_log(r,d_process_log):
	d_cached = dict(r)
	d_cached.update(d_process_log)
	new_data = pd.Series(d_cached)
	r = new_data
	return(r)

d1 = {"operator":"test",
		"url":"test",
		'year': "test",
		'source': "test",
		"processed":0,
		"frequencies" : "",
		"stopsfile" : "",
		"frequencies_error" : "",
		"stopsfile_error" : "",
		"db_load_error" : "",
		"db_clear_error" : "",
		"hopsfile" : "",
		"s3pathname" : ""}

def main():
#	d = get_511_operators_dict()
#	operator_acronyms = get_operator_acronyms_from_511(d)
	df = pd.read_csv(cached_gtfs_csv)
	df = df.set_index('index')
	df_upd = df.copy()
	df = df[df.stops_processed==0]
	df["frequencies"] = ""
	df["stopsfile"] = ""
	df["hopsfile"] = ""
	operator_urls = list(df.s3pathname)
	operator_names = list(df.operator)
	dao = Dao(dbstring)
	for i,r in df.iterrows():
		operator = r['operator']
		url = r['s3pathname']
		print("fetching:" + operator)
		#create an empty dict to capture s3 uploads/processing in
		processing_dict = {"operator":operator,
				"url":url,
				'year': r['year'],
				'source': r['source'],
				"processed":0,
				"frequencies" : "",
				"stopsfile" : "",
				"frequencies_error" : "",
				"stopsfile_error" : "",
				"db_load_error" : "",
				"db_clear_error" : "",
				"hopsfile" : "",
				"s3pathname" : ""}

		operator_base_filename = '{}/{}/{}/{}/processed/{}'.format(
			working_dir,
			r['year'],
			operator,
			r['source'],
			timestamp)

		processing_dict = process_one(dao, operator, url, processing_dict, operator_base_filename)
		
		if len(processing_dict)>0:
			df_upd[i] = update_df_log(r,processing_dict)
		else:
			next
		df_upd.to_csv('data/cached_gtfs_log_test.csv')

if __name__ == "__main__":
	main()